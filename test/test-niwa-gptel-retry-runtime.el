;;; test-niwa-gptel-retry-runtime.el --- Retained retry settlement tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise real curl completion, agent callbacks, runtime settlement and agent
;; terminal FSM handlers together.  Conversation saves use temporary files.

;;; Code:

(require 'gptel-retry-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "gptel-retry-helpers"))
(require 'niwa-agent-runtime)
(require 'niwa-agent-conversation)
(require 'niwa-session-persistence)
(require 'niwa-hooks)
(require 'niwa-tools)
(require 'niwa-reminders)
(require 'niwa-session-test-support
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "niwa-session-test-support"))

(defmacro niwa-retry-runtime-test--with-agent (responses &rest body)
  "Run BODY with RESPONSES and a real retained agent's terminal lifecycle.
Only WAIT preparation uses the native HTTP fixture.  DONE, ERRS and ABRT use
the production agent handlers and the provider callback settles the real
runtime.  BEFORE-RESULT can interrupt inside an accepted terminal callback."
  (declare (indent 1) (debug t))
  `(niwa-retry-test--with-http ,responses
     (let* ((directory (make-temp-file "niwa-retry-runtime-" t))
            (session (test-niwa-session-persistence--leased-session directory))
            (parent-buffer (generate-new-buffer " *retry-runtime-parent*"))
            (relative "agents/retry-runtime.chat.org")
            (transcript (expand-file-name relative directory))
            (invocation
             (niwa-agent-invocation--create
              :agent-id "retry-runtime" :path "/root/retry_runtime"
              :description "Runtime retry regression"
              :agent (niwa-agent--create :name "explorer")
              :buffer data-buf :parent-session session :parent-data-buffer parent-buffer
              :transcript-status 'running :transcript-relative-path relative
              :runtime-fsm fsm))
            (partial (list "Agent prefix: "))
            (results nil) (terminal-events nil) (stop-statuses nil)
            (settlement-contexts nil) (post-responses 0)
            (before-result nil) (saved-info nil) (saved-callback nil)
            (niwa-agent-conversation-save-debounce nil)
            (gptel-post-response-functions
             (list (lambda (_start _end) (cl-incf post-responses))))
            (gptel-post-request-hook
             (list (lambda ()
                     (setq saved-info (gptel-fsm-info fsm)
                           saved-callback (plist-get saved-info :callback))))))
       (unwind-protect
           (progn
             (make-directory (file-name-directory transcript) t)
             (with-temp-file transcript (insert ""))
             (setf (niwa-session-save-path session) directory
                   (niwa-request-session request) session
                   (niwa-session-root-buffer session) parent-buffer
                   (niwa-session-agent-transcripts session)
                   (list (cons "retry-runtime" (list :status 'running)))
                   (niwa-agent-invocation-runtime-settle-callback invocation)
                   (lambda (_invocation visible event &optional _token)
                     (push (list :visible visible :event event) results)
                     nil))
             (with-current-buffer parent-buffer
               (org-mode)
               (setq-local niwa--session session)
               (setq-local niwa-subagent-stop-functions
                           (list (lambda (event)
                                   (push (plist-get event :status) stop-statuses)))))
             (niwa-session-artifacts-save session parent-buffer)
             (with-current-buffer data-buf
               (setq-local niwa--session session)
               (setq-local buffer-file-name transcript)
               (setq-local niwa--agent-invocation invocation)
               (set-visited-file-modtime))
             (let ((callback
                    (niwa-agent-exec--make-callback
                     (lambda (response)
                       (let ((state (plist-get (gptel-fsm-info fsm)
                                               :niwa-retry-state)))
                         (push (list :fsm-state (gptel-fsm-state fsm)
                                     :completing (plist-get state :completing)
                                     :terminal-callback (plist-get state :terminal-callback))
                               settlement-contexts))
                       (when before-result (funcall before-result))
                       (niwa-agent-runtime--handle-provider-result invocation response))
                     "explorer" "Runtime retry regression"
                     (plist-get (gptel-fsm-info fsm) :position) partial)))
               (plist-put (gptel-fsm-info fsm) :niwa-agent-invocation invocation)
               (plist-put (gptel-fsm-info fsm) :niwa-agent-terminal-callback callback)
               (plist-put (gptel-fsm-info fsm) :callback
                          (niwa-agent-exec--wrap-callback callback 'unavailable)))
             (dolist (terminal '(DONE ERRS ABRT))
               (let ((state terminal))
                 (setf (alist-get state (gptel-fsm-handlers fsm))
                       (cons (lambda (_machine) (push state terminal-events))
                             (copy-sequence
                              (alist-get state niwa-agent-exec--handlers))))))
             ,@body)
         (niwa-agent-conversation--cancel-save invocation)
         (when (buffer-live-p data-buf)
           (with-current-buffer data-buf (set-buffer-modified-p nil))
           (kill-buffer data-buf))
         (kill-buffer parent-buffer)
         (niwa-session-persistence-lock-release directory session)
         (niwa-workspace-clear-registry)
         (delete-directory directory t)))))

(ert-deftest niwa-gptel-retry-runtime/success-keeps-native-done ()
  (niwa-retry-runtime-test--with-agent
      (list niwa-retry-test--failure niwa-retry-test--success)
    (gptel--fsm-transition fsm)
    (niwa-retry-test--drain-native)
    (should (= 2 (length requests)))
    (should (= 1 waits))
    (should (equal '(DONE) terminal-events))
    (should (eq 'DONE (gptel-fsm-state fsm)))
    (should (equal '(completed) stop-statuses))
    (should (= 1 post-responses))
    (should (equal '((:fsm-state TYPE :completing t :terminal-callback t))
                   settlement-contexts))
    (should (niwa-agent-invocation-runtime-settled-p invocation))
    (should (eq 'completed (niwa-agent-invocation-transcript-status invocation)))
    (should (= 1 (length results)))
    (should (equal "Recovered answer" (plist-get (car results) :visible)))
    (should (equal "Agent prefix: Recovered answer" (car partial)))
    (should-not (buffer-local-value 'niwa--current-request data-buf))
    (should-not gptel--request-alist)
    (should-not niwa-gptel-retry--states)
    (with-temp-buffer
      (insert (decode-coding-string
               (niwa-session-artifacts-read-artifact session relative t) 'utf-8-unix))
      (should (string-search "Recovered answer" (buffer-string)))
      (should-not (string-search "Abandoned output" (buffer-string))))
    (funcall saved-callback "Late abandoned text" saved-info)
    (funcall saved-callback t saved-info)
    (should (= 1 (length results)))
    (should (equal '(DONE) terminal-events))
    (should-not (string-search "Late abandoned text"
                               (with-current-buffer data-buf (buffer-string))))))

(ert-deftest niwa-gptel-retry-runtime/http-503-error-page-recovers ()
  "A non-JSON 503 page is retried before the agent turn can settle."
  (let ((page "<html><body>503 Service Unavailable</body></html>"))
    (niwa-retry-runtime-test--with-agent
        (list (format "HTTP/2 503\r\nContent-Type: text/html\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                      (string-bytes page) page)
              niwa-retry-test--success)
      (gptel--fsm-transition fsm)
      (niwa-retry-test--drain-native)
      (should (= 2 (length requests)))
      (should (equal '(DONE) terminal-events))
      (should (equal '(completed) stop-statuses))
      (should (equal "Recovered answer" (plist-get (car results) :visible))))))

(ert-deftest niwa-gptel-retry-runtime/agent-dispatch-recovers-http-503 ()
  "The real agent dispatch retries without a synthetic root request."
  (let ((page "<html><body>503 Service Unavailable</body></html>"))
    (niwa-retry-runtime-test--with-agent
        (list (format "HTTP/2 503\r\nContent-Type: text/html\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                      (string-bytes page) page)
              niwa-retry-test--success)
      (let ((gptel--known-backends (list (cons "Codex test" backend)))
            (gptel--openai-oauth-token-file (file-name-concat directory "token.el"))
            (gptel--fsm-last nil)
            (gptel-stream t)
            (gptel-use-tools nil)
            (gptel-system-prompt "Answer briefly."))
        (with-temp-file gptel--openai-oauth-token-file
          (prin1 (list :access_token "synthetic-local-only"
                       :expires_at (+ (float-time) 3600)) (current-buffer)))
        (setf (niwa-agent-invocation-frozen-configuration invocation)
              (niwa-agent-configuration--create
               :agent (niwa-agent-invocation-agent invocation)
               :request-locals
               (niwa-agent-exec-request-snapshot
                (list :backend backend :model 'test-model :effort nil))))
        (with-current-buffer data-buf
          ;; Production retained agents own their invocation, not a root request.
          (setq-local niwa--current-request nil)
          (insert "Hello"))
        (niwa-test--with-captured-diagnostics nil
          (setq fsm (niwa-agent-exec-run
                     (lambda (response)
                       (niwa-agent-runtime--handle-provider-result invocation response))
                     "explorer" "Runtime retry regression" invocation data-buf))
          (niwa-retry-test--drain-native t)
          (should-not results)
          (let ((state (plist-get (gptel-fsm-info fsm) :niwa-retry-state)))
            (should state)
            (should-not (plist-get state :request))
            (should (niwa-gptel-retry--live-p state))
            (unwind-protect
                (progn
                  (setf (niwa-agent-invocation-runtime-fsm invocation) (gptel-make-fsm))
                  (should-not (niwa-gptel-retry--live-p state)))
              (setf (niwa-agent-invocation-runtime-fsm invocation) fsm)))
          (niwa-retry-test--drain-native))
        (should (= 2 (length requests)))
        (should (eq 'DONE (gptel-fsm-state fsm)))
        (should (equal '(completed) stop-statuses))
        (should (= 1 (length results)))
        (should (equal "Recovered answer" (plist-get (car results) :visible)))
        (should-not (buffer-local-value 'niwa--current-request data-buf))
        (should-not gptel--request-alist)
        (should-not niwa-gptel-retry--states)))))

(ert-deftest niwa-gptel-retry-runtime/nonstreaming-success-keeps-native-done ()
  "A native final string must settle the retained request without aborting it."
  (niwa-retry-runtime-test--with-agent
      (list (concat "{\"status\":\"completed\",\"error\":null,"
                    "\"output\":[{\"type\":\"message\",\"content\":["
                    "{\"type\":\"output_text\",\"text\":\"Recovered answer\"}]}],"
                    "\"usage\":{\"input_tokens\":12,\"output_tokens\":3}}"))
    (plist-put (gptel-fsm-info fsm) :stream nil)
    (plist-put (plist-get (gptel-fsm-info fsm) :data) :stream :json-false)
    (gptel--fsm-transition fsm)
    (niwa-retry-test--drain-native)
    (should (= 1 (length requests)))
    (should (equal '(DONE) terminal-events))
    (should (eq 'DONE (gptel-fsm-state fsm)))
    (should (equal '(completed) stop-statuses))
    (should (= 1 post-responses))
    (should (equal '((:fsm-state TYPE :completing t :terminal-callback t))
                   settlement-contexts))
    (should (niwa-agent-invocation-runtime-settled-p invocation))
    (should (= 1 (length results)))
    (should (equal "Recovered answer" (plist-get (car results) :visible)))
    (should-not gptel--request-alist)
    (should-not niwa-gptel-retry--states)))

(ert-deftest niwa-gptel-retry-runtime/nonstreaming-retry-accounts-for-attempts ()
  "A final string totals failed-attempt usage without aborting settlement."
  (niwa-retry-runtime-test--with-agent
      (list (concat "{\"status\":\"failed\",\"error\":{\"code\":\"server_error\"},"
                    "\"output\":[{\"type\":\"message\",\"content\":["
                    "{\"type\":\"output_text\",\"text\":\"Abandoned output\"}]}],"
                    "\"usage\":{\"input_tokens\":7,\"output_tokens\":2}}")
            (concat "{\"status\":\"completed\",\"error\":null,"
                    "\"output\":[{\"type\":\"message\",\"content\":["
                    "{\"type\":\"output_text\",\"text\":\"Recovered answer\"}]}],"
                    "\"usage\":{\"input_tokens\":12,\"output_tokens\":3}}"))
    (plist-put (gptel-fsm-info fsm) :stream nil)
    (plist-put (plist-get (gptel-fsm-info fsm) :data) :stream :json-false)
    (gptel--fsm-transition fsm)
    (niwa-retry-test--drain-native)
    (should (= 2 (length requests)))
    (should (= 1 waits))
    (should (equal '(DONE) terminal-events))
    (should (equal '(completed) stop-statuses))
    (should (= 1 (length results)))
    (should (equal "Recovered answer" (plist-get (car results) :visible)))
    (should (equal "Agent prefix: Recovered answer" (car partial)))
    (should (equal '(:input 19 :output 5 :cached 0)
                   (plist-get (gptel-fsm-info fsm) :tokens)))
    (should (equal '(:input 19 :output 5 :cached 0)
                   (plist-get (gptel-fsm-info fsm) :tokens-full)))
    (should-not gptel--request-alist)
    (should-not niwa-gptel-retry--states)))

(ert-deftest niwa-gptel-retry-runtime/exhaustion-keeps-native-error ()
  (niwa-retry-runtime-test--with-agent (make-list 11 niwa-retry-test--failure)
    (let (messages)
      (niwa-test--with-captured-messages messages
        (gptel--fsm-transition fsm)
        (niwa-retry-test--drain-native))
      (should (string-match-p "Codex test error:.*Model at capacity" messages)))
    (should (= 11 (length requests)))
    (should (= 1 waits))
    (should (equal '(ERRS) terminal-events))
    (should (eq 'ERRS (gptel-fsm-state fsm)))
    (should (equal '(error) stop-statuses))
    (should (= 1 post-responses))
    (should (equal '((:fsm-state TYPE :completing t :terminal-callback t))
                   settlement-contexts))
    (should (niwa-agent-invocation-runtime-settled-p invocation))
    (should (eq 'error (niwa-agent-invocation-transcript-status invocation)))
    (should (= 1 (length results)))
    (should (eq 'error
                (plist-get (plist-get (car results) :event)
                           :niwa-agent-terminal-status)))
    (should (string-search "Model at capacity" (plist-get (car results) :visible)))
    (should (plist-get (gptel-fsm-info fsm) :niwa-retry-exhausted))
    (should (= 11 (length (plist-get (gptel-fsm-info fsm) :niwa-retry-errors))))
    (should-not (buffer-local-value 'niwa--current-request data-buf))
    (should-not gptel--request-alist)
    (should-not niwa-gptel-retry--states)
    (funcall saved-callback t saved-info)
    (funcall saved-callback nil saved-info)
    (should (= 1 (length results)))
    (should (equal '(ERRS) terminal-events))))

(ert-deftest niwa-gptel-retry-runtime/interrupt-preempts-terminal-callback ()
  (niwa-retry-runtime-test--with-agent (list niwa-retry-test--success)
    (setq before-result
          (lambda ()
            (should (plist-get (plist-get (gptel-fsm-info fsm) :niwa-retry-state)
                               :terminal-callback))
            (niwa-agent-runtime-interrupt invocation "Interrupted during completion")))
    (gptel--fsm-transition fsm)
    (niwa-retry-test--drain-native)
    (should (= 1 (length requests)))
    (should (equal '(ABRT) terminal-events))
    (should (eq 'ABRT (gptel-fsm-state fsm)))
    (should (equal '(aborted) stop-statuses))
    (should (= 1 post-responses))
    (should (niwa-agent-invocation-runtime-settled-p invocation))
    (should (eq 'aborted (niwa-agent-invocation-transcript-status invocation)))
    (should (= 1 (length results)))
    (should (eq 'aborted
                (plist-get (plist-get (car results) :event)
                           :niwa-agent-terminal-status)))
    (should (string-search "Interrupted during completion"
                           (plist-get (car results) :visible)))
    (should-not (buffer-local-value 'niwa--current-request data-buf))
    (should-not gptel--request-alist)
    (should-not niwa-gptel-retry--states)
    (funcall saved-callback "Late success" saved-info)
    (funcall saved-callback t saved-info)
    (niwa-agent-runtime-interrupt invocation "Repeated interruption")
    (gptel--fsm-transition fsm)
    (should (= 1 (length results)))
    (should (equal '(ABRT) terminal-events))
    (should (eq 'ABRT (gptel-fsm-state fsm)))))

(defmacro niwa-retry-runtime-test--with-active-transport (&rest body)
  "Evaluate BODY while curl is streaming but has not received its terminal event."
  (declare (indent 0) (debug t))
  `(niwa-retry-test--with-http
       (list (list "event: response.output_text.delta\ndata: {\"delta\":\"Still streaming\"}\n\n"
                   niwa-retry-test--success))
     (gptel--fsm-transition fsm)
     (let ((deadline (+ (float-time) 10)))
       (while (and (< (float-time) deadline)
                   (not (plist-get (gptel-fsm-info fsm) :http-status)))
         (accept-process-output nil 0.001))
       (should (< (float-time) deadline)))
     (should (eq 'TYPE (gptel-fsm-state fsm)))
     (should (= 1 (length gptel--request-alist)))
     (let* ((process (caar gptel--request-alist))
            (transport-buffer (process-buffer process))
            (info (gptel-fsm-info fsm))
            (callback (plist-get info :callback)))
       (should (process-live-p process))
       ,@body
       (should-not (process-live-p process))
       (should-not (buffer-live-p transport-buffer))
       (should-not gptel--request-alist)
       (should-not niwa-gptel-retry--states)
       (should (eq 'ABRT (gptel-fsm-state fsm)))
       (should (= 1 aborted))
       (funcall callback "Late physical chunk" info)
       (funcall callback t info)
       (should (= 1 aborted))
       (should (= 0 finished))
       (should (= 0 failed))
       (should (= 1 (length requests))))))

(ert-deftest niwa-gptel-retry-runtime/physical-buffer-death ()
  (niwa-retry-runtime-test--with-active-transport
    (kill-buffer data-buf)))

(ert-deftest niwa-gptel-retry-runtime/deadline-during-backoff ()
  (niwa-retry-runtime-test--with-agent (list niwa-retry-test--failure)
    (with-current-buffer data-buf (setq-local niwa--current-request nil))
    (gptel--fsm-transition fsm)
    (niwa-retry-test--drain-native t)
    (should (eq 'MRETRY (gptel-fsm-state fsm)))
    (should scheduled)
    (should-not gptel--request-alist)
    (let ((entry (pop scheduled)))
      (niwa-agent-runtime--budget-expired invocation 5)
      (apply (nth 1 entry) (nth 2 entry)))
    (should (= 1 (length requests)))
    (should (equal '(ABRT) terminal-events))
    (should (equal '(aborted) stop-statuses))
    (should (= 1 (length results)))
    (should (string-search "budget expired" (plist-get (car results) :visible)))
    (should-not niwa-gptel-retry--states)))

(ert-deftest niwa-gptel-retry-runtime/zero-after-tool-still-interrupts ()
  (let ((niwa-gptel-retry-max-retries 10)
        (tool-response
         (concat "event: response.output_item.done\n"
                 "data: {\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"DisableRetries\",\"arguments\":\"{}\"}}\n\n"
                 "event: response.completed\ndata: {\"response\":{\"status\":\"completed\"}}\n\n")))
    (niwa-retry-runtime-test--with-agent
        (list tool-response
              (list "event: response.output_text.delta\ndata: {\"delta\":\"Streaming\"}\n\n"
                    niwa-retry-test--success))
      (plist-put (gptel-fsm-info fsm) :tools
                 (list (gptel-make-tool :name "DisableRetries" :description "Disable for next sample"
                                        :args nil :function (lambda () (setq niwa-gptel-retry-max-retries 0) "Done"))))
      (gptel--fsm-transition fsm)
      (let ((deadline (+ (float-time) 10)))
        (while (and (< (float-time) deadline)
                    (not (and (= 2 waits) gptel--request-alist
                              (not (plist-get (gptel-fsm-info fsm) :niwa-retry-state)))))
          (accept-process-output nil 0.001))
        (should (< (float-time) deadline)))
      (let ((process (caar gptel--request-alist)))
        (niwa-agent-runtime-interrupt invocation "Interrupted after disabling retries")
        (should-not (process-live-p process)))
      (should-not gptel--request-alist)
      (should (equal '(ABRT) terminal-events))
      (should (= 1 (length results)))
      (funcall saved-callback t saved-info)
      (should (= 1 (length results))))))

(ert-deftest niwa-gptel-retry-runtime/uninstall-active-transport ()
  (niwa-retry-runtime-test--with-active-transport
    (niwa-gptel-retry-uninstall)))

(ert-deftest niwa-gptel-retry-runtime/interrupt-with-pending-async-tool ()
  "A tool finishing after interruption advances the machine without sampling.
This holds with and without bounded recovery: a settled invocation spends no
further provider request and settles once."
  (dolist (limit '(0 10))
    (ert-info ((format "retry limit %s" limit))
      (let ((niwa-gptel-retry-max-retries limit)
            (tool-callback nil)
            (tool-response
             (concat "event: response.output_item.done\n"
                     "data: {\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"Deferred\",\"arguments\":\"{}\"}}\n\n"
                     "event: response.completed\ndata: {\"response\":{\"status\":\"completed\"}}\n\n")))
        (niwa-retry-runtime-test--with-agent (list tool-response niwa-retry-test--success)
          (plist-put (gptel-fsm-info fsm) :tools
                     (list (gptel-make-tool :name "Deferred" :description "Pending when interrupted"
                                            :args nil :async t
                                            :function (lambda (callback) (setq tool-callback callback)))))
          (gptel--fsm-transition fsm)
          (let ((deadline (+ (float-time) 10)))
            (while (and (< (float-time) deadline)
                        (or (not tool-callback) gptel--request-alist))
              (accept-process-output nil 0.01))
            (should (< (float-time) deadline)))
          (should (eq 'TOOL (gptel-fsm-state fsm)))
          (niwa-agent-runtime-interrupt invocation "Interrupted with a pending tool")
          (should (= 1 (length results)))
          (funcall tool-callback "Late tool output")
          (should (eq 'ABRT (gptel-fsm-state fsm)))
          (should (= 1 (length requests)))
          (should (= 1 (length results)))
          (should-not gptel--request-alist)
          (should-not niwa-gptel-retry--states))))))

(provide 'test-niwa-gptel-retry-runtime)
;;; test-niwa-gptel-retry-runtime.el ends here
