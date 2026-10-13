;;; test-mevedel-gptel-retry-runtime.el --- Retained retry settlement tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise real curl completion, agent callbacks, runtime settlement and agent
;; terminal FSM handlers together.  Conversation saves use temporary files.

;;; Code:

(require 'gptel-retry-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "gptel-retry-helpers"))
(require 'mevedel-agent-runtime)
(require 'mevedel-agent-conversation)
(require 'mevedel-session-persistence)
(require 'mevedel-hooks)
(require 'mevedel-tools)
(require 'mevedel-reminders)

(defmacro mevedel-retry-runtime-test--with-agent (responses &rest body)
  "Run BODY with RESPONSES and a real retained agent's terminal lifecycle.
Only WAIT preparation uses the native HTTP fixture.  DONE, ERRS and ABRT use
the production agent handlers and the provider callback settles the real
runtime.  BEFORE-RESULT can interrupt inside an accepted terminal callback."
  (declare (indent 1) (debug t))
  `(mevedel-retry-test--with-http ,responses
     (let* ((directory (make-temp-file "mevedel-retry-runtime-" t))
            (parent-buffer (generate-new-buffer " *retry-runtime-parent*"))
            (relative "agents/retry-runtime.chat.org")
            (transcript (expand-file-name relative directory))
            (invocation
             (mevedel-agent-invocation--create
              :agent-id "retry-runtime" :path "/root/retry_runtime"
              :description "Runtime retry regression"
              :agent (mevedel-agent--create :name "explorer")
              :buffer data-buf :parent-session session :parent-data-buffer parent-buffer
              :transcript-status 'running :transcript-relative-path relative
              :runtime-fsm fsm))
            (partial (list "Agent prefix: "))
            (results nil) (terminal-events nil) (stop-statuses nil)
            (settlement-contexts nil) (post-responses 0)
            (before-result nil) (saved-info nil) (saved-callback nil)
            (mevedel-agent-conversation-save-debounce nil)
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
             (setf (mevedel-session-save-path session) directory
                   (mevedel-session-authority-mode session) 'pid-lock
                   (mevedel-session-root-buffer session) parent-buffer
                   (mevedel-session-agent-transcripts session)
                   (list (cons "retry-runtime" (list :status 'running)))
                   (mevedel-agent-invocation-runtime-settle-callback invocation)
                   (lambda (_invocation visible event)
                     (push (list :visible visible :event event) results)
                     nil))
             (with-current-buffer parent-buffer
               (setq-local mevedel--session session)
               (setq-local mevedel-subagent-stop-functions
                           (list (lambda (event)
                                   (push (plist-get event :status) stop-statuses)))))
             (with-current-buffer data-buf
               (setq-local buffer-file-name transcript)
               (setq-local mevedel--agent-invocation invocation)
               (set-visited-file-modtime))
             (let ((callback
                    (mevedel-agent-exec--make-callback
                     (lambda (response)
                       (let ((state (plist-get (gptel-fsm-info fsm)
                                               :mevedel-retry-state)))
                         (push (list :fsm-state (gptel-fsm-state fsm)
                                     :completing (plist-get state :completing)
                                     :terminal-callback (plist-get state :terminal-callback))
                               settlement-contexts))
                       (when before-result (funcall before-result))
                       (mevedel-agent-runtime--handle-provider-result invocation response))
                     "explorer" "Runtime retry regression"
                     (plist-get (gptel-fsm-info fsm) :position) partial)))
               (plist-put (gptel-fsm-info fsm) :mevedel-agent-invocation invocation)
               (plist-put (gptel-fsm-info fsm) :mevedel-agent-terminal-callback callback)
               (plist-put (gptel-fsm-info fsm) :callback
                          (mevedel-agent-exec--wrap-callback callback)))
             (dolist (terminal '(DONE ERRS ABRT))
               (let ((state terminal))
                 (setf (alist-get state (gptel-fsm-handlers fsm))
                       (cons (lambda (_machine) (push state terminal-events))
                             (copy-sequence
                              (alist-get state mevedel-agent-exec--handlers))))))
             ,@body)
         (mevedel-agent-conversation--cancel-save invocation)
         (when (buffer-live-p data-buf)
           (with-current-buffer data-buf (set-buffer-modified-p nil))
           (kill-buffer data-buf))
         (kill-buffer parent-buffer)
         (delete-directory directory t)))))

(ert-deftest mevedel-gptel-retry-runtime/success-keeps-native-done ()
  (mevedel-retry-runtime-test--with-agent
      (list mevedel-retry-test--failure mevedel-retry-test--success)
    (gptel--fsm-transition fsm)
    (mevedel-retry-test--drain-native)
    (should (= 2 (length requests)))
    (should (= 1 waits))
    (should (equal '(DONE) terminal-events))
    (should (eq 'DONE (gptel-fsm-state fsm)))
    (should (equal '(completed) stop-statuses))
    (should (= 1 post-responses))
    (should (equal '((:fsm-state TYPE :completing t :terminal-callback t))
                   settlement-contexts))
    (should (mevedel-agent-invocation-runtime-settled-p invocation))
    (should (eq 'completed (mevedel-agent-invocation-transcript-status invocation)))
    (should (= 1 (length results)))
    (should (equal "Recovered answer" (plist-get (car results) :visible)))
    (should (equal "Agent prefix: Recovered answer" (car partial)))
    (should-not (buffer-local-value 'mevedel--current-request data-buf))
    (should-not gptel--request-alist)
    (should-not mevedel-gptel-retry--states)
    (with-temp-buffer
      (insert-file-contents transcript)
      (should (string-search "Recovered answer" (buffer-string)))
      (should-not (string-search "Abandoned output" (buffer-string))))
    (funcall saved-callback "Late abandoned text" saved-info)
    (funcall saved-callback t saved-info)
    (should (= 1 (length results)))
    (should (equal '(DONE) terminal-events))
    (should-not (string-search "Late abandoned text"
                               (with-current-buffer data-buf (buffer-string))))))

(ert-deftest mevedel-gptel-retry-runtime/nonstreaming-success-keeps-native-done ()
  "A native final string must settle the retained request without aborting it."
  (mevedel-retry-runtime-test--with-agent
      (list (concat "{\"status\":\"completed\",\"error\":null,"
                    "\"output\":[{\"type\":\"message\",\"content\":["
                    "{\"type\":\"output_text\",\"text\":\"Recovered answer\"}]}],"
                    "\"usage\":{\"input_tokens\":12,\"output_tokens\":3}}"))
    (plist-put (gptel-fsm-info fsm) :stream nil)
    (plist-put (plist-get (gptel-fsm-info fsm) :data) :stream :json-false)
    (gptel--fsm-transition fsm)
    (mevedel-retry-test--drain-native)
    (should (= 1 (length requests)))
    (should (equal '(DONE) terminal-events))
    (should (eq 'DONE (gptel-fsm-state fsm)))
    (should (equal '(completed) stop-statuses))
    (should (= 1 post-responses))
    (should (equal '((:fsm-state TYPE :completing t :terminal-callback t))
                   settlement-contexts))
    (should (mevedel-agent-invocation-runtime-settled-p invocation))
    (should (= 1 (length results)))
    (should (equal "Recovered answer" (plist-get (car results) :visible)))
    (should-not gptel--request-alist)
    (should-not mevedel-gptel-retry--states)))

(ert-deftest mevedel-gptel-retry-runtime/nonstreaming-retry-accounts-for-attempts ()
  "A final string totals failed-attempt usage without aborting settlement."
  (mevedel-retry-runtime-test--with-agent
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
    (mevedel-retry-test--drain-native)
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
    (should-not mevedel-gptel-retry--states)))

(ert-deftest mevedel-gptel-retry-runtime/exhaustion-keeps-native-error ()
  (mevedel-retry-runtime-test--with-agent (make-list 11 mevedel-retry-test--failure)
    (let (messages)
      (mevedel-test--with-captured-messages messages
        (gptel--fsm-transition fsm)
        (mevedel-retry-test--drain-native))
      (should (string-match-p "Codex test error:.*Model at capacity" messages)))
    (should (= 11 (length requests)))
    (should (= 1 waits))
    (should (equal '(ERRS) terminal-events))
    (should (eq 'ERRS (gptel-fsm-state fsm)))
    (should (equal '(error) stop-statuses))
    (should (= 1 post-responses))
    (should (equal '((:fsm-state TYPE :completing t :terminal-callback t))
                   settlement-contexts))
    (should (mevedel-agent-invocation-runtime-settled-p invocation))
    (should (eq 'error (mevedel-agent-invocation-transcript-status invocation)))
    (should (= 1 (length results)))
    (should (eq 'error
                (plist-get (plist-get (car results) :event)
                           :mevedel-agent-terminal-status)))
    (should (string-search "Model at capacity" (plist-get (car results) :visible)))
    (should (plist-get (gptel-fsm-info fsm) :mevedel-retry-exhausted))
    (should (= 11 (length (plist-get (gptel-fsm-info fsm) :mevedel-retry-errors))))
    (should-not (buffer-local-value 'mevedel--current-request data-buf))
    (should-not gptel--request-alist)
    (should-not mevedel-gptel-retry--states)
    (funcall saved-callback t saved-info)
    (funcall saved-callback nil saved-info)
    (should (= 1 (length results)))
    (should (equal '(ERRS) terminal-events))))

(ert-deftest mevedel-gptel-retry-runtime/interrupt-preempts-terminal-callback ()
  (mevedel-retry-runtime-test--with-agent (list mevedel-retry-test--success)
    (setq before-result
          (lambda ()
            (should (plist-get (plist-get (gptel-fsm-info fsm) :mevedel-retry-state)
                               :terminal-callback))
            (mevedel-agent-runtime-interrupt invocation "Interrupted during completion")))
    (gptel--fsm-transition fsm)
    (mevedel-retry-test--drain-native)
    (should (= 1 (length requests)))
    (should (equal '(ABRT) terminal-events))
    (should (eq 'ABRT (gptel-fsm-state fsm)))
    (should (equal '(aborted) stop-statuses))
    (should (= 1 post-responses))
    (should (mevedel-agent-invocation-runtime-settled-p invocation))
    (should (eq 'aborted (mevedel-agent-invocation-transcript-status invocation)))
    (should (= 1 (length results)))
    (should (eq 'aborted
                (plist-get (plist-get (car results) :event)
                           :mevedel-agent-terminal-status)))
    (should (string-search "Interrupted during completion"
                           (plist-get (car results) :visible)))
    (should-not (buffer-local-value 'mevedel--current-request data-buf))
    (should-not gptel--request-alist)
    (should-not mevedel-gptel-retry--states)
    (funcall saved-callback "Late success" saved-info)
    (funcall saved-callback t saved-info)
    (mevedel-agent-runtime-interrupt invocation "Repeated interruption")
    (gptel--fsm-transition fsm)
    (should (= 1 (length results)))
    (should (equal '(ABRT) terminal-events))
    (should (eq 'ABRT (gptel-fsm-state fsm)))))

(defmacro mevedel-retry-runtime-test--with-active-transport (&rest body)
  "Evaluate BODY while curl is streaming but has not received its terminal event."
  (declare (indent 0) (debug t))
  `(mevedel-retry-test--with-http
       (list (list "event: response.output_text.delta\ndata: {\"delta\":\"Still streaming\"}\n\n"
                   mevedel-retry-test--success))
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
       (should-not mevedel-gptel-retry--states)
       (should (eq 'ABRT (gptel-fsm-state fsm)))
       (should (= 1 aborted))
       (funcall callback "Late physical chunk" info)
       (funcall callback t info)
       (should (= 1 aborted))
       (should (= 0 finished))
       (should (= 0 failed))
       (should (= 1 (length requests))))))

(ert-deftest mevedel-gptel-retry-runtime/physical-buffer-death ()
  (mevedel-retry-runtime-test--with-active-transport
    (kill-buffer data-buf)))

(ert-deftest mevedel-gptel-retry-runtime/deadline-during-backoff ()
  (mevedel-retry-runtime-test--with-agent (list mevedel-retry-test--failure)
    (gptel--fsm-transition fsm)
    (mevedel-retry-test--drain-native t)
    (should-not gptel--request-alist)
    (let ((entry (pop scheduled)))
      (mevedel-agent-runtime--budget-expired invocation 5)
      (apply (nth 1 entry) (nth 2 entry)))
    (should (= 1 (length requests)))
    (should (equal '(ABRT) terminal-events))
    (should (equal '(aborted) stop-statuses))
    (should (= 1 (length results)))
    (should (string-search "budget expired" (plist-get (car results) :visible)))
    (should-not mevedel-gptel-retry--states)))

(ert-deftest mevedel-gptel-retry-runtime/zero-after-tool-still-interrupts ()
  (let ((mevedel-gptel-retry-max-retries 10)
        (tool-response
         (concat "event: response.output_item.done\n"
                 "data: {\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"DisableRetries\",\"arguments\":\"{}\"}}\n\n"
                 "event: response.completed\ndata: {\"response\":{\"status\":\"completed\"}}\n\n")))
    (mevedel-retry-runtime-test--with-agent
        (list tool-response
              (list "event: response.output_text.delta\ndata: {\"delta\":\"Streaming\"}\n\n"
                    mevedel-retry-test--success))
      (plist-put (gptel-fsm-info fsm) :tools
                 (list (gptel-make-tool :name "DisableRetries" :description "Disable for next sample"
                                        :args nil :function (lambda () (setq mevedel-gptel-retry-max-retries 0) "Done"))))
      (gptel--fsm-transition fsm)
      (let ((deadline (+ (float-time) 10)))
        (while (and (< (float-time) deadline)
                    (not (and (= 2 waits) gptel--request-alist
                              (not (plist-get (gptel-fsm-info fsm) :mevedel-retry-state)))))
          (accept-process-output nil 0.001))
        (should (< (float-time) deadline)))
      (let ((process (caar gptel--request-alist)))
        (mevedel-agent-runtime-interrupt invocation "Interrupted after disabling retries")
        (should-not (process-live-p process)))
      (should-not gptel--request-alist)
      (should (equal '(ABRT) terminal-events))
      (should (= 1 (length results)))
      (funcall saved-callback t saved-info)
      (should (= 1 (length results))))))

(ert-deftest mevedel-gptel-retry-runtime/uninstall-active-transport ()
  (mevedel-retry-runtime-test--with-active-transport
    (mevedel-gptel-retry-uninstall)))

(ert-deftest mevedel-gptel-retry-runtime/interrupt-with-pending-async-tool ()
  "A tool finishing after interruption advances the machine without sampling.
This holds with and without bounded recovery: a settled invocation spends no
further provider request and settles once."
  (dolist (limit '(0 10))
    (ert-info ((format "retry limit %s" limit))
      (let ((mevedel-gptel-retry-max-retries limit)
            (tool-callback nil)
            (tool-response
             (concat "event: response.output_item.done\n"
                     "data: {\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"Deferred\",\"arguments\":\"{}\"}}\n\n"
                     "event: response.completed\ndata: {\"response\":{\"status\":\"completed\"}}\n\n")))
        (mevedel-retry-runtime-test--with-agent (list tool-response mevedel-retry-test--success)
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
          (mevedel-agent-runtime-interrupt invocation "Interrupted with a pending tool")
          (should (= 1 (length results)))
          (funcall tool-callback "Late tool output")
          (should (eq 'ABRT (gptel-fsm-state fsm)))
          (should (= 1 (length requests)))
          (should (= 1 (length results)))
          (should-not gptel--request-alist)
          (should-not mevedel-gptel-retry--states))))))

(provide 'test-mevedel-gptel-retry-runtime)
;;; test-mevedel-gptel-retry-runtime.el ends here
