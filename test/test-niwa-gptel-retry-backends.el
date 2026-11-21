;;; test-niwa-gptel-retry-backends.el --- Native backend retry matrix -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise native backend dispatch and real local HTTP recovery without
;; credentials.  In particular, Gemini streams use parse-response's optional
;; argument, and URL retrieval has no curl sentinel to release retry ownership.

;;; Code:

(require 'gptel-retry-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "gptel-retry-helpers"))
(require 'gptel-anthropic)
(require 'gptel-bedrock)
(require 'gptel-gemini)
(require 'gptel-gh)
(require 'gptel-kagi)
(require 'gptel-ollama)
(require 'gptel-openai-extras)
(require 'niwa-claude-code-backend)

(cl-defstruct (niwa-retry-backends-test--custom-backend
               (:include gptel-backend)
               (:constructor niwa-retry-backends-test--make-custom-backend)))

(cl-defmethod gptel-curl--parse-stream
  ((_backend niwa-retry-backends-test--custom-backend) _info)
  "Parse the test backend's custom text envelopes without stop metadata."
  (let (strings)
    (while (re-search-forward "^data: " nil t)
      (push (plist-get (gptel--json-read) :text) strings))
    (apply #'concat (nreverse strings))))

(defconst niwa-retry-backends-test--constructors
  '(gptel--make-openai gptel--make-openai-responses gptel--make-openai-oauth
    gptel--make-deepseek gptel--make-privategpt gptel--make-perplexity
    gptel--make-gh gptel--make-anthropic gptel--make-bedrock
    gptel--make-gemini gptel--make-ollama gptel--make-kagi)
  "Concrete native gptel backend constructors, including inherited protocols.")

(defconst niwa-retry-backends-test--wire-cases
  '((:constructor gptel--make-openai
     :data (:model "test-model" :messages [(:role "user" :content "question")])
     :stream-failure
     "data: {\"choices\":[{\"delta\":{\"content\":\"Abandoned output\"}}]}\n\ndata: {\"error\":{\"code\":\"server_error\",\"message\":\"Model at capacity\"}}\n\n"
     :stream-success
     "data: {\"error\":null,\"choices\":[{\"delta\":{\"content\":\"Recovered answer\"},\"finish_reason\":\"stop\"}]}\n\ndata: [DONE]\n\n"
     :failure
     "{\"error\":{\"code\":\"server_error\",\"message\":\"Model at capacity\"},\"usage\":{\"prompt_tokens\":7,\"completion_tokens\":2}}"
     :success
     "{\"error\":null,\"choices\":[{\"message\":{\"content\":\"Recovered answer\"},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":12,\"completion_tokens\":3}}"
     :diagnostic (:code "server_error" :type nil :message "Model at capacity")
     :tokens (:input 19 :output 5 :cached 0))
    (:constructor gptel--make-anthropic
     :data (:model "test-model" :messages [(:role "user" :content "question")])
     :stream-failure
     "event: content_block_delta\ndata: {\"delta\":{\"text\":\"Abandoned output\"}}\n\nevent: error\ndata: {\"type\":\"error\",\"error\":{\"type\":\"overloaded_error\",\"message\":\"Temporarily overloaded\"}}\n\n"
     :stream-success
     "event: content_block_delta\ndata: {\"delta\":{\"text\":\"Recovered answer\"}}\n\nevent: message_delta\ndata: {\"delta\":{\"stop_reason\":\"end_turn\"}}\n\nevent: message_stop\ndata: {\"error\":null}\n\n"
     :failure
     "{\"type\":\"error\",\"error\":{\"type\":\"overloaded_error\",\"message\":\"Temporarily overloaded\"},\"content\":[],\"usage\":{\"input_tokens\":7,\"output_tokens\":2}}"
     :success
     "{\"error\":null,\"content\":[{\"type\":\"text\",\"text\":\"Recovered answer\"}],\"stop_reason\":\"end_turn\",\"usage\":{\"input_tokens\":12,\"output_tokens\":3}}"
     :diagnostic (:code nil :type "overloaded_error" :message "Temporarily overloaded")
     :tokens (:input 19 :output 5 :cached 0 :cache 0))
    (:constructor gptel--make-gemini
     :data (:contents [(:role "user" :parts [(:text "question")])])
     :stream-failure
     "[{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Abandoned output\"}]}}]},{\"error\":{\"code\":503,\"status\":\"UNAVAILABLE\",\"message\":\"Model at capacity\"}}]\n"
     :stream-success
     "[{\"error\":null,\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Recovered answer\"}]},\"finishReason\":\"STOP\"}]}]\n"
     :failure
     "{\"error\":{\"code\":503,\"status\":\"UNAVAILABLE\",\"message\":\"Model at capacity\"},\"candidates\":[{\"content\":{\"parts\":[]},\"finishReason\":\"OTHER\"}],\"usageMetadata\":{\"promptTokenCount\":7,\"totalTokenCount\":9}}"
     :success
     "{\"error\":null,\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Recovered answer\"}]},\"finishReason\":\"STOP\"}],\"usageMetadata\":{\"promptTokenCount\":12,\"totalTokenCount\":15}}"
     :diagnostic (:code "503" :type nil :message "Model at capacity")
     :tokens (:input 19 :output 5 :cached 0))
    (:constructor gptel--make-ollama
     :data (:model "test-model" :messages [(:role "user" :content "question")])
     :stream-failure
     "{\"message\":{\"content\":\"Abandoned output\"},\"done\":false}\n{\"error\":\"Model at capacity\"}\n"
     :stream-success
     "{\"error\":null,\"message\":{\"content\":\"Recovered answer\"},\"done\":true,\"done_reason\":\"stop\"}\n"
     :failure
     "{\"error\":\"Model at capacity\",\"message\":{\"content\":\"\"},\"prompt_eval_count\":7,\"eval_count\":2}"
     :success
     "{\"error\":null,\"message\":{\"content\":\"Recovered answer\"},\"done\":true,\"done_reason\":\"stop\",\"prompt_eval_count\":12,\"eval_count\":3}"
     :diagnostic (:code nil :type nil :message "Model at capacity")
     :tokens (:input 19 :output 5)))
  "Provider-shaped transient failures and successes for real HTTP tests.")

(defmacro niwa-retry-backends-test--configure (constructor data stream)
  "Configure the HTTP fixture for CONSTRUCTOR, prepared DATA and STREAM."
  `(progn
     (setq backend
           (funcall ,constructor :name "native-retry-local" :header nil
                    :stream ,stream :models '(test-model)
                    :url (gptel-backend-url backend)))
     (with-current-buffer data-buf
       (setq-local gptel-backend backend))
     (plist-put (gptel-fsm-info fsm) :backend backend)
     (plist-put (gptel-fsm-info fsm) :stream ,stream)
     (plist-put (gptel-fsm-info fsm) :data (copy-tree ,data t))
     (plist-put (gptel-fsm-info fsm) :callback
                (if ,stream #'gptel-curl--stream-insert-response
                  #'gptel--insert-response))))

(defmacro niwa-retry-backends-test--assert-settled (attempts)
  "Assert successful settlement after ATTEMPTS with no live retry ownership."
  `(progn
     (should (= ,attempts (length requests)))
     (should (= 1 waits))
     (should (= 1 finished))
     (should (= 0 failed))
     (should (= 0 aborted))
     (should (eq 'DONE (gptel-fsm-state fsm)))
     (should-not remaining)
     (should-not scheduled)
     (should-not gptel--request-alist)
     (should-not niwa-gptel-retry--states)
     (let* ((info (gptel-fsm-info fsm))
            (owner (plist-get info :niwa-retry-state)))
       (should owner)
       (should (plist-get owner :closed))
       (should-not (plist-get owner :timer))
       (should-not (plist-get owner :data))
       (should-not (plist-get info :error))
       (should-not (plist-get info :niwa-retry-exhausted)))
     (with-current-buffer data-buf
       (should (equal "Recovered answer" (string-trim (buffer-string)))))))

(ert-deftest niwa-gptel-retry-dispatch/native-backend-opt-in-matrix ()
  "Every admitted native concrete backend opts in; background and ACP do not."
  (let ((niwa-gptel-retry--states nil)
        (gptel--request-alist nil))
    (unwind-protect
        (dolist (constructor
                 (append niwa-retry-backends-test--constructors
                         '(niwa-claude-code--make-backend)))
          (dolist (admitted '(nil t))
            (dolist (limit '(0 2))
              (ert-info ((format "%s admitted=%s limit=%s" constructor admitted limit))
                (with-temp-buffer
                  (let* ((niwa-gptel-retry-max-retries limit)
                         (request (niwa-request--create :id "native-matrix"))
                         (backend (funcall constructor :name "native-matrix"))
                         (fsm (gptel-make-fsm
                               :info (list :buffer (current-buffer) :backend backend
                                           :data '(:prepared "unchanged")
                                           :callback #'ignore)))
                         (expected (and admitted
                                        (not (eq constructor
                                                 'niwa-claude-code--make-backend))))
                         (calls 0))
                    (when admitted (setq-local niwa--current-request request))
                    (cl-letf (((symbol-function 'gptel--handle-wait)
                               (lambda (machine)
                                 (should (eq machine fsm))
                                 (cl-incf calls))))
                      (niwa-gptel-retry-dispatch fsm))
                    (should (= 1 calls))
                    (let ((owner (plist-get (gptel-fsm-info fsm) :niwa-retry-state)))
                      (if expected
                          (progn
                            (should (eq request (plist-get owner :request)))
                            (should (= limit (plist-get owner :limit)))
                            (should (= 0 (plist-get owner :retries)))
                            (should (equal '(:prepared "unchanged") (plist-get owner :data))))
                        (should-not owner)))
                    ;; Close while the owning buffer is still alive.
                    (niwa-gptel-retry-uninstall)
                    (should-not niwa-gptel-retry--states)))))))
      (niwa-gptel-retry-uninstall))))

(ert-deftest niwa-gptel-retry-dispatch/native-stream-failure-then-success ()
  "OpenAI, Anthropic, Gemini and Ollama recover through real curl streams."
  (niwa-test--with-captured-diagnostics nil
    (dolist (spec niwa-retry-backends-test--wire-cases)
      (ert-info ((format "Backend: %s" (plist-get spec :constructor)))
        (niwa-retry-test--with-http
            (list (plist-get spec :stream-failure) (plist-get spec :stream-success))
          (niwa-retry-backends-test--configure
           (plist-get spec :constructor) (plist-get spec :data) t)
          (gptel--fsm-transition fsm)
          (niwa-retry-test--drain-native t)
          (should (eq 'MRETRY (gptel-fsm-state fsm)))
          (when (eq (plist-get spec :constructor) 'gptel--make-gemini)
            (should (eql 503 (plist-get (plist-get (gptel-fsm-info fsm) :error) :code))))
          (niwa-retry-test--drain-native)
          (niwa-retry-backends-test--assert-settled 2)
          (should (equal (car requests) (cadr requests)))
          (let* ((errors (plist-get (gptel-fsm-info fsm) :niwa-retry-errors))
                 (diagnostic (car errors))
                 (expected (plist-get spec :diagnostic)))
            (should (= 1 (length errors)))
            (should (= 1 (plist-get diagnostic :attempt)))
            (should (equal "200" (plist-get diagnostic :http-status)))
            (dolist (key '(:code :type :message))
              (should (equal (plist-get expected key) (plist-get diagnostic key))))))))))

(ert-deftest niwa-gptel-retry-dispatch/native-nonstream-usage-and-url ()
  "Curl and URL retrieval recover nonstream failures and total reported usage."
  (niwa-test--with-captured-diagnostics nil
    (dolist (spec niwa-retry-backends-test--wire-cases)
      (dolist (curl '(t nil))
        (ert-info ((format "Backend: %s curl=%s" (plist-get spec :constructor) curl))
          (niwa-retry-test--with-http
              (list (plist-get spec :failure) (plist-get spec :success))
            (let ((gptel-use-curl curl)
                  (url-proxy-services nil))
              (niwa-retry-backends-test--configure
               (plist-get spec :constructor) (plist-get spec :data) nil)
              (gptel--fsm-transition fsm)
              (niwa-retry-test--drain-native)
              (niwa-retry-backends-test--assert-settled 2)
              (should (equal (car requests) (cadr requests)))
              (should (equal (plist-get spec :tokens)
                             (plist-get (gptel-fsm-info fsm) :tokens)))
              (should (equal (plist-get spec :tokens)
                             (plist-get (gptel-fsm-info fsm) :tokens-full)))
              (should-not (plist-get (gptel-fsm-info fsm)
                                     :niwa-retry-usage-incomplete)))))))))

(ert-deftest niwa-gptel-retry-dispatch/native-null-error-success ()
  "Explicit JSON null errors never retry successful native responses."
  (niwa-test--with-captured-diagnostics nil
    (dolist (spec niwa-retry-backends-test--wire-cases)
      (dolist (mode '(stream curl url))
        (ert-info ((format "Backend: %s mode=%s" (plist-get spec :constructor) mode))
          (niwa-retry-test--with-http
              (list (plist-get spec (if (eq mode 'stream) :stream-success :success)))
            (let ((gptel-use-curl (not (eq mode 'url)))
                  (url-proxy-services nil))
              (niwa-retry-backends-test--configure
               (plist-get spec :constructor) (plist-get spec :data) (eq mode 'stream))
              (gptel--fsm-transition fsm)
              (niwa-retry-test--drain-native)
              (niwa-retry-backends-test--assert-settled 1)
              (should-not (plist-get (gptel-fsm-info fsm) :niwa-retry-errors)))))))))

(ert-deftest niwa-gptel-retry--parse-response/native-gemini-optional-argument ()
  "Installed advice forwards Gemini's include-text argument and native mutation."
  (unwind-protect
      (progn
        (niwa-gptel-retry-install)
        (dolist (include '(nil include))
          (let* ((backend (gptel--make-gemini :name "gemini-optional-local"))
                 (response '(:error :null
                             :candidates [(:content (:parts [(:text "answer")])
                                           :finishReason "STOP")]))
                 (info (list :backend backend
                             :data (copy-tree '(:contents [(:role "user"
                                                                 :parts [(:text "question")])]) t))))
            (should (equal "answer" (gptel--parse-response backend response info include)))
            (should (equal "STOP" (plist-get info :stop-reason)))
            (let ((contents (plist-get (plist-get info :data) :contents)))
              (should (= (if include 2 1) (length contents)))
              (when include
                (should (equal '(:role "model" :parts [(:text "answer")])
                               (aref contents 1))))))))
    (niwa-gptel-retry-uninstall)))

(ert-deftest niwa-gptel-retry-dispatch/native-truncated-stream-matrix ()
  "Missing native completion markers retry instead of accepting partial text."
  (niwa-test--with-captured-diagnostics nil
    (dolist (spec niwa-retry-backends-test--wire-cases)
      (let* ((constructor (plist-get spec :constructor))
             (truncated
              (pcase constructor
                ('gptel--make-openai
                 "data: {\"choices\":[{\"delta\":{\"content\":\"Abandoned output\"}}]}\n\n")
                ('gptel--make-anthropic
                 "event: content_block_delta\ndata: {\"delta\":{\"text\":\"Abandoned output\"}}\n\n")
                ('gptel--make-gemini
                 "[{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Abandoned output\"}]}}]}]\n")
                ('gptel--make-ollama
                 "{\"message\":{\"content\":\"Abandoned output\"},\"done\":false}\n"))))
        (ert-info ((format "Backend: %s" constructor))
          (niwa-retry-test--with-http
              (list truncated (plist-get spec :stream-success))
            (niwa-retry-backends-test--configure constructor (plist-get spec :data) t)
            (gptel--fsm-transition fsm)
            (niwa-retry-test--drain-native)
            (niwa-retry-backends-test--assert-settled 2)
            (should (equal (car requests) (cadr requests)))
            (should (equal "stream_interrupted"
                           (plist-get (car (plist-get (gptel-fsm-info fsm)
                                                     :niwa-retry-errors)) :code)))))))))

(ert-deftest niwa-gptel-retry-dispatch/native-stream-completion-markers ()
  "Inherited OpenAI protocols and native terminal metadata complete once."
  (niwa-test--with-captured-diagnostics nil
    (let ((openai
           "data: {\"choices\":[{\"delta\":{\"content\":\"Recovered answer\"}}]}\n\ndata: [DONE]\n\n")
          ;; Some compatible servers close after the finishing chunk (and an
          ;; optional usage-only chunk) without the [DONE] sentinel.
          (openai-finish
           "data: {\"choices\":[{\"delta\":{\"content\":\"Recovered answer\"},\"finish_reason\":\"stop\"}]}\n\ndata: {\"choices\":[],\"usage\":{\"prompt_tokens\":12,\"completion_tokens\":3}}\n\n")
          (messages '(:messages [(:role "user" :content "question")]))
          (anthropic
           "event: content_block_delta\ndata: {\"delta\":{\"text\":\"Recovered answer\"}}\n\nevent: message_delta\ndata: {\"delta\":{\"stop_reason\":\"end_turn\"}}\n\n"))
      (dolist (spec
               `((gptel--make-openai ,openai ,messages)
                 (gptel--make-openai ,openai-finish ,messages)
                 (gptel--make-deepseek ,openai-finish ,messages)
                 (gptel--make-deepseek ,openai ,messages)
                 (gptel--make-privategpt ,openai ,messages)
                 (gptel--make-perplexity ,openai ,messages)
                 (gptel--make-gh ,openai ,messages)
                 (gptel--make-anthropic ,anthropic ,messages)
                 (gptel--make-anthropic
                  "event: content_block_delta\ndata: {\"delta\":{\"text\":\"Recovered answer\"}}\n\nevent: message_stop\ndata: {}\n\n"
                  ,messages)
                 (gptel--make-openai-responses ,niwa-retry-test--success
                  (:input []))))
        (ert-info ((format "Backend: %s" (car spec)))
          (niwa-retry-test--with-http (list (nth 1 spec))
            (niwa-retry-backends-test--configure (car spec) (nth 2 spec) t)
            (gptel--fsm-transition fsm)
            (niwa-retry-test--drain-native)
            (niwa-retry-backends-test--assert-settled 1)))))))

(ert-deftest niwa-gptel-retry-dispatch/native-openai-async-tool-not-replayed ()
  "A native async tool survives HTTP cleanup and runs once before sample retry."
  (niwa-test--with-captured-diagnostics nil
    (let ((tool-callback nil)
          (tool-runs 0)
          (spec (car niwa-retry-backends-test--wire-cases))
          (tool-response
           "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"id\":\"native-async-1\",\"type\":\"function\",\"function\":{\"name\":\"Deferred\",\"arguments\":\"{}\"}}]}}]}\n\ndata: [DONE]\n\n"))
      (niwa-retry-test--with-http
          (list tool-response (plist-get spec :stream-failure) (plist-get spec :stream-success))
        (niwa-retry-backends-test--configure
         (plist-get spec :constructor) (plist-get spec :data) t)
        (plist-put (gptel-fsm-info fsm) :tools
                   (list (gptel-make-tool
                          :name "Deferred" :description "Finish after native HTTP cleanup"
                          :args nil :async t
                          :function (lambda (callback)
                                      (cl-incf tool-runs)
                                      (setq tool-callback callback)))))
        (gptel--fsm-transition fsm)
        (let ((deadline (+ (float-time) 10)))
          (while (and (< (float-time) deadline)
                      (or (not tool-callback) gptel--request-alist))
            (accept-process-output nil 0.01))
          (should (< (float-time) deadline)))
        (should (eq 'TOOL (gptel-fsm-state fsm)))
        (should (= 1 tool-runs))
        (should (= 1 (length requests)))
        (funcall tool-callback "Deferred output")
        (niwa-retry-test--drain-native)
        (should (= 1 tool-runs))
        (should (= 3 (length requests)))
        (should (= 2 waits))
        (should (= 1 finished))
        (should (= 0 failed))
        (should (equal (car requests) (cadr requests)))
        (should (string-match-p "Deferred output" (car requests)))
        (should-not scheduled)
        (should-not gptel--request-alist)
        (should-not niwa-gptel-retry--states)
        (with-current-buffer data-buf
          (should (string-match-p "Recovered answer" (buffer-string)))
          (should-not (string-match-p "Abandoned output" (buffer-string))))))))

(ert-deftest niwa-gptel-retry-dispatch/abort-with-pending-async-tool-settles ()
  "Aborting during an async tool lets its aborted result reach a terminal state.
As in `niwa-abort', the request's cancellers run in registration order, so
the sample's canceller runs before the later tool canceller delivers its
aborted result.  Any follow-up transport is then torn down."
  (niwa-test--with-captured-diagnostics nil
    (dolist (limit '(0 10))
      (ert-info ((format "retry limit %s" limit))
        (let ((niwa-gptel-retry-max-retries limit)
              (tool-callback nil)
              (spec (car niwa-retry-backends-test--wire-cases))
              (tool-response
               "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"id\":\"abort-async-1\",\"type\":\"function\",\"function\":{\"name\":\"Deferred\",\"arguments\":\"{}\"}}]}}]}\n\ndata: [DONE]\n\n"))
          (niwa-retry-test--with-http
              (list tool-response (plist-get spec :stream-success))
            (niwa-retry-backends-test--configure
             (plist-get spec :constructor) (plist-get spec :data) t)
            (plist-put (gptel-fsm-info fsm) :tools
                       (list (gptel-make-tool
                              :name "Deferred" :description "Pending when aborted"
                              :args nil :async t
                              :function (lambda (callback)
                                          (setq tool-callback callback)
                                          (niwa-request-push-canceller
                                           request (lambda () (funcall callback "Error: aborted")))))))
            (gptel--fsm-transition fsm)
            (let ((deadline (+ (float-time) 10)))
              (while (and (< (float-time) deadline)
                          (or (not tool-callback) gptel--request-alist))
                (accept-process-output nil 0.01))
              (should (< (float-time) deadline)))
            (should (eq 'TOOL (gptel-fsm-state fsm)))
            (niwa-request-cancel request)
            (while gptel--request-alist (gptel-abort data-buf))
            (should (eq 'ABRT (gptel-fsm-state fsm)))
            (should (= 1 aborted))
            (should (= 0 finished))
            (should-not scheduled)
            (should-not gptel--request-alist)
            (should-not niwa-gptel-retry--states)
            (when (> limit 0)
              ;; Recovery does not spend a provider call on a cancelled request.
              (should (= 1 (length requests))))))))))

(defmacro niwa-retry-backends-test--with-http-status (responses &rest body)
  "Run BODY using the shared fixture with HTTP status/header RESPONSES.
Each response is a plist with :status, :body and optional :headers alist.
Bind `delays' to the actual delays requested by retry scheduling."
  (declare (indent 1) (debug t))
  `(niwa-retry-test--with-http nil
     (setq remaining ,responses)
     (let ((delays nil)
           (schedule (symbol-function 'niwa-transport-run-at-time)))
       ;; Extend only the local server's wire responses; gptel transport,
       ;; response parsing and callbacks are unchanged.
       (set-process-filter
        server
        (lambda (client text)
          (let ((input (concat (process-get client 'input) text))
                (case-fold-search t))
            (process-put client 'input input)
            (when (and (not (process-get client 'answered))
                       (string-match "\r\n\r\n" input)
                       (let ((header-end (match-end 0)))
                         (and (string-match "Content-Length: \\([0-9]+\\)" input)
                              (>= (- (string-bytes input) header-end)
                                  (string-to-number (match-string 1 input))))))
              (process-put client 'answered t)
              (push (substring input (+ 4 (string-match "\r\n\r\n" input))) requests)
              (let* ((response (pop remaining))
                     (status (plist-get response :status))
                     (body (plist-get response :body))
                     (headers (mapconcat (lambda (header)
                                           (format "%s: %s\r\n" (car header) (cdr header)))
                                         (plist-get response :headers) "")))
                (should response)
                (process-send-string
                 client
                 (format "HTTP/1.1 %d %s\r\nContent-Type: %s\r\nContent-Length: %d\r\n%sConnection: close\r\n\r\n%s"
                         status (if (= status 200) "OK" "Synthetic refusal")
                         (if (string-prefix-p "data:" body)
                             "text/event-stream" "application/json")
                         (string-bytes body) headers body)))))))
       (cl-letf (((symbol-function 'niwa-transport-run-at-time)
                  (lambda (seconds function &rest args)
                    (push seconds delays)
                    (apply schedule seconds function args))))
         ,@body))))

(ert-deftest niwa-gptel-retry--parse-stream/native-gemini-header-only-filter ()
  "A fresh process filter can receive Gemini headers before any JSON."
  (niwa-retry-test--with-http nil
    (let (;; Force headers across multiple filter deliveries.
          (read-process-output-max 10)
          (spec (nth 2 niwa-retry-backends-test--wire-cases))
          done caught results)
      (niwa-retry-backends-test--configure
       (plist-get spec :constructor) (plist-get spec :data) t)
      (let ((process
             (make-process
              :name "gemini-header-only" :noquery t :connection-type 'pipe
              :command '("printf" "HTTP/1.1 200 OK\r\n\r\n")
              :sentinel #'ignore
              :filter
              (lambda (_process headers)
                (unless done
                  (condition-case err
                      (with-temp-buffer
                        (insert headers)
                        (goto-char (point-min))
                        (push (gptel-curl--parse-stream backend (gptel-fsm-info fsm)) results)
                        (push (point) results)
                        (save-excursion
                          (goto-char (point-max))
                          (insert (plist-get spec :stream-success)))
                        (push (gptel-curl--parse-stream backend (gptel-fsm-info fsm)) results))
                    (error (setq caught err)))
                  (setq done t))))))
        (unwind-protect
            (progn
              (let ((deadline (+ (float-time) 5)))
                (while (and (not done) (< (float-time) deadline))
                  (accept-process-output nil 0.01)))
              (should done)
              (should-not caught)
              (should (equal '("Recovered answer" 1 "") results)))
          (delete-process process))))))

(ert-deftest niwa-gptel-retry-dispatch/native-gemini-fragmented-prefix ()
  "Gemini headers and array prefixes may arrive before the first object."
  (dolist (limit '(0 2))
    (dolist (prefix '("" "["))
      (let* ((niwa-gptel-retry-max-retries limit)
             (spec (nth 2 niwa-retry-backends-test--wire-cases))
             (success (plist-get spec :stream-success)))
        (niwa-retry-test--with-http
            (list (list prefix (substring success (length prefix))))
          (niwa-retry-backends-test--configure
           (plist-get spec :constructor) (plist-get spec :data) t)
          (gptel--fsm-transition fsm)
          (niwa-retry-test--drain-native)
          (should (= 1 (length requests)))
          (should (= 1 finished))
          (should (= 0 failed))
          (should-not scheduled)
          (should-not gptel--request-alist)
          (should-not niwa-gptel-retry--states)
          (with-current-buffer data-buf
            (should (equal "Recovered answer" (string-trim (buffer-string))))))))))

(ert-deftest niwa-gptel-retry-dispatch/native-http-transient-status-matrix ()
  "Real HTTP 429 and 503 retry once on native streaming, curl and URL paths."
  (niwa-test--with-captured-diagnostics nil
    (dolist (spec niwa-retry-backends-test--wire-cases)
      (dolist (status '(429 503))
        (dolist (mode '(stream curl url))
          (ert-info ((format "Backend: %s status=%s mode=%s"
                             (plist-get spec :constructor) status mode))
            (niwa-retry-backends-test--with-http-status
                (list (list :status status :body "{\"error\":{\"message\":\"Synthetic HTTP refusal\"}}")
                      (list :status 200 :body
                            (plist-get spec (if (eq mode 'stream) :stream-success :success))))
              (let ((gptel-use-curl (not (eq mode 'url)))
                    (url-proxy-services nil))
                (niwa-retry-backends-test--configure
                 (plist-get spec :constructor) (plist-get spec :data) (eq mode 'stream))
                (gptel--fsm-transition fsm)
                (niwa-retry-test--drain-native)
                (niwa-retry-backends-test--assert-settled 2)
                (should (equal (car requests) (cadr requests)))
                (should (= 1 (length delays)))
                (let ((errors (plist-get (gptel-fsm-info fsm) :niwa-retry-errors)))
                  (should (= 1 (length errors)))
                  (should (equal (number-to-string status) (plist-get (car errors) :http-status)))
                  (should (equal "Synthetic HTTP refusal" (plist-get (car errors) :message))))))))))))

(ert-deftest niwa-gptel-retry-dispatch/native-http-retry-after-curl ()
  "Streaming and nonstreaming curl honor actual HTTP Retry-After headers."
  (niwa-test--with-captured-diagnostics nil
    (dolist (mode '(stream curl))
      (let ((spec (car niwa-retry-backends-test--wire-cases)))
        (niwa-retry-backends-test--with-http-status
            (list '(:status 429 :headers (("Retry-After" . "2.5"))
                    :body "{\"error\":{\"message\":\"Synthetic HTTP refusal\"}}")
                  (list :status 200 :body
                        (plist-get spec (if (eq mode 'stream) :stream-success :success))))
          (niwa-retry-backends-test--configure
           (plist-get spec :constructor) (plist-get spec :data) (eq mode 'stream))
          (gptel--fsm-transition fsm)
          (niwa-retry-test--drain-native)
          (niwa-retry-backends-test--assert-settled 2)
          (should (equal '(2.5) delays)))))))

(ert-deftest niwa-gptel-retry-dispatch/native-http-retry-after-url ()
  "URL retrieval honors actual HTTP Retry-After, without a curl sentinel."
  (niwa-test--with-captured-diagnostics nil
    (let ((spec (car niwa-retry-backends-test--wire-cases)))
      (niwa-retry-backends-test--with-http-status
          (list '(:status 503 :headers (("Retry-After" . "2.5"))
                  :body "{\"error\":{\"message\":\"Synthetic HTTP refusal\"}}")
                (list :status 200 :body (plist-get spec :success)))
        (let ((gptel-use-curl nil)
              (url-proxy-services nil))
          (niwa-retry-backends-test--configure
           (plist-get spec :constructor) (plist-get spec :data) nil)
          (gptel--fsm-transition fsm)
          (niwa-retry-test--drain-native)
          (niwa-retry-backends-test--assert-settled 2)
          (should (equal '(2.5) delays)))))))

(ert-deftest niwa-gptel-retry-dispatch/native-http-date-retry-after ()
  "Curl and URL use a real HTTP-date Retry-After rather than default backoff."
  (niwa-test--with-captured-diagnostics nil
    (dolist (mode '(stream curl url))
      (let* ((spec (car niwa-retry-backends-test--wire-cases))
             (retry-at (+ (float-time) 120))
             (header (format-time-string "%a, %d %b %Y %H:%M:%S GMT"
                                         (seconds-to-time retry-at) t)))
        (niwa-retry-backends-test--with-http-status
            (list (list :status 503 :headers (list (cons "Retry-After" header))
                        :body "{\"error\":{\"message\":\"Synthetic HTTP refusal\"}}")
                  (list :status 200 :body
                        (plist-get spec (if (eq mode 'stream) :stream-success :success))))
          (let ((gptel-use-curl (not (eq mode 'url)))
                (url-proxy-services nil))
            (niwa-retry-backends-test--configure
             (plist-get spec :constructor) (plist-get spec :data) (eq mode 'stream))
            (gptel--fsm-transition fsm)
            (niwa-retry-test--drain-native)
            (niwa-retry-backends-test--assert-settled 2)
            (should (= 1 (length delays)))
            (should (<= (- (float-time (date-to-time header)) (float-time))
                        (car delays) 120))))))))

(ert-deftest niwa-gptel-retry-dispatch/native-http-401-permanent-matrix ()
  "HTTP 401 remains permanent even with transient text and retry advice."
  (niwa-test--with-captured-diagnostics nil
    (dolist (spec niwa-retry-backends-test--wire-cases)
      (dolist (mode '(stream curl url))
        (ert-info ((format "Backend: %s mode=%s" (plist-get spec :constructor) mode))
          (niwa-retry-backends-test--with-http-status
              (list '(:status 401 :headers (("Retry-After" . "2.5"))
                      :body "{\"error\":{\"code\":\"server_error\",\"message\":\"Model at capacity\"}}"))
            (let ((gptel-use-curl (not (eq mode 'url)))
                  (url-proxy-services nil))
              (niwa-retry-backends-test--configure
               (plist-get spec :constructor) (plist-get spec :data) (eq mode 'stream))
              ;; URL retrieval retries authentication interactively when no
              ;; Authorization was supplied.  Exercise an API rejecting an
              ;; already-authenticated request instead, using only localhost.
              (setf (gptel-backend-header backend)
                    '(("Authorization" . "Bearer synthetic-local-only")))
              (gptel--fsm-transition fsm)
              (niwa-retry-test--drain-native)
              (should (= 1 (length requests)))
              (should (= 1 waits))
              (should (= 0 finished))
              (should (= 1 failed))
              (should (eq 'ERRS (gptel-fsm-state fsm)))
              (should-not remaining)
              (should-not scheduled)
              (should-not delays)
              (should-not gptel--request-alist)
              (should-not niwa-gptel-retry--states)
              (let* ((info (gptel-fsm-info fsm))
                     (owner (plist-get info :niwa-retry-state)))
                (should (equal "401" (plist-get info :http-status)))
                (should (equal "server_error" (plist-get (plist-get info :error) :code)))
                (should (plist-get info :niwa-retry-permanent))
                (should-not (plist-get info :niwa-retry-exhausted))
                (should (plist-get owner :closed))
                (should-not (plist-get owner :timer))
                (should-not (plist-get owner :data))))))))))

(ert-deftest niwa-gptel-retry-dispatch/native-null-false-error-events ()
  "Explicit null/false event:error fields cannot fabricate an outer failure."
  (niwa-test--with-captured-diagnostics nil
    (dolist (value '("null" "false"))
      (ert-info ((format "Error value: %s" value))
        (niwa-retry-test--with-http
            (list (concat (format "event: error\ndata: {\"error\":%s}\n\n" value)
                          niwa-retry-test--success))
          (niwa-retry-backends-test--configure
           'gptel--make-openai-responses '(:input []) t)
          (gptel--fsm-transition fsm)
          (niwa-retry-test--drain-native)
          (niwa-retry-backends-test--assert-settled 1)
          (let ((info (gptel-fsm-info fsm)))
            (should (equal "completed" (plist-get info :stop-reason)))
            (should-not (plist-get info :niwa-retry-provider-error))
            (should-not (plist-get info :niwa-retry-errors))))))))

(ert-deftest niwa-gptel-retry-dispatch/native-async-next-sample-reasoning ()
  "Reasoning is consumed on both sides of a successful async tool boundary."
  (niwa-test--with-captured-diagnostics nil
    (let ((tool-callback nil)
          (tool-runs 0)
          (reasons nil)
          (reason-ends 0)
          (spec (car niwa-retry-backends-test--wire-cases)))
      (niwa-retry-test--with-http
          (list (concat
                 "data: {\"choices\":[{\"delta\":{\"reasoning_content\":\"first thought\"}}]}\n\n"
                 "data: {\"choices\":[{\"delta\":{\"content\":\"Working\"}}]}\n\n"
                 "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"id\":\"reasoning-async-1\",\"function\":{\"name\":\"Deferred\",\"arguments\":\"{}\"}}]}}]}\n\n"
                 "data: [DONE]\n\n")
                (concat
                 "data: {\"choices\":[{\"delta\":{\"reasoning_content\":\"second thought\"}}]}\n\n"
                 "data: {\"choices\":[{\"delta\":{\"content\":\"Recovered answer\"}}]}\n\n"
                 "data: [DONE]\n\n"))
        (niwa-retry-backends-test--configure
         (plist-get spec :constructor) (plist-get spec :data) t)
        (plist-put (gptel-fsm-info fsm) :callback
                   (lambda (response info &optional raw)
                     (when (and (consp response) (eq (car response) 'reasoning))
                       (if (stringp (cdr response))
                           (push (cdr response) reasons)
                         (when (eq (cdr response) t) (cl-incf reason-ends))))
                     (gptel-curl--stream-insert-response response info raw)))
        (plist-put (gptel-fsm-info fsm) :tools
                   (list (gptel-make-tool
                          :name "Deferred" :description "Finish after the first reasoning sample"
                          :args nil :async t
                          :function (lambda (callback)
                                      (cl-incf tool-runs)
                                      (setq tool-callback callback)))))
        (gptel--fsm-transition fsm)
        (let ((deadline (+ (float-time) 10)))
          (while (and (< (float-time) deadline)
                      (or (not tool-callback) gptel--request-alist))
            (accept-process-output nil 0.01))
          (should (< (float-time) deadline)))
        (should (eq 'TOOL (gptel-fsm-state fsm)))
        (should (equal '("first thought") reasons))
        (should (= 1 reason-ends))
        (should (eq 'done (plist-get (gptel-fsm-info fsm) :reasoning-block)))
        (should-not (plist-get (gptel-fsm-info fsm) :reasoning))
        (funcall tool-callback "Deferred output")
        (niwa-retry-test--drain-native)
        (should (eq 'DONE (gptel-fsm-state fsm)))
        (should (= 1 tool-runs))
        (should (= 2 waits))
        (should (= 2 (length requests)))
        (should (= 1 finished))
        (should (= 0 failed))
        (should (equal '("first thought" "second thought") (nreverse reasons)))
        (should (= 2 reason-ends))
        (should-not (plist-get (gptel-fsm-info fsm) :reasoning))
        (should-not (plist-get (gptel-fsm-info fsm) :niwa-retry-errors))
        (should-not remaining)
        (should-not scheduled)
        (should-not gptel--request-alist)
        (should-not niwa-gptel-retry--states)
        (let ((owner (plist-get (gptel-fsm-info fsm) :niwa-retry-state)))
          (should (plist-get owner :closed))
          (should-not (plist-get owner :timer)))
        (with-current-buffer data-buf
          (should (string-match-p "Working" (buffer-string)))
          (should (string-match-p "Recovered answer" (buffer-string))))))))

(ert-deftest niwa-gptel-retry-dispatch/native-unknown-stream-eof-transparent ()
  "A custom native parser without known stop metadata retains its EOF policy."
  (niwa-test--with-captured-diagnostics nil
    (dolist (admitted '(nil t))
      (ert-info ((format "Admitted: %s" admitted))
        (niwa-retry-test--with-http
            (list "data: {\"text\":\"Recovered answer\",\"final\":true}\n\n")
          (niwa-retry-backends-test--configure
           'niwa-retry-backends-test--make-custom-backend '(:prepared "unchanged") t)
          (unless admitted
            (with-current-buffer data-buf (setq-local niwa--current-request nil)))
          (gptel--fsm-transition fsm)
          (niwa-retry-test--drain-native)
          (should (eq 'DONE (gptel-fsm-state fsm)))
          (should (= 1 (length requests)))
          (should (= 1 waits))
          (should (= 1 finished))
          (should (= 0 failed))
          (should-not remaining)
          (should-not scheduled)
          (should-not gptel--request-alist)
          (should-not niwa-gptel-retry--states)
          (let ((info (gptel-fsm-info fsm)))
            (should-not (plist-get info :stop-reason))
            (should-not (plist-get info :niwa-retry-check-completion))
            (should-not (plist-get info :niwa-retry-terminal))
            (should-not (plist-get info :niwa-retry-errors))
            (should-not (plist-get info :error)))
          (if admitted
              (niwa-retry-backends-test--assert-settled 1)
            (should-not (plist-get (gptel-fsm-info fsm) :niwa-retry-state))
            (with-current-buffer data-buf
              (should (equal "Recovered answer" (string-trim (buffer-string)))))))))))

(provide 'test-niwa-gptel-retry-backends)
;;; test-niwa-gptel-retry-backends.el ends here
