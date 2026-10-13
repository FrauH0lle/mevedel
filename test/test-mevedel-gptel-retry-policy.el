;;; test-mevedel-gptel-retry-policy.el --- Codex retry policy tests -*- lexical-binding: t -*-

;;; Commentary:

;; Isolated policy and native Responses parser coverage.  No request dispatch,
;; provider credentials, processes, or retry timers are needed by these tests.

;;; Code:

(require 'ert)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-gptel-retry)
(require 'mevedel-view)
(require 'mevedel-view-stream)

(defmacro test-mevedel-retry-policy--with-live (&rest body)
  "Run BODY with an admitted request, OAuth backend and owned attempt.
Bind request, backend, fsm, state and info for assertions."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let* ((request (mevedel-request--create :id "retry-policy-test"))
            (backend (gptel--make-openai-oauth :name "retry-policy-test"))
            (info (list :buffer (current-buffer) :backend backend
                        :http-status "200" :error nil :stream t
                        :mevedel-retry-attempt (gensym "retry-policy-attempt-")))
            (fsm (gptel-make-fsm :info info :state 'TYPE))
            (state (list :fsm fsm :request request :retries 0 :limit 10)))
       ;; Admission is represented by the buffer's current request identity;
       ;; turn preparation and transport are outside this unit fixture.
       (setq-local mevedel--current-request request)
       (plist-put info :mevedel-retry-state state)
       ,@body)))

(defmacro test-mevedel-retry-policy--with-view (&rest body)
  "Run BODY with a real paired view and a multiline composer draft."
  (declare (indent 0) (debug t))
  `(let ((mevedel-user-dir
          (file-name-as-directory (make-temp-file "mevedel-retry-view-user-" t)))
         (mevedel-plugin-extra-roots nil)
         (mevedel-permission-mode 'ask))
     (unwind-protect
         (test-mevedel-retry-policy--with-live
           (let ((data-buf (current-buffer))
                 (view-buf (generate-new-buffer " *mevedel-retry-policy-view*")))
             (unwind-protect
                 (progn
                   (mevedel--transcript-org-mode)
                   (setq-local mevedel--current-request request)
                   (setq-local mevedel--session nil)
                   (setq-local mevedel-view--abort-function
                               #'mevedel-view-test--abort-interactions)
                   (mevedel-view--setup view-buf data-buf)
                   (with-current-buffer view-buf
                     (mevedel-view-stream-begin-turn
                      mevedel-view--input-marker
                      (with-current-buffer data-buf (copy-marker (point-max))))
                     (let ((start (mevedel-view--input-start))
                           (inhibit-read-only t))
                       (goto-char start)
                       (insert "> quoted\nsecond line")
                       (remove-text-properties
                        start (point)
                        '(read-only nil mevedel-view-prompt nil font-lock-face nil
                          face nil front-sticky nil rear-nonsticky nil))
                       (goto-char (+ start 4))))
                   ,@body)
               (when (buffer-live-p view-buf)
                 (with-current-buffer view-buf (mevedel-view-stream-stop))
                 (kill-buffer view-buf))
               (setq-local mevedel--current-request nil))))
       (delete-directory mevedel-user-dir t))))

(mevedel-deftest mevedel-gptel-retry--retryable-p ()
  ,test
  (test)
  :doc "policy: transient structured codes retry even with HTTP200"
  (dolist (code '("server_error" "internal_server_error" "overloaded_error"
                  "server_is_overloaded" "model_at_capacity" "model_capacity_exceeded"
                  "service_unavailable_error" "rate_limit_exceeded" "slow_down"
                  "stream_interrupted" "timeout_error"))
    (should (mevedel-gptel-retry--retryable-p (list :code code) "200"))
    (should (mevedel-gptel-retry--retryable-p (list :type code) "200")))

  :doc "policy: transient HTTP and recognized curl failures retry"
  (progn
    (dolist (status '("408" "429" "500" "502" "503" "504" "599"))
      (should (mevedel-gptel-retry--retryable-p "Synthetic transport failure" status)))
    (dolist (code '(6 7 18 28 35 52 55 56 92))
      (should (mevedel-gptel-retry--retryable-p
               (format "Curl failed with exit code %d. Synthetic failure" code) nil)))
    (should-not (mevedel-gptel-retry--retryable-p "Curl failed with exit code 23." nil)))

  :doc "policy: capacity text needs no Retry-After and generic error type is neutral"
  (progn
    (dolist (message '("Model at capacity" "Model is at capacity"
                       "Temporarily overloaded" "Server is overloaded"))
      (should (mevedel-gptel-retry--retryable-p message "200"))
      (should (mevedel-gptel-retry--retryable-p
               (list :type "error" :message message) "200")))
    (should-not (mevedel-gptel-retry--retryable-p
                 '(:code "unknown_error" :message "Model at capacity") "200")))

  :doc "policy: permanent structured codes override transient status and capacity text"
  (dolist (code '("invalid_request_error" "invalid_request" "invalid_prompt"
                  "authentication_error" "permission_error" "model_not_found"
                  "context_length_exceeded" "context_window_exceeded"
                  "insufficient_quota" "usage_limit_reached" "quota_exceeded"
                  "credit_balance_exhausted" "organization_spend_limit_exceeded"
                  "project_spend_limit_exceeded" "usage_not_included"
                  "content_filter" "max_output_tokens" "cancelled"))
    (should-not (mevedel-gptel-retry--retryable-p
                 (list :code code :type "server_error" :message "Model at capacity") "503"))
    (should-not (mevedel-gptel-retry--retryable-p
                 (list :type code :code "rate_limit_exceeded") "429")))

  :doc "policy: permanent HTTP and exhausted quota text override transient codes"
  (progn
    (dolist (status '("400" "401" "402" "403" "404" "413"))
      (should-not (mevedel-gptel-retry--retryable-p '(:code "server_error") status)))
    (dolist (message '("Quota exhausted" "Usage limit reached" "Insufficient quota"))
      (should-not (mevedel-gptel-retry--retryable-p
                   (list :code "rate_limit_exceeded" :message message) "429")))
    (should-not (mevedel-gptel-retry--retryable-p '(:code "unknown_error") "200"))
    (should-not (mevedel-gptel-retry--retryable-p nil nil))))

(mevedel-deftest mevedel-gptel-retry--delay ()
  ,test
  (test)
  :doc "delay: numeric server advice overrides backoff without jitter or a cap"
  (cl-letf (((symbol-function 'random)
             (lambda (&rest _) (ert-fail "Server advice must not use jitter"))))
    (dolist (advice '(0 2 2.5 "0" "2" "2.5" "120"))
      (should (= (if (stringp advice) (string-to-number advice) advice)
                 (mevedel-gptel-retry--delay 3 advice)))))

  :doc "delay: HTTP date advice uses the clock and past dates clamp to zero"
  (let ((now (float-time (date-to-time "Wed, 21 Oct 2015 07:28:00 GMT"))))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional time)
                 (if time (time-convert time 'integer) now)))
              ((symbol-function 'random)
               (lambda (&rest _) (ert-fail "Date advice must not use jitter"))))
      (should (= 60 (mevedel-gptel-retry--delay 1 "Wed, 21 Oct 2015 07:29:00 GMT")))
      (should (= 0 (mevedel-gptel-retry--delay 5 "Wed, 21 Oct 2015 07:27:00 GMT")))))

  :doc "delay: absent and invalid advice use capped exponential backoff"
  (cl-letf (((symbol-function 'random) (lambda (_limit) 100)))
    (dolist (advice '(nil -1 "-1" "" "not a date" "NaN" "1 second"))
      (should (= 1.0 (mevedel-gptel-retry--delay 1 advice)))
      (should (= 4.0 (mevedel-gptel-retry--delay 3 advice))))
    (should (= 30.0 (mevedel-gptel-retry--delay 10 nil))))

  :doc "delay: jitter stays within the documented backoff bounds"
  (progn
    (cl-letf (((symbol-function 'random) (lambda (_limit) 0)))
      (should (= 0.9 (mevedel-gptel-retry--delay 1 nil)))
      (should (= 27.0 (mevedel-gptel-retry--delay 10 nil))))
    (cl-letf (((symbol-function 'random) (lambda (_limit) 200)))
      (should (< (abs (- 1.1 (mevedel-gptel-retry--delay 1 nil))) 0.000001))
      (should (= 33.0 (mevedel-gptel-retry--delay 10 nil))))))

(mevedel-deftest mevedel-gptel-retry--pending-p ()
  ,test
  (test)
  :doc "pending: accepted failure records one diagnostic without consuming a retry"
  (test-mevedel-retry-policy--with-live
    (plist-put info :error '(:type "error" :code "server_error" :message "Synthetic overload"))
    (should (mevedel-gptel-retry--pending-p info))
    (should (mevedel-gptel-retry--pending-p info))
    (should (plist-get state :pending))
    (should (= 0 (plist-get state :retries)))
    (should (equal '((:attempt 1 :http-status "200" :type "error"
                              :code "server_error" :message "Synthetic overload"))
                   (plist-get state :errors)))
    (should (eq (plist-get state :errors) (plist-get info :mevedel-retry-errors)))
    (should-not (plist-get info :mevedel-retry-exhausted)))

  :doc "pending: retry budget exhaustion is terminal and still records only once"
  (test-mevedel-retry-policy--with-live
    (plist-put state :retries 10)
    (plist-put info :error '(:code "server_error"))
    (should-not (mevedel-gptel-retry--pending-p info))
    (should-not (mevedel-gptel-retry--pending-p info))
    (should-not (plist-get state :pending))
    (should (plist-get info :mevedel-retry-exhausted))
    (should (= 1 (length (plist-get state :errors))))
    (should (= 11 (plist-get (car (plist-get state :errors)) :attempt))))

  :doc "pending: permanent errors, absent errors and unowned requests remain terminal"
  (progn
    (test-mevedel-retry-policy--with-live
      (should-not (mevedel-gptel-retry--pending-p info))
      (plist-put info :http-status "503")
      (plist-put info :error '(:code "insufficient_quota"))
      (should-not (mevedel-gptel-retry--pending-p info))
      (should-not (plist-get state :errors))
      (plist-put info :error '(:code "server_error"))
      (setq-local mevedel--current-request nil)
      (should-not (mevedel-gptel-retry--pending-p info))
      (should-not (plist-get state :errors)))
    (should-not (mevedel-gptel-retry--pending-p '(:error (:code "server_error")))))

  :doc "pending: stale attempt tokens and cancelled or closed owners cannot accept errors"
  (test-mevedel-retry-policy--with-live
    (plist-put info :error '(:code "server_error"))
    (let ((stale (copy-sequence info)))
      (plist-put stale :mevedel-retry-attempt (gensym "stale-"))
      (should-not (mevedel-gptel-retry--pending-p stale)))
    (setf (mevedel-request-cancelled-p request) t)
    (should-not (mevedel-gptel-retry--pending-p info))
    (setf (mevedel-request-cancelled-p request) nil)
    (plist-put state :closed t)
    (should-not (mevedel-gptel-retry--pending-p info))
    (plist-put state :closed nil)
    (plist-put state :cancelled t)
    (should-not (mevedel-gptel-retry--pending-p info))
    (should-not (plist-get state :errors)))

  :doc "pending: prepended native INFO remains live through its matching attempt token"
  (test-mevedel-retry-policy--with-live
    (let ((prepended (cons :native-field (cons t (copy-sequence info)))))
      (should-not (eq prepended info))
      (plist-put prepended :error '(:code "server_error"))
      (should (mevedel-gptel-retry--pending-p prepended))
      (should (plist-get state :pending))
      (should (= 1 (length (plist-get state :errors))))))

  :doc "pending: cached permanent SSE refusal survives a later curl failure and HTTP503"
  (dolist (payload '("event: error\ndata: {\"error\":{\"code\":\"insufficient_quota\",\"message\":\"Quota exhausted\"}}\n\n"
                     "event: response.failed\ndata: {\"response\":{\"error\":{\"code\":\"invalid_request_error\",\"message\":\"Invalid prompt\"}}}\n\n"
                     "event: response.incomplete\ndata: {\"response\":{\"incomplete_details\":{\"reason\":\"content_filter\"}}}\n\n"))
    (test-mevedel-retry-policy--with-live
      (with-temp-buffer
        (insert payload)
        (goto-char (point-min))
        (mevedel-gptel-retry--parse-stream #'gptel-curl--parse-stream backend info))
      (let ((permanent (plist-get info :error)))
        (should permanent)
        (should (eq permanent (plist-get info :mevedel-retry-provider-error)))
        (plist-put info :error "Curl failed with exit code 56. Synthetic receive failure")
        (plist-put info :http-status "503")
        (should-not (mevedel-gptel-retry--pending-p info))
        (should (eq permanent (plist-get info :error)))
        (should-not (plist-get state :pending))
        (should-not (plist-get state :errors))))))

(mevedel-deftest mevedel-gptel-retry--status ()
  ,test
  (test)
  :doc "status: root retry progress preserves multiline quoted draft and input point"
  (test-mevedel-retry-policy--with-view
    (mevedel-gptel-retry--status state "Codex temporarily unavailable: retry 1/10")
    (with-current-buffer view-buf
      (should (equal "Codex temporarily unavailable: retry 1/10"
                     mevedel-view--spinner-status))
      (should (eq 'provider-retry mevedel-view--spinner-owner))
      (should (mevedel-view--request-progress-visible-p))
      (should (string-match-p "Codex temporarily unavailable: retry 1/10" (buffer-string)))
      (should (equal "> quoted\nsecond line" (mevedel-view--input-text)))
      (should (= 4 (- (point) (mevedel-view--input-start))))))

  :doc "status: replaced sample owner cannot revive stopped UI or disturb the composer"
  (test-mevedel-retry-policy--with-view
    (with-current-buffer view-buf (mevedel-view-stream-stop))
    (plist-put info :mevedel-retry-state (copy-sequence state))
    (with-current-buffer view-buf
      (let ((before (buffer-string))
            (generation mevedel-view--spinner-generation))
        (mevedel-gptel-retry--status state "Stale retry status")
        (should (equal before (buffer-string)))
        (should (= generation mevedel-view--spinner-generation))
        (should-not (mevedel-view--request-progress-visible-p))
        (should-not mevedel-view--spinner-timer)
        (should (equal "> quoted\nsecond line" (mevedel-view--input-text)))
        (should (= 4 (- (point) (mevedel-view--input-start)))))))

  :doc "status: retired attempt callback cannot revive root retry progress"
  (test-mevedel-retry-policy--with-view
    (let ((stale (copy-sequence info)) called)
      (plist-put stale :mevedel-retry-attempt (gensym "retired-retry-attempt-"))
      (with-current-buffer view-buf
        (mevedel-view-stream-stop)
        (let ((before (buffer-string))
              (generation mevedel-view--spinner-generation))
          (mevedel-gptel-retry--callback
           state stale
           (lambda (&rest _)
             (setq called t)
             (mevedel-gptel-retry--status state "Stale attempt status"))
           "old response" stale)
          (should-not called)
          (should (equal before (buffer-string)))
          (should (= generation mevedel-view--spinner-generation))
          (should-not (mevedel-view--request-progress-visible-p))
          (should-not mevedel-view--spinner-timer)
          (should (equal "> quoted\nsecond line" (mevedel-view--input-text)))
          (should (= 4 (- (point) (mevedel-view--input-start)))))))))

(mevedel-deftest mevedel-gptel-retry--parse-stream ()
  ,test
  (test)
  :doc "parser: fragmented HTTP200 nested error waits for complete native JSON"
  (test-mevedel-retry-policy--with-live
    (with-temp-buffer
      (insert "event: response.output_text.delta\ndata: {\"delta\":\"provisional\"}\n\n"
              "event: error\ndata: {\"error\":{\"code\":\"server_error\",\"message\":\"Over")
      (goto-char (point-min))
      (should (equal "provisional"
                     (mevedel-gptel-retry--parse-stream #'gptel-curl--parse-stream backend info)))
      (should (looking-at "event: error"))
      (should-not (plist-get info :error))
      (should-not (plist-get info :mevedel-retry-terminal))
      (save-excursion
        (goto-char (point-max))
        (insert "loaded\"}}\n\n"))
      (should (equal "" (mevedel-gptel-retry--parse-stream
                         #'gptel-curl--parse-stream backend info)))
      (should (equal '(:code "server_error" :message "Overloaded") (plist-get info :error)))
      (should (plist-get info :mevedel-retry-terminal))
      (should (mevedel-gptel-retry--pending-p info))
      (should (= 1 (length (plist-get state :errors))))))

  :doc "parser: flat capacity errors retry without Retry-After advice"
  (test-mevedel-retry-policy--with-live
    (with-temp-buffer
      (insert "event: error\ndata: {\"type\":\"error\",\"message\":\"Model is at capacity\"}\n\n")
      (goto-char (point-min))
      (should (equal "" (mevedel-gptel-retry--parse-stream
                         #'gptel-curl--parse-stream backend info))))
    (should (equal '(:type "error" :message "Model is at capacity") (plist-get info :error)))
    (should-not (plist-get info :mevedel-retry-advice))
    (should (mevedel-gptel-retry--pending-p info)))

  :doc "parser: SSE nested Retry-After headers and retry_after override jittered delay"
  (dolist (case '(("{\"code\":\"server_error\",\"headers\":{\"retry-after\":\"12.5\"}}" "12.5" 12.5)
                  ("{\"code\":\"server_error\",\"headers\":{\"Retry-After\":\"9\"}}" "9" 9)
                  ("{\"code\":\"server_error\",\"retry_after\":7}" 7 7)))
    (test-mevedel-retry-policy--with-live
      (with-temp-buffer
        (insert "event: error\ndata: {\"error\":" (car case) "}\n\n")
        (goto-char (point-min))
        (mevedel-gptel-retry--parse-stream #'gptel-curl--parse-stream backend info))
      (should (equal (cadr case) (plist-get info :mevedel-retry-advice)))
      (should (eq (plist-get info :error) (plist-get info :mevedel-retry-provider-error)))
      (should (mevedel-gptel-retry--pending-p info))
      (cl-letf (((symbol-function 'random)
                 (lambda (&rest _) (ert-fail "SSE server advice must bypass jitter"))))
        (should (= (caddr case)
                   (mevedel-gptel-retry--delay 1 (plist-get info :mevedel-retry-advice)))))))

  :doc "parser: SSE HTTP-date advice is honored and invalid advice falls back"
  (dolist (advice '("Wed, 21 Oct 2015 07:29:00 GMT" "not a date"))
    (test-mevedel-retry-policy--with-live
      (with-temp-buffer
        (insert "event: response.failed\ndata: {\"response\":{\"error\":{\"code\":\"server_error\","
                "\"headers\":{\"retry-after\":\"" advice "\"}}}}\n\n")
        (goto-char (point-min))
        (mevedel-gptel-retry--parse-stream #'gptel-curl--parse-stream backend info))
      (should (equal advice (plist-get info :mevedel-retry-advice)))
      (let ((now (float-time (date-to-time "Wed, 21 Oct 2015 07:28:00 GMT"))))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional time)
                     (if time (time-convert time 'integer) now)))
                  ((symbol-function 'random) (lambda (_limit) 100)))
          (should (= (if (equal advice "not a date") 1.0 60)
                     (mevedel-gptel-retry--delay 1 (plist-get info :mevedel-retry-advice))))))))

  :doc "parser: response.failed retains nested permanent error and native usage"
  (test-mevedel-retry-policy--with-live
    (with-temp-buffer
      (insert "event: response.failed\ndata: {\"response\":{\"error\":{\"code\":\"insufficient_quota\","
              "\"message\":\"Model at capacity\"},\"usage\":{\"input_tokens\":20,\"output_tokens\":3,"
              "\"input_tokens_details\":{\"cached_tokens\":5}}}}\n\n")
      (goto-char (point-min))
      (mevedel-gptel-retry--parse-stream #'gptel-curl--parse-stream backend info))
    (should (equal '(:code "insufficient_quota" :message "Model at capacity") (plist-get info :error)))
    (should (equal '(:input 15 :output 3 :cached 5) (plist-get info :tokens)))
    (should (plist-get info :mevedel-retry-terminal))
    (plist-put info :http-status "503")
    (should-not (mevedel-gptel-retry--pending-p info)))

  :doc "parser: response.failed without structured error defaults to transient server error"
  (test-mevedel-retry-policy--with-live
    (with-temp-buffer
      (insert "event: response.failed\ndata: {\"response\":{\"status\":\"failed\"}}\n\n")
      (goto-char (point-min))
      (mevedel-gptel-retry--parse-stream #'gptel-curl--parse-stream backend info))
    (should (equal "server_error" (plist-get (plist-get info :error) :code)))
    (should (mevedel-gptel-retry--pending-p info)))

  :doc "parser: incomplete interruption retries but output limit and content filter do not"
  (dolist (reason '("interrupted" "max_output_tokens" "content_filter"))
    (test-mevedel-retry-policy--with-live
      (with-temp-buffer
        (insert (format "event: response.incomplete\ndata: {\"response\":{\"incomplete_details\":{\"reason\":\"%s\"},\"usage\":{\"input_tokens\":8,\"output_tokens\":2}}}\n\n" reason))
        (goto-char (point-min))
        (mevedel-gptel-retry--parse-stream #'gptel-curl--parse-stream backend info))
      (should (equal (if (equal reason "interrupted") "stream_interrupted" reason)
                     (plist-get (plist-get info :error) :code)))
      (should (equal '(:input 8 :output 2 :cached 0) (plist-get info :tokens)))
      (should (plist-get info :mevedel-retry-terminal))
      (should (eq (equal reason "interrupted")
                  (not (null (mevedel-gptel-retry--pending-p info)))))))

  :doc "parser: native success preserves text, reasoning, stop reason and usage"
  (test-mevedel-retry-policy--with-live
    (with-temp-buffer
      (insert "event: response.reasoning_summary_text.delta\ndata: {\"delta\":\"thinking\"}\n\n"
              "event: response.output_text.delta\ndata: {\"delta\":\"answer\"}\n\n"
              "event: response.completed\ndata: {\"response\":{\"status\":\"completed\","
              "\"usage\":{\"input_tokens\":10,\"output_tokens\":2}}}\n\n")
      (goto-char (point-min))
      (should (equal "answer" (mevedel-gptel-retry--parse-stream
                              #'gptel-curl--parse-stream backend info))))
    (should (equal "thinking" (plist-get info :reasoning)))
    (should (equal "completed" (plist-get info :stop-reason)))
    (should (equal '(:input 10 :output 2 :cached 0) (plist-get info :tokens)))
    (should (plist-get info :mevedel-retry-terminal))
    (should-not (plist-get info :error))
    (should-not (mevedel-gptel-retry--pending-p info)))

  :doc "parser: unowned streams use native behavior and stale attempts do not parse"
  (test-mevedel-retry-policy--with-live
    (with-temp-buffer
      (insert "event: response.output_text.delta\ndata: {\"delta\":\"native\"}\n\n")
      (goto-char (point-min))
      (let ((unowned (list :backend backend)))
        (should (equal "native" (mevedel-gptel-retry--parse-stream
                                #'gptel-curl--parse-stream backend unowned)))
        (should-not (plist-get unowned :mevedel-retry-terminal)))
      (goto-char (point-min))
      (let ((stale (copy-sequence info)) (start (point)))
        (plist-put stale :mevedel-retry-attempt (gensym "stale-"))
        (should-not (mevedel-gptel-retry--parse-stream #'gptel-curl--parse-stream backend stale))
        (should (= start (point)))
        (should-not (plist-get stale :error))))))

(mevedel-deftest mevedel-gptel-retry--parse-response ()
  ,test
  (test)
  :doc "response parser: HTTP200 nested transient error is normalized before native parsing"
  (test-mevedel-retry-policy--with-live
    (plist-put info :stream nil)
    (should-not
     (mevedel-gptel-retry--parse-response
      #'gptel--parse-response backend
      '(:error (:code "server_error" :message "Synthetic overload")) info))
    (should (equal '(:code "server_error" :message "Synthetic overload")
                   (plist-get info :error)))
    (should (mevedel-gptel-retry--pending-p info)))

  :doc "response parser: permanent nested error overrides a transient transport status"
  (test-mevedel-retry-policy--with-live
    (plist-put info :http-status "503")
    (mevedel-gptel-retry--parse-response
     #'gptel--parse-response backend
     '(:error (:code "invalid_request_error" :message "Model at capacity")) info)
    (should (equal "invalid_request_error" (plist-get (plist-get info :error) :code)))
    (should-not (mevedel-gptel-retry--pending-p info)))

  :doc "response parser: successful native content and token usage are preserved"
  (test-mevedel-retry-policy--with-live
    (should
     (equal "answer"
            (mevedel-gptel-retry--parse-response
             #'gptel--parse-response backend
             '(:status "completed"
               :output [(:type "message" :content [(:type "output_text" :text "answer")])]
               :usage (:input_tokens 20 :output_tokens 3
                       :input_tokens_details (:cached_tokens 5)))
             info)))
    (should (equal "completed" (plist-get info :stop-reason)))
    (should (equal '(:input 15 :output 3 :cached 5) (plist-get info :tokens)))
    (should-not (plist-get info :error)))

  :doc "response parser: unowned and non-OAuth Responses backends retain native behavior"
  (test-mevedel-retry-policy--with-live
    (let ((unowned (list :backend backend)))
      (mevedel-gptel-retry--parse-response
       #'gptel--parse-response backend '(:error (:code "server_error")) unowned)
      (should-not (plist-get unowned :error)))
    (let ((other (gptel--make-openai-responses :name "retry-policy-non-oauth")))
      (mevedel-gptel-retry--parse-response
       #'gptel--parse-response other '(:error (:code "server_error")) info)
      (should-not (plist-get info :error)))))

(ert-deftest mevedel-gptel-retry--pending-p/permanent-overrides-earlier-pending ()
  (test-mevedel-retry-policy--with-live
    (plist-put info :error '(:code "server_error" :message "Model at capacity"))
    (should (mevedel-gptel-retry--pending-p info))
    (plist-put info :mevedel-retry-provider-error
               '(:code "insufficient_quota" :message "Model at capacity"))
    (plist-put info :http-status "503")
    (let ((terminal 0))
      (mevedel-gptel-retry--callback state info (lambda (&rest _) (cl-incf terminal)) t info)
      (should (= 1 terminal)))
    (should-not (plist-get state :pending))
    (should (plist-get info :mevedel-retry-permanent))
    (should-not (plist-get info :mevedel-retry-exhausted))))

(ert-deftest mevedel-gptel-retry--pending-p/diagnostic-scalars-are-bounded-strings ()
  (test-mevedel-retry-policy--with-live
    (plist-put info :error '(:code 500 :type "server_error" :message 500))
    (should (mevedel-gptel-retry--pending-p info))
    (let ((diagnostic (car (plist-get info :mevedel-retry-errors))))
      (should (equal "500" (plist-get diagnostic :code)))
      (should (equal "500" (plist-get diagnostic :message))))))

(provide 'test-mevedel-gptel-retry-policy)
;;; test-mevedel-gptel-retry-policy.el ends here
