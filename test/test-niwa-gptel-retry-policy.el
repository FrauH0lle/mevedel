;;; test-niwa-gptel-retry-policy.el --- Native retry policy tests -*- lexical-binding: t -*-

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
(require 'niwa-gptel-retry)
(require 'gptel-openai-oauth)
(require 'niwa-view)
(require 'niwa-view-stream)

(defmacro test-niwa-retry-policy--with-live (&rest body)
  "Run BODY with an admitted request, OAuth backend and owned attempt.
Bind request, backend, fsm, state and info for assertions."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let* ((request (niwa-request--create :id "retry-policy-test"))
            (backend (gptel--make-openai-oauth :name "retry-policy-test"))
            (info (list :buffer (current-buffer) :backend backend
                        :http-status "200" :error nil :stream t
                        :niwa-retry-attempt (gensym "retry-policy-attempt-")))
            (fsm (gptel-make-fsm :info info :state 'TYPE))
            (state (list :fsm fsm :request request :retries 0 :limit 10)))
       ;; Admission is represented by the buffer's current request identity;
       ;; turn preparation and transport are outside this unit fixture.
       (setq-local niwa--current-request request)
       (plist-put info :niwa-retry-state state)
       ,@body)))

(defmacro test-niwa-retry-policy--with-view (&rest body)
  "Run BODY with a real paired view and a multiline composer draft."
  (declare (indent 0) (debug t))
  `(let ((niwa-user-dir
          (file-name-as-directory (make-temp-file "niwa-retry-view-user-" t)))
         (niwa-plugin-extra-roots nil)
         (niwa-permission-mode 'ask))
     (unwind-protect
         (test-niwa-retry-policy--with-live
           (let ((data-buf (current-buffer))
                 (view-buf (generate-new-buffer " *niwa-retry-policy-view*")))
             (unwind-protect
                 (progn
                   (niwa--transcript-org-mode)
                   (setq-local niwa--current-request request)
                   (setq-local niwa--session nil)
                   (setq-local niwa-view--abort-function
                               #'niwa-view-test--abort-interactions)
                   (niwa-view--setup view-buf data-buf)
                   (with-current-buffer view-buf
                     (niwa-view-stream-begin-turn
                      (niwa-view-transcript-tail-marker)
                      (with-current-buffer data-buf (copy-marker (point-max))))
                     (niwa-view-test--insert-composer-draft
                      "> quoted\nsecond line" 4))
                   ,@body)
               (when (buffer-live-p view-buf)
                 (with-current-buffer view-buf (niwa-view-stream-stop))
                 (kill-buffer view-buf))
               (setq-local niwa--current-request nil))))
       (delete-directory niwa-user-dir t))))

(niwa-deftest niwa-gptel-retry--retryable-p ()
  ,test
  (test)
  :doc "policy: transient structured codes retry even with HTTP200"
  (dolist (code '("server_error" "internal_server_error" "overloaded_error"
                  "server_is_overloaded" "model_at_capacity" "model_capacity_exceeded"
                  "service_unavailable_error" "rate_limit_exceeded" "rate_limit_error" "slow_down"
                  "stream_interrupted" "timeout_error"))
    (should (niwa-gptel-retry--retryable-p (list :code code) "200"))
    (should (niwa-gptel-retry--retryable-p (list :type code) "200")))

  :doc "policy: transient HTTP and recognized curl failures retry"
  (progn
    (dolist (status '("408" "429" "500" "502" "503" "504" "599"))
      (should (niwa-gptel-retry--retryable-p "Synthetic transport failure" status)))
    (dolist (code '(6 7 18 28 35 52 55 56 92))
      (should (niwa-gptel-retry--retryable-p
               (format "Curl failed with exit code %d. Synthetic failure" code) nil)))
    (should-not (niwa-gptel-retry--retryable-p "Curl failed with exit code 23." nil)))

  :doc "policy: capacity text needs no Retry-After and generic error type is neutral"
  (progn
    (dolist (message '("Model at capacity" "Model is at capacity"
                       "Temporarily overloaded" "Server is overloaded"))
      (should (niwa-gptel-retry--retryable-p message "200"))
      (should (niwa-gptel-retry--retryable-p
               (list :type "error" :message message) "200")))
    (should-not (niwa-gptel-retry--retryable-p
                 '(:code "unknown_error" :message "Model at capacity") "200")))

  :doc "policy: permanent structured codes override transient status and capacity text"
  (dolist (code '("invalid_request_error" "invalid_request" "invalid_prompt"
                  "authentication_error" "permission_error" "model_not_found"
                  "context_length_exceeded" "context_window_exceeded"
                  "insufficient_quota" "usage_limit_reached" "quota_exceeded"
                  "credit_balance_exhausted" "organization_spend_limit_exceeded"
                  "project_spend_limit_exceeded" "usage_not_included"
                  "content_filter" "max_output_tokens" "cancelled"))
    (should-not (niwa-gptel-retry--retryable-p
                 (list :code code :type "server_error" :message "Model at capacity") "503"))
    (should-not (niwa-gptel-retry--retryable-p
                 (list :type code :code "rate_limit_exceeded") "429")))

  :doc "policy: permanent HTTP and exhausted quota text override transient codes"
  (progn
    (dolist (status '("400" "401" "402" "403" "404" "413"))
      (should-not (niwa-gptel-retry--retryable-p '(:code "server_error") status)))
    (dolist (message '("Quota exhausted" "Usage limit reached" "Insufficient quota"))
      (should-not (niwa-gptel-retry--retryable-p
                   (list :code "rate_limit_exceeded" :message message) "429")))
    (should-not (niwa-gptel-retry--retryable-p '(:code "unknown_error") "200"))
    (should-not (niwa-gptel-retry--retryable-p nil nil))))

(niwa-deftest niwa-gptel-retry--delay ()
  ,test
  (test)
  :doc "delay: numeric server advice overrides backoff without jitter or a cap"
  (cl-letf (((symbol-function 'random)
             (lambda (&rest _) (ert-fail "Server advice must not use jitter"))))
    (dolist (advice '(0 2 2.5 "0" "2" "2.5" "120"))
      (should (= (if (stringp advice) (string-to-number advice) advice)
                 (niwa-gptel-retry--delay 3 advice)))))

  :doc "delay: HTTP date advice uses the clock and past dates clamp to zero"
  (let ((now (float-time (date-to-time "Wed, 21 Oct 2015 07:28:00 GMT"))))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional time)
                 (if time (time-convert time 'integer) now)))
              ((symbol-function 'random)
               (lambda (&rest _) (ert-fail "Date advice must not use jitter"))))
      (should (= 60 (niwa-gptel-retry--delay 1 "Wed, 21 Oct 2015 07:29:00 GMT")))
      (should (= 0 (niwa-gptel-retry--delay 5 "Wed, 21 Oct 2015 07:27:00 GMT")))))

  :doc "delay: absent and invalid advice use capped exponential backoff"
  (cl-letf (((symbol-function 'random) (lambda (_limit) 100)))
    (dolist (advice '(nil -1 "-1" "" "not a date" "NaN" "1 second"))
      (should (= 1.0 (niwa-gptel-retry--delay 1 advice)))
      (should (= 4.0 (niwa-gptel-retry--delay 3 advice))))
    (should (= 30.0 (niwa-gptel-retry--delay 10 nil))))

  :doc "delay: jitter stays within the documented backoff bounds"
  (progn
    (cl-letf (((symbol-function 'random) (lambda (_limit) 0)))
      (should (= 0.9 (niwa-gptel-retry--delay 1 nil)))
      (should (= 27.0 (niwa-gptel-retry--delay 10 nil))))
    (cl-letf (((symbol-function 'random) (lambda (_limit) 200)))
      (should (< (abs (- 1.1 (niwa-gptel-retry--delay 1 nil))) 0.000001))
      (should (= 33.0 (niwa-gptel-retry--delay 10 nil))))))

(niwa-deftest niwa-gptel-retry--pending-p ()
  ,test
  (test)
  :doc "pending: accepted failure records one diagnostic without consuming a retry"
  (test-niwa-retry-policy--with-live
    (plist-put info :error '(:type "error" :code "server_error" :message "Synthetic overload"))
    (should (niwa-gptel-retry--pending-p info))
    (should (niwa-gptel-retry--pending-p info))
    (should (plist-get state :pending))
    (should (= 0 (plist-get state :retries)))
    (should (equal '((:attempt 1 :http-status "200" :type "error"
                              :code "server_error" :message "Synthetic overload"))
                   (plist-get state :errors)))
    (should (eq (plist-get state :errors) (plist-get info :niwa-retry-errors)))
    (should-not (plist-get info :niwa-retry-exhausted)))

  :doc "pending: retry budget exhaustion is terminal and still records only once"
  (test-niwa-retry-policy--with-live
    (plist-put state :retries 10)
    (plist-put info :error '(:code "server_error"))
    (should-not (niwa-gptel-retry--pending-p info))
    (should-not (niwa-gptel-retry--pending-p info))
    (should-not (plist-get state :pending))
    (should (plist-get info :niwa-retry-exhausted))
    (should (= 1 (length (plist-get state :errors))))
    (should (= 11 (plist-get (car (plist-get state :errors)) :attempt))))

  :doc "pending: permanent errors, absent errors and unowned requests remain terminal"
  (progn
    (test-niwa-retry-policy--with-live
      (should-not (niwa-gptel-retry--pending-p info))
      (plist-put info :http-status "503")
      (plist-put info :error '(:code "insufficient_quota"))
      (should-not (niwa-gptel-retry--pending-p info))
      (should-not (plist-get state :errors))
      (plist-put info :error '(:code "server_error"))
      (setq-local niwa--current-request nil)
      (should-not (niwa-gptel-retry--pending-p info))
      (should-not (plist-get state :errors)))
    (should-not (niwa-gptel-retry--pending-p '(:error (:code "server_error")))))

  :doc "pending: stale attempt tokens and cancelled or closed owners cannot accept errors"
  (test-niwa-retry-policy--with-live
    (plist-put info :error '(:code "server_error"))
    (let ((stale (copy-sequence info)))
      (plist-put stale :niwa-retry-attempt (gensym "stale-"))
      (should-not (niwa-gptel-retry--pending-p stale)))
    (setf (niwa-request-cancelled-p request) t)
    (should-not (niwa-gptel-retry--pending-p info))
    (setf (niwa-request-cancelled-p request) nil)
    (plist-put state :closed t)
    (should-not (niwa-gptel-retry--pending-p info))
    (plist-put state :closed nil)
    (plist-put state :cancelled t)
    (should-not (niwa-gptel-retry--pending-p info))
    (should-not (plist-get state :errors)))

  :doc "pending: prepended native INFO remains live through its matching attempt token"
  (test-niwa-retry-policy--with-live
    (let ((prepended (cons :native-field (cons t (copy-sequence info)))))
      (should-not (eq prepended info))
      (plist-put prepended :error '(:code "server_error"))
      (should (niwa-gptel-retry--pending-p prepended))
      (should (plist-get state :pending))
      (should (= 1 (length (plist-get state :errors))))))

  :doc "pending: cached permanent SSE refusal survives a later curl failure and HTTP503"
  (dolist (payload '("event: error\ndata: {\"error\":{\"code\":\"insufficient_quota\",\"message\":\"Quota exhausted\"}}\n\n"
                     "event: response.failed\ndata: {\"response\":{\"error\":{\"code\":\"invalid_request_error\",\"message\":\"Invalid prompt\"}}}\n\n"
                     "event: response.incomplete\ndata: {\"response\":{\"incomplete_details\":{\"reason\":\"content_filter\"}}}\n\n"))
    (test-niwa-retry-policy--with-live
      (with-temp-buffer
        (insert payload)
        (goto-char (point-min))
        (niwa-gptel-retry--parse-stream #'gptel-curl--parse-stream backend info))
      (let ((permanent (plist-get info :error)))
        (should permanent)
        (should (eq permanent (plist-get info :niwa-retry-provider-error)))
        (plist-put info :error "Curl failed with exit code 56. Synthetic receive failure")
        (plist-put info :http-status "503")
        (should-not (niwa-gptel-retry--pending-p info))
        (should (eq permanent (plist-get info :error)))
        (should-not (plist-get state :pending))
        (should-not (plist-get state :errors))))))

(niwa-deftest niwa-gptel-retry--status ()
  ,test
  (test)
  :doc "status: root retry progress preserves multiline quoted draft and input point"
  (test-niwa-retry-policy--with-view
    (niwa-gptel-retry--status state "Codex temporarily unavailable: retry 1/10")
    (with-current-buffer view-buf
      (should (equal "Codex temporarily unavailable: retry 1/10"
                     niwa-view--spinner-status))
      (should (eq 'provider-retry niwa-view--spinner-owner))
      (should (string-search "Codex temporarily unavailable: retry 1/10"
                             (niwa-view-test--rail-text)))
      (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
      (should (= 4 (niwa-view-test--composer-point-offset)))))

  :doc "status: replaced sample owner cannot revive stopped UI or disturb the composer"
  (test-niwa-retry-policy--with-view
    (with-current-buffer view-buf (niwa-view-stream-stop))
    (plist-put info :niwa-retry-state (copy-sequence state))
    (with-current-buffer view-buf
      (let ((before (buffer-string))
            (rail (niwa-view-test--rail-text))
            (generation niwa-view--spinner-generation))
        (niwa-gptel-retry--status state "Stale retry status")
        (should (equal before (buffer-string)))
        (should (equal rail (niwa-view-test--rail-text)))
        (should (= generation niwa-view--spinner-generation))
        (should-not niwa-view--spinner-status)
        (should-not niwa-view--spinner-owner)
        (should-not niwa-view--spinner-timer)
        (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
        (should (= 4 (niwa-view-test--composer-point-offset))))))

  :doc "status: retired attempt callback cannot revive root retry progress"
  (test-niwa-retry-policy--with-view
    (let ((stale (copy-sequence info)) called)
      (plist-put stale :niwa-retry-attempt (gensym "retired-retry-attempt-"))
      (with-current-buffer view-buf
        (niwa-view-stream-stop)
        (let ((before (buffer-string))
              (rail (niwa-view-test--rail-text))
              (generation niwa-view--spinner-generation))
          (niwa-gptel-retry--callback
           state stale
           (lambda (&rest _)
             (setq called t)
             (niwa-gptel-retry--status state "Stale attempt status"))
           "old response" stale)
          (should-not called)
          (should (equal before (buffer-string)))
          (should (equal rail (niwa-view-test--rail-text)))
          (should (= generation niwa-view--spinner-generation))
          (should-not niwa-view--spinner-status)
          (should-not niwa-view--spinner-owner)
          (should-not niwa-view--spinner-timer)
          (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
          (should (= 4 (niwa-view-test--composer-point-offset)))))))

  :doc "status: recovered output restores the progress status the retry replaced"
  (test-niwa-retry-policy--with-view
    (with-current-buffer view-buf
      (niwa-view--update-spinner "Thinking..."))
    (niwa-gptel-retry--status state "Provider temporarily unavailable · retry 1/10 in 1s")
    (niwa-gptel-retry--status state "Retrying provider request…")
    (with-current-buffer view-buf
      (should (eq 'provider-retry niwa-view--spinner-owner)))
    (let (seen)
      (niwa-gptel-retry--callback
       state info (lambda (response &rest _) (push response seen)) "Recovered" info)
      (should (equal '("Recovered") seen)))
    (with-current-buffer view-buf
      (should (equal "Thinking..." niwa-view--spinner-status))
      (should (eq 'request niwa-view--spinner-owner))
      (should (string-search "Working" (niwa-view-test--rail-text)))
      (should-not (string-search "Retrying provider request"
                                 (niwa-view-test--rail-text)))
      (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
      (should (= 4 (niwa-view-test--composer-point-offset)))))

  :doc "status: recovered tool-only sample restores progress on entering TOOL"
  (test-niwa-retry-policy--with-view
    (with-current-buffer view-buf
      (niwa-view--update-spinner "Thinking..."))
    (niwa-gptel-retry--status state "Retrying provider request…")
    (niwa-gptel-retry--transition #'ignore fsm 'TOOL)
    (with-current-buffer view-buf
      (should (equal "Thinking..." niwa-view--spinner-status))
      (should (eq 'request niwa-view--spinner-owner))))

  :doc "status: a newer progress owner is not overwritten by retry recovery"
  (test-niwa-retry-policy--with-view
    (with-current-buffer view-buf
      (niwa-view--update-spinner "Thinking..."))
    (niwa-gptel-retry--status state "Retrying provider request…")
    (with-current-buffer view-buf
      (niwa-view--update-spinner "Hook running" 'hook))
    (niwa-gptel-retry--callback state info #'ignore "Recovered" info)
    (with-current-buffer view-buf
      (should (equal "Hook running" niwa-view--spinner-status))
      (should (eq 'hook niwa-view--spinner-owner))))

  :doc "status: recovery from an empty status releases rather than restores retry text"
  (test-niwa-retry-policy--with-view
    (with-current-buffer view-buf (niwa-view--stop-spinner))
    (niwa-gptel-retry--status state "Provider temporarily unavailable")
    (niwa-gptel-retry--status state "Retrying provider request")
    (niwa-gptel-retry--callback state info #'ignore "Recovered" info)
    (with-current-buffer view-buf
      (should-not (eq 'provider-retry niwa-view--spinner-owner))
      (should-not (string-match-p "Retrying provider request" (buffer-string)))
      (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
      (should (= 4 (niwa-view-test--composer-point-offset))))))

(niwa-deftest niwa-gptel-retry--parse-stream ()
  ,test
  (test)
  :doc "parser: fragmented HTTP200 nested error waits for complete native JSON"
  (test-niwa-retry-policy--with-live
    (with-temp-buffer
      (insert "event: response.output_text.delta\ndata: {\"delta\":\"provisional\"}\n\n"
              "event: error\ndata: {\"error\":{\"code\":\"server_error\",\"message\":\"Over")
      (goto-char (point-min))
      (should (equal "provisional"
                     (niwa-gptel-retry--parse-stream #'gptel-curl--parse-stream backend info)))
      (should (looking-at "event: error"))
      (should-not (plist-get info :error))
      (should-not (plist-get info :niwa-retry-terminal))
      (save-excursion
        (goto-char (point-max))
        (insert "loaded\"}}\n\n"))
      (should (equal "" (niwa-gptel-retry--parse-stream
                         #'gptel-curl--parse-stream backend info)))
      (should (equal '(:code "server_error" :message "Overloaded") (plist-get info :error)))
      (should (plist-get info :niwa-retry-terminal))
      (should (niwa-gptel-retry--pending-p info))
      (should (= 1 (length (plist-get state :errors))))))

  :doc "parser: flat capacity errors retry without Retry-After advice"
  (test-niwa-retry-policy--with-live
    (with-temp-buffer
      (insert "event: error\ndata: {\"type\":\"error\",\"message\":\"Model is at capacity\"}\n\n")
      (goto-char (point-min))
      (should (equal "" (niwa-gptel-retry--parse-stream
                         #'gptel-curl--parse-stream backend info))))
    (should (equal '(:type "error" :message "Model is at capacity") (plist-get info :error)))
    (should-not (plist-get info :niwa-retry-advice))
    (should (niwa-gptel-retry--pending-p info)))

  :doc "parser: SSE nested Retry-After headers and retry_after override jittered delay"
  (dolist (case '(("{\"code\":\"server_error\",\"headers\":{\"retry-after\":\"12.5\"}}" "12.5" 12.5)
                  ("{\"code\":\"server_error\",\"headers\":{\"Retry-After\":\"9\"}}" "9" 9)
                  ("{\"code\":\"server_error\",\"retry_after\":7}" 7 7)))
    (test-niwa-retry-policy--with-live
      (with-temp-buffer
        (insert "event: error\ndata: {\"error\":" (car case) "}\n\n")
        (goto-char (point-min))
        (niwa-gptel-retry--parse-stream #'gptel-curl--parse-stream backend info))
      (should (equal (cadr case) (plist-get info :niwa-retry-advice)))
      (should (eq (plist-get info :error) (plist-get info :niwa-retry-provider-error)))
      (should (niwa-gptel-retry--pending-p info))
      (cl-letf (((symbol-function 'random)
                 (lambda (&rest _) (ert-fail "SSE server advice must bypass jitter"))))
        (should (= (caddr case)
                   (niwa-gptel-retry--delay 1 (plist-get info :niwa-retry-advice)))))))

  :doc "parser: SSE HTTP-date advice is honored and invalid advice falls back"
  (dolist (advice '("Wed, 21 Oct 2015 07:29:00 GMT" "not a date"))
    (test-niwa-retry-policy--with-live
      (with-temp-buffer
        (insert "event: response.failed\ndata: {\"response\":{\"error\":{\"code\":\"server_error\","
                "\"headers\":{\"retry-after\":\"" advice "\"}}}}\n\n")
        (goto-char (point-min))
        (niwa-gptel-retry--parse-stream #'gptel-curl--parse-stream backend info))
      (should (equal advice (plist-get info :niwa-retry-advice)))
      (let ((now (float-time (date-to-time "Wed, 21 Oct 2015 07:28:00 GMT"))))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional time)
                     (if time (time-convert time 'integer) now)))
                  ((symbol-function 'random) (lambda (_limit) 100)))
          (should (= (if (equal advice "not a date") 1.0 60)
                     (niwa-gptel-retry--delay 1 (plist-get info :niwa-retry-advice))))))))

  :doc "parser: response.failed retains nested permanent error and native usage"
  (test-niwa-retry-policy--with-live
    (with-temp-buffer
      (insert "event: response.failed\ndata: {\"response\":{\"error\":{\"code\":\"insufficient_quota\","
              "\"message\":\"Model at capacity\"},\"usage\":{\"input_tokens\":20,\"output_tokens\":3,"
              "\"input_tokens_details\":{\"cached_tokens\":5}}}}\n\n")
      (goto-char (point-min))
      (niwa-gptel-retry--parse-stream #'gptel-curl--parse-stream backend info))
    (should (equal '(:code "insufficient_quota" :message "Model at capacity") (plist-get info :error)))
    (should (equal '(:input 15 :output 3 :cached 5) (plist-get info :tokens)))
    (should (plist-get info :niwa-retry-terminal))
    (plist-put info :http-status "503")
    (should-not (niwa-gptel-retry--pending-p info)))

  :doc "parser: response.failed without structured error defaults to transient server error"
  (test-niwa-retry-policy--with-live
    (with-temp-buffer
      (insert "event: response.failed\ndata: {\"response\":{\"status\":\"failed\"}}\n\n")
      (goto-char (point-min))
      (niwa-gptel-retry--parse-stream #'gptel-curl--parse-stream backend info))
    (should (equal "server_error" (plist-get (plist-get info :error) :code)))
    (should (niwa-gptel-retry--pending-p info)))

  :doc "parser: incomplete interruption retries but output limit and content filter do not"
  (dolist (reason '("interrupted" "max_output_tokens" "content_filter"))
    (test-niwa-retry-policy--with-live
      (with-temp-buffer
        (insert (format "event: response.incomplete\ndata: {\"response\":{\"incomplete_details\":{\"reason\":\"%s\"},\"usage\":{\"input_tokens\":8,\"output_tokens\":2}}}\n\n" reason))
        (goto-char (point-min))
        (niwa-gptel-retry--parse-stream #'gptel-curl--parse-stream backend info))
      (should (equal (if (equal reason "interrupted") "stream_interrupted" reason)
                     (plist-get (plist-get info :error) :code)))
      (should (equal '(:input 8 :output 2 :cached 0) (plist-get info :tokens)))
      (should (plist-get info :niwa-retry-terminal))
      (should (eq (equal reason "interrupted")
                  (not (null (niwa-gptel-retry--pending-p info)))))))

  :doc "parser: native success preserves text, reasoning, stop reason and usage"
  (test-niwa-retry-policy--with-live
    (with-temp-buffer
      (insert "event: response.reasoning_summary_text.delta\ndata: {\"delta\":\"thinking\"}\n\n"
              "event: response.output_text.delta\ndata: {\"delta\":\"answer\"}\n\n"
              "event: response.completed\ndata: {\"response\":{\"status\":\"completed\","
              "\"usage\":{\"input_tokens\":10,\"output_tokens\":2}}}\n\n")
      (goto-char (point-min))
      (should (equal "answer" (niwa-gptel-retry--parse-stream
                              #'gptel-curl--parse-stream backend info))))
    (should (equal "thinking" (plist-get info :reasoning)))
    (should (equal "completed" (plist-get info :stop-reason)))
    (should (equal '(:input 10 :output 2 :cached 0) (plist-get info :tokens)))
    (should (plist-get info :niwa-retry-terminal))
    (should-not (plist-get info :error))
    (should-not (niwa-gptel-retry--pending-p info)))

  :doc "parser: unowned streams use native behavior and stale attempts do not parse"
  (test-niwa-retry-policy--with-live
    (with-temp-buffer
      (insert "event: response.output_text.delta\ndata: {\"delta\":\"native\"}\n\n")
      (goto-char (point-min))
      (let ((unowned (list :backend backend)))
        (should (equal "native" (niwa-gptel-retry--parse-stream
                                #'gptel-curl--parse-stream backend unowned)))
        (should-not (plist-get unowned :niwa-retry-terminal)))
      (goto-char (point-min))
      (let ((stale (copy-sequence info)) (start (point)))
        (plist-put stale :niwa-retry-attempt (gensym "stale-"))
        (should-not (niwa-gptel-retry--parse-stream #'gptel-curl--parse-stream backend stale))
        (should (= start (point)))
        (should-not (plist-get stale :error))))))

(niwa-deftest niwa-gptel-retry--parse-response ()
  ,test
  (test)
  :doc "response parser: HTTP200 nested transient error is normalized before native parsing"
  (test-niwa-retry-policy--with-live
    (plist-put info :stream nil)
    (should-not
     (niwa-gptel-retry--parse-response
      #'gptel--parse-response backend
      '(:error (:code "server_error" :message "Synthetic overload")) info))
    (should (equal '(:code "server_error" :message "Synthetic overload")
                   (plist-get info :error)))
    (should (niwa-gptel-retry--pending-p info)))

  :doc "response parser: permanent nested error overrides a transient transport status"
  (test-niwa-retry-policy--with-live
    (plist-put info :http-status "503")
    (niwa-gptel-retry--parse-response
     #'gptel--parse-response backend
     '(:error (:code "invalid_request_error" :message "Model at capacity")) info)
    (should (equal "invalid_request_error" (plist-get (plist-get info :error) :code)))
    (should-not (niwa-gptel-retry--pending-p info)))

  :doc "response parser: successful native content and token usage are preserved"
  (test-niwa-retry-policy--with-live
    (should
     (equal "answer"
            (niwa-gptel-retry--parse-response
             #'gptel--parse-response backend
             '(:status "completed"
               :output [(:type "message" :content [(:type "output_text" :text "answer")])]
               :usage (:input_tokens 20 :output_tokens 3
                       :input_tokens_details (:cached_tokens 5)))
             info)))
    (should (equal "completed" (plist-get info :stop-reason)))
    (should (equal '(:input 15 :output 3 :cached 5) (plist-get info :tokens)))
    (should-not (plist-get info :error)))

  :doc "response parser: unowned requests bypass recovery; owned non-OAuth errors normalize"
  (test-niwa-retry-policy--with-live
    (let ((unowned (list :backend backend)))
      (niwa-gptel-retry--parse-response
       #'gptel--parse-response backend '(:error (:code "server_error")) unowned)
      (should-not (plist-get unowned :error)))
    (let ((other (gptel--make-openai-responses :name "retry-policy-non-oauth")))
      (niwa-gptel-retry--parse-response
       #'gptel--parse-response other '(:error (:code "server_error")) info)
      (should (equal '(:code "server_error") (plist-get info :error))))))

(niwa-deftest niwa-gptel-retry--totalize-usage ()
  ,test
  (test)
  :doc "terminal usage: totals are idempotent and absent reports remain incomplete"
  (let ((state (list :reported-tokens '(:input 7 :output 2)))
        (info (list :tokens '(:input 12 :output 3))))
    (niwa-gptel-retry--totalize-usage state info)
    (niwa-gptel-retry--totalize-usage state info)
    (should (equal '(:input 19 :output 5) (plist-get info :tokens)))
    (should-not (plist-get info :niwa-retry-usage-incomplete))
    (setq info (list :tokens nil))
    (niwa-gptel-retry--totalize-usage state info)
    (should (equal '(:input 7 :output 2) (plist-get info :tokens)))
    (should (plist-get info :niwa-retry-usage-incomplete)))
  :doc "terminal usage: a missing failed report remains incomplete after success"
  (let ((state (list :usage-incomplete t))
        (info (list :tokens '(:input 12))))
    (niwa-gptel-retry--totalize-usage state info)
    (should (equal '(:input 12) (plist-get info :tokens)))
    (should (plist-get info :niwa-retry-usage-incomplete))))

(ert-deftest niwa-gptel-retry--retryable-p/numeric-provider-status ()
  (dolist (code '(408 429 500 503))
    (should (niwa-gptel-retry--retryable-p (list :code code) "200")))
  (dolist (code '(400 401 402 403 404 413))
    (should-not (niwa-gptel-retry--retryable-p (list :code code) "503"))))

(niwa-deftest niwa-gptel-retry--parse-http ()
  ,test
  (test)
  :doc "HTTP advice: curl and URL signatures preserve native result, args and point"
  (dolist (url '(nil t))
    (with-temp-buffer
      (insert "HTTP/1.1 503 Unavailable\r\nRetry-After: 12.5\r\n\r\n{}")
      (goto-char (point-max))
      (let* ((info (list :niwa-retry-state t))
             (args (if url (list 'backend info) (list info)))
             (position (point)))
        (should (equal 'native
                       (apply #'niwa-gptel-retry--parse-http
                              (lambda (&rest received)
                                (should (equal args received))
                                (should (= position (point)))
                                'native)
                              args)))
        (should (= position (point)))
        (should (equal "12.5" (plist-get info :niwa-retry-advice))))))
  :doc "HTTP advice: body text cannot forge a retry header; unowned requests are native"
  (dolist (owned '(nil t))
    (with-temp-buffer
      (insert "HTTP/1.1 503 Unavailable\n\nretry-after: 999\n")
      (let ((info (list :niwa-retry-state owned)))
        (should (eq 'native (niwa-gptel-retry--parse-http
                             (lambda (_) 'native) info)))
        (should-not (plist-get info :niwa-retry-advice)))))
  :doc "HTTP advice: normalized in-band errors reach the native transport error result"
  (let* ((error-data '(:code "server_error"))
         (info (list :niwa-retry-state t :error error-data)))
    (with-temp-buffer
      (should (equal (list nil "200" "HTTP/1.1 200 OK" error-data)
                     (niwa-gptel-retry--parse-http
                      (lambda (_) '(nil "200" "HTTP/1.1 200 OK")) info))))))

(ert-deftest niwa-gptel-retry--pending-p/permanent-overrides-earlier-pending ()
  (test-niwa-retry-policy--with-live
    (plist-put info :error '(:code "server_error" :message "Model at capacity"))
    (should (niwa-gptel-retry--pending-p info))
    (plist-put info :niwa-retry-provider-error
               '(:code "insufficient_quota" :message "Model at capacity"))
    (plist-put info :http-status "503")
    (let ((terminal 0))
      (niwa-gptel-retry--callback state info (lambda (&rest _) (cl-incf terminal)) t info)
      (should (= 1 terminal)))
    (should-not (plist-get state :pending))
    (should (plist-get info :niwa-retry-permanent))
    (should-not (plist-get info :niwa-retry-exhausted))))

(ert-deftest niwa-gptel-retry--pending-p/diagnostic-scalars-are-bounded-strings ()
  (test-niwa-retry-policy--with-live
    (plist-put info :error '(:code 500 :type "server_error" :message 500))
    (should (niwa-gptel-retry--pending-p info))
    (let ((diagnostic (car (plist-get info :niwa-retry-errors))))
      (should (equal "500" (plist-get diagnostic :code)))
      (should (equal "500" (plist-get diagnostic :message))))))

(ert-deftest niwa-gptel-retry--json-error/native-null-and-false ()
  (dolist (empty '(nil :null :json-false))
    (should-not (niwa-gptel-retry--json-error empty)))
  (let ((error '(:code "server_error")))
    (should (eq error (niwa-gptel-retry--json-error error)))))

(ert-deftest niwa-gptel-retry--pending-p/null-codes-are-absent ()
  (test-niwa-retry-policy--with-live
    (plist-put info :error '(:type :null :code :null :message "Model at capacity"))
    (should (niwa-gptel-retry--pending-p info))
    (let ((diagnostic (car (plist-get info :niwa-retry-errors))))
      (should-not (plist-get diagnostic :type))
      (should-not (plist-get diagnostic :code)))))

(niwa-deftest niwa-gptel-retry--owner-p ()
  ,test
  (test)
  :doc "a root request owns its machine; without one, only the invocation's own FSM"
  (let* ((fsm (gptel-make-fsm))
         (other (gptel-make-fsm))
         (invocation (niwa-agent-invocation--create :runtime-fsm fsm)))
    (should (niwa-gptel-retry--owner-p fsm 'request nil))
    (should (niwa-gptel-retry--owner-p fsm nil invocation))
    (should-not (niwa-gptel-retry--owner-p other nil invocation))
    (should-not (niwa-gptel-retry--owner-p fsm nil nil))
    (should-not (niwa-gptel-retry--owner-p fsm nil 'not-an-invocation))))

(provide 'test-niwa-gptel-retry-policy)
;;; test-niwa-gptel-retry-policy.el ends here
