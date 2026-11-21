;;; test-niwa-gptel-retry.el --- Native retry lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:
;; Local HTTP responses exercise real curl, gptel parsing, callbacks and FSMs.
;; No provider credentials or paid requests are used.

;;; Code:
(require 'gptel-retry-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "gptel-retry-helpers"))
(require 'niwa-view)
(require 'niwa-view-render)

(niwa-deftest niwa-gptel-retry-dispatch ()
  ,test
  (test)
  :quiet t
  :doc "HTTP200 failure recovers through real curl without repeating WAIT or output"
  (niwa-retry-test--with-http (list niwa-retry-test--failure niwa-retry-test--success)
    (gptel--fsm-transition fsm)
    (niwa-retry-test--drain-native)
    (should (= 2 (length requests)))
    (should (equal (car requests) (cadr requests)))
    (should (= 1 waits))
    (should (= 1 finished))
    (should (= 0 failed))
    (with-current-buffer data-buf
      (should (equal "Recovered answer\n" (buffer-string))))
    (should (equal '(:input 19 :output 5 :cached 0)
                   (plist-get (gptel-fsm-info fsm) :tokens-full))))

  :doc "all eleven physical failures exhaust once and retain attempt history"
  (niwa-retry-test--with-http (make-list 11 niwa-retry-test--failure)
    (gptel--fsm-transition fsm)
    (niwa-retry-test--drain-native)
    (should (= 11 (length requests)))
    (should (= 1 waits))
    (should (= 1 failed))
    (should (= 0 finished))
    (let ((info (gptel-fsm-info fsm)))
      (should (plist-get info :niwa-retry-exhausted))
      (should (equal '(:input 77 :output 22 :cached 0) (plist-get info :tokens)))
      (should (= 11 (length (plist-get info :niwa-retry-errors))))))

  :doc "tenth retry may succeed without exhausting the request"
  (niwa-retry-test--with-http
      (append (make-list 10 niwa-retry-test--failure) (list niwa-retry-test--success))
    (gptel--fsm-transition fsm)
    (niwa-retry-test--drain-native)
    (should (= 11 (length requests)))
    (should (= 1 finished))
    (should (equal '(:input 82 :output 23 :cached 0) (plist-get (gptel-fsm-info fsm) :tokens)))
    (should-not (plist-get (gptel-fsm-info fsm) :niwa-retry-exhausted)))

  :doc "HTTP 503 with a non-JSON error page recovers like other 5xx failures"
  (let ((page "<html><body>503 Service Unavailable</body></html>"))
    (niwa-retry-test--with-http
        (list (format "HTTP/1.1 503 Service Unavailable\r\nContent-Type: text/html\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                      (string-bytes page) page)
              niwa-retry-test--success)
      (gptel--fsm-transition fsm)
      (niwa-retry-test--drain-native)
      (should (= 2 (length requests)))
      (should (= 1 finished))
      (should (= 0 failed))
      (with-current-buffer data-buf
        (should (equal "Recovered answer\n" (buffer-string))))))

  :doc "zero disables automatic recovery but retains the prepared sample for explicit retry"
  (let ((niwa-gptel-retry-max-retries 0))
    (niwa-retry-test--with-http
        (list "event: error\ndata: {\"error\":{\"code\":\"server_error\"}}\n\n")
      (gptel--fsm-transition fsm)
      (niwa-retry-test--drain-native)
      (should (= 1 (length requests)))
      (should (= 1 failed))
      (should (plist-get (gptel-fsm-info fsm) :niwa-retry-state))
      (should-not (plist-get (gptel-fsm-info fsm) :niwa-retry-exhausted)))))

(niwa-deftest niwa-gptel-retry--cancel ()
  ,test
  (test)
  :quiet t
  :doc "cancellation during backoff suppresses captured timer and late chunks"
  (niwa-retry-test--with-http (list niwa-retry-test--failure)
    (gptel--fsm-transition fsm)
    (niwa-retry-test--drain-native t)
    (let* ((info (gptel-fsm-info fsm))
           (callback (plist-get info :callback))
           (entry (pop scheduled)))
      (niwa-request-cancel request)
      (should (= 1 aborted))
      (apply (nth 1 entry) (nth 2 entry))
      (funcall callback "Late chunk" info)
      (funcall callback t info)
      (should (= 1 (length requests)))
      (should-not (string-match-p "Late chunk" (with-current-buffer data-buf (buffer-string))))
      (should (= 0 finished))))

  :doc "buffer death cancels a transport-free retry"
  (niwa-retry-test--with-http (list niwa-retry-test--failure)
    (gptel--fsm-transition fsm)
    (niwa-retry-test--drain-native t)
    (let ((entry (pop scheduled)))
      (kill-buffer data-buf)
      (apply (nth 1 entry) (nth 2 entry))
      (should (= 1 aborted))
      (should (= 1 (length requests))))))

(niwa-deftest niwa-gptel-retry--transition ()
  ,test
  (test)
  :doc "cancelled native cleanup cannot advance beyond ABRT"
  (let* ((state (list :cancelled t))
         (fsm (gptel-make-fsm :state 'ABRT :info (list :niwa-retry-state state)))
         calls)
    (niwa-gptel-retry--transition (lambda (&rest args) (push args calls)) fsm)
    (should-not calls)
    (niwa-gptel-retry--transition (lambda (&rest args) (push args calls)) fsm 'ABRT)
    (should (= 1 (length calls)))))

(ert-deftest niwa-gptel-retry--cancel/exact-transport ()
  (let* ((buffer (generate-new-buffer " *retry-overlap*"))
         (old-process (make-pipe-process :name "retry-old" :noquery t))
         (new-process (make-pipe-process :name "retry-new" :noquery t))
         (old-request (niwa-request--create :id "old"))
         (new-request (niwa-request--create :id "new"))
         (old-aborts 0) (new-aborts 0) (old-callbacks 0)
         (old-fsm (gptel-make-fsm
                   :state 'TYPE :info (list :buffer buffer)
                   :handlers `((ABRT ,(lambda (_) (cl-incf old-aborts))))))
         (new-fsm (gptel-make-fsm
                   :state 'TYPE :info (list :buffer buffer)
                   :handlers `((ABRT ,(lambda (_) (cl-incf new-aborts))))))
         (state (list :fsm old-fsm :request old-request))
         (niwa-gptel-retry--states (list state))
         (gptel--request-alist
          (list (cons new-process (cons new-fsm (lambda () (delete-process new-process))))
                (cons old-process (cons old-fsm (lambda () (delete-process old-process)))))))
    (unwind-protect
        (progn
          (with-current-buffer buffer (setq-local niwa--current-request new-request))
          (plist-put (gptel-fsm-info old-fsm) :niwa-retry-state state)
          (niwa-gptel-retry--cancel state)
          (should-not (process-live-p old-process))
          (should (process-live-p new-process))
          (should (= 1 old-aborts))
          (should (= 0 new-aborts))
          (should (eq 'TYPE (gptel-fsm-state new-fsm)))
          (should (= 1 (length gptel--request-alist)))
          (should (eq new-process (caar gptel--request-alist)))
          (niwa-gptel-retry--callback state (gptel-fsm-info old-fsm)
                                        (lambda (&rest _) (cl-incf old-callbacks))
                                        t (gptel-fsm-info old-fsm))
          (should (= 0 old-callbacks)))
      (delete-process old-process)
      (delete-process new-process)
      (kill-buffer buffer))))

(ert-deftest niwa-gptel-retry--rollback/streamed-partial-and-concurrent-audit ()
  (let* ((split (string-match "event: response.failed" niwa-retry-test--failure))
         (chunks (list (substring niwa-retry-test--failure 0 split)
                       (substring niwa-retry-test--failure split)))
         partial-seen)
    (niwa-retry-test--with-http (list chunks niwa-retry-test--success)
      (plist-put (gptel-fsm-info fsm) :callback
                 (lambda (response info &optional raw)
                   (gptel-curl--stream-insert-response response info raw)
                   (when (equal response "Abandoned output")
                     (setq partial-seen t)
                     (with-current-buffer data-buf
                       (goto-char (point-max))
                       (insert (propertize "Concurrent notice\n" 'gptel 'ignore))))))
      (gptel--fsm-transition fsm)
      (niwa-retry-test--drain-native)
      (should partial-seen)
      (should (= 1 finished))
      (with-current-buffer data-buf
        (should (string-match-p "Recovered answer" (buffer-string)))
        (should (string-match-p "Concurrent notice" (buffer-string)))
        (should-not (string-match-p "Abandoned output" (buffer-string)))))))

(ert-deftest niwa-gptel-retry-dispatch/null-error-completes-successfully ()
  "Codex marks success with an explicit `\"error\": null'; that is no failure."
  (niwa-retry-test--with-http
      (list (concat "event: response.output_text.delta\n"
                    "data: {\"delta\":\"Fine answer\"}\n\n"
                    "event: response.completed\n"
                    "data: {\"response\":{\"status\":\"completed\",\"error\":null,"
                    "\"usage\":{\"input_tokens\":4,\"output_tokens\":2}}}\n\n"))
    (gptel--fsm-transition fsm)
    (niwa-retry-test--drain-native)
    (should (= 1 (length requests)))
    (should (= 1 finished))
    (should (= 0 failed))
    (should-not (plist-get (gptel-fsm-info fsm) :error))
    (with-current-buffer data-buf
      (should (string-match-p "Fine answer" (buffer-string))))))

(ert-deftest niwa-gptel-retry--parse-response/ignores-null-error ()
  (let ((info (list :niwa-retry-state (list :retries 0)))
        (backend (gptel--make-openai-oauth :name "Codex test" :models '(test-model))))
    (should (equal "parsed"
                   (niwa-gptel-retry--parse-response
                    (lambda (_backend _response _info) "parsed") backend '(:error :null) info)))
    (should-not (plist-get info :error))
    (niwa-gptel-retry--parse-response
     #'ignore backend '(:error (:code "server_error")) info)
    (should (equal '(:code "server_error") (plist-get info :error)))))

(ert-deftest niwa-gptel-retry-dispatch/completed-tools-are-not-replayed ()
  (let ((tool-runs 0)
        (tool-response
         (concat "event: response.output_item.done\n"
                 "data: {\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"SideEffect\",\"arguments\":\"{}\"}}\n\n"
                 "event: response.completed\n"
                 "data: {\"response\":{\"status\":\"completed\"}}\n\n")))
    (niwa-retry-test--with-http
        (list tool-response niwa-retry-test--failure niwa-retry-test--success)
      (plist-put (gptel-fsm-info fsm) :tools
                 (list (gptel-make-tool :name "SideEffect" :description "Count a side effect"
                                        :args nil :function (lambda () (cl-incf tool-runs) "Done"))))
      (gptel--fsm-transition fsm)
      (niwa-retry-test--drain-native)
      (should (= 3 (length requests)))
      (should (= 2 waits))
      (should (= 1 tool-runs))
      (should (= 1 finished))
      (should (equal (car requests) (cadr requests)))
      (dolist (entry transports)
        (should (eq (cdr entry)
                    (plist-get (process-get (car entry) 'niwa-retry-info) :niwa-retry-attempt))))
      (should (string-match-p "function_call_output" (car requests))))))

(ert-deftest niwa-gptel-retry-dispatch/async-tool-results-remain-visible ()
  "HTTP cleanup must not discard tools that settle after the stream ends."
  (dolist (agent-p '(nil t))
    (let ((tool-callback nil)
          (agent-results nil)
          (gptel-include-tool-results t)
          (tool-response
           (concat "event: response.output_item.done\n"
                   "data: {\"item\":{\"type\":\"function_call\",\"call_id\":\"async-1\",\"name\":\"Deferred\",\"arguments\":\"{}\"}}\n\n"
                   "event: response.completed\n"
                   "data: {\"response\":{\"status\":\"completed\",\"error\":null}}\n\n")))
      (niwa-retry-test--with-http
          (list tool-response niwa-retry-test--failure niwa-retry-test--success)
        (when agent-p
          (let ((invocation (niwa-agent-invocation--create
                             :path "/root/test" :buffer data-buf
                             :agent (niwa-agent--create :name "explorer"))))
            (with-current-buffer data-buf
              (setq-local niwa--agent-invocation invocation))
            (plist-put (gptel-fsm-info fsm) :niwa-agent-invocation invocation)
            (plist-put (gptel-fsm-info fsm) :callback
                       (niwa-agent-exec--wrap-callback
                        (niwa-agent-exec--make-callback
                         (lambda (result) (push result agent-results))
                         "explorer" "Async tool test"
                         (plist-get (gptel-fsm-info fsm) :position) (list ""))
                        'unavailable))))
        (plist-put (gptel-fsm-info fsm) :tools
                   (list (gptel-make-tool
                          :name "Deferred" :description "Return after HTTP cleanup"
                          :args nil :async t
                          :function (lambda (callback) (setq tool-callback callback)))))
        (gptel--fsm-transition fsm)
        (let ((deadline (+ (float-time) 10)))
          (while (and (< (float-time) deadline)
                      (or (not tool-callback) gptel--request-alist))
            (accept-process-output nil 0.01))
          (should (< (float-time) deadline)))
        (should (eq (gptel-fsm-state fsm) 'TOOL))
        (should (= 1 (length requests)))
        (should-not agent-results)
        ;; Deliver only after the physical transport's sentinel has returned.
        (let* ((old-info (gptel-fsm-info fsm))
               (old-callback (plist-get old-info :callback)))
          (funcall tool-callback "Deferred output")
          (niwa-retry-test--drain-native)
          (let ((text (with-current-buffer data-buf (buffer-string))))
            (funcall old-callback "Retired sample output" old-info)
            (should (equal text (with-current-buffer data-buf (buffer-string))))))
        (should (= 3 (length requests)))
        (should (= 1 finished))
        (when agent-p (should (equal '("Recovered answer") agent-results)))
        (with-current-buffer data-buf
          (goto-char (point-min))
          (should (search-forward "Deferred output" nil t))
          (should (equal (get-text-property (1- (point)) 'gptel)
                         '(tool . "async-1"))))
        (let ((segments (with-current-buffer data-buf
                          (cl-remove-if-not
                           (lambda (segment) (eq (car segment) 'tool))
                           (niwa-transcript-segments (point-min) (point-max))))))
          (should (= 1 (length segments)))
          (with-temp-buffer
            (setq-local niwa--data-buffer data-buf)
            (niwa-view-render-initialize)
            (niwa-view--render-tool-activity segments data-buf)
            (should (string-match-p "Deferred" (buffer-string)))))))))

(ert-deftest niwa-gptel-retry-dispatch/agent-result-and-accumulator ()
  (let* ((split (string-match "event: response.failed" niwa-retry-test--failure))
         (chunks (list (substring niwa-retry-test--failure 0 split)
                       (substring niwa-retry-test--failure split)))
         (partial (list "Agent prefix: ")) (results nil))
    (niwa-retry-test--with-http (list chunks niwa-retry-test--success)
      (let* ((inv (niwa-agent-invocation--create
                   :path "/root/test" :buffer data-buf
                   :agent (niwa-agent--create :name "explorer")))
             (callback (niwa-agent-exec--make-callback
                        (lambda (result) (push result results))
                        "explorer" "Retry test" (plist-get (gptel-fsm-info fsm) :position) partial)))
        (with-current-buffer data-buf (setq-local niwa--agent-invocation inv))
        (plist-put (gptel-fsm-info fsm) :niwa-agent-invocation inv)
        (plist-put (gptel-fsm-info fsm) :callback
                   (niwa-agent-exec--wrap-callback callback 'unavailable))
        (gptel--fsm-transition fsm)
        (niwa-retry-test--drain-native)
        (should (equal '("Recovered answer") results))
        (should (equal "Agent prefix: Recovered answer" (car partial)))
        (should (= 1 waits))
        (should (= 1 finished))
        (should (= 0 failed))))))

(ert-deftest niwa-gptel-retry-dispatch/independent-root-and-child ()
  (niwa-retry-test--with-http
      (list niwa-retry-test--failure niwa-retry-test--failure niwa-retry-test--success)
    (let* ((child-buffer (generate-new-buffer " *retry-child*"))
           (child-request (niwa-request--create :id "child" :session session))
           (child (gptel-copy-fsm fsm)))
      (unwind-protect
          (progn
            (with-current-buffer child-buffer
              (setq-local niwa--current-request child-request))
            (setf (gptel-fsm-info child)
                  (list :buffer child-buffer :backend backend :model 'test-model :stream t
                        :position (with-current-buffer child-buffer (point-max-marker))
                        :data '(:model "test-model" :input [] :stream t)
                        :callback #'gptel-curl--stream-insert-response))
            (gptel--fsm-transition fsm)
            (gptel--fsm-transition child)
            (let ((deadline (+ (float-time) 10)))
              (while (and (< (float-time) deadline)
                          (not (and (eq (gptel-fsm-state fsm) 'MRETRY)
                                    (eq (gptel-fsm-state child) 'MRETRY))))
                (accept-process-output nil 0.01))
              (should (< (float-time) deadline)))
            (let ((child-state (plist-get (gptel-fsm-info child) :niwa-retry-state)))
              (should-not (eq child-state (plist-get (gptel-fsm-info fsm) :niwa-retry-state)))
              (niwa-request-cancel request)
              (should (eq (gptel-fsm-state child) 'MRETRY))
              (should (= 0 (plist-get child-state :retries)))
              (let ((deadline (+ (float-time) 10)))
                (while (and (< (float-time) deadline) (not (eq (gptel-fsm-state child) 'DONE)))
                  (when-let* ((entry (pop scheduled)))
                    (cancel-timer (car entry))
                    (apply (nth 1 entry) (nth 2 entry)))
                  (accept-process-output nil 0.01))
                (should (< (float-time) deadline)))
              (should (= 3 (length requests)))
              (should (= 1 finished))
              (should (= 1 aborted))))
        (kill-buffer child-buffer)))))

(ert-deftest niwa-gptel-retry--cleanup/usage-observer-install-order ()
  (let (events)
    (niwa-retry-test--with-http
        (list "event: response.output_text.delta\ndata: {\"delta\":\"Truncated\"}\n\n"
              niwa-retry-test--success)
      (unwind-protect
          (progn
            (niwa-gptel-stream-bridge-install)
            (niwa-telemetry-usage-install)
            ;; Reinstall after the normal bridge/observer startup order.
            (niwa-gptel-retry-install)
            (cl-letf (((symbol-function 'niwa-telemetry-usage--emit)
                       (lambda (call stage &rest props)
                         (push (append (list :stage stage)
                                       (plist-get call :identity) props) events))))
              (gptel--fsm-transition fsm)
              (niwa-retry-test--drain-native))
            (let ((starts (cl-remove-if-not (lambda (event) (eq (plist-get event :stage) 'start)) events))
                  (finishes (reverse (cl-remove-if-not
                                      (lambda (event) (eq (plist-get event :stage) 'finish)) events))))
              (should (= 2 (length starts)))
              (should (= 2 (length finishes)))
              (should-not (equal (plist-get (car starts) :call-id) (plist-get (cadr starts) :call-id)))
              (should (eq 'error (plist-get (car finishes) :outcome)))
              (should (eq 'missing (plist-get (car finishes) :usage-status)))
              (should-not (plist-get (car finishes) :input-tokens))
              (should (eq 'success (plist-get (cadr finishes) :outcome)))))
        (niwa-telemetry-usage-uninstall)
        (niwa-gptel-stream-bridge-uninstall)))))

(ert-deftest niwa-gptel-retry--converter/retry-disposes-native-org-state ()
  (niwa-retry-test--with-http (list niwa-retry-test--failure niwa-retry-test--success)
    (with-current-buffer data-buf
      (org-mode)
      (setq-local niwa--current-request request)
      (setq-local niwa--session session))
    (let ((gptel-org-convert-response t))
      (setf (gptel-fsm-handlers fsm)
            (cons (list 'DONE
                        (lambda (machine)
                          (cl-incf finished)
                          (let ((info (gptel-fsm-info machine)))
                            (with-current-buffer data-buf
                              (run-hook-with-args 'gptel-post-response-functions
                                                  (marker-position (plist-get info :position))
                                                  (marker-position (plist-get info :tracking-marker)))))))
                  (assq-delete-all 'DONE (gptel-fsm-handlers fsm))))
      (gptel--fsm-transition fsm)
      (niwa-retry-test--drain-native)
      (should (= 1 finished))
      (should-not gptel-post-response-functions))))

(ert-deftest niwa-gptel-retry-dispatch/nonstream-physical-usage-is-not-totalized ()
  "Physical usage observers see each attempt, not the logical sample total."
  (dolist (curl '(nil t))
    (let (events)
      (niwa-retry-test--with-http
          (list "{\"error\":{\"code\":\"server_error\"},\"usage\":{\"input_tokens\":7,\"output_tokens\":2}}"
                "{\"status\":\"completed\",\"output\":[{\"type\":\"message\",\"content\":[{\"type\":\"output_text\",\"text\":\"Recovered answer\"}]}],\"usage\":{\"input_tokens\":12,\"output_tokens\":3}}")
        (let ((gptel-use-curl curl) (url-proxy-services nil))
          (plist-put (gptel-fsm-info fsm) :stream nil)
          (plist-put (gptel-fsm-info fsm) :callback #'gptel--insert-response)
          (unwind-protect
              (progn
                (niwa-telemetry-usage-install)
                (cl-letf (((symbol-function 'niwa-telemetry-usage--emit)
                           (lambda (_call stage &rest props)
                             (when (eq stage 'finish) (push props events)))))
                  (gptel--fsm-transition fsm)
                  (niwa-retry-test--drain-native))
                (setq events (nreverse events))
                (should (= 2 (length events)))
                (should (equal '(error success) (mapcar (lambda (event) (plist-get event :outcome)) events)))
                (should (equal '(7 12) (mapcar (lambda (event) (plist-get event :input-tokens)) events)))
                (should (equal '(2 3) (mapcar (lambda (event) (plist-get event :output-tokens)) events)))
                (should (equal '(:input 19 :output 5 :cached 0)
                               (plist-get (gptel-fsm-info fsm) :tokens))))
            (niwa-telemetry-usage-uninstall)))))))

(niwa-deftest niwa-gptel-retry--sum-tokens ()
  ,test
  (test)
  :doc "reported usage: absent reports stay absent"
  (should-not (niwa-gptel-retry--sum-tokens nil nil))
  :doc "reported usage: sum only reported fields, preserving zero and sparse data"
  (let ((previous '(:input 5 :output 2)))
    (should (equal '(:input 12 :output 2 :cached 0)
                   (niwa-gptel-retry--sum-tokens previous '(:input 7 :cached 0))))
    (should (equal '(:input 5 :output 2) previous))))

(niwa-deftest niwa-gptel-retry--close ()
  ,test
  (test)
  :doc "accepted completion releases snapshots and registry without cancellation"
  (let* ((state (list :data '(:input [])))
         (niwa-gptel-retry--states (list state)))
    (niwa-gptel-retry--close state)
    (should (plist-get state :closed))
    (should-not (plist-get state :data))
    (should-not (plist-get state :cancelled))
    (should-not niwa-gptel-retry--states)))

(ert-deftest niwa-gptel-retry--send/late-registration-after-cancellation ()
  (let* ((buffer (generate-new-buffer " *retry-late-register*"))
         (process (make-pipe-process :name "retry-late" :noquery t))
         (request (niwa-request--create :id "late"))
         (aborts 0)
         (fsm (gptel-make-fsm :state 'WAIT :info (list :buffer buffer)
                              :handlers `((ABRT ,(lambda (_) (cl-incf aborts))))))
         (state (list :fsm fsm :request request :callback #'ignore))
         (gptel--request-alist nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer (setq-local niwa--current-request request))
          (plist-put (gptel-fsm-info fsm) :niwa-retry-state state)
          ;; Inject the native registration gap, leaving actual retirement real.
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_)
                       (niwa-gptel-retry--cancel state)
                       (push (cons process (cons fsm (lambda () (delete-process process))))
                             gptel--request-alist))))
            (niwa-gptel-retry--send fsm))
          (should-not (process-live-p process))
          (should-not gptel--request-alist)
          (should (= 1 aborts)))
      (delete-process process)
      (kill-buffer buffer))))

(ert-deftest niwa-gptel-retry--rollback/exhaustion-removes-final-partial ()
  (let* ((split (string-match "event: response.failed" niwa-retry-test--failure))
         (chunks (list (substring niwa-retry-test--failure 0 split)
                       (substring niwa-retry-test--failure split))))
    (niwa-retry-test--with-http (make-list 11 chunks)
      (gptel--fsm-transition fsm)
      (niwa-retry-test--drain-native)
      (should (= 1 failed))
      (should (= 11 (length requests)))
      (should-not (string-match-p "Abandoned output" (with-current-buffer data-buf (buffer-string)))))))

(ert-deftest niwa-gptel-retry--tag-output/notices-inside-native-hooks ()
  (dolist (at-response-boundary '(nil t))
    (let* ((split (string-match "event: response.failed" niwa-retry-test--failure))
           (chunks (list (substring niwa-retry-test--failure 0 split)
                         (substring niwa-retry-test--failure split)))
           appended)
      (niwa-retry-test--with-http (list chunks niwa-retry-test--success)
        (let ((gptel-post-stream-hook
               (list (lambda ()
                       (unless appended
                         (setq appended t)
                         (save-excursion
                           (unless at-response-boundary (goto-char (point-max)))
                           (let ((start (point)))
                             (insert (propertize "Canonical notice\n" 'gptel 'ignore))
                             (should-not (get-text-property start 'niwa-retry-output)))))))))
          (gptel--fsm-transition fsm)
          (niwa-retry-test--drain-native))
        (should appended)
        (with-current-buffer data-buf
          (should (string-match-p "Canonical notice" (buffer-string)))
          (should (string-match-p "Recovered answer" (buffer-string)))
          (should-not (string-match-p "Abandoned output" (buffer-string))))))))

(ert-deftest niwa-gptel-retry-dispatch/vector-checkpoint-is-independent ()
  (niwa-retry-test--with-http (list niwa-retry-test--failure niwa-retry-test--success)
    (plist-put (plist-get (gptel-fsm-info fsm) :data) :input
               (vector (list :role "user" :content "Frozen input")))
    (gptel--fsm-transition fsm)
    (plist-put (aref (plist-get (plist-get (gptel-fsm-info fsm) :data) :input) 0)
               :content "Abandoned mutation")
    (niwa-retry-test--drain-native)
    (should (= 2 (length requests)))
    (should (equal (car requests) (cadr requests)))
    (should (string-match-p "Frozen input" (car requests)))
    (should-not (string-match-p "Abandoned mutation" (car requests)))))

(ert-deftest niwa-gptel-retry-dispatch/cancelled-admission-does-not-send ()
  (niwa-retry-test--with-http (list niwa-retry-test--success)
    (setf (niwa-request-cancelled-p request) t)
    (gptel--fsm-transition fsm)
    (should-not requests)
    (should-not gptel--request-alist)
    (should (= 1 aborted))
    (should (eq 'ABRT (gptel-fsm-state fsm)))))

(provide 'test-niwa-gptel-retry)

;;; test-niwa-gptel-retry.el ends here
