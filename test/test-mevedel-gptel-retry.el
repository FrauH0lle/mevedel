;;; test-mevedel-gptel-retry.el --- Native retry lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:
;; Local HTTP responses exercise real curl, gptel parsing, callbacks and FSMs.
;; No provider credentials or paid requests are used.

;;; Code:
(require 'gptel-retry-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "gptel-retry-helpers"))

(mevedel-deftest mevedel-gptel-retry-dispatch ()
  ,test
  (test)
  :quiet t
  :doc "HTTP200 failure recovers through real curl without repeating WAIT or output"
  (mevedel-retry-test--with-http (list mevedel-retry-test--failure mevedel-retry-test--success)
    (gptel--fsm-transition fsm)
    (mevedel-retry-test--drain-native)
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
  (mevedel-retry-test--with-http (make-list 11 mevedel-retry-test--failure)
    (gptel--fsm-transition fsm)
    (mevedel-retry-test--drain-native)
    (should (= 11 (length requests)))
    (should (= 1 waits))
    (should (= 1 failed))
    (should (= 0 finished))
    (let ((info (gptel-fsm-info fsm)))
      (should (plist-get info :mevedel-retry-exhausted))
      (should (equal '(:input 77 :output 22 :cached 0) (plist-get info :tokens)))
      (should (= 11 (length (plist-get info :mevedel-retry-errors))))))

  :doc "tenth retry may succeed without exhausting the request"
  (mevedel-retry-test--with-http
      (append (make-list 10 mevedel-retry-test--failure) (list mevedel-retry-test--success))
    (gptel--fsm-transition fsm)
    (mevedel-retry-test--drain-native)
    (should (= 11 (length requests)))
    (should (= 1 finished))
    (should (equal '(:input 82 :output 23 :cached 0) (plist-get (gptel-fsm-info fsm) :tokens)))
    (should-not (plist-get (gptel-fsm-info fsm) :mevedel-retry-exhausted)))

  :doc "zero disables recovery and leaves the native error path unchanged"
  (let ((mevedel-gptel-retry-max-retries 0))
    (mevedel-retry-test--with-http
        (list "event: error\ndata: {\"error\":{\"code\":\"server_error\"}}\n\n")
      (gptel--fsm-transition fsm)
      (mevedel-retry-test--drain-native)
      (should (= 1 (length requests)))
      (should (= 1 failed))
      (should-not (plist-get (gptel-fsm-info fsm) :mevedel-retry-state)))))

(mevedel-deftest mevedel-gptel-retry--cancel ()
  ,test
  (test)
  :quiet t
  :doc "cancellation during backoff suppresses captured timer and late chunks"
  (mevedel-retry-test--with-http (list mevedel-retry-test--failure)
    (gptel--fsm-transition fsm)
    (mevedel-retry-test--drain-native t)
    (let* ((info (gptel-fsm-info fsm))
           (callback (plist-get info :callback))
           (entry (pop scheduled)))
      (mevedel-request-cancel request)
      (should (= 1 aborted))
      (apply (nth 1 entry) (nth 2 entry))
      (funcall callback "Late chunk" info)
      (funcall callback t info)
      (should (= 1 (length requests)))
      (should-not (string-match-p "Late chunk" (with-current-buffer data-buf (buffer-string))))
      (should (= 0 finished))))

  :doc "buffer death cancels a transport-free retry"
  (mevedel-retry-test--with-http (list mevedel-retry-test--failure)
    (gptel--fsm-transition fsm)
    (mevedel-retry-test--drain-native t)
    (let ((entry (pop scheduled)))
      (kill-buffer data-buf)
      (apply (nth 1 entry) (nth 2 entry))
      (should (= 1 aborted))
      (should (= 1 (length requests))))))

(mevedel-deftest mevedel-gptel-retry--transition ()
  ,test
  (test)
  :doc "cancelled native cleanup cannot advance beyond ABRT"
  (let* ((state (list :cancelled t))
         (fsm (gptel-make-fsm :state 'ABRT :info (list :mevedel-retry-state state)))
         calls)
    (mevedel-gptel-retry--transition (lambda (&rest args) (push args calls)) fsm)
    (should-not calls)
    (mevedel-gptel-retry--transition (lambda (&rest args) (push args calls)) fsm 'ABRT)
    (should (= 1 (length calls)))))

(ert-deftest mevedel-gptel-retry--cancel/exact-transport ()
  (let* ((buffer (generate-new-buffer " *retry-overlap*"))
         (old-process (make-pipe-process :name "retry-old" :noquery t))
         (new-process (make-pipe-process :name "retry-new" :noquery t))
         (old-request (mevedel-request--create :id "old"))
         (new-request (mevedel-request--create :id "new"))
         (old-aborts 0) (new-aborts 0) (old-callbacks 0)
         (old-fsm (gptel-make-fsm
                   :state 'TYPE :info (list :buffer buffer)
                   :handlers `((ABRT ,(lambda (_) (cl-incf old-aborts))))))
         (new-fsm (gptel-make-fsm
                   :state 'TYPE :info (list :buffer buffer)
                   :handlers `((ABRT ,(lambda (_) (cl-incf new-aborts))))))
         (state (list :fsm old-fsm :request old-request))
         (mevedel-gptel-retry--states (list state))
         (gptel--request-alist
          (list (cons new-process (cons new-fsm (lambda () (delete-process new-process))))
                (cons old-process (cons old-fsm (lambda () (delete-process old-process)))))))
    (unwind-protect
        (progn
          (with-current-buffer buffer (setq-local mevedel--current-request new-request))
          (plist-put (gptel-fsm-info old-fsm) :mevedel-retry-state state)
          (mevedel-gptel-retry--cancel state)
          (should-not (process-live-p old-process))
          (should (process-live-p new-process))
          (should (= 1 old-aborts))
          (should (= 0 new-aborts))
          (should (eq 'TYPE (gptel-fsm-state new-fsm)))
          (should (= 1 (length gptel--request-alist)))
          (should (eq new-process (caar gptel--request-alist)))
          (mevedel-gptel-retry--callback state (gptel-fsm-info old-fsm)
                                        (lambda (&rest _) (cl-incf old-callbacks))
                                        t (gptel-fsm-info old-fsm))
          (should (= 0 old-callbacks)))
      (delete-process old-process)
      (delete-process new-process)
      (kill-buffer buffer))))

(ert-deftest mevedel-gptel-retry--rollback/streamed-partial-and-concurrent-audit ()
  (let* ((split (string-match "event: response.failed" mevedel-retry-test--failure))
         (chunks (list (substring mevedel-retry-test--failure 0 split)
                       (substring mevedel-retry-test--failure split)))
         partial-seen)
    (mevedel-retry-test--with-http (list chunks mevedel-retry-test--success)
      (plist-put (gptel-fsm-info fsm) :callback
                 (lambda (response info &optional raw)
                   (gptel-curl--stream-insert-response response info raw)
                   (when (equal response "Abandoned output")
                     (setq partial-seen t)
                     (with-current-buffer data-buf
                       (goto-char (point-max))
                       (insert (propertize "Concurrent notice\n" 'gptel 'ignore))))))
      (gptel--fsm-transition fsm)
      (mevedel-retry-test--drain-native)
      (should partial-seen)
      (should (= 1 finished))
      (with-current-buffer data-buf
        (should (string-match-p "Recovered answer" (buffer-string)))
        (should (string-match-p "Concurrent notice" (buffer-string)))
        (should-not (string-match-p "Abandoned output" (buffer-string)))))))

(ert-deftest mevedel-gptel-retry-dispatch/completed-tools-are-not-replayed ()
  (let ((tool-runs 0)
        (tool-response
         (concat "event: response.output_item.done\n"
                 "data: {\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"SideEffect\",\"arguments\":\"{}\"}}\n\n"
                 "event: response.completed\n"
                 "data: {\"response\":{\"status\":\"completed\"}}\n\n")))
    (mevedel-retry-test--with-http
        (list tool-response mevedel-retry-test--failure mevedel-retry-test--success)
      (plist-put (gptel-fsm-info fsm) :tools
                 (list (gptel-make-tool :name "SideEffect" :description "Count a side effect"
                                        :args nil :function (lambda () (cl-incf tool-runs) "Done"))))
      (gptel--fsm-transition fsm)
      (mevedel-retry-test--drain-native)
      (should (= 3 (length requests)))
      (should (= 2 waits))
      (should (= 1 tool-runs))
      (should (= 1 finished))
      (should (equal (car requests) (cadr requests)))
      (dolist (entry transports)
        (should (eq (cdr entry)
                    (plist-get (process-get (car entry) 'mevedel-retry-info) :mevedel-retry-attempt))))
      (should (string-match-p "function_call_output" (car requests))))))

(ert-deftest mevedel-gptel-retry-dispatch/agent-result-and-accumulator ()
  (let* ((split (string-match "event: response.failed" mevedel-retry-test--failure))
         (chunks (list (substring mevedel-retry-test--failure 0 split)
                       (substring mevedel-retry-test--failure split)))
         (partial (list "Agent prefix: ")) (results nil))
    (mevedel-retry-test--with-http (list chunks mevedel-retry-test--success)
      (let* ((inv (mevedel-agent-invocation--create
                   :path "/root/test" :buffer data-buf
                   :agent (mevedel-agent--create :name "explorer")))
             (callback (mevedel-agent-exec--make-callback
                        (lambda (result) (push result results))
                        "explorer" "Retry test" (plist-get (gptel-fsm-info fsm) :position) partial)))
        (with-current-buffer data-buf (setq-local mevedel--agent-invocation inv))
        (plist-put (gptel-fsm-info fsm) :mevedel-agent-invocation inv)
        (plist-put (gptel-fsm-info fsm) :callback (mevedel-agent-exec--wrap-callback callback))
        (gptel--fsm-transition fsm)
        (mevedel-retry-test--drain-native)
        (should (equal '("Recovered answer") results))
        (should (equal "Agent prefix: Recovered answer" (car partial)))
        (should (= 1 waits))
        (should (= 1 finished))
        (should (= 0 failed))))))

(ert-deftest mevedel-gptel-retry-dispatch/independent-root-and-child ()
  (mevedel-retry-test--with-http
      (list mevedel-retry-test--failure mevedel-retry-test--failure mevedel-retry-test--success)
    (let* ((child-buffer (generate-new-buffer " *retry-child*"))
           (child-request (mevedel-request--create :id "child" :session session))
           (child (gptel-copy-fsm fsm)))
      (unwind-protect
          (progn
            (with-current-buffer child-buffer
              (setq-local mevedel--current-request child-request))
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
            (let ((child-state (plist-get (gptel-fsm-info child) :mevedel-retry-state)))
              (should-not (eq child-state (plist-get (gptel-fsm-info fsm) :mevedel-retry-state)))
              (mevedel-request-cancel request)
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

(ert-deftest mevedel-gptel-retry--cleanup/usage-observer-install-order ()
  (let (events)
    (mevedel-retry-test--with-http
        (list "event: response.output_text.delta\ndata: {\"delta\":\"Truncated\"}\n\n"
              mevedel-retry-test--success)
      (unwind-protect
          (progn
            (mevedel-gptel-stream-bridge-install)
            (mevedel-telemetry-usage-install)
            ;; Reinstall after the normal bridge/observer startup order.
            (mevedel-gptel-retry-install)
            (cl-letf (((symbol-function 'mevedel-telemetry-usage--emit)
                       (lambda (call stage &rest props)
                         (push (append (list :stage stage)
                                       (plist-get call :identity) props) events))))
              (gptel--fsm-transition fsm)
              (mevedel-retry-test--drain-native))
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
        (mevedel-telemetry-usage-uninstall)
        (mevedel-gptel-stream-bridge-uninstall)))))

(ert-deftest mevedel-gptel-retry--converter/retry-disposes-native-org-state ()
  (mevedel-retry-test--with-http (list mevedel-retry-test--failure mevedel-retry-test--success)
    (with-current-buffer data-buf
      (org-mode)
      (setq-local mevedel--current-request request)
      (setq-local mevedel--session session))
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
      (mevedel-retry-test--drain-native)
      (should (= 1 finished))
      (should-not gptel-post-response-functions))))

(mevedel-deftest mevedel-gptel-retry--sum-tokens ()
  ,test
  (test)
  :doc "reported usage: absent reports stay absent"
  (should-not (mevedel-gptel-retry--sum-tokens nil nil))
  :doc "reported usage: sum only reported fields, preserving zero and sparse data"
  (let ((previous '(:input 5 :output 2)))
    (should (equal '(:input 12 :output 2 :cached 0)
                   (mevedel-gptel-retry--sum-tokens previous '(:input 7 :cached 0))))
    (should (equal '(:input 5 :output 2) previous))))

(mevedel-deftest mevedel-gptel-retry--close ()
  ,test
  (test)
  :doc "accepted completion releases snapshots and registry without cancellation"
  (let* ((state (list :data '(:input [])))
         (mevedel-gptel-retry--states (list state)))
    (mevedel-gptel-retry--close state)
    (should (plist-get state :closed))
    (should-not (plist-get state :data))
    (should-not (plist-get state :cancelled))
    (should-not mevedel-gptel-retry--states)))

(ert-deftest mevedel-gptel-retry--send/late-registration-after-cancellation ()
  (let* ((buffer (generate-new-buffer " *retry-late-register*"))
         (process (make-pipe-process :name "retry-late" :noquery t))
         (request (mevedel-request--create :id "late"))
         (aborts 0)
         (fsm (gptel-make-fsm :state 'WAIT :info (list :buffer buffer)
                              :handlers `((ABRT ,(lambda (_) (cl-incf aborts))))))
         (state (list :fsm fsm :request request :callback #'ignore))
         (gptel--request-alist nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer (setq-local mevedel--current-request request))
          (plist-put (gptel-fsm-info fsm) :mevedel-retry-state state)
          ;; Inject the native registration gap, leaving actual retirement real.
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_)
                       (mevedel-gptel-retry--cancel state)
                       (push (cons process (cons fsm (lambda () (delete-process process))))
                             gptel--request-alist))))
            (mevedel-gptel-retry--send fsm))
          (should-not (process-live-p process))
          (should-not gptel--request-alist)
          (should (= 1 aborts)))
      (delete-process process)
      (kill-buffer buffer))))

(ert-deftest mevedel-gptel-retry--rollback/exhaustion-removes-final-partial ()
  (let* ((split (string-match "event: response.failed" mevedel-retry-test--failure))
         (chunks (list (substring mevedel-retry-test--failure 0 split)
                       (substring mevedel-retry-test--failure split))))
    (mevedel-retry-test--with-http (make-list 11 chunks)
      (gptel--fsm-transition fsm)
      (mevedel-retry-test--drain-native)
      (should (= 1 failed))
      (should (= 11 (length requests)))
      (should-not (string-match-p "Abandoned output" (with-current-buffer data-buf (buffer-string)))))))

(ert-deftest mevedel-gptel-retry--tag-output/notices-inside-native-hooks ()
  (dolist (at-response-boundary '(nil t))
    (let* ((split (string-match "event: response.failed" mevedel-retry-test--failure))
           (chunks (list (substring mevedel-retry-test--failure 0 split)
                         (substring mevedel-retry-test--failure split)))
           appended)
      (mevedel-retry-test--with-http (list chunks mevedel-retry-test--success)
        (let ((gptel-post-stream-hook
               (list (lambda ()
                       (unless appended
                         (setq appended t)
                         (save-excursion
                           (unless at-response-boundary (goto-char (point-max)))
                           (let ((start (point)))
                             (insert (propertize "Canonical notice\n" 'gptel 'ignore))
                             (should-not (get-text-property start 'mevedel-retry-output)))))))))
          (gptel--fsm-transition fsm)
          (mevedel-retry-test--drain-native))
        (should appended)
        (with-current-buffer data-buf
          (should (string-match-p "Canonical notice" (buffer-string)))
          (should (string-match-p "Recovered answer" (buffer-string)))
          (should-not (string-match-p "Abandoned output" (buffer-string))))))))

(ert-deftest mevedel-gptel-retry-dispatch/vector-checkpoint-is-independent ()
  (mevedel-retry-test--with-http (list mevedel-retry-test--failure mevedel-retry-test--success)
    (plist-put (plist-get (gptel-fsm-info fsm) :data) :input
               (vector (list :role "user" :content "Frozen input")))
    (gptel--fsm-transition fsm)
    (plist-put (aref (plist-get (plist-get (gptel-fsm-info fsm) :data) :input) 0)
               :content "Abandoned mutation")
    (mevedel-retry-test--drain-native)
    (should (= 2 (length requests)))
    (should (equal (car requests) (cadr requests)))
    (should (string-match-p "Frozen input" (car requests)))
    (should-not (string-match-p "Abandoned mutation" (car requests)))))

(ert-deftest mevedel-gptel-retry-dispatch/cancelled-admission-does-not-send ()
  (mevedel-retry-test--with-http (list mevedel-retry-test--success)
    (setf (mevedel-request-cancelled-p request) t)
    (gptel--fsm-transition fsm)
    (should-not requests)
    (should-not gptel--request-alist)
    (should (= 1 aborted))
    (should (eq 'ABRT (gptel-fsm-state fsm)))))

(provide 'test-mevedel-gptel-retry)

;;; test-mevedel-gptel-retry.el ends here
