;;; test-mevedel-memory-review-provider.el --- Review provider timing -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise sessionless review timing through native gptel request preparation,
;; transport completion, parsing and scoped tool loops.  URL replies and a local
;; stdin-draining curl substitute never open a connection or contact a provider.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel-request)
(require 'gptel-openai)
(require 'mevedel-context-summary)
(require 'mevedel-journal-worker)
(require 'mevedel-memory-investigation)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-pass)
(require 'mevedel-memory-review)
(require 'mevedel-memory-scope)
(require 'mevedel-models)
(require 'mevedel-native)
(require 'mevedel-telemetry)
(require 'mevedel-telemetry-usage)
(require 'mevedel-workspace-identity)

(defconst test-mevedel-review-provider--none
  "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes."
  "Valid no-action review returned by the synthetic provider.")

(defun test-mevedel-review-provider--response (text &optional tool)
  "Encode a native OpenAI response containing TEXT or a Read TOOL call."
  (gptel--json-encode
   (list :choices
         (vector
          (list :index 0 :finish_reason (if tool "tool_calls" "stop")
                :message
                (if tool
                    '(:role "assistant" :content :null
                      :tool_calls [(:id "read-source" :type "function"
                                    :function (:name "Read" :arguments "{\"root\":\"workspace\",\"path\":\"source.el\"}"))])
                  (list :role "assistant" :content text))))
         :usage '(:prompt_tokens 100 :completion_tokens 10 :total_tokens 110))))

(defun test-mevedel-review-provider--events (workspace &optional event)
  "Read WORKSPACE's persisted telemetry, optionally selecting EVENT."
  (let ((path (file-name-concat (mevedel-workspace-root workspace)
                                ".mevedel" "state" "diagnostics"
                                mevedel-telemetry-file-name))
        rows)
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (let ((read-eval nil))
          (condition-case nil
              (while t (push (read (current-buffer)) rows))
            (end-of-file)))))
    (seq-filter (lambda (row) (or (null event) (eq event (plist-get row :event))))
                (nreverse rows))))

(mevedel-deftest mevedel-memory-review-request/provider
    (:quiet t
     :vars* ((directory (make-temp-file "mevedel-review-provider-" t))
             (workspace (mevedel-workspace--create :root directory))
             (mevedel-memory-dirs nil)

             (gptel--known-backends nil)
             (gptel--request-alist nil)
             (gptel-use-curl nil)
             (gptel-stream nil)
             (gptel-log-level nil)
             (gptel-confirm-tool-calls t)
             (mevedel-telemetry-enabled t)
             (model (make-symbol "review-provider-model"))
             (backend (gptel-make-openai "review-provider" :key "synthetic"
                                         :host "example.invalid" :stream t :models (list model)))
             (real-investigate (symbol-function 'mevedel-memory-investigation-call))
             (real-prepare (symbol-function 'mevedel-memory-pass--prepare))
             (observer-advice
              '((gptel--handle-wait . mevedel-telemetry-usage--dispatch)
                (gptel-curl--parse-response . mevedel-telemetry-usage--parse-response)
                (gptel--url-parse-response . mevedel-telemetry-usage--parse-response)
                (gptel-curl--stream-cleanup . mevedel-telemetry-usage--stream-cleanup)
                (gptel-curl--parse-stream . mevedel-telemetry-usage--parse-stream)
                (gptel--fsm-transition . mevedel-telemetry-usage--abort)))
             (new-advice (seq-remove (lambda (pair) (advice-member-p (cdr pair) (car pair)))
                                     observer-advice))
             (clock 100.0) (tool-wait 0) (investigations 0) (settlements 0)
             (serial 0) pending transport-buffers sent result handle pass-id pass-state)
     :before-each ((put model :context-window 128)
                   (put model :capabilities '(tool-use))
                   (write-region ";; Actual source evidence.\n" nil
                                 (file-name-concat directory "source.el") nil 'silent))
     :after-each ((when handle (funcall (plist-get handle :cancel)))
                  (when pass-state
                    (mevedel-memory-pass-cancel workspace)
                    (mevedel-telemetry--lag-stop))
                  (dolist (buffer transport-buffers)
                    (when (buffer-live-p buffer) (kill-buffer buffer)))
                  (dolist (pair new-advice) (advice-remove (car pair) (cdr pair)))
                  (delete-directory directory t)))
  (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
             (lambda (&rest _) (list :backend backend :model model)))
            ((symbol-function 'mevedel-journal-worker-local-p) (lambda (_ &optional _roots) nil))
            ((symbol-function 'mevedel-telemetry--monotonic-now) (lambda () clock))
            ((symbol-function 'url-retrieve)
             (lambda (url callback &rest _)
               (should (string-prefix-p "https://example.invalid/" url))
               (let ((buffer (generate-new-buffer " *review-provider-url*")))
                 (push buffer transport-buffers)
                 (push (json-parse-string url-request-data :object-type 'plist :array-type 'list) sent)
                 (setq pending (append pending (list (list :buffer buffer :callback callback))))
                 buffer)))
            ((symbol-function 'mevedel-memory-investigation-call)
             (lambda (&rest args)
               (cl-incf investigations)
               ;; The preceding tool-only HTTP response must have completed
               ;; before any investigation or its simulated wait begins.
               (should (= 1 (length
                             (seq-filter
                              (lambda (row) (and (equal pass-id (plist-get row :pass-id))
                                                (eq 'finish (plist-get row :stage))))
                              (test-mevedel-review-provider--events workspace 'provider-call)))))
               (should (= 250 (plist-get (funcall (plist-get handle :usage)) :provider-duration-ms)))
               (cl-incf clock tool-wait)
               (apply real-investigate args))))
    (cl-labels
        ((start (&optional scope)
           (setq result nil settlements 0 pass-id (format "review-pass-%d" (cl-incf serial)))
           (setq handle
                 (mevedel-memory-review-request
                  (or scope (mevedel-memory-scope-capture workspace)) nil
                  (lambda (value) (cl-incf settlements) (setq result value))
                  :memory-only t :workspace workspace :pass-id pass-id)))
         (receive (body duration &optional status)
           (should pending)
           (let* ((transport (pop pending))
                  (buffer (plist-get transport :buffer)))
             (cl-incf clock duration)
             (with-current-buffer buffer
               (insert (format "HTTP/1.1 %s\r\nContent-Type: application/json\r\n\r\n"
                               (or status "200 OK")))
               (setq-local url-http-end-of-headers (copy-marker (point))
                           url-http-response-status (if status 503 200))
               (insert body)
               (goto-char (point-min))
               (funcall (plist-get transport :callback) nil))))
         (events (&optional stage)
           (seq-filter (lambda (row)
                         (and (equal pass-id (plist-get row :pass-id))
                              (or (not stage) (eq stage (plist-get row :stage)))))
                       (test-mevedel-review-provider--events workspace 'provider-call)))
         (check-totals (count duration)
           (should (= 1 settlements))
           (dolist (value (list result (funcall (plist-get handle :usage))))
             (should (= count (plist-get value :provider-call-count)))
             (should (= duration (plist-get value :provider-duration-ms))))
           (should (= count (length (events 'start))))
           (should (= count (length (events 'finish))))
           (should (= duration (apply #'+ (mapcar (lambda (row) (plist-get row :duration-ms))
                                                 (events 'finish)))))
           (should (equal (mapcar (lambda (row) (plist-get row :call-id)) (events 'start))
                          (mapcar (lambda (row) (plist-get row :call-id)) (events 'finish))))
           (dolist (row (events))
             (should (eq 'memory (plist-get row :workload)))
             (dolist (key '(:session-id :request-id :workspace :session :dropped-keys))
               (should-not (plist-member row key)))))
         (chunk-scope ()
           (let* ((shared (file-name-concat directory ".mevedel" "shared"))
                  (path (file-name-concat shared "large.md")))
             (mevedel-workspace-identity-ensure directory)
             (make-directory shared t)
             (write-region (concat (make-string (* 33 1024) ?x) "\nLast finding.\n")
                           nil path nil 'silent)
             (set-file-times path (seconds-to-time (- (float-time) (* 15 86400))))
             (let ((capture (mevedel-memory-notes-capture workspace nil)))
               (should (= 1 (plist-get capture :notes)))
               (let ((scope (list :workspace-root directory :roots (list (plist-get capture :root)))))
                 (should (plist-get (car (mevedel-memory-notes-documents scope)) :chunked))
                 scope))))
         (start-pass (&optional automatic)
           (let ((noninteractive nil)
                 (mevedel-memory-pass--inhibit-scheduling nil)
                 (mevedel-native--load-state "unavailable in provider tests")
                 (mevedel-memory-consolidation-mode 'propose))
             (cl-letf (((symbol-function 'mevedel-memory-pass--prepare)
                        (lambda (owner payload)
                          (setq pass-state (mevedel-memory-pass-running owner)
                                pass-id (plist-get (plist-get pass-state :claim) :owner))
                          ;; Registration must precede preparation, not merely
                          ;; inference.  Observe the real fallback watch handle.
                          (should (plist-get pass-state :lag-watch))
                          (should-not (plist-get (cdr (plist-get pass-state :lag-watch)) :until))
                          (cl-incf clock 0.5)
                          (funcall real-prepare owner payload))))
               (mevedel-memory-pass-start
                workspace (lambda (value) (cl-incf settlements) (setq result value))
                :memory-only (not automatic) :automatic automatic))
             (setq handle (plist-get pass-state :request))))
         (check-pass (event &optional count duration)
           (should (plist-get pass-state :settled))
           (should-not (mevedel-memory-pass-running workspace))
           (should (numberp (plist-get (cdr (plist-get pass-state :lag-watch)) :until)))
           (when event
             (let ((rows (test-mevedel-review-provider--events workspace event)))
               (should (= 1 (length rows)))
               (should (equal pass-id (plist-get (car rows) :pass-id)))
               (dolist (value (list result (car rows)))
                 (if count
                     (progn
                       (should (= count (plist-get value :provider-call-count)))
                       (should (= duration (plist-get value :provider-duration-ms))))
                   (should-not (plist-member value :provider-call-count))
                   (should-not (plist-member value :provider-duration-ms)))))))
         (interrupt-pass (deadline)
           (start-pass)
           (should-not result)
           (let* ((buffer (plist-get (car pending) :buffer))
                  (fsm (cadr (assq buffer gptel--request-alist)))
                  (info (gptel-fsm-info fsm))
                  (late (plist-get info :callback))
                  (timer (plist-get pass-state :timer))
                  (terminal (if deadline 'memory-consolidation-failed 'memory-consolidation-killed)))
             (should (timerp timer))
             (cl-incf clock 0.625)
             (if deadline
                 ;; Deliver the production deadline closure, without waiting
                 ;; for a real provider or changing the durable claim clock.
                 (apply (timer--function timer) (timer--args timer))
               (mevedel-memory-pass-cancel workspace))
             (should (eq (if deadline 'error 'aborted) (plist-get result :outcome)))
             (when deadline (should (equal "Memory pass timed out" (plist-get result :error))))
             (check-totals 1 625)
             (check-pass terminal 1 625)
             (should (eq 'aborted (plist-get (car (events 'finish)) :outcome)))
             (should-not (buffer-live-p buffer))
             (cl-incf clock 5)
             (funcall late t info)
             (gptel--fsm-transition fsm 'ABRT)
             (check-totals 1 625)
             (check-pass terminal 1 625))))
      ,test))
  (test)
  :doc "native tool-only parsing closes spans before real scoped Read waits and redispatch"
  (dolist (wait '(0 9))
    (setq tool-wait wait)
    (let ((began clock))
      (start)
      (let ((snapshot (funcall (plist-get handle :usage))))
        (should (= 1 (plist-get snapshot :provider-call-count)))
        (should (= 0 (plist-get snapshot :provider-duration-ms))))
      (receive (test-mevedel-review-provider--response nil t) 0.25)
      (should-not result)
      (should (= 1 (length pending)))
      (let ((tool-message (seq-find (lambda (message) (equal "tool" (plist-get message :role)))
                                    (plist-get (car sent) :messages))))
        (should (string-match-p "Actual source evidence" (plist-get tool-message :content))))
      (receive (test-mevedel-review-provider--response test-mevedel-review-provider--none) 0.5)
      (should (eq 'success (plist-get result :outcome)))
      (should (= investigations (if (zerop wait) 1 2)))
      (should (= 1 (plist-get result :tool-call-count)))
      (should (= 2 (plist-get result :rounds)))
      (should (= (+ 750 (* 1000 wait)) (round (* 1000 (- clock began)))))
      (check-totals 2 750)
      (should-not pending)
      (should-not gptel--request-alist)))
  :doc "provider HTTP errors contribute one finished dispatch span"
  (progn
    (start)
    (receive "{\"error\":{\"message\":\"Synthetic unavailable\",\"type\":\"server_error\"}}" 0.375 "503 Service Unavailable")
    (should (eq 'error (plist-get result :outcome)))
    (check-totals 1 375)
    (should (eq 'error (plist-get (car (events 'finish)) :outcome))))
  :doc "disabled telemetry preserves the native request but leaves provider totals absent"
  (let ((mevedel-telemetry-enabled nil))
    (start)
    (should pending)
    (receive (test-mevedel-review-provider--response test-mevedel-review-provider--none) 0.25)
    (should (eq 'success (plist-get result :outcome)))
    (should (= 1 settlements))
    (dolist (value (list result (funcall (plist-get handle :usage))))
      (should-not (plist-member value :provider-call-count))
      (should-not (plist-member value :provider-duration-ms)))
    (should-not (events))
    (should-not (file-exists-p (file-name-concat directory ".mevedel"))))
  :doc "admission failure without a native dispatch does not synthesize zero totals"
  (let ((mevedel-memory-review-max-tokens 0))
    (start)
    (should (eq 'error (plist-get result :outcome)))
    (should (= 1 settlements))
    (should-not pending)
    (should-not sent)
    (should-not (events))
    (dolist (value (list result (funcall (plist-get handle :usage))))
      (should-not (plist-member value :provider-call-count))
      (should-not (plist-member value :provider-duration-ms))))
  :doc "cancel freezes the active span once despite late native completion and repeated abort"
  (progn
    (start)
    (let* ((buffer (plist-get (car pending) :buffer))
           (fsm (cadr (assq buffer gptel--request-alist)))
           (info (gptel-fsm-info fsm))
           (late (plist-get info :callback)))
      (cl-incf clock 0.125)
      (funcall (plist-get handle :cancel))
      (should (eq 'aborted (plist-get result :outcome)))
      (check-totals 1 125)
      (should (eq 'aborted (plist-get (car (events 'finish)) :outcome)))
      (should-not (buffer-live-p buffer))
      (cl-incf clock 5)
      (funcall late t info)
      (gptel--fsm-transition fsm 'ABRT)
      (funcall (plist-get handle :cancel))
      (check-totals 1 125)))
  :doc "chunk summaries and final review share the pass and aggregate distinct dispatch spans"
  (let* ((scope (chunk-scope))
         (summary (mapconcat (lambda (heading) (concat "## " heading "\n- Retained finding."))
                             '("Scope" "Constraints & Preferences" "Work & Evidence" "Key Decisions"
                               "Open Questions & Risks" "Critical Context" "Relevant Files"
                               "Skills Invoked" "Next Steps") "\n"))
         (reply test-mevedel-review-provider--none))
    (start scope)
    (receive (test-mevedel-review-provider--response summary) 0.125)
    (should-not result)
    (should (string-search "Running summary of earlier chunks" (prin1-to-string (car sent))))
    (receive (test-mevedel-review-provider--response summary) 0.25)
    (should-not result)
    (should (string-search "Retained finding" (prin1-to-string (car sent))))
    (should-not (string-search (make-string 100 ?x) (prin1-to-string (car sent))))
    (receive (test-mevedel-review-provider--response reply) 0.5)
    (ert-info ((format "Chunk review failed: %S" result))
      (should (eq 'success (plist-get result :outcome))))
    (should (equal '("work://shared/large.md") (plist-get result :notes)))
    (should (= 3 (length sent)))
    (check-totals 3 875)
    (should (equal '(continuation continuation nil)
                   (mapcar (lambda (row) (plist-get row :purpose)) (events 'start))))
    (should-not pending)
    (should-not gptel--request-alist))
  :doc "chunk output guard aborts a native stream before callback totals freeze"
  (let ((scope (chunk-scope))
        (gptel-use-curl t)
        (gptel-stream t)
        (mevedel-memory-review-max-bytes 100)
        (real-make-process (symbol-function 'make-process))
        processes)
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (should (equal "gptel-curl" (plist-get args :name)))
                     ;; Native dispatch sends the complete config synchronously.
                     ;; Drain it, then stay live for stream injection and abort.
                     (let ((process (funcall real-make-process
                                             :name "review-provider-draining"
                                             :buffer (plist-get args :buffer)
                                             :command (list (executable-find "python3") "-c"
                                                            "import sys,time; sys.stdin.buffer.read(); time.sleep(60)")
                                             :connection-type 'pipe
                                             :noquery t :sentinel #'ignore)))
                       (push (process-buffer process) transport-buffers)
                       (push process processes)
                       process))))
          (with-temp-buffer
            (setq-local gptel-stream t)
            (start scope))
          (should (= 1 (length processes)))
          (let* ((process (car processes))
                 (fsm (cadr (assq process gptel--request-alist)))
                 (info (gptel-fsm-info fsm))
                 (late (plist-get info :callback)))
            (should (process-live-p process))
            (should (plist-get info :stream))
            (cl-incf clock 0.625)
            (gptel-curl--stream-filter
             process
             (concat "HTTP/1.1 200 OK\r\n\r\n"
                     "data: " (gptel--json-encode
                                (list :choices
                                      (vector (list :index 0 :delta (list :content (make-string 101 ?x))))))
                     "\n\n"))
            (should (eq 'error (plist-get result :outcome)))
            (should (equal "Note chunk review output byte budget exceeded" (plist-get result :error)))
            (check-totals 1 625)
            (should (eq 'aborted (plist-get (car (events 'finish)) :outcome)))
            (should-not (process-live-p process))
            (cl-incf clock 5)
            (funcall late t info)
            (check-totals 1 625)))
      (dolist (process processes)
        (when (process-live-p process)
          (set-process-sentinel process #'ignore)
          (delete-process process)))))
  :doc "production pass forwards provider totals to completed telemetry and settles its early watch"
  (progn
    (start-pass)
    (should-not result)
    (should-not (plist-get (cdr (plist-get pass-state :lag-watch)) :until))
    (receive (test-mevedel-review-provider--response test-mevedel-review-provider--none) 0.25)
    (should (eq 'success (plist-get result :outcome)))
    (check-totals 1 250)
    (check-pass 'memory-consolidation-completed 1 250)
    (let ((row (car (test-mevedel-review-provider--events workspace 'memory-consolidation-completed))))
      (should (= 750 (plist-get row :duration-ms)))
      (should (= 500 (plist-get row :preparation-duration-ms)))
      (should (= 250 (plist-get row :review-request-duration-ms)))))
  :doc "production pass retains errored dispatch totals in failed terminal telemetry"
  (progn
    (start-pass)
    (receive "{\"error\":{\"message\":\"Synthetic unavailable\",\"type\":\"server_error\"}}" 0.375 "503 Service Unavailable")
    (should (eq 'error (plist-get result :outcome)))
    (check-totals 1 375)
    (check-pass 'memory-consolidation-failed 1 375))
  :doc "production pass without dispatch settles its watch and omits provider fields"
  (let ((mevedel-memory-review-max-tokens 0))
    (start-pass)
    (should (eq 'error (plist-get result :outcome)))
    (should-not pending)
    (should-not (events))
    (check-pass 'memory-consolidation-failed))
  :doc "production pass cancellation closes the active span before terminal totals freeze"
  (interrupt-pass nil)
  :doc "production pass deadline closes the active span before failed terminal totals freeze"
  (interrupt-pass t)
  :doc "automatic empty admission settles the preparation watch without inference or callback"
  (progn
    (start-pass t)
    (should (plist-get pass-state :skipped))
    (should-not result)
    (should (= 0 settlements))
    (should-not pending)
    (should-not (events))
    (check-pass nil))
  :doc "provider start append cancellation fences transport before the review handle is stored"
  (let ((record (symbol-function 'mevedel-telemetry-record-workspace))
        (cancellations 0))
    (cl-letf (((symbol-function 'mevedel-telemetry-record-workspace)
               (lambda (owner event &rest props)
                 (prog1 (apply record owner event props)
                   (when (and (eq event 'provider-call)
                              (eq (plist-get props :stage) 'start))
                     (should (eq owner workspace))
                     (should (eq pass-state (mevedel-memory-pass-running workspace)))
                     ;; The request call has not returned to --prepared yet.
                     ;; Its eventual handle must be retired after cancellation,
                     ;; but dispatch must already be fenced at this boundary.
                     (should-not (plist-get pass-state :request))
                     (cl-incf cancellations)
                     (mevedel-memory-pass-cancel workspace))))))
      (start-pass))
    (should (= 1 cancellations))
    (should (= 1 settlements))
    (should (eq 'aborted (plist-get result :outcome)))
    (check-pass 'memory-consolidation-killed)
    (should-not pending)
    (should-not sent)
    (should-not gptel--request-alist)
    (should handle)
    (should-not (buffer-live-p (plist-get handle :buffer)))
    (funcall (plist-get handle :cancel))
    (should (= 1 settlements))
    (check-pass 'memory-consolidation-killed)))

(provide 'test-mevedel-memory-review-provider)
;;; test-mevedel-memory-review-provider.el ends here
