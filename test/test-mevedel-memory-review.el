;;; test-mevedel-memory-review.el -- Sessionless consolidation requests -*- lexical-binding: t -*-

;;; Commentary:

;; Fake inference at gptel-request while retaining its real dry-run preparation.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel-request)
(require 'gptel-openai)
(require 'mevedel-memory-scope)
(require 'mevedel-memory-review)
(require 'mevedel-system)

(defconst mevedel-test-memory-review--none
  "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes.")

(mevedel-deftest mevedel-memory-review-request
    (:vars* ((directory (make-temp-file "mevedel-memory-review-" t))
             (workspace (mevedel-workspace--create :root directory))
             (mevedel-memory-dirs nil)
             (gptel--known-backends nil)
             (model (make-symbol "review-model"))
             (backend (gptel-make-openai "review-test" :key "test-only" :models (list model)))
             (real-request (symbol-function 'gptel-request))
             (entries (list (list :id (make-string 64 ?a) :file "digest.md" :session "session"
                                  :created "2026-09-07T12:00:00Z" :body "## Done\n- Observed: Tests passed.")))
             (settlements 0) workload sent result handle request callback)
     :before-each ((put model :context-window 128) (put model :capabilities '(tool-use)))
     :after-each ((when handle (funcall (plist-get handle :cancel)))
                  (delete-directory directory t)))
  (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
             (lambda (kind &rest _) (setq workload kind) (list :backend backend :model model)))
            ((symbol-function 'gptel-request)
             (lambda (prompt &rest args)
               (let ((fsm (apply real-request prompt (plist-put (copy-sequence args) :dry-run t))))
                 (unless (plist-get args :dry-run)
                   (setq sent prompt request fsm callback (plist-get args :callback)))
                 fsm))))
    (cl-labels ((start (&rest options)
                  (setq handle
                        (apply #'mevedel-memory-review-request
                               (mevedel-memory-scope-capture workspace) entries
                               (lambda (value) (cl-incf settlements) (setq result value)) options)))
                (respond (text)
                  (funcall callback text (gptel-fsm-info request))))
      ,test))
  (test)
  :doc "uses the buddy workload, scoped tools, and terminal validation without publishing coverage"
  (progn
    (start)
    (should (eq 'buddy workload))
    (should sent)
    (with-current-buffer (plist-get handle :buffer)
      (should-not mevedel--session)
      (should (equal '("Read" "Glob" "Grep") (mapcar #'gptel-tool-name gptel-tools))))
    (respond mevedel-test-memory-review--none)
    (respond t)
    (should-not result)
    (gptel--fsm-transition request 'DONE)
    (ert-info ((format "Review error: %s" (plist-get result :error)))
      (should (eq 'success (plist-get result :outcome))))
    (should (= 1 (length (plist-get result :entries))))
    (should-not (plist-get result :proposals))
    (should (equal mevedel-test-memory-review--none (plist-get result :reply)))
    (should-not (buffer-live-p (plist-get handle :buffer)))
    (should-not (file-exists-p (file-name-concat directory ".mevedel"))))
  :doc "Unicode evidence remains text through request admission and the inspectable buffer"
  (let ((body "## Learned\n- PostgreSQL \u2013 user correction; test \u754c condition."))
    (setf (plist-get (car entries) :body) body)
    (start)
    (ert-info ((format "Review error: %s" (plist-get result :error)))
      (should sent))
    (should (multibyte-string-p sent))
    (should (equal body (gethash "body" (aref (gethash "digests" (json-parse-string sent)) 0))))
    (with-current-buffer (plist-get handle :buffer)
      (should (string-match-p "PostgreSQL \u2013" (buffer-string))))
    (respond mevedel-test-memory-review--none)
    (gptel--fsm-transition request 'DONE)
    (should (eq 'success (plist-get result :outcome))))
  :doc "runs an admitted Read without inheriting interactive tool confirmation"
  (let ((gptel-confirm-tool-calls t) tool-result)
    (with-temp-file (file-name-concat directory "source.el") (insert ";; Source evidence.\n"))
    (start)
    ;; Stop the fake inference boundary after gptel has executed the real tool.
    (setf (gptel-fsm-handlers request) (copy-alist (gptel-fsm-handlers request)))
    (setf (alist-get 'TRET (gptel-fsm-handlers request))
          (list (lambda (fsm) (setq tool-result (plist-get (gptel-fsm-info fsm) :tool-result)))))
    (setf (plist-get (gptel-fsm-info request) :tool-use)
          (list (list :name "Read" :args '(:root "workspace" :path "source.el"))))
    (gptel--fsm-transition request 'TOOL)
    (should tool-result)
    (should (string-match-p "Source evidence" (nth 2 (car tool-result))))
    (should-not result)
    (respond mevedel-test-memory-review--none)
    (gptel--fsm-transition request 'DONE)
    (should (eq 'success (plist-get result :outcome)))
    (should (= 1 settlements)))
  :doc "rejects unavailable tool names before dispatch without entering an unbounded repair loop"
  (let (diagnostics)
    (start)
    (setf (gptel-fsm-handlers request) (copy-alist (gptel-fsm-handlers request)))
    (setf (alist-get 'TRET (gptel-fsm-handlers request)) (list #'ignore))
    (setf (plist-get (gptel-fsm-info request) :tool-use)
          (list (list :name "Bash" :args '(:command "true"))))
    (mevedel-test--with-captured-diagnostics diagnostics
      (gptel--fsm-transition request 'TOOL))
    (should (eq 'error (plist-get result :outcome)))
    (should (string-match-p "Unavailable review tool" (plist-get result :error)))
    (should (string-empty-p diagnostics))
    (should-not (buffer-live-p (plist-get handle :buffer))))
  :doc "cancellation and expired ownership settle once and reject late completion"
  (dolist (mode '(cancel expired))
    (let ((current t))
      (setq result nil settlements 0)
      (start :currentp (lambda () current))
      (if (eq mode 'cancel)
          (progn (funcall (plist-get handle :cancel)) (funcall (plist-get handle :cancel)))
        (setq current nil))
      (respond mevedel-test-memory-review--none)
      (gptel--fsm-transition request 'DONE)
      (should (eq (if (eq mode 'cancel) 'aborted 'error) (plist-get result :outcome)))
      (should (= 1 settlements))
      (should-not (buffer-live-p (plist-get handle :buffer)))))
  :doc "malformed and oversized replies never become successful reviews"
  (dolist (reply (list "## Promote\n- none" (make-string 33000 ?x)))
    (setq result nil settlements 0)
    (start)
    (setf (plist-get (gptel-fsm-info request) :tokens) '(:input 100 :cached 20 :output 7000)
          (plist-get (gptel-fsm-info request) :stream) nil)
    (respond reply)
    (gptel--fsm-transition request 'DONE)
    (should (eq 'error (plist-get result :outcome)))
    (should (= 7000 (plist-get result :output-tokens)))
    (let ((snapshot (funcall (plist-get handle :usage))))
      (setf (plist-get snapshot :output-tokens) 0)
      (should (= 7000 (plist-get (funcall (plist-get handle :usage)) :output-tokens))))
    (should-not (plist-get result :proposals))
    (should (= 1 settlements))
    (should-not (buffer-live-p (plist-get handle :buffer))))
  :doc "admits a complete digest prefix and reports every omitted candidate"
  (progn
    (setq entries (list (car entries)
                        (list :id (make-string 64 ?b) :body (make-string 150000 ?x))
                        (list :id (make-string 64 ?c) :body "Later evidence.")))
    (start)
    (let* ((input (json-parse-string sent :object-type 'plist :array-type 'list))
           (digests (plist-get input :digests)))
      (should (= 1 (length digests)))
      (should (equal (plist-get (car entries) :body) (plist-get (car digests) :body)))
      (should (= 2 (plist-get input :omitted_digests))))
    (respond mevedel-test-memory-review--none)
    (gptel--fsm-transition request 'DONE)
    (should (eq 'success (plist-get result :outcome)))
    (should (equal (list (car entries)) (plist-get result :entries)))
    (should (= 2 (plist-get result :omitted-digests))))
  :doc "an oversized first digest fails before dispatch and empty batches require explicit memory-only admission"
  (progn
    (setf (plist-get (car entries) :body) (make-string 150000 ?x))
    (start)
    (should-not sent)
    (should (eq 'error (plist-get result :outcome)))
    (setq entries nil result nil)
    (start)
    (should-not sent)
    (should (eq 'error (plist-get result :outcome)))
    (setq result nil)
    (start :memory-only t)
    (should sent)
    (respond mevedel-test-memory-review--none)
    (gptel--fsm-transition request 'DONE)
    (should (eq 'success (plist-get result :outcome)))
    (should-not (plist-get result :entries)))
  :doc "checks the prepared payload at dispatch and rejects tool-expanded context before transport"
  (progn
    (start)
    (setf (plist-get (gptel-fsm-info request) :data)
          (plist-put (plist-get (gptel-fsm-info request) :data) :expanded (make-string 600000 ?x)))
    (gptel--fsm-transition request 'WAIT)
    (should (eq 'error (plist-get result :outcome)))
    (should (string-match-p "usable input context" (plist-get result :error)))
    (should-not (buffer-live-p (plist-get handle :buffer))))
  :doc "the running buffer shows admitted evidence and response and killing it cancels the review"
  (progn
    (start)
    (with-current-buffer (plist-get handle :buffer)
      (should buffer-read-only)
      (should (string-match-p "Observed: Tests passed" (buffer-string))))
    (respond "Inspecting the evidence.")
    (with-current-buffer (plist-get handle :buffer)
      (should (string-match-p "Inspecting the evidence" (buffer-string))))
    (kill-buffer (plist-get handle :buffer))
    (should (eq 'aborted (plist-get result :outcome)))
    (respond mevedel-test-memory-review--none)
    (gptel--fsm-transition request 'DONE)
    (should (= 1 settlements)))
  :doc "the real deadline timer retires the request and late callbacks cannot revive it"
  (let ((timers (copy-sequence timer-list)))
    (start)
    (let ((added (cl-set-difference timer-list timers)))
      (should (= 1 (length added)))
      (let ((timeout (car added)))
        (should (<= 179 (float-time (time-subtract (timer--time timeout) (current-time))) 180))
        ;; Deliver the scheduled timeout now without waiting three minutes.
        (cancel-timer timeout)
        (timer-set-time timeout (current-time))
        (timer-activate timeout)
        (with-timeout (2 (ert-fail "Review timeout did not settle"))
          (while (not result) (accept-process-output nil 0.01)))
        (should-not (memq timeout timer-list))))
    (should (eq 'error (plist-get result :outcome)))
    (should (equal "Review timed out" (plist-get result :error)))
    (respond mevedel-test-memory-review--none)
    (gptel--fsm-transition request 'DONE)
    (should (= 1 settlements))
    (should-not (buffer-live-p (plist-get handle :buffer))))
  :doc "provider token usage and a twenty-first tool call each exhaust the whole review"
  (progn
    (start)
    (setf (plist-get (gptel-fsm-info request) :tokens) '(:input 100 :output 8001))
    (respond mevedel-test-memory-review--none)
    (respond t)
    (should (eq 'error (plist-get result :outcome)))
    (should (= 8001 (plist-get result :output-tokens)))
    (setq result nil settlements 0)
    (with-temp-file (file-name-concat directory "source.el") (insert ";; Source.\n"))
    (start)
    (setf (gptel-fsm-handlers request) (copy-alist (gptel-fsm-handlers request)))
    (setf (alist-get 'TRET (gptel-fsm-handlers request)) (list #'ignore))
    (setf (plist-get (gptel-fsm-info request) :tool-use)
          (cl-loop repeat 21 collect (list :name "Read" :args '(:root "workspace" :path "source.el"))))
    (gptel--fsm-transition request 'TOOL)
    (should (eq 'error (plist-get result :outcome)))
    (should (equal "Tool call budget exhausted" (plist-get result :error)))
    (should (= 1 settlements))
    (should-not (buffer-live-p (plist-get handle :buffer)))))

(mevedel-deftest mevedel-memory-review-request/transport ()
  ,test
  (test)
  :doc "a real provider tool round resumes with its result and validates only the final reply"
  (let* ((directory (make-temp-file "mevedel-memory-review-http-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs nil)
         (gptel--known-backends nil)
         (gptel-confirm-tool-calls t)
         (gptel-use-curl t)
         (gptel-stream nil)
         (model (make-symbol "review-http"))
         (responses
          (list
           (list :choices
                 (vector (list :index 0 :finish_reason "tool_calls"
                               :message (list :role "assistant" :content "I will inspect the source."
                                              :tool_calls
                                              (vector (list :id "read-1" :type "function"
                                                            :function (list :name "Read"
                                                                            :arguments "{\"root\":\"workspace\",\"path\":\"source.el\"}"))))))
                 :usage '(:prompt_tokens 100 :completion_tokens 10 :total_tokens 110))
           (list :choices (vector (list :index 0 :finish_reason "stop"
                                       :message (list :role "assistant" :content mevedel-test-memory-review--none)))
                 :usage '(:prompt_tokens 200 :completion_tokens 20 :total_tokens 220))))
         server children received result handle backend)
    (put model :context-window 128)
    (put model :capabilities '(tool-use))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat directory "source.el") (insert ";; Source evidence.\n"))
          (setq server
                (make-network-process
                 :name "memory-review-http" :server t :host 'local :service t
                 :family 'ipv4 :noquery t :coding 'binary :sentinel #'ignore
                 :filter
                 (lambda (process chunk)
                   (cl-pushnew process children)
                   (let ((text (concat (process-get process 'input) chunk)))
                     (process-put process 'input text)
                     (when (and (not (process-get process 'answered))
                                (string-match "\r\n\r\n" text))
                       (let* ((end (match-end 0))
                              (headers (substring text 0 end))
                              (case-fold-search t)
                              (length (and (string-match "Content-Length: *\\([0-9]+\\)" headers)
                                           (string-to-number (match-string 1 headers)))))
                         (when (and length (>= (- (length text) end) length))
                           (process-put process 'answered t)
                           (push (json-parse-string (substring text end (+ end length))
                                                   :object-type 'plist :array-type 'list) received)
                           (let ((body (gptel--json-encode (pop responses))))
                             (process-send-string
                              process (format "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %s\r\nConnection: close\r\n\r\n%s"
                                              (string-bytes body) body))
                             (process-send-eof process)))))))))
          (setq backend (gptel-make-openai
                         "review-http" :key "test-only" :models (list model)
                         :protocol "http" :host (format "127.0.0.1:%s" (plist-get (process-contact server t) :service))
                         :curl-args '("--noproxy" "*")))
          (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                     (lambda (&rest _) (list :backend backend :model model))))
            (setq handle (mevedel-memory-review-request
                          (mevedel-memory-scope-capture workspace) nil
                          (lambda (value) (setq result value)) :memory-only t))
            (with-timeout (10 (ert-fail
                              (format "Review transport did not settle: %s requests, connections %S, active states %S"
                                      (length received)
                                      (mapcar #'process-status children)
                                      (mapcar (lambda (row) (list (gptel-fsm-state (cadr row))
                                                                  (plist-get (gptel-fsm-info (cadr row)) :error)))
                                              gptel--request-alist))))
              (while (not result) (accept-process-output nil 0.01))))
          (ert-info ((format "Review error: %s" (plist-get result :error)))
            (should (eq 'success (plist-get result :outcome))))
          (should (= 2 (length received)))
          (let ((tool-message (cl-find "tool" (plist-get (car received) :messages)
                                       :key (lambda (message) (plist-get message :role)) :test #'equal)))
            (should (string-match-p "Source evidence" (plist-get tool-message :content))))
          (should (= 300 (plist-get result :input-tokens)))
          (should (= 30 (plist-get result :output-tokens)))
          (should-not (buffer-live-p (plist-get handle :buffer))))
      (when handle
        (mevedel-test--with-captured-diagnostics nil (funcall (plist-get handle :cancel))))
      (dolist (process children) (when (process-live-p process) (delete-process process)))
      (when (process-live-p server) (delete-process server))
      (delete-directory directory t))))

(provide 'test-mevedel-memory-review)
;;; test-mevedel-memory-review.el ends here
