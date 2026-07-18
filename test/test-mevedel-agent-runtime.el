;;; test-mevedel-agent-runtime.el --- Agent runtime tests -*- lexical-binding: t -*-

;;; Commentary:

;; Agent dispatch, lifecycle, BWAIT, watchdog, stopping, and completion tests.

;;; Code:

(require 'gptel)
(require 'gptel-request)
(require 'mevedel)
(require 'mevedel-agent-exec)
(require 'mevedel-agent-runtime)
(require 'mevedel-agents)
(require 'mevedel-reminders)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-tool-task)
(require 'mevedel-tool-ui)
(require 'mevedel-tools)
(require 'mevedel-view)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defvar mevedel-agent-runtime-test--agent-registry
  (copy-sequence mevedel-agent--registry)
  "Stable built-in agent registry for isolated runtime tests.")

(defun mevedel-agent-runtime-test--make-session ()
  "Create a fresh runtime-test session."
  (let ((workspace (mevedel-workspace-get-or-create
                    'project "/tmp/mt/" "/tmp/mt/" "mt")))
    (mevedel-session-create "main" workspace)))

(defun mevedel-agent-runtime-test--register-agent-tools ()
  "Register the built-in tool surface needed by bundled agents."
  (mevedel-tools-register))

(defun mevedel-agent-runtime-test--register-agent-fsm (invocation fsm)
  "Register FSM like the canonical runtime for INVOCATION."
  (setf (gptel-fsm-info fsm)
        (plist-put (gptel-fsm-info fsm)
                   :mevedel-agent-invocation invocation))
  (with-current-buffer
      (mevedel-agent-invocation-parent-data-buffer invocation)
    (setf (alist-get (mevedel-agent-invocation-agent-id invocation)
                     mevedel-agent-runtime--fsms nil nil #'equal)
          fsm))
  fsm)

(defun mevedel-agent-runtime-test--make-workspace ()
  "Return (WORKSPACE . TEMPDIR) rooted in a fresh temporary directory."
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "mevedel-test-runtime-" t)))
         (basename (file-name-nondirectory (directory-file-name tempdir)))
         (_ (mevedel-workspace-clear-registry))
         (workspace (mevedel-workspace-get-or-create
                     'project basename tempdir basename)))
    (cons workspace tempdir)))

(defun mevedel-agent-runtime-test--make-persistent-session ()
  "Return (SESSION . TEMPDIR) for a fresh persistent runtime session."
  (cl-destructuring-bind (workspace . tempdir)
      (mevedel-agent-runtime-test--make-workspace)
    (cons (mevedel-session-create "main" workspace) tempdir)))

(defun mevedel-agent-runtime-test--release-and-kill (buffer session)
  "Release SESSION's lock and kill BUFFER if it is live."
  (when (and session (mevedel-session-save-path session))
    (mevedel-session-persistence-lock-release
     (mevedel-session-save-path session)))
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (set-buffer-modified-p nil))
    (kill-buffer buffer)))


(mevedel-deftest mevedel-agent-runtime-queue-execution-completion ()
  ,test
  (test)
  :doc "queues main completions without transitioning or launching a request"
  (let ((session (mevedel-agent-runtime-test--make-session))
        transitions requests)
    (cl-letf (((symbol-function 'gptel-fsm-transition)
               (lambda (&rest args) (push args transitions)))
              ((symbol-function 'gptel-request)
               (lambda (&rest args) (push args requests))))
      (should
       (mevedel-agent-runtime-queue-execution-completion
        session "main" "finished\n<bash-execution/>")))
    (should-not transitions)
    (should-not requests)
    (should (= 1 (length (mevedel-session-messages session))))
    (should (equal "finished\n<bash-execution/>"
                   (plist-get (car (mevedel-session-messages session))
                              :body))))
  :doc "queues an agent completion through its durable invocation object"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (parent-buffer (generate-new-buffer " *execution-owner*"))
         (agent-id "explorer--execution-owner")
         (invocation
          (mevedel-agent-invocation--create
           :agent-id agent-id
           :parent-session session
           :parent-data-buffer parent-buffer))
         (fsm
          (gptel-make-fsm
           :state 'WAIT
           :info (list :mevedel-agent-invocation invocation))))
    (unwind-protect
        (progn
          (with-current-buffer parent-buffer
            (setq-local mevedel-agent-runtime--fsms
                        (list (cons agent-id fsm))))
          (should
           (mevedel-agent-runtime-queue-execution-completion
            invocation agent-id "agent finished"))
          (should-not (mevedel-session-messages session))
          (should (= 1 (length
                        (mevedel-agent-invocation-messages invocation))))
          (should (equal "agent finished"
                         (plist-get
                          (car (mevedel-agent-invocation-messages invocation))
                          :body)))
          (should-not
           (mevedel-agent-runtime-queue-execution-completion
            invocation "explorer--missing" "lost")))
      (kill-buffer parent-buffer)))
  :doc "settles an execution-only BWAIT agent without a model request"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (parent-buffer (generate-new-buffer " *execution-parent*"))
         (agent-buffer (generate-new-buffer " *execution-agent*"))
         (agent-id "explorer--execution-bwait")
         (invocation
          (mevedel-agent-invocation--create
           :agent-id agent-id
           :parent-session session
           :parent-data-buffer parent-buffer
           :buffer agent-buffer))
         result requests
         (fsm
          (gptel-make-fsm
           :state 'BWAIT
           :info
           (list :buffer agent-buffer
                 :mevedel-agent-invocation invocation
                 :mevedel-agent-terminal-callback
                 (lambda (response)
                   (mevedel-agent-runtime--complete-foreground-agent
                    invocation response))))))
    (unwind-protect
        (progn
          (setf
           (mevedel-agent-invocation-parent-tool-callback invocation)
           (lambda (response)
             (setq result response)
             (setf
              (mevedel-agent-invocation-foreground-result-reported-p
               invocation)
              t)))
          (with-current-buffer parent-buffer
            (setq-local mevedel-agent-runtime--fsms
                        (list (cons agent-id fsm))))
          (cl-letf (((symbol-function 'mevedel-agent-exec--final-response-text)
                     (lambda (_invocation) "Agent answer."))
                    ((symbol-function 'mevedel-execution-owner-live-p)
                     (lambda (_session _owner) nil))
                    ((symbol-function 'gptel-request)
                     (lambda (&rest args) (push args requests))))
            (should
             (mevedel-agent-runtime-queue-execution-completion
              invocation agent-id "Bash completed with exit code 0.")))
          (should-not requests)
          (should (eq 'DONE (gptel-fsm-state fsm)))
          (should (string-match-p "Agent answer" result))
          (should (string-match-p "Bash completed" result))
          (should-not (mevedel-agent-invocation-messages invocation)))
      (kill-buffer agent-buffer)
      (kill-buffer parent-buffer)))
  :doc "retries a completion queued just before the agent enters BWAIT"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (parent-buffer (generate-new-buffer " *execution-latched-parent*"))
         (agent-buffer (generate-new-buffer " *execution-latched-agent*"))
         (agent-id "explorer--execution-latched")
         (invocation
          (mevedel-agent-invocation--create
           :agent-id agent-id :parent-session session
           :parent-data-buffer parent-buffer :buffer agent-buffer))
         result requests
         (fsm
          (gptel-make-fsm
           :state 'TYPE
           :info
           (list :buffer agent-buffer
                 :mevedel-agent-invocation invocation
                 :mevedel-agent-terminal-callback
                 (lambda (response) (setq result response))))))
    (unwind-protect
        (progn
          (with-current-buffer parent-buffer
            (setq-local mevedel-agent-runtime--fsms
                        (list (cons agent-id fsm))))
          (cl-letf (((symbol-function 'mevedel-agent-exec--final-response-text)
                     (lambda (_invocation) "Latched answer."))
                    ((symbol-function 'mevedel-execution-owner-live-p)
                     (lambda (_session _owner) nil))
                    ((symbol-function 'gptel-request)
                     (lambda (&rest args) (push args requests))))
            (should
             (mevedel-agent-runtime-queue-execution-completion
              invocation agent-id "Latched Bash completion."))
            (should-not result)
            (should (mevedel-agent-invocation-messages invocation))
            (setf (gptel-fsm-state fsm) 'BWAIT)
            (mevedel-agent-runtime--handle-bwait fsm))
          (should-not requests)
          (should (eq 'DONE (gptel-fsm-state fsm)))
          (should (string-match-p "Latched answer" result))
          (should (string-match-p "Latched Bash completion" result))
          (should-not (mevedel-agent-invocation-messages invocation)))
      (kill-buffer agent-buffer)
      (kill-buffer parent-buffer)))
  :doc "retries transient failure after durable BWAIT delivery"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (parent-buffer (generate-new-buffer " *execution-retry-parent*"))
         (agent-buffer (generate-new-buffer " *execution-retry-agent*"))
         (agent-id "explorer--execution-retry")
         (invocation
          (mevedel-agent-invocation--create
           :agent-id agent-id :parent-session session
           :parent-data-buffer parent-buffer :buffer agent-buffer))
         (attempts 0)
         result requests
         (fsm
          (gptel-make-fsm
           :state 'BWAIT
           :info
           (list :buffer agent-buffer
                 :mevedel-agent-invocation invocation
                 :mevedel-agent-terminal-callback
                 (lambda (response)
                   (cl-incf attempts)
                   (if (= attempts 1)
                       (error "Transient callback failure")
                     (setq result response)))))))
    (unwind-protect
        (progn
          (with-current-buffer parent-buffer
            (setq-local mevedel-agent-runtime--fsms
                        (list (cons agent-id fsm))))
          (cl-letf (((symbol-function 'mevedel-agent-exec--final-response-text)
                     (lambda (_invocation) "Retry answer."))
                    ((symbol-function 'mevedel-execution-owner-live-p)
                     (lambda (_session _owner) nil))
                    ((symbol-function 'gptel-request)
                     (lambda (&rest args) (push args requests))))
            (should
             (mevedel-agent-runtime-queue-execution-completion
              invocation agent-id "Retry Bash completion."))
            (with-timeout (2 (error "Timed out waiting for retry"))
              (while (not (eq (gptel-fsm-state fsm) 'DONE))
                (accept-process-output nil 0.02))))
          (should-not requests)
          (should (= 2 attempts))
          (should (string-match-p "Retry Bash completion" result))
          (should-not (mevedel-agent-invocation-messages invocation)))
      (kill-buffer agent-buffer)
      (kill-buffer parent-buffer)))
  :doc "bounds permanent callback failure and stops the owning agent"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (parent-buffer (generate-new-buffer " *execution-failed-parent*"))
         (agent-buffer (generate-new-buffer " *execution-failed-agent*"))
         (agent-id "explorer--execution-failed")
         (invocation
          (mevedel-agent-invocation--create
           :agent-id agent-id :parent-session session
           :parent-data-buffer parent-buffer :buffer agent-buffer))
         (attempts 0)
         stopped requests
         (fsm
          (gptel-make-fsm
           :state 'BWAIT
           :info
           (list :buffer agent-buffer
                 :mevedel-agent-invocation invocation
                 :mevedel-agent-terminal-callback
                 (lambda (_response)
                   (cl-incf attempts)
                   (error "Permanent callback failure"))))))
    (unwind-protect
        (progn
          (with-current-buffer parent-buffer
            (setq-local mevedel-agent-runtime--fsms
                        (list (cons agent-id fsm))))
          (let ((mevedel-agent-runtime--execution-settlement-retry-delays
                 '(0.01 0.01)))
            (cl-letf (((symbol-function 'mevedel-agent-exec--final-response-text)
                       (lambda (_invocation) "Failed answer."))
                      ((symbol-function 'mevedel-execution-owner-live-p)
                       (lambda (_session _owner) nil))
                      ((symbol-function 'gptel-request)
                       (lambda (&rest args) (push args requests)))
                      ((symbol-function 'mevedel-agent-runtime-stop)
                       (lambda (seen-id reason seen-buffer)
                         (setq stopped (list seen-id reason seen-buffer))
                         (setf (gptel-fsm-state fsm) 'ABRT))))
              (should
               (mevedel-agent-runtime-queue-execution-completion
                invocation agent-id "Failed Bash completion."))
              (with-timeout (2 (error "Timed out waiting for bounded retry"))
                (while (not stopped)
                  (accept-process-output nil 0.02)))))
          (should-not requests)
          (should (= 3 attempts))
          (should (equal agent-id (car stopped)))
          (should (eq parent-buffer (caddr stopped)))
          (should-not
           (mevedel-agent-invocation-execution-settlement-retry-timer
            invocation))
          (should (zerop
                   (mevedel-agent-invocation-execution-settlement-retry-count
                    invocation)))
          (should (mevedel-agent-invocation-messages invocation)))
      (kill-buffer agent-buffer)
      (kill-buffer parent-buffer)))
  :doc "retries background parent delivery before retiring the agent"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (parent-buffer
          (generate-new-buffer " *execution-background-parent*"))
         (agent-buffer
          (generate-new-buffer " *execution-background-agent*"))
         (agent-id "explorer--execution-background")
         (agent (mevedel-agent--create :name "explorer"))
         (invocation
          (mevedel-agent-invocation--create
           :agent agent
           :agent-id agent-id
           :description "Run a background command"
           :parent-session session
           :parent-context session
           :parent-data-buffer parent-buffer
           :buffer agent-buffer
           :background-p t))
         (original-push
          (symbol-function 'mevedel-agent-runtime--ctx-push-message))
         (parent-push-attempts 0)
         requests
         (fsm
          (gptel-make-fsm
           :state 'BWAIT
           :info
           (list :buffer agent-buffer
                 :mevedel-agent-invocation invocation
                 :mevedel-agent-terminal-callback
                 (lambda (response)
                   (mevedel-agent-runtime--complete-background-agent
                    invocation response))))))
    (unwind-protect
        (progn
          (setf (mevedel-session-background-agents session) (list agent-id))
          (with-current-buffer parent-buffer
            (setq-local mevedel-agent-runtime--fsms
                        (list (cons agent-id fsm))))
          (let ((mevedel-agent-runtime--execution-settlement-retry-delays
                 '(0.01 0.01)))
            (cl-letf
                (((symbol-function 'mevedel-agent-exec--final-response-text)
                  (lambda (_invocation) "Background answer."))
                 ((symbol-function 'mevedel-execution-owner-live-p)
                  (lambda (_session _owner) nil))
                 ((symbol-function 'gptel-request)
                  (lambda (&rest args) (push args requests)))
                 ((symbol-function 'mevedel-agent-runtime--ctx-push-message)
                  (lambda (context message)
                    (when (eq context session)
                      (cl-incf parent-push-attempts)
                      (when (= parent-push-attempts 1)
                        (error "Transient parent mailbox failure")))
                    (funcall original-push context message))))
              (should
               (mevedel-agent-runtime-queue-execution-completion
                invocation agent-id "Background Bash completion."))
              (with-timeout (2 (error "Timed out waiting for parent retry"))
                (while (not (eq (gptel-fsm-state fsm) 'DONE))
                  (accept-process-output nil 0.02)))))
          (should-not requests)
          (should (= 2 parent-push-attempts))
          (should (= 1 (length (mevedel-session-messages session))))
          (should-not (mevedel-agent-invocation-messages invocation))
          (should-not (mevedel-session-background-agents session))
          (with-current-buffer parent-buffer
            (should-not (assoc agent-id mevedel-agent-runtime--fsms))))
      (when (buffer-live-p agent-buffer) (kill-buffer agent-buffer))
      (when (buffer-live-p parent-buffer) (kill-buffer parent-buffer)))))



(mevedel-deftest mevedel-agent-runtime--agent-result-format ()
  ,test
  (test)
  :doc "escapes nested mailbox delimiters in the result body"
  (let ((body
         (mevedel-agent-runtime--agent-result-format
          "verifier--1" "verifier" "Verify"
          (concat "Before.\n"
                  "<agent-result agent-id=\"inner\">\ninner\n</agent-result>\n"
                  "<agent-message from=\"inner\">\nmsg\n</agent-message>\n"
                  "After."))))
    (should (string-match-p
             "\\`<agent-result agent-id=\"verifier--1\""
             body))
    (should (string-match-p "&lt;agent-result agent-id=\"inner\"" body))
    (should (string-match-p "&lt;/agent-result&gt;" body))
    (should (string-match-p "&lt;agent-message from=\"inner\"" body))
    (should (string-match-p "&lt;/agent-message&gt;" body))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (should (= 1 (how-many "<agent-result" (point-min) (point-max))))
      (goto-char (point-min))
      (should (= 1 (how-many "</agent-result>" (point-min)
                             (point-max))))))

:doc "round-trips agent-id through format -> parse"
  (let ((s (mevedel-agent-runtime--agent-result-format
            "explorer--abc" "explorer" "test" "body content")))
    (should (equal (mevedel-agent-runtime--agent-result-parse-id s)
                   "explorer--abc")))

  :doc "XML-escapes embedded quotes in description"
  (let ((s (mevedel-agent-runtime--agent-result-format
            "x--1" "x" "He said \"hi\"" "body")))
    (should (string-match-p "&quot;hi&quot;" s))
    (should-not (string-match-p
                 "description=\"He said \"hi\"\"" s)))

  :doc "XML-escapes ampersands"
  (let ((s (mevedel-agent-runtime--agent-result-format
            "x--1" "x" "A&B" "body")))
    (should (string-match-p "A&amp;B" s))))

(mevedel-deftest mevedel-agent-runtime--bound-background-agent-result ()
  ,test
  (test)
  :doc "passes through small responses"
  (let ((inv (mevedel-agent-invocation--create)))
    (should (equal "small"
                   (mevedel-agent-runtime--bound-background-agent-result inv "small"))))
  :doc "bounds large responses and points to transcript when available"
  (let* ((inv (mevedel-agent-invocation--create
               :transcript-relative-path "agents/explorer--1.chat.org"))
         (response (make-string (* 2 mevedel-agent-runtime--background-agent-result-max-chars)
                                ?x))
         (bounded (mevedel-agent-runtime--bound-background-agent-result inv response)))
    (should (< (length bounded) (length response)))
    (should (string-match-p "Background agent result too large" bounded))
    (should (string-match-p "agents/explorer--1.chat.org" bounded))))

(mevedel-deftest mevedel-agent-runtime--agent-error-response ()
  ,test
  (test)

  :doc "error response prefers a safe transcript path"
  (let* ((session (mevedel-session--create :name "main"))
         (tempdir (file-name-as-directory
                   (make-temp-file "mevedel-agent-error-ui" t)))
         (rel-path "agents/explorer--error.chat.org")
         (abs-path (expand-file-name rel-path tempdir))
         (agent (mevedel-agent--create :name "explorer"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "explorer--error-ui"
               :parent-session session
               :transcript-relative-path rel-path)))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) tempdir)
          (make-directory (file-name-directory abs-path) t)
          (with-temp-file abs-path
            (insert "saved transcript"))
          (let ((body (mevedel-agent-runtime--agent-error-response
                       "explorer--error-ui" "explorer" "survey files"
                       "Malformed JSON in response." inv
                       "fallback should not be used")))
            (should (string-match-p "Malformed JSON in response" body))
            (should (string-match-p
                     (regexp-quote (format "Transcript: %s" abs-path))
                     body))
            (should (string-match-p
                     (regexp-quote (format "Read(file_path=%S)" abs-path))
                     body))
            (should-not (string-match-p "fallback should not be used" body))))
      (when (file-directory-p tempdir) (delete-directory tempdir t))))

  :doc "error response inlines fallback partial when transcript is absent"
  (let* ((agent (mevedel-agent--create :name "explorer"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "explorer--partial-ui")))
    (cl-letf (((symbol-function 'mevedel-agent-exec--final-response-text)
               (lambda (_invocation) nil)))
      (let ((body (mevedel-agent-runtime--agent-error-response
                   "explorer--partial-ui" "explorer" "survey files"
                   "boom" inv
                   "Explorer result for task: survey files\n\npartial result")))
        (should (string-match-p "Partial response recovered" body))
        (should (string-match-p "partial result" body))
        (should-not (string-match-p "Explorer result for task" body)))))

  :doc "error response keeps explicit fallback when no partial exists"
  (let* ((agent (mevedel-agent--create :name "explorer"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "explorer--empty-ui")))
    (cl-letf (((symbol-function 'mevedel-agent-exec--final-response-text)
               (lambda (_invocation) nil)))
      (let ((body (mevedel-agent-runtime--agent-error-response
                   "explorer--empty-ui" "explorer" "survey files"
                   "boom" inv)))
        (should (string-match-p "No saved transcript path" body)))))

  :doc "background error response queues one agent-result with recovered partial"
  (let* ((session (mevedel-session--create :name "main"))
         (parent-buf (generate-new-buffer " *mev-bg-error-parent*"))
         (agent-id "explorer--bg-error-ui")
         (agent (mevedel-agent--create :name "explorer"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id agent-id
               :description "survey files"
               :parent-context session
               :parent-session session
               :parent-data-buffer parent-buf
               :background-p t
               :transcript-status 'error)))
    (unwind-protect
        (progn
          (setf (mevedel-session-background-agents session) (list agent-id))
          (cl-letf (((symbol-function 'mevedel-agent-exec--final-response-text)
                     (lambda (_invocation) "partial background result")))
            (let ((body (mevedel-agent-runtime--agent-error-response
                         agent-id "explorer" "survey files" "boom" inv)))
              (mevedel-agent-runtime--complete-background-agent inv body)
              (mevedel-agent-runtime--complete-background-agent inv body)))
          (should (null (mevedel-session-background-agents session)))
          (should (= 1 (length (mevedel-session-messages session))))
          (let ((body (plist-get (car (mevedel-session-messages session))
                                 :body)))
            (should (string-match-p
                     (format "<agent-result agent-id=\"%s\"" agent-id)
                     body))
            (should (string-match-p "Error details: \\\"boom\\\"" body))
            (should (string-match-p "Partial response recovered" body))
            (should (string-match-p "partial background result" body))))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-agent-runtime-stop
  (:doc "stops a running background agent and resumes parent BWAIT")
  (let* ((session (mevedel-session--create :name "main"))
         (tempdir (file-name-as-directory
                   (make-temp-file "mevedel-stop-agent-bg" t)))
         (rel-path "agents/reviewer--stopped.chat.org")
         (abs-path (expand-file-name rel-path tempdir))
         (parent-buf (generate-new-buffer " *mev-stop-parent*"))
         (agent-buf (generate-new-buffer " *mev-stop-agent*"))
         (agent (mevedel-agent--create :name "reviewer"
                                       :description "Review"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "reviewer--735123142194f47363852069e3f42083"
               :description "review current diff"
               :parent-session session
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p t
               :transcript-relative-path rel-path
               :transcript-status 'running))
         (parent-fsm (gptel-make-fsm
                      :info (list :buffer parent-buf)
                      :handlers nil
                      :state 'BWAIT))
         (child-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv
                                 :callback
                                 (lambda (resp _info)
                                   (when (eq resp 'abort)
                                     (setf
                                      (mevedel-agent-invocation-transcript-status
                                       inv)
                                      'aborted)
                                     (mevedel-agent-runtime--complete-background-agent
                                      inv "abort callback body"))))
                     :handlers nil
                     :state 'WAIT))
         (gptel--request-alist
          (list (cons 'fake-process (cons child-fsm #'ignore))))
         aborted
         result)
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) tempdir)
          (make-directory (file-name-directory abs-path) t)
          (with-temp-file abs-path
            (insert "saved transcript"))
          (setf (mevedel-agent-invocation-parent-fsm inv) parent-fsm)
          (setf (mevedel-session-background-agents session)
                '("reviewer--735123142194f47363852069e3f42083"))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-agent-runtime--fsms
                        `(("reviewer--735123142194f47363852069e3f42083"
                           . ,child-fsm))))
          (cl-letf (((symbol-function 'gptel-abort)
                     (lambda (&optional _buf)
                       (setq aborted t)
                       (funcall (plist-get (gptel-fsm-info child-fsm)
                                           :callback)
                                'abort (gptel-fsm-info child-fsm))))
                    ((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_invocation) t))
                    ((symbol-function 'mevedel-agent-exec--handle-update)
                     (lambda (_invocation) nil))
                    ((symbol-function 'mevedel-agent-exec--run-stop-hook)
                     (lambda (_invocation _status) nil))
                    ((symbol-function
                      'mevedel-session-persistence--update-transcript-entry)
                     (lambda (_session _agent-id _updates) nil))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (_session _buffer) t)))
            (setq result
                  (with-current-buffer parent-buf
                    (mevedel-agent-runtime-stop
                     "reviewer--73512314" "stranded in BWAIT"))))
          (should (eq 'running (plist-get result :previous-status)))
          (should (eq 'aborted (plist-get result :status)))
          (should (plist-get result :resumed-bwait))
          (should (eq 'aborted
                      (mevedel-agent-invocation-transcript-status inv)))
          (should (equal "stranded in BWAIT"
                         (mevedel-agent-invocation-terminal-reason inv)))
          (should (null (mevedel-session-background-agents session)))
          (should (= 1 (length (mevedel-session-messages session))))
          (let ((body (plist-get (car (mevedel-session-messages session))
                                 :body)))
            (should (string-match-p "<agent-result agent-id=\"reviewer--735123142194f47363852069e3f42083\"" body))
            (should (string-match-p "was stopped" body))
            (should (string-match-p "stranded in BWAIT" body))
            (should (string-match-p (regexp-quote (format "Transcript: %s" abs-path))
                                    body))
            (should (string-match-p
                     (regexp-quote (format "Read(file_path=%S)" abs-path))
                     body)))
          (with-current-buffer parent-buf
            (should-not (assoc "reviewer--735123142194f47363852069e3f42083"
                               mevedel-agent-runtime--fsms)))
          (should (eq 'WAIT (gptel-fsm-state parent-fsm)))
          (should aborted))
      (when (file-directory-p tempdir) (delete-directory tempdir t))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-agent-runtime-stop/foreground-completes-parent-tool
  (:doc "stops a foreground agent by completing the parent Agent tool callback")
  (let* ((session (mevedel-session--create :name "main"))
         (tempdir (file-name-as-directory
                   (make-temp-file "mevedel-stop-agent-fg" t)))
         (rel-path "agents/verifier--stopped.chat.org")
         (abs-path (expand-file-name rel-path tempdir))
         (parent-buf (generate-new-buffer " *mev-stop-fg-parent*"))
         (agent-buf (generate-new-buffer " *mev-stop-fg-agent*"))
         (agent-id "verifier--foreground1234567890abcdef123456")
         (agent (mevedel-agent--create :name "verifier"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id agent-id
               :description "verify current diff"
               :parent-session session
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-relative-path rel-path
               :transcript-status 'running))
         (child-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv
                                 :callback #'ignore)
                     :handlers nil
                     :state 'WAIT))
         (gptel--request-alist nil)
         (callback-count 0)
         parent-result
         result)
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) tempdir)
          (make-directory (file-name-directory abs-path) t)
          (with-temp-file abs-path
            (insert "saved transcript"))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-agent-runtime--fsms
                        `((,agent-id . ,child-fsm))))
          (setf
           (mevedel-agent-invocation-parent-tool-callback inv)
           (lambda (response &rest _)
             (unless
                 (mevedel-agent-invocation-foreground-result-reported-p
                  inv)
               (cl-incf callback-count)
               (setq parent-result response)
               (setf
                (mevedel-agent-invocation-foreground-result-reported-p
                 inv)
                t)
               (mevedel-agent-runtime--remove-agent-registry-entry
                inv agent-id parent-buf))))
          (cl-letf (((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_invocation) t))
                    ((symbol-function 'mevedel-agent-exec--handle-update)
                     (lambda (_invocation) nil))
                    ((symbol-function 'mevedel-agent-exec--run-stop-hook)
                     (lambda (_invocation _status) nil))
                    ((symbol-function
                      'mevedel-session-persistence--update-transcript-entry)
                     (lambda (_session _agent-id _updates) nil))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (_session _buffer) t)))
            (setq result
                  (with-current-buffer parent-buf
                    (mevedel-agent-runtime-stop
                     agent-id "no longer needed"))))
          (should (= 1 callback-count))
          (should (string-match-p "was stopped" parent-result))
          (should (string-match-p "no longer needed" parent-result))
          (should (string-match-p (regexp-quote (format "Transcript: %s" abs-path))
                                  parent-result))
          (should (string-match-p
                   (regexp-quote (format "Read(file_path=%S)" abs-path))
                   parent-result))
          (should (plist-get result :completed-tool-callback))
          (should-not (plist-get result :resumed-bwait))
          (should (eq 'aborted
                      (mevedel-agent-invocation-transcript-status inv)))
          (should (null (mevedel-session-messages session)))
          (with-current-buffer parent-buf
            (should-not (assoc agent-id mevedel-agent-runtime--fsms))))
      (when (file-directory-p tempdir) (delete-directory tempdir t))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-agent-runtime-stop/foreground-no-transcript-inlines-partial
  (:doc "stopped foreground agent inlines recovered partial when transcript is absent")
  (let* ((session (mevedel-session--create :name "main"))
         (parent-buf (generate-new-buffer " *mev-stop-fg-partial-parent*"))
         (agent-buf (generate-new-buffer " *mev-stop-fg-partial-agent*"))
         (agent-id "verifier--partial1234567890abcdef123456")
         (agent (mevedel-agent--create :name "verifier"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id agent-id
               :description "verify current diff"
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (child-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv
                                 :callback #'ignore)
                     :handlers nil
                     :state 'WAIT))
         (gptel--request-alist nil)
         parent-result)
    (unwind-protect
        (progn
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-agent-runtime--fsms
                        `((,agent-id . ,child-fsm))))
          (setf
           (mevedel-agent-invocation-parent-tool-callback inv)
           (lambda (response &rest _)
             (setq parent-result response)
             (setf
              (mevedel-agent-invocation-foreground-result-reported-p inv)
              t)
             (mevedel-agent-runtime--remove-agent-registry-entry
              inv agent-id parent-buf)))
          (cl-letf (((symbol-function 'mevedel-agent-exec--final-response-text)
                     (lambda (_invocation)
                       "partial analysis from stopped verifier"))
                    ((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_invocation) t))
                    ((symbol-function 'mevedel-agent-exec--handle-update)
                     (lambda (_invocation) nil))
                    ((symbol-function 'mevedel-agent-exec--run-stop-hook)
                     (lambda (_invocation _status) nil))
                    ((symbol-function
                      'mevedel-session-persistence--update-transcript-entry)
                     (lambda (_session _agent-id _updates) nil))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (_session _buffer) t)))
            (with-current-buffer parent-buf
              (mevedel-agent-runtime-stop agent-id "operator stop")))
          (should (string-match-p "Partial response recovered" parent-result))
          (should (string-match-p "partial analysis from stopped verifier"
                                  parent-result))
          (should-not (string-match-p "Transcript:" parent-result)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-agent-runtime-stop/foreground-abort-race-is-once
  (:doc "foreground stop wins the abort race and reports to parent once")
  (let* ((session (mevedel-session--create :name "main"))
         (parent-buf (generate-new-buffer " *mev-stop-fg-race-parent*"))
         (agent-buf (generate-new-buffer " *mev-stop-fg-race-agent*"))
         (agent-id "verifier--race1234567890abcdef1234567890")
         (agent (mevedel-agent--create :name "verifier"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id agent-id
               :description "verify current diff"
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (child-fsm nil)
         (gptel--request-alist nil)
         (callback-count 0)
         parent-result
         aborted
         result)
    (unwind-protect
        (progn
          (setf
           (mevedel-agent-invocation-parent-tool-callback inv)
           (lambda (response &rest _)
             (unless
                 (mevedel-agent-invocation-foreground-result-reported-p
                  inv)
               (cl-incf callback-count)
               (setq parent-result response)
               (setf
                (mevedel-agent-invocation-foreground-result-reported-p
                 inv)
                t)
               (mevedel-agent-runtime--remove-agent-registry-entry
                inv agent-id parent-buf))))
          (setq child-fsm
                (gptel-make-fsm
                 :info (list :buffer agent-buf
                             :mevedel-agent-invocation inv
                             :callback
                             (lambda (resp _info)
                               (when (eq resp 'abort)
                                 (funcall
                                  (mevedel-agent-invocation-parent-tool-callback
                                   inv)
                                  "Error: generic abort"))))
                 :handlers nil
                 :state 'WAIT))
          (setq gptel--request-alist
                (list (cons 'fake-process (cons child-fsm #'ignore))))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-agent-runtime--fsms
                        `((,agent-id . ,child-fsm))))
          (cl-letf (((symbol-function 'gptel-abort)
                     (lambda (&optional _buf)
                       (setq aborted t)
                       (funcall (plist-get (gptel-fsm-info child-fsm)
                                           :callback)
                                'abort (gptel-fsm-info child-fsm))))
                    ((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_invocation) t))
                    ((symbol-function 'mevedel-agent-exec--handle-update)
                     (lambda (_invocation) nil))
                    ((symbol-function 'mevedel-agent-exec--run-stop-hook)
                     (lambda (_invocation _status) nil))
                    ((symbol-function
                      'mevedel-session-persistence--update-transcript-entry)
                     (lambda (_session _agent-id _updates) nil))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (_session _buffer) t)))
            (setq result
                  (with-current-buffer parent-buf
                    (mevedel-agent-runtime-stop
                     agent-id "operator stop"))))
          (should aborted)
          (should (= 1 callback-count))
          (should (string-match-p "operator stop" parent-result))
          (should-not (string-match-p "generic abort" parent-result))
          (should (plist-get result :completed-tool-callback))
          (with-current-buffer parent-buf
            (should-not (assoc agent-id mevedel-agent-runtime--fsms))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-agent-runtime-stop/recovers-parent-fsm-from-request-alist
  (:doc "resumes parent BWAIT even when invocation lost its parent-fsm slot")
  (let* ((session (mevedel-session--create :name "main"))
         (parent-buf (generate-new-buffer " *mev-stop-recover-parent*"))
         (agent-buf (generate-new-buffer " *mev-stop-recover-agent*"))
         (agent (mevedel-agent--create :name "verifier"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "verifier--cf9dca9d45d108008685cd1c40a86a09"
               :description "verify current diff"
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p t
               :transcript-status 'running))
         (parent-fsm (gptel-make-fsm
                      :info (list :buffer parent-buf)
                      :state 'BWAIT))
         (child-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv
                                 :callback #'ignore)
                     :state 'WAIT))
         (gptel--request-alist
          (list (cons 'parent-process (cons parent-fsm #'ignore))))
         result)
    (unwind-protect
        (progn
          (setf (mevedel-session-background-agents session)
                '("verifier--cf9dca9d45d108008685cd1c40a86a09"))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-agent-runtime--fsms
                        `(("verifier--cf9dca9d45d108008685cd1c40a86a09"
                           . ,child-fsm))))
          (cl-letf (((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_invocation) t))
                    ((symbol-function 'mevedel-agent-exec--handle-update)
                     (lambda (_invocation) nil))
                    ((symbol-function 'mevedel-agent-exec--run-stop-hook)
                     (lambda (_invocation _status) nil))
                    ((symbol-function 'gptel--fsm-transition)
                     (lambda (fsm state)
                       (setf (gptel-fsm-state fsm) state)))
                    ((symbol-function
                      'mevedel-session-persistence--update-transcript-entry)
                     (lambda (_session _agent-id _updates) nil))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (_session _buffer) t)))
            (setq result
                  (with-current-buffer parent-buf
                    (mevedel-agent-runtime-stop
                     "verifier--cf9dca9d" "recover parent"))))
          (should (eq parent-fsm
                      (mevedel-agent-invocation-parent-fsm inv)))
          (should (plist-get result :resumed-bwait))
          (should (eq 'WAIT (gptel-fsm-state parent-fsm)))
          (should (null (mevedel-session-background-agents session)))
          (should (= 1 (length (mevedel-session-messages session)))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-agent-runtime--resolve-agent-stop-target
  (:doc "accepts exact ids and rejects ambiguous displayed short ids")
  (let* ((parent-buf (generate-new-buffer " *mev-stop-resolve-parent*"))
         (agent-buf-a (generate-new-buffer " *mev-stop-resolve-agent-a*"))
         (agent-buf-b (generate-new-buffer " *mev-stop-resolve-agent-b*"))
         (agent (mevedel-agent--create :name "reviewer"))
         (inv-a (mevedel-agent-invocation--create
                 :agent agent
                 :agent-id "reviewer--aaaaaaaa111111111111111111111111"
                 :buffer agent-buf-a
                 :transcript-status 'running))
         (inv-b (mevedel-agent-invocation--create
                 :agent agent
                 :agent-id "reviewer--aaaaaaaa222222222222222222222222"
                 :buffer agent-buf-b
                 :transcript-status 'running))
         (fsm-a (gptel-make-fsm
                 :info (list :buffer agent-buf-a
                             :mevedel-agent-invocation inv-a)
                 :state 'WAIT))
         (fsm-b (gptel-make-fsm
                 :info (list :buffer agent-buf-b
                             :mevedel-agent-invocation inv-b)
                 :state 'WAIT)))
    (unwind-protect
        (progn
          (with-current-buffer parent-buf
            (setq-local mevedel-agent-runtime--fsms
                        `(("reviewer--aaaaaaaa111111111111111111111111"
                           . ,fsm-a)
                          ("reviewer--aaaaaaaa222222222222222222222222"
                           . ,fsm-b))))
	          (should (equal "reviewer--aaaaaaaa111111111111111111111111"
	                         (car (mevedel-agent-runtime--resolve-agent-stop-target
	                               "reviewer--aaaaaaaa111111111111111111111111"
	                               parent-buf))))
	          (should-error
	           (mevedel-agent-runtime--resolve-agent-stop-target
	            "reviewer--aaaaaaaa" parent-buf)))
	      (when (buffer-live-p agent-buf-a) (kill-buffer agent-buf-a))
	      (when (buffer-live-p agent-buf-b) (kill-buffer agent-buf-b))
	      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-agent-runtime--foreground-watchdog-expire ()
  ,test
  (test)
  :doc "stops a foreground agent when its progress snapshot is unchanged"
  (let* ((mevedel-agent-runtime--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent*"))
         (agent-id "verifier--fg-watchdog")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (clock 100.0)
         stopped)
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (insert "prompt"))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _args) 'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'mevedel-agent-runtime-stop)
                     (lambda (id reason parent)
                       (setq stopped (list id reason parent)))))
            (mevedel-agent-runtime--foreground-watchdog-arm inv)
            (setq clock 110.0)
            (mevedel-agent-runtime--foreground-watchdog-expire agent-id))
          (should (equal agent-id (car stopped)))
          (should (string-match-p "no progress" (cadr stopped)))
          (should (eq parent-buf (caddr stopped)))
          (should-not (gethash agent-id
                               mevedel-agent-runtime--foreground-watchdogs)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

  :doc "reschedules instead of stopping when buffer content progresses"
  (let* ((mevedel-agent-runtime--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent-progress*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent-progress*"))
         (agent-id "verifier--fg-progress")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (clock 100.0)
         (timer-count 0)
         delays
         stopped)
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (insert "prompt"))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (delay &rest _args)
                       (cl-incf timer-count)
                       (push delay delays)
                       'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'mevedel-agent-runtime-stop)
                     (lambda (&rest args) (setq stopped args))))
            (mevedel-agent-runtime--foreground-watchdog-arm inv)
            (with-current-buffer agent-buf
              (insert "\npartial response"))
            (setq clock 110.0)
            (mevedel-agent-runtime--foreground-watchdog-expire agent-id))
          (should-not stopped)
          (should (= 2 timer-count))
          (should (equal '(10 10) (nreverse delays)))
          (should (gethash agent-id mevedel-agent-runtime--foreground-watchdogs)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

  :doc "reschedules from latest activity time instead of launch time"
  (let* ((mevedel-agent-runtime--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent-activity*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent-activity*"))
         (agent-id "verifier--fg-activity")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (clock 100.0)
         delays
         stopped)
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (insert "prompt"))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (delay &rest _args)
                       (push delay delays)
                       'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'mevedel-agent-runtime-stop)
                     (lambda (&rest args) (setq stopped args))))
            (mevedel-agent-runtime--foreground-watchdog-arm inv)
            (setf (mevedel-agent-invocation-activity inv)
                  '((:type tool-start :time 105.0)))
            (setq clock 109.0)
            (mevedel-agent-runtime--foreground-watchdog-expire agent-id))
          (should-not stopped)
          (should (equal '(10 6) (nreverse delays)))
          (should (gethash agent-id mevedel-agent-runtime--foreground-watchdogs)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

  :doc "early foreground watchdog timer reschedules remaining grace"
  (let* ((mevedel-agent-runtime--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent-early*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent-early*"))
         (agent-id "verifier--fg-early")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (clock 100.0)
         delays
         stopped)
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (insert "prompt"))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (delay &rest _args)
                       (push delay delays)
                       'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'mevedel-agent-runtime-stop)
                     (lambda (&rest args) (setq stopped args))))
            (mevedel-agent-runtime--foreground-watchdog-arm inv)
            (setq clock 105.0)
            (mevedel-agent-runtime--foreground-watchdog-expire agent-id))
          (should-not stopped)
          (should (equal '(10 5) (nreverse delays)))
          (should (gethash agent-id mevedel-agent-runtime--foreground-watchdogs)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

  :doc "records foreground watchdog reschedule reason in transcript metadata"
  (let* ((mevedel-agent-runtime--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (session (mevedel-session--create :name "main"))
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent-log*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent-log*"))
         (agent-id "verifier--fg-log")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-session session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (clock 100.0)
         (timer-count 0)
         logged
         stopped)
    (unwind-protect
        (progn
          (setf (mevedel-session-agent-transcripts session)
                `((,agent-id :status running)))
          (with-current-buffer agent-buf
            (insert "prompt"))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _args)
                       (cl-incf timer-count)
                       'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq logged (apply #'format fmt args))))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (_session _buffer) t))
                    ((symbol-function 'mevedel-agent-runtime-stop)
                     (lambda (&rest args) (setq stopped args))))
            (mevedel-agent-runtime--foreground-watchdog-arm inv)
            (with-current-buffer agent-buf
              (insert "\npartial response"))
            (setq clock 110.0)
            (mevedel-agent-runtime--foreground-watchdog-expire agent-id))
          (should-not stopped)
          (should (= 2 timer-count))
          (should (string-match-p "rescheduled" logged))
          (let* ((entry (cdr (assoc agent-id
                                    (mevedel-session-agent-transcripts
                                     session))))
                 (watchdog (plist-get entry :watchdog)))
            (should (eq 'rescheduled (plist-get watchdog :state)))
            (should (eq 'progress (plist-get watchdog :reason)))
            (should (plist-member watchdog :current-size))
            (should (plist-member watchdog :last-size))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

  :doc "reschedules while the foreground agent is waiting on child work"
  (let* ((mevedel-agent-runtime--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent-child*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent-child*"))
         (agent-id "coordinator--fg-child")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (clock 100.0)
         (timer-count 0)
         stopped)
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (insert "prompt"))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _args)
                       (cl-incf timer-count)
                       'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'mevedel-agent-runtime-stop)
                     (lambda (&rest args) (setq stopped args))))
            (mevedel-agent-runtime--foreground-watchdog-arm inv)
            (mevedel-agent-runtime--ctx-push-background-agent inv "explorer--child")
            (setq clock 110.0)
            (mevedel-agent-runtime--foreground-watchdog-expire agent-id))
          (should-not stopped)
          (should (= 2 timer-count))
          (should (gethash agent-id mevedel-agent-runtime--foreground-watchdogs)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

  :doc "stale tool-pending flag outside TOOL does not suppress watchdog stop"
  (let* ((mevedel-agent-runtime--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent-stale-tool*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent-stale-tool*"))
         (agent-id "verifier--fg-stale-tool")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (agent-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :tool-pending t
                                 :mevedel-agent-invocation inv)
                     :state 'WAIT))
         (clock 100.0)
         stopped)
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (insert "prompt"))
          (with-current-buffer parent-buf
            (setq-local mevedel-agent-runtime--fsms
                        `((,agent-id . ,agent-fsm))))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _args) 'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'mevedel-agent-runtime-stop)
                     (lambda (id reason parent)
                       (setq stopped (list id reason parent)))))
            (mevedel-agent-runtime--foreground-watchdog-arm inv)
            (setq clock 110.0)
            (mevedel-agent-runtime--foreground-watchdog-expire agent-id))
          (should (equal agent-id (car stopped)))
          (should (string-match-p "no progress" (cadr stopped)))
          (should (eq parent-buf (caddr stopped)))
          (should-not (gethash agent-id
                               mevedel-agent-runtime--foreground-watchdogs)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

  :doc "stops a foreground TOOL-state agent after no progress grace"
  (let* ((mevedel-agent-runtime--foreground-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-no-progress-timeout 10)
         (parent-buf (generate-new-buffer " *mev-fg-watchdog-parent-tool*"))
         (agent-buf (generate-new-buffer " *mev-fg-watchdog-agent-tool*"))
         (agent-id "verifier--fg-tool")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p nil
               :transcript-status 'running))
         (agent-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv)
                     :state 'TOOL))
         (clock 100.0)
         stopped)
    (unwind-protect
        (progn
          (with-current-buffer agent-buf
            (insert "prompt"))
          (with-current-buffer parent-buf
            (setq-local mevedel-agent-runtime--fsms
                        `((,agent-id . ,agent-fsm))))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _args) 'fake-timer))
                    ((symbol-function 'float-time)
                     (lambda (&optional _time) clock))
                    ((symbol-function 'mevedel-agent-runtime-stop)
                     (lambda (id reason parent)
                       (setq stopped (list id reason parent)))))
            (mevedel-agent-runtime--foreground-watchdog-arm inv)
            (setq clock 110.0)
            (mevedel-agent-runtime--foreground-watchdog-expire agent-id))
          (should (equal agent-id (car stopped)))
          (should (string-match-p "no progress" (cadr stopped)))
          (should (eq parent-buf (caddr stopped)))
          (should-not (gethash agent-id
                               mevedel-agent-runtime--foreground-watchdogs)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-agent-runtime--bwait-watchdog-expire
  (:doc "running-agent warning advertises StopAgent recovery")
  (let* ((session (mevedel-session--create :name "main"))
         (parent-buf (generate-new-buffer " *mev-watchdog-parent*"))
         (agent-buf (generate-new-buffer " *mev-watchdog-agent*"))
         (agent (mevedel-agent--create :name "reviewer"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "reviewer--WATCHDOG"
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p t
               :transcript-status 'running))
         (parent-fsm (gptel-make-fsm
                      :info (list :buffer parent-buf)
                      :state 'BWAIT))
         (child-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv)
                     :state 'WAIT))
         logged)
    (unwind-protect
        (progn
          (setf (mevedel-session-background-agents session)
                '("reviewer--WATCHDOG"))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-agent-runtime--fsms
                        `(("reviewer--WATCHDOG" . ,child-fsm))))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq logged (apply #'format fmt args))))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _args) nil)))
            (let ((mevedel-agent-background-timeout 600))
              (mevedel-agent-runtime--bwait-watchdog-expire parent-fsm)))
          (should (string-match-p "StopAgent" logged))
          (should (string-match-p "mevedel-stop-agent" logged)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-agent-runtime--ctx-messages
  (:after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)))
  ,test
  (test)

  :doc "reads and writes a session mailbox"
  (let ((session (mevedel-agent-runtime-test--make-session)))
    (should (null (mevedel-agent-runtime--ctx-messages session)))
    (mevedel-agent-runtime--ctx-push-message
     session '(:from "alpha" :body "hi"))
    (should (equal '((:from "alpha" :body "hi"))
                   (mevedel-agent-runtime--ctx-messages session)))
    (setf (mevedel-agent-runtime--ctx-messages session) nil)
    (should (null (mevedel-session-messages session))))

  :doc "reads and writes an agent-invocation mailbox"
  (let* ((_ (mevedel-define-agent msg-a :description "a" :tools nil))
         (agent (mevedel-agent-get "msg-a"))
         (inv (mevedel-agent-invocation-create agent)))
    (mevedel-agent-runtime--ctx-push-message inv '(:from "main" :body "ping"))
    (mevedel-agent-runtime--ctx-push-message inv '(:from "main" :body "pong"))
    ;; Messages are pushed onto the head for O(1) enqueue; the drain
    ;; reverses so arrival order is preserved at delivery time.
    (should (equal '((:from "main" :body "ping")
                     (:from "main" :body "pong"))
                   (nreverse (mevedel-agent-invocation-messages inv))))))

(mevedel-deftest mevedel-agent-runtime-dispatch
  (:before-each (progn (mevedel-tool-clear-registry)
                       ;; Built-in agents reference tool groups (read,
                       ;; code, web, ...) that `mevedel-agent-invocation-create'
                       ;; resolves eagerly, so every tool category must
                       ;; be registered.  The task now rejects unknown
                       ;; agent types up front, so agents must be
                       ;; re-registered per subtest as well.
                       (mevedel-agent-runtime-test--register-agent-tools)
                       (setq mevedel-agent--registry
                             (copy-sequence
                              mevedel-agent-runtime-test--agent-registry)))
   :after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)
                      (mevedel-tool-clear-registry)))
  ,test
  (test)

  :doc "background mode calls main-cb immediately with launch status"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-bg*"))
         (result nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          ;; Mock gptel-agent--task: capture the callback and return a
          ;; fake FSM with an overlay context.
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (_cb _type _desc _prompt invocation _agent-buffer
                               &optional _configure-fsm)
                        (mevedel-agent-runtime-test--register-agent-fsm
                         invocation fake-fsm))))
            (let ((mevedel-tools--current-fsm nil))
              (mevedel-agent-runtime-dispatch
               (lambda (resp &rest _) (setq result resp))
               (mevedel-agent-get "explorer") "survey" "survey files"
               :background t))
            ;; main-cb should have been called synchronously.
            ;; The launch status may be wrapped with render-data for
            ;; the running-handle badge when a transcript path is set
            ;; on the invocation; extract the visible string from
            ;; either shape.
            (let ((launch-string
                   (cond
                    ((stringp result) result)
                    ((and (listp result) (plist-get result :result))
                     (plist-get result :result))
	                    (t (error "Unexpected main-cb shape: %S" result)))))
              (should (stringp launch-string))
              (should (string-match-p "background" launch-string))
              (should (string-match-p "explorer" launch-string)))
            (when (and (listp result) (plist-get result :render-data))
              (should (eq t (plist-get (plist-get result :render-data)
                                        :background))))
            ;; FSM should be registered
            (should (= 1 (length mevedel-agent-runtime--fsms)))))
      (kill-buffer buf)))

  :doc "background agent result is delivered to parent mailbox"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-bg2*"))
         (captured-cb nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &rest _)
                        (setq captured-cb cb)
                        fake-fsm)))
            (let ((mevedel-tools--current-fsm nil))
              (mevedel-agent-runtime-dispatch
               #'ignore (mevedel-agent-get "explorer") "survey" "survey files"
               :background t))
            ;; Simulate sub-agent completing
            (funcall captured-cb "The exploration found 5 issues.")
            ;; Result should be in the parent session's mailbox
            (let* ((msgs (mevedel-session-messages session))
                   (msg (car msgs)))
              (should (= 1 (length msgs)))
              (should (string-match-p "explorer" (plist-get msg :from)))
              (should (string-match-p "5 issues" (plist-get msg :body))))))
      (kill-buffer buf)))

  :doc "foreground mode does not call main-cb until sub-agent finishes"
  (let* ((buf (generate-new-buffer " *mt-fg*"))
         (captured-cb nil)
         (result nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel-agent-runtime--fsms nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &rest _)
                        (setq captured-cb cb)
                        fake-fsm)))
            (mevedel-agent-runtime-dispatch
             (lambda (resp &rest _) (setq result resp))
             (mevedel-agent-get "explorer") "survey" "survey files")
            ;; main-cb should NOT have been called yet
            (should (null result))
            ;; Simulate sub-agent completing
            (funcall captured-cb "Done.")
            (should (equal "Done." result))))
      (kill-buffer buf)))

  :doc "foreground stop completes the parent Agent tool callback"
  (require 'mevedel-tool-ui)
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-fg-stop*"))
         (result nil)
         (main-calls 0)
         (inv nil)
         (child-fsm nil)
         (stop-result nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (_cb _type _desc _prompt
                                   &optional invocation _agent-buffer
                                   _configure-fsm)
                        (setq inv invocation)
                        (setq child-fsm
                              (gptel-make-fsm
                               :info (list :context ov
                                           :buffer buf
                                           :mevedel-agent-invocation inv
                                           :callback #'ignore)
                               :state 'WAIT))
                        (mevedel-agent-runtime-test--register-agent-fsm
                         inv child-fsm)))
                     ((symbol-function
                       'mevedel-agent-exec--save-transcript-buffer)
                      (lambda (_invocation) t))
                     ((symbol-function 'mevedel-agent-exec--handle-update)
                      (lambda (_invocation) nil))
                     ((symbol-function 'mevedel-agent-exec--run-stop-hook)
                      (lambda (_invocation _status) nil))
                     ((symbol-function
                       'mevedel-session-persistence--update-transcript-entry)
                      (lambda (_session _agent-id _updates) nil))
                     ((symbol-function
                       'mevedel-session-persistence--write-sidecar-now)
                      (lambda (_session _buffer) t)))
            (mevedel-agent-runtime-dispatch
             (lambda (resp &rest _)
               (cl-incf main-calls)
               (setq result resp))
             (mevedel-agent-get "explorer") "survey" "survey files")
            (should inv)
            (should (functionp
                     (mevedel-agent-invocation-parent-tool-callback inv)))
            (should (assoc (mevedel-agent-invocation-agent-id inv)
                           mevedel-agent-runtime--fsms))
            (setq stop-result
                  (mevedel-agent-runtime-stop
                   (mevedel-agent-invocation-agent-id inv)
                   "manual stop"))
            (should (= 1 main-calls))
            (should (string-match-p "was stopped" result))
            (should (string-match-p "manual stop" result))
            (should (plist-get stop-result :completed-tool-callback))
            (should (eq 'aborted
                        (mevedel-agent-invocation-transcript-status inv)))
            (should-not (assoc (mevedel-agent-invocation-agent-id inv)
                               mevedel-agent-runtime--fsms))))
      (kill-buffer buf)))

  :doc "Agent handler treats `:run_in_background :json-false' as foreground"
  (require 'mevedel-tool-ui)
  (let* ((buf (generate-new-buffer " *mt-agent-false*"))
         (captured-cb nil)
         (result nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel-agent-runtime--fsms nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &rest _)
                        (setq captured-cb cb)
                        fake-fsm)))
            (mevedel-tool-ui--agent
             (lambda (resp &rest _) (setq result resp))
             '(:subagent_type "explorer"
               :description "survey"
               :prompt "survey files"
               :run_in_background :json-false))
            ;; Foreground path: main-cb must not fire yet, and the
            ;; launch-status string must NOT be returned synchronously.
            (should (null result))
            (funcall captured-cb "Done.")
            (should (equal "Done." (plist-get result :result)))))
      (kill-buffer buf)))

  :doc "Agent handler forwards optional model tier"
  (require 'mevedel-tool-ui)
  (let ((captured nil)
        (result nil))
    (cl-letf (((symbol-function 'mevedel-agent-runtime-dispatch)
               (lambda (_cb agent description prompt &rest options)
                 (setq captured
                       (list :agent-type (mevedel-agent-name agent)
                             :description description
                             :prompt prompt
                             :background (plist-get options :background)
                             :model-tier (plist-get options :model-tier))))))
      (mevedel-tool-ui--agent
       (lambda (resp &rest _) (setq result resp))
       '(:subagent_type "explorer"
         :description "survey"
         :prompt "survey files"
         :model "fast"))
      (should (null result))
      (should (equal "explorer" (plist-get captured :agent-type)))
      (should (eq 'fast (plist-get captured :model-tier)))))

  :doc "Agent handler rejects concrete provider strings"
  (require 'mevedel-tool-ui)
  (let ((called nil)
        (result nil))
    (cl-letf (((symbol-function 'mevedel-agent-runtime-dispatch)
               (lambda (&rest _)
                 (setq called t))))
      (mevedel-tool-ui--agent
       (lambda (resp &rest _) (setq result resp))
       '(:subagent_type "explorer"
         :description "survey"
         :prompt "survey files"
         :model "Claude:claude-sonnet-4-5"))
      (should (null called))
      (should (string-match-p "Unknown model tier"
                              (plist-get result :result)))))

  :doc "unknown agent type is rejected up front with an Error response"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-unknown*"))
         (result nil)
         (runner-called nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
	          (cl-letf (((symbol-function 'mevedel-agent-exec--run)
	                     (lambda (&rest _)
	                       (setq runner-called t)
	                       (error "Runner must not be called for unknown agent"))))
            (mevedel-tool-ui--agent
             (lambda (resp &rest _) (setq result resp))
             '(:subagent_type "no-such-agent-type"
               :description "oops" :prompt "do nothing"))
            (should (null runner-called))
            (should (stringp (plist-get result :result)))
            (should (string-match-p "Unknown agent type: no-such-agent-type"
                                    (plist-get result :result)))
            ;; No background tracking should have been created.
            (should (null (mevedel-session-background-agents session)))
            (should (null mevedel-agent-runtime--fsms))))
      (kill-buffer buf)))

  :doc "background launch failure does not leave stale running reminder"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-bg-start-fail*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
	          (cl-letf (((symbol-function 'mevedel-agent-exec--run)
	                     (lambda (&rest _)
	                       (error "Boom"))))
            (let ((mevedel-tools--current-fsm nil))
              (should-error
               (mevedel-agent-runtime-dispatch
                #'ignore (mevedel-agent-get "explorer")
                "survey" "survey files" :background t))))
          (should (null (mevedel-session-background-agents session)))
          (should (null (mevedel-session-pending-reminders session)))
          (should (null mevedel-agent-runtime--fsms)))
      (kill-buffer buf))))

(mevedel-deftest mevedel-agent-runtime--background-agents-pending-p
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "returns nil when no background agents are pending"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-bwait1*"))
         (info (list :buffer buf :context nil)))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (should-not (mevedel-agent-runtime--background-agents-pending-p info)))
      (kill-buffer buf)))

  :doc "returns non-nil when session has background agents"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-bwait2*"))
         (info (list :buffer buf :context nil)))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (mevedel-agent-runtime--ctx-push-background-agent session "explorer--abc123")
          (should (mevedel-agent-runtime--background-agents-pending-p info)))
      (kill-buffer buf)))

  :doc "returns non-nil when invocation has background agents"
  (require 'mevedel-tool-ui)
  (let* ((buf (generate-new-buffer " *mt-bwait3*"))
         (inv (mevedel-agent-invocation--create
               :agent (mevedel-agent--create :name "coordinator")))
         (info (list :buffer buf :mevedel-agent-invocation inv)))
    (unwind-protect
        (progn
          (mevedel-agent-runtime--ctx-push-background-agent inv "explorer--abc123")
          (should (mevedel-agent-runtime--background-agents-pending-p info)))
      (kill-buffer buf)))

  :doc "returns non-nil when mailbox has messages but no background agents"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-bwait4*"))
         (info (list :buffer buf :context nil)))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (mevedel-agent-runtime--ctx-push-message
           session '(:from "explorer--x" :body "done"))
          (should (mevedel-agent-runtime--background-agents-pending-p info)))
      (kill-buffer buf)))

  :doc "parks an invocation while its captured owner has a live execution"
  (require 'mevedel-tool-ui)
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (agent-id "explorer--bash-owner")
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :agent (mevedel-agent--create :name "explorer")
               :parent-session session))
         (info (list :mevedel-agent-invocation inv))
         captured)
    (cl-letf (((symbol-function 'mevedel-execution-owner-live-p)
               (lambda (seen-session seen-owner)
                 (setq captured (list seen-session seen-owner))
                 t)))
      (should (mevedel-agent-runtime--background-agents-pending-p info)))
    (should (equal (list session agent-id) captured))))

(mevedel-deftest mevedel-agent-runtime--bwait-injected-table
  ()
  ,test
  (test)

  :doc "inserts BWAIT before DONE in TYPE and TRET transitions"
  (let* ((table `((INIT . ((t . WAIT)))
                  (WAIT . ((t . TYPE)))
                  (TYPE . ((tool-p . TPRE) (t . DONE)))
                  (TRET . ((error-p . ERRS) (result-p . WAIT) (t . DONE)))))
         (result (mevedel-agent-runtime--bwait-injected-table table)))
    ;; TYPE should have BWAIT before DONE
    (let ((type-transitions (cdr (assq 'TYPE result))))
      (should (= 3 (length type-transitions)))
      (should (eq 'TPRE (cdar type-transitions)))
      (should (eq 'BWAIT (cdadr type-transitions)))
      (should (eq 'DONE (cdaddr type-transitions))))
    ;; TRET should have BWAIT before DONE
    (let ((tret-transitions (cdr (assq 'TRET result))))
      (should (= 4 (length tret-transitions)))
      (should (eq 'BWAIT (cdr (nth 2 tret-transitions))))
      (should (eq 'DONE (cdr (nth 3 tret-transitions)))))
    ;; BWAIT state should exist with no outgoing transitions
    (let ((bwait-entry (assq 'BWAIT result)))
      (should bwait-entry)
      (should (null (cdr bwait-entry)))))

  :doc "does not modify unrelated states"
  (let* ((table `((INIT . ((t . WAIT)))
                  (WAIT . ((t . TYPE)))
                  (TYPE . ((t . DONE)))
                  (TRET . ((t . DONE)))))
         (result (mevedel-agent-runtime--bwait-injected-table table)))
    (should (equal '((t . WAIT)) (cdr (assq 'INIT result))))
    (should (equal '((t . TYPE)) (cdr (assq 'WAIT result)))))

  :doc "re-injecting an already-injected table is a no-op (no duplicate predicates)"
  (let* ((mevedel-agent-runtime--bwait-table-cache nil)
         (table `((INIT . ((t . WAIT)))
                  (WAIT . ((t . TYPE)))
                  (TYPE . ((tool-p . TPRE) (t . DONE)))
                  (TRET . ((error-p . ERRS) (result-p . WAIT) (t . DONE)))))
         (once (mevedel-agent-runtime--bwait-injected-table table))
         (twice (mevedel-agent-runtime--bwait-injected-table once)))
    ;; Re-injection returns the same object unchanged -- the injector
    ;; bails out when BWAIT is already present.
    (should (eq once twice))
    (let ((type-transitions (cdr (assq 'TYPE twice))))
      (should (= 3 (length type-transitions)))
      (should (eq 'BWAIT (cdadr type-transitions))))))

(mevedel-deftest mevedel-agent-runtime-dispatch-bwait
  (:before-each (progn (mevedel-tool-clear-registry)
                       (mevedel-agent-runtime-test--register-agent-tools)
                       (setq mevedel-agent--registry
                             (copy-sequence
                              mevedel-agent-runtime-test--agent-registry)))
   :after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)
                      (mevedel-tool-clear-registry)))
  ,test
  (test)

  :doc "background spawn tracks agent on parent context"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-bwait-track*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (_cb _type _desc _prompt &rest _)
                        fake-fsm)))
            (let ((mevedel-tools--current-fsm nil))
              (mevedel-agent-runtime-dispatch
               #'ignore (mevedel-agent-get "explorer") "survey" "survey files"
               :background t))
            ;; Agent ID should be tracked on the session.
            (should (= 1 (length (mevedel-session-background-agents session))))))
      (kill-buffer buf)))

  :doc "background completion removes agent from tracking"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-bwait-rm*"))
         (captured-cb nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &rest _)
                        (setq captured-cb cb)
                        fake-fsm)))
            (let ((mevedel-tools--current-fsm nil))
              (mevedel-agent-runtime-dispatch
               #'ignore (mevedel-agent-get "explorer") "survey" "survey files"
               :background t))
            ;; Complete the background agent.
            (funcall captured-cb "Done.")
            ;; Agent should be removed from tracking.
            (should (null (mevedel-session-background-agents session)))))
      (kill-buffer buf)))

  :doc "background completion resumes parent FSM from BWAIT"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-bwait-resume*"))
         (captured-cb nil)
         (resumed nil)
         ;; A WAIT handler that records the transition instead of
         ;; firing an HTTP request.
         (parent-fsm (gptel-make-fsm
                      :table `((BWAIT) (WAIT . ((t . TYPE))))
                      :handlers `((WAIT ,(lambda (_fsm) (setq resumed t))))
                      :info (list :buffer buf))))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &rest _)
                        (setq captured-cb cb)
                        fake-fsm)))
            ;; Simulate parent FSM dispatching a background agent.
            (let ((mevedel-tools--current-fsm parent-fsm))
              (mevedel-agent-runtime-dispatch
               #'ignore (mevedel-agent-get "explorer") "survey" "survey files"
               :background t :parent-fsm parent-fsm))
            ;; Park the parent FSM in BWAIT.
            (setf (gptel-fsm-state parent-fsm) 'BWAIT)
            ;; Complete the background agent.
            (funcall captured-cb "Done.")
            ;; Parent FSM should have been resumed to WAIT.
            (should (eq 'WAIT (gptel-fsm-state parent-fsm)))
            (should resumed)))
      (kill-buffer buf)))

  :doc "direct foreground settlement retains state until delivery succeeds"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-bwait-foreground-execution*"))
         (live-p t)
         (attempts 0)
         captured-cb invocation result)
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (let ((mevedel-agent-no-progress-timeout nil))
            (cl-letf (((symbol-function 'mevedel-execution-owner-live-p)
                       (lambda (_session _owner) live-p))
                      ((symbol-function 'mevedel-agent-exec--run)
                       (lambda (cb _type _desc _prompt inv agent-buffer
                                   &optional configure)
                         (setq captured-cb cb
                               invocation inv)
                         (let ((fsm
                                (gptel-make-fsm
                                 :state 'TYPE
                                 :info (list :buffer agent-buffer))))
                           (funcall configure fsm inv)
                           (setf (gptel-fsm-info fsm)
                                 (plist-put
                                  (gptel-fsm-info fsm)
                                  :mevedel-agent-terminal-callback cb))
                           fsm))))
              (mevedel-agent-runtime-dispatch
               (lambda (value)
                 (cl-incf attempts)
                 (if (= attempts 1)
                     (error "Transient parent callback failure")
                   (setq result value)))
               (mevedel-agent-get "explorer") "survey" "survey files")
              (funcall captured-cb "Done.")
              (should-not result)
              (should (zerop attempts))
              (should (assoc
                       (mevedel-agent-invocation-agent-id invocation)
                      mevedel-agent-runtime--fsms))
              (setq live-p nil)
              (let ((mevedel-agent-runtime--direct-execution-settlement-p t))
                (should-error (funcall captured-cb "Done.")))
              (should (= 1 attempts))
              (should-not
               (mevedel-agent-invocation-foreground-result-reported-p
                invocation))
              (should (assoc
                       (mevedel-agent-invocation-agent-id invocation)
                       mevedel-agent-runtime--fsms))
              (let ((mevedel-agent-runtime--direct-execution-settlement-p t))
                (funcall captured-cb "Done."))
              (should (= 2 attempts))
              (should result)
              (should-not
               (assoc (mevedel-agent-invocation-agent-id invocation)
                      mevedel-agent-runtime--fsms)))))
      (when (and invocation
                 (buffer-live-p (mevedel-agent-invocation-buffer invocation)))
        (kill-buffer (mevedel-agent-invocation-buffer invocation)))
      (kill-buffer buf)))

  :doc "ordinary foreground callback failure cannot strand the Agent tool"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-foreground-one-shot-failure*"))
         (attempts 0)
         raw-callback callback-info invocation)
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (let ((mevedel-agent-no-progress-timeout nil))
            (cl-letf
                (((symbol-function 'mevedel-execution-owner-live-p)
                  (lambda (_session _owner) nil))
                 ((symbol-function 'mevedel-agent-exec--run)
                  (lambda (cb agent-type description _prompt inv agent-buffer
                              &optional configure)
                    (setq invocation inv
                          callback-info
                          (list :buffer agent-buffer
                                :stream t
                                :mevedel-agent-invocation inv)
                          raw-callback
                          (mevedel-agent-exec--make-callback
                           cb agent-type description nil (list "")))
                    (let ((fsm
                           (gptel-make-fsm
                            :state 'TYPE
                            :info callback-info)))
                      (funcall configure fsm inv)
                      fsm))))
              (mevedel-agent-runtime-dispatch
               (lambda (_value)
                 (cl-incf attempts)
                 (error "Rejected terminal Agent result"))
               (mevedel-agent-get "explorer") "survey" "survey files")
              (should
               (assoc (mevedel-agent-invocation-agent-id invocation)
                      mevedel-agent-runtime--fsms))
              (funcall raw-callback "Done." callback-info)
              (funcall raw-callback t callback-info)
              (should (= 1 attempts))
              (should
               (mevedel-agent-invocation-foreground-result-reported-p
                invocation))
              (should-not
               (assoc (mevedel-agent-invocation-agent-id invocation)
                      mevedel-agent-runtime--fsms))
              (funcall raw-callback t callback-info)
              (should (= 1 attempts)))))
      (when (and invocation
                 (buffer-live-p (mevedel-agent-invocation-buffer invocation)))
        (kill-buffer (mevedel-agent-invocation-buffer invocation)))
      (kill-buffer buf)))

  :doc "watchdog keeps BWAIT parked when messages and live agents coexist"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (parent (generate-new-buffer " *mt-bwait-mixed*"))
         (agent-buf (generate-new-buffer " *mt-bwait-mixed-agent*"))
         (live-inv (mevedel-agent-invocation--create
                    :buffer agent-buf
                    :background-p t))
         (live-fsm (gptel-make-fsm
                    :state 'WAIT
                    :info (list :buffer agent-buf
                                :mevedel-agent-invocation live-inv)))
         (fsm (gptel-make-fsm
               :state 'BWAIT
               :info (list :buffer parent)))
         transitioned
         rearmed)
    (unwind-protect
        (progn
          (with-current-buffer parent
            (setq-local mevedel--session session)
            (setq-local mevedel-agent-runtime--fsms
                        (list (cons "agent-live" live-fsm))))
          (setf (mevedel-session-background-agents session)
                (list "agent-live"))
          (setf (mevedel-session-messages session)
                (list (list :from "done" :body "result")))
          (let ((mevedel-agent-background-timeout 1))
            (cl-letf (((symbol-function 'gptel--fsm-transition)
                       (lambda (_fsm state) (setq transitioned state)))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _args) (setq rearmed t))))
              (mevedel-agent-runtime--bwait-watchdog-expire fsm)))
          (should-not transitioned)
          (should rearmed)
          (should (equal '("agent-live")
                         (mevedel-session-background-agents session)))
          (should (mevedel-session-messages session)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer parent))))

(mevedel-deftest mevedel-agent-runtime-dispatch-foreground-stash
  (:before-each (progn (mevedel-tool-clear-registry)
                       (mevedel-agent-runtime-test--register-agent-tools)
                       (setq mevedel-agent--registry
                             (copy-sequence
                              mevedel-agent-runtime-test--agent-registry)))
   :after-each (progn (mevedel-workspace-clear-registry)
                      (mevedel-tool-clear-registry)
                      (setq mevedel-agent--registry nil)))
  ,test
  (test)

  :doc "foreground callback defers main-cb while background agents are pending"
  (let* ((state (mevedel-agent-runtime-test--make-persistent-session))
         (session (car state))
         (tempdir (cdr state))
         (buf (generate-new-buffer " *mt-stash1*"))
         (coordinator-cb nil)
         (result nil)
         (call-count 0)
         (inv nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          ;; Step 1: Spawn the coordinator (foreground).
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-coordinator-fsm
                      (gptel-make-fsm
                       :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &optional invocation
                                  _agent-buffer _configure-fsm)
                        (setq coordinator-cb cb
                              inv invocation)
                        (mevedel-agent-runtime-test--register-agent-fsm
                         inv fake-coordinator-fsm))))
            (mevedel-agent-runtime-dispatch
             (lambda (resp &rest _)
               (cl-incf call-count)
               (setq result resp))
             (mevedel-agent-get "coordinator") "orchestrate" "do stuff")
            (should inv)
            ;; Step 2: Simulate the coordinator spawning a background
            ;; agent.  Directly push onto the invocation's
            ;; background-agents.
            (mevedel-agent-runtime--ctx-push-background-agent inv "explorer--fake")
            ;; Step 3: The coordinator's LLM returns text-only while
            ;; children still pending -- main-cb must not fire.
            (funcall coordinator-cb "Waiting for results...")
            (should (null result))
            (should (zerop call-count))
            (should (assoc (mevedel-agent-invocation-agent-id inv)
                           mevedel-agent-runtime--fsms))
            ;; Step 4: Child finishes, then coordinator fires final.
            (mevedel-agent-runtime--ctx-remove-background-agent inv "explorer--fake")
            (funcall coordinator-cb "Final summary with results.")
            (should (equal "Final summary with results."
                           (plist-get result :result)))
            (should (= 1 call-count))
            (should-not (assoc (mevedel-agent-invocation-agent-id inv)
                               mevedel-agent-runtime--fsms))
            ;; Step 5: A late duplicate 't' event must NOT double-fire.
            (funcall coordinator-cb "Redundant late response.")
            (should (= 1 call-count))
            (should (string-match-p "Final summary"
                                    (plist-get result :result)))))
      (when (and inv
                 (buffer-live-p (mevedel-agent-invocation-buffer inv)))
        (with-current-buffer (mevedel-agent-invocation-buffer inv)
          (set-buffer-modified-p nil)
          (setq kill-buffer-hook nil))
        (kill-buffer (mevedel-agent-invocation-buffer inv)))
      (mevedel-agent-runtime-test--release-and-kill buf session)
      (delete-directory tempdir t)))

  :doc "foreground callback defers main-cb while mailbox holds pending results (race)"
  (let* ((state (mevedel-agent-runtime-test--make-persistent-session))
         (session (car state))
         (tempdir (cdr state))
         (buf (generate-new-buffer " *mt-stash2*"))
         (coordinator-cb nil)
         (result nil)
         (call-count 0)
         (inv nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-coordinator-fsm
                      (gptel-make-fsm
                       :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &optional invocation
                                  _agent-buffer _configure-fsm)
                        (setq coordinator-cb cb
                              inv invocation)
                        (mevedel-agent-runtime-test--register-agent-fsm
                         inv fake-coordinator-fsm))))
            (mevedel-agent-runtime-dispatch
             (lambda (resp &rest _)
               (cl-incf call-count)
               (setq result resp))
             (mevedel-agent-get "coordinator") "orchestrate" "do stuff")
            (should inv)
            ;; Race: the background child finished BEFORE the parent
            ;; produced its text-only turn, so by callback time
            ;; `background-agents' is empty but `messages' still holds
            ;; an undelivered result.
            (mevedel-agent-runtime--ctx-push-message
             inv (list :from "explorer--fake" :body "done"))
            (funcall coordinator-cb "Preliminary handoff.")
            ;; Must NOT fire yet -- the mailbox still has to drain.
            (should (null result))
            (should (zerop call-count))
            (should (assoc (mevedel-agent-invocation-agent-id inv)
                           mevedel-agent-runtime--fsms))
            ;; Mailbox drains (simulating WAIT); coordinator fires final.
            (setf (mevedel-agent-invocation-messages inv) nil)
            (funcall coordinator-cb "Final summary with results.")
            (should (= 1 call-count))
            (should (string-match-p "Final summary"
                                    (plist-get result :result)))
            (should-not (assoc (mevedel-agent-invocation-agent-id inv)
                               mevedel-agent-runtime--fsms))))
      (when (and inv
                 (buffer-live-p (mevedel-agent-invocation-buffer inv)))
        (with-current-buffer (mevedel-agent-invocation-buffer inv)
          (set-buffer-modified-p nil)
          (setq kill-buffer-hook nil))
        (kill-buffer (mevedel-agent-invocation-buffer inv)))
      (mevedel-agent-runtime-test--release-and-kill buf session)
      (delete-directory tempdir t)))

  :doc "foreground callback removes parent registry from an agent buffer"
  (let* ((state (mevedel-agent-runtime-test--make-persistent-session))
         (session (car state))
         (tempdir (cdr state))
         (parent-buf (generate-new-buffer " *mt-stash-agent-parent*"))
         (callback-buf (generate-new-buffer " *mt-stash-agent-callback*"))
         (coordinator-cb nil)
         (result nil)
         (inv nil))
    (unwind-protect
        (progn
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-agent-runtime--fsms nil)
            (cl-letf* ((ov (progn (insert "x")
                                  (make-overlay (point-min) (point-max))))
                       (fake-coordinator-fsm
                        (gptel-make-fsm
                         :info (list :context ov :buffer parent-buf)))
                       ((symbol-function 'mevedel-agent-exec--run)
                        (lambda (cb _type _desc _prompt
                                    &optional invocation _agent-buffer
                                    _configure-fsm)
                          (setq coordinator-cb cb
                                inv invocation)
                          (mevedel-agent-runtime-test--register-agent-fsm
                           inv fake-coordinator-fsm))))
              (mevedel-agent-runtime-dispatch
               (lambda (resp &rest _) (setq result resp))
               (mevedel-agent-get "coordinator") "orchestrate" "do stuff")
              (should inv)
              (should (assoc (mevedel-agent-invocation-agent-id inv)
                             mevedel-agent-runtime--fsms))))
          (with-current-buffer callback-buf
            (setq-local mevedel-agent-runtime--fsms nil)
            (funcall coordinator-cb "Final summary from agent buffer."))
          (should (string-match-p "Final summary"
                                  (plist-get result :result)))
          (with-current-buffer parent-buf
            (should-not (assoc (mevedel-agent-invocation-agent-id inv)
                               mevedel-agent-runtime--fsms)))
          (with-current-buffer callback-buf
            (should-not mevedel-agent-runtime--fsms)))
      (when (and inv
                 (buffer-live-p (mevedel-agent-invocation-buffer inv)))
        (with-current-buffer (mevedel-agent-invocation-buffer inv)
          (set-buffer-modified-p nil)
          (setq kill-buffer-hook nil))
        (kill-buffer (mevedel-agent-invocation-buffer inv)))
      (when (buffer-live-p callback-buf) (kill-buffer callback-buf))
      (mevedel-agent-runtime-test--release-and-kill parent-buf session)
      (delete-directory tempdir t)))

  :doc "foreground callback bypasses gate on error/abort responses"
  (let* ((state (mevedel-agent-runtime-test--make-persistent-session))
         (session (car state))
         (tempdir (cdr state))
         (buf (generate-new-buffer " *mt-stash3*"))
         (coordinator-cb nil)
         (result nil)
         (inv nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-coordinator-fsm
                      (gptel-make-fsm
                       :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &optional invocation
                                  _agent-buffer _configure-fsm)
                        (setq coordinator-cb cb
                              inv invocation)
                        (when inv
                          (setf (gptel-fsm-info fake-coordinator-fsm)
                                (plist-put
                                 (gptel-fsm-info fake-coordinator-fsm)
                                 :mevedel-agent-invocation inv)))
                        fake-coordinator-fsm)))
            (mevedel-agent-runtime-dispatch
             (lambda (resp &rest _) (setq result resp))
             (mevedel-agent-get "coordinator") "orchestrate" "do stuff")
            (mevedel-agent-runtime--ctx-push-background-agent inv "explorer--fake")
            ;; Error response must forward immediately so the parent
            ;; tool call doesn't hang on a dead child.
            (funcall coordinator-cb "Error: Task aborted by the user.")
            (should (stringp result))
            (should (string-match-p "Error:" result))
            (should-not (assoc (mevedel-agent-invocation-agent-id inv)
                               mevedel-agent-runtime--fsms))))
      (when (and inv
                 (buffer-live-p (mevedel-agent-invocation-buffer inv)))
        (with-current-buffer (mevedel-agent-invocation-buffer inv)
          (set-buffer-modified-p nil)
          (setq kill-buffer-hook nil))
        (kill-buffer (mevedel-agent-invocation-buffer inv)))
      (mevedel-agent-runtime-test--release-and-kill buf session)
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-agent-runtime--bwait-watchdog-expire/case-2
  (:before-each (progn (mevedel-tool-clear-registry)
                       (mevedel-agent-runtime-test--register-agent-tools)
                       (setq mevedel-agent--registry
                             (copy-sequence
                              mevedel-agent-runtime-test--agent-registry)))
   :after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)
                      (mevedel-tool-clear-registry)))
  ,test
  (test)

  :doc "stranded agents synthesize mailbox results and transition parent to WAIT"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-wd-done*"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (insert "aa")
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (setf (mevedel-session-background-agents session)
                  '("explorer--A" "explorer--B"))
            (setf (mevedel-session-messages session) nil)
            (mevedel-agent-runtime--bwait-watchdog-expire parent)
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))
            (should (= 2 (length (mevedel-session-messages session))))
            (dolist (msg (mevedel-session-messages session))
              (let ((body (plist-get msg :body)))
                (should (string-match-p "<agent-result" body))
                (should (string-match-p "became stranded" body))
                (should (string-match-p "No saved transcript path" body))))
            (should (null mevedel-agent-runtime--fsms))))
      (kill-buffer buf)))

  :doc "stranded agent with queued result only clears stale tracking"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-wd-queued-result*"))
         (body (mevedel-agent-runtime--agent-result-format
                "explorer--A" "explorer" "survey" "real result"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session)
                  (list (list :from "explorer--A"
                              :body body
                              :agent-result-p t
                              :timestamp (current-time))))
            (mevedel-agent-runtime--bwait-watchdog-expire parent)
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))
            (should (= 1 (length (mevedel-session-messages session))))
            (let ((queued (plist-get (car (mevedel-session-messages session))
                                     :body)))
              (should (string-match-p "real result" queued))
              (should-not (string-match-p "became stranded" queued)))
            (should (null mevedel-agent-runtime--fsms))))
      (kill-buffer buf)))

  :doc "stranded agent result includes Read-able transcript path from sidecar metadata"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (tempdir (file-name-as-directory
                   (make-temp-file "mevedel-bwait-stranded" t)))
         (rel-path "agents/explorer--partial.chat.org")
         (abs-path (expand-file-name rel-path tempdir))
         (buf (generate-new-buffer " *mt-wd-transcript*"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (setf (mevedel-session-save-path session) tempdir)
          (make-directory (file-name-directory abs-path) t)
          (with-temp-file abs-path
            (insert "saved transcript"))
          (setf (mevedel-session-agent-transcripts session)
                `(("explorer--A" :agent-type "explorer"
                   :description "survey files"
                   :path ,rel-path
                   :status running)))
          (setf (mevedel-session-background-agents session)
                '("explorer--A"))
          (setf (mevedel-session-messages session) nil)
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (mevedel-agent-runtime--bwait-watchdog-expire parent)
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))
            (should (= 1 (length (mevedel-session-messages session))))
            (let* ((msg (car (mevedel-session-messages session)))
                   (body (plist-get msg :body)))
              (should (string-match-p "<agent-result agent-id=\"explorer--A\""
                                      body))
              (should (string-match-p "survey files" body))
              (should (string-match-p
                       (regexp-quote (format "Transcript: %s" abs-path))
                       body))
              (should (string-match-p
                       (regexp-quote (format "Read(file_path=%S)" abs-path))
                       body)))
            (let ((entry (cdr (assoc "explorer--A"
                                     (mevedel-session-agent-transcripts
                                      session)))))
              (should (eq 'incomplete (plist-get entry :status))))))
      (when (file-directory-p tempdir) (delete-directory tempdir t))
      (kill-buffer buf)))

  :doc "stranded agent result rejects symlinked transcript path"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (tempdir (file-name-as-directory
                   (make-temp-file "mevedel-bwait-symlink" t)))
         (target (make-temp-file "mevedel-bwait-symlink-target"))
         (rel-path "agents/explorer--partial.chat.org")
         (abs-path (expand-file-name rel-path tempdir))
         (buf (generate-new-buffer " *mt-wd-symlink*"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (setf (mevedel-session-save-path session) tempdir)
          (make-directory (file-name-directory abs-path) t)
          (make-symbolic-link target abs-path)
          (setf (mevedel-session-agent-transcripts session)
                `(("explorer--A" :agent-type "explorer"
                   :description "survey files"
                   :path ,rel-path
                   :status running)))
          (setf (mevedel-session-background-agents session)
                '("explorer--A"))
          (setf (mevedel-session-messages session) nil)
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (mevedel-agent-runtime--bwait-watchdog-expire parent)
            (let* ((msg (car (mevedel-session-messages session)))
                   (body (plist-get msg :body)))
              (should-not (string-match-p "Transcript:" body))
              (should-not (string-match-p "Read(file_path=" body))
              (should (string-match-p "No saved transcript path" body)))))
      (when (file-exists-p target) (delete-file target))
      (when (file-directory-p tempdir) (delete-directory tempdir t))
      (kill-buffer buf)))

  :doc "non-empty mailbox transitions parent to WAIT so drain fires"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-wd-wait*"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (setf (mevedel-session-background-agents session) '("explorer--X"))
          (setf (mevedel-session-messages session)
                '((:from "explorer--finished" :body "done")))
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (mevedel-agent-runtime--bwait-watchdog-expire parent)
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))))
      (kill-buffer buf)))

  :doc "execution-only mailbox stays parked without an unsolicited request"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-wd-execution-only*"))
         scheduled)
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setf (mevedel-session-messages session)
                '((:from "bash:main" :body "done"
                   :execution-result-p t)))
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (&rest args)
                         (setq scheduled args)
                         'timer))
                      ((symbol-function 'gptel--update-status)
                       (lambda (&rest _args) nil)))
              (mevedel-agent-runtime--handle-bwait parent))
            (should (eq 'BWAIT (gptel-fsm-state parent)))
            (should-not scheduled)))
      (kill-buffer buf)))

  :doc "watchdog never resumes an execution-only mailbox"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-wd-execution-watchdog*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setf (mevedel-session-messages session)
                '((:from "bash:main" :body "done"
                   :execution-result-p t)))
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (mevedel-agent-runtime--bwait-watchdog-expire parent)
            (should (eq 'BWAIT (gptel-fsm-state parent)))))
      (kill-buffer buf)))

  :doc "slow live agents remain parked in BWAIT and re-arm watchdog"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-wd-live*"))
         (agent-buf (generate-new-buffer " *mt-wd-live-agent*"))
         (mevedel-agent-runtime--background-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-background-timeout 600)
         (timer-count 0))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation-create agent))
                 (child (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv)
                         :handlers nil :state 'TOOL))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (setf (mevedel-agent-invocation-buffer inv) agent-buf)
            (setf (alist-get "explorer--A" mevedel-agent-runtime--fsms nil nil #'equal)
                  child)
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session) nil)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (&rest _args)
                         (cl-incf timer-count)
                         'timer)))
              (mevedel-agent-runtime--bwait-watchdog-expire parent))
            (should (eq 'BWAIT (gptel-fsm-state parent)))
            (should (equal '("explorer--A")
                           (mevedel-session-background-agents session)))
            (should (assoc "explorer--A" mevedel-agent-runtime--fsms))
            (should (gethash "explorer--A"
                             mevedel-agent-runtime--background-watchdogs))
            (should (= 1 timer-count))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer buf)))

  :doc "background progress resets no-progress grace while parked in BWAIT"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-wd-bg-progress*"))
         (agent-buf (generate-new-buffer " *mt-wd-bg-progress-agent*"))
         (mevedel-agent-runtime--background-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-background-timeout 600)
         (mevedel-agent-no-progress-timeout 10)
         (clock 100.0)
         delays
         stopped)
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation--create
                       :agent agent
                       :agent-id "explorer--A"
                       :parent-session session
                       :parent-context session
                       :parent-data-buffer buf
                       :buffer agent-buf
                       :background-p t
                       :transcript-status 'running))
                 (child (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv)
                         :handlers nil :state 'WAIT))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (setf (alist-get "explorer--A" mevedel-agent-runtime--fsms nil nil #'equal)
                  child)
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session) nil)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (delay &rest _args)
                         (push delay delays)
                         'timer))
                      ((symbol-function 'float-time)
                       (lambda (&optional _time) clock))
                      ((symbol-function 'mevedel-agent-runtime-stop)
                       (lambda (&rest args) (setq stopped args))))
              (mevedel-agent-runtime--bwait-watchdog-expire parent)
              (setf (mevedel-agent-invocation-activity inv)
                    '((:type tool-start :time 105.0)))
              (setq clock 109.0)
              (mevedel-agent-runtime--bwait-watchdog-expire parent))
            (should-not stopped)
            (should (eq 'BWAIT (gptel-fsm-state parent)))
            (should (equal '(10 6) (nreverse delays)))
            (should (equal '("explorer--A")
                           (mevedel-session-background-agents session)))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer buf)))

  :doc "BWAIT handler arms background no-progress without stranded watchdog"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-wd-bg-handle*"))
         (agent-buf (generate-new-buffer " *mt-wd-bg-handle-agent*"))
         (mevedel-agent-runtime--background-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-background-timeout nil)
         (mevedel-agent-no-progress-timeout 10)
         (clock 100.0)
         delays
         stopped)
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation--create
                       :agent agent
                       :agent-id "explorer--A"
                       :parent-session session
                       :parent-context session
                       :parent-data-buffer buf
                       :buffer agent-buf
                       :background-p t
                       :transcript-status 'running))
                 (child (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv)
                         :handlers nil :state 'WAIT))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (setf (alist-get "explorer--A" mevedel-agent-runtime--fsms nil nil #'equal)
                  child)
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session) nil)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (delay &rest _args)
                         (push delay delays)
                         'timer))
                      ((symbol-function 'float-time)
                       (lambda (&optional _time) clock))
                      ((symbol-function 'gptel--update-status)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'mevedel-agent-runtime-stop)
                       (lambda (agent-id reason parent-buffer)
                         (setq stopped (list agent-id reason parent-buffer))
                         (setf (mevedel-session-background-agents session)
                               nil)
                         (gptel--fsm-transition parent 'WAIT))))
              (mevedel-agent-runtime--handle-bwait parent)
              (should (equal '(10) (nreverse delays)))
              (should (gethash "explorer--A"
                               mevedel-agent-runtime--background-watchdogs))
              (setq clock 110.0)
              (mevedel-agent-runtime--bwait-watchdog-expire parent))
            (should (equal "explorer--A" (car stopped)))
            (should (string-match-p "no progress" (cadr stopped)))
            (should (eq buf (caddr stopped)))
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should-not (gethash "explorer--A"
                                 mevedel-agent-runtime--background-watchdogs))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer buf)))

  :doc "early BWAIT watchdog timer reschedules remaining background grace"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-wd-bg-early*"))
         (agent-buf (generate-new-buffer " *mt-wd-bg-early-agent*"))
         (mevedel-agent-runtime--background-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-background-timeout 600)
         (mevedel-agent-no-progress-timeout 10)
         (clock 100.0)
         delays
         stopped)
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation--create
                       :agent agent
                       :agent-id "explorer--A"
                       :parent-session session
                       :parent-context session
                       :parent-data-buffer buf
                       :buffer agent-buf
                       :background-p t
                       :transcript-status 'running))
                 (child (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv)
                         :handlers nil :state 'WAIT))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (setf (alist-get "explorer--A" mevedel-agent-runtime--fsms nil nil #'equal)
                  child)
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session) nil)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (delay &rest _args)
                         (push delay delays)
                         'timer))
                      ((symbol-function 'float-time)
                       (lambda (&optional _time) clock))
                      ((symbol-function 'mevedel-agent-runtime-stop)
                       (lambda (&rest args) (setq stopped args))))
              (mevedel-agent-runtime--bwait-watchdog-expire parent)
              (setq clock 105.0)
              (mevedel-agent-runtime--bwait-watchdog-expire parent))
            (should-not stopped)
            (should (eq 'BWAIT (gptel-fsm-state parent)))
            (should (equal '(10 5) (nreverse delays)))
            (should (equal '("explorer--A")
                           (mevedel-session-background-agents session)))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer buf)))

  :doc "background TOOL-state agent stops after no-progress grace"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-wd-bg-stop*"))
         (agent-buf (generate-new-buffer " *mt-wd-bg-stop-agent*"))
         (mevedel-agent-runtime--background-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-background-timeout 600)
         (mevedel-agent-no-progress-timeout 10)
         (clock 100.0)
         stopped)
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation--create
                       :agent agent
                       :agent-id "explorer--A"
                       :parent-session session
                       :parent-context session
                       :parent-data-buffer buf
                       :buffer agent-buf
                       :background-p t
                       :transcript-status 'running))
                 (child (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv)
                         :handlers nil :state 'TOOL))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (setf (alist-get "explorer--A" mevedel-agent-runtime--fsms nil nil #'equal)
                  child)
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session) nil)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (&rest _args) 'timer))
                      ((symbol-function 'float-time)
                       (lambda (&optional _time) clock))
                      ((symbol-function 'mevedel-agent-runtime-stop)
                       (lambda (agent-id reason parent-buffer)
                         (setq stopped (list agent-id reason parent-buffer))
                         (setf (mevedel-session-background-agents session)
                               nil)
                         (gptel--fsm-transition parent 'WAIT))))
              (mevedel-agent-runtime--bwait-watchdog-expire parent)
              (setq clock 110.0)
              (mevedel-agent-runtime--bwait-watchdog-expire parent))
            (should (equal "explorer--A" (car stopped)))
            (should (string-match-p "no progress" (cadr stopped)))
            (should (eq buf (caddr stopped)))
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))
            (should-not (gethash "explorer--A"
                                 mevedel-agent-runtime--background-watchdogs))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer buf)))

  :doc "failed background no-progress stop keeps parent in BWAIT"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-wd-bg-stop-fail*"))
         (agent-buf (generate-new-buffer " *mt-wd-bg-stop-fail-agent*"))
         (mevedel-agent-runtime--background-watchdogs
          (make-hash-table :test #'equal))
         (mevedel-agent-background-timeout nil)
         (mevedel-agent-no-progress-timeout 10)
         (clock 100.0)
         (timer-count 0)
         stopped)
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation--create
                       :agent agent
                       :agent-id "explorer--A"
                       :parent-session session
                       :parent-context session
                       :parent-data-buffer buf
                       :buffer agent-buf
                       :background-p t
                       :transcript-status 'running))
                 (child (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv)
                         :handlers nil :state 'WAIT))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (setf (alist-get "explorer--A" mevedel-agent-runtime--fsms nil nil #'equal)
                  child)
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session) nil)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (&rest _args)
                         (cl-incf timer-count)
                         'timer))
                      ((symbol-function 'float-time)
                       (lambda (&optional _time) clock))
	                      ((symbol-function 'mevedel-agent-runtime-stop)
	                       (lambda (&rest args)
	                         (setq stopped args)
	                         (error "Simulated stop failure"))))
              (mevedel-agent-runtime--bwait-watchdog-expire parent)
              (setq clock 110.0)
              (mevedel-agent-runtime--bwait-watchdog-expire parent))
            (should (equal "explorer--A" (car stopped)))
            (should (eq 'BWAIT (gptel-fsm-state parent)))
            (should (equal '("explorer--A")
                           (mevedel-session-background-agents session)))
            (should (gethash "explorer--A"
                             mevedel-agent-runtime--background-watchdogs))
            (should (= 2 timer-count))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer buf)))

  :doc "stale child FSM with killed buffer recovers as stranded"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-wd-killed-child*"))
         (agent-buf (generate-new-buffer " *mt-wd-killed-agent*"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation--create
                       :agent agent
                       :agent-id "explorer--A"
                       :description "stale child"
                       :parent-session session
                       :parent-context session
                       :parent-data-buffer buf
                       :buffer agent-buf
                       :background-p t
                       :transcript-status 'running))
                 (child (gptel-make-fsm
                         :info (list :buffer agent-buf
                                     :mevedel-agent-invocation inv)
                         :handlers nil :state 'WAIT))
                 (parent (gptel-make-fsm :info (list :buffer buf)
                                         :handlers nil :state 'BWAIT)))
            (kill-buffer agent-buf)
            (setf (alist-get "explorer--A" mevedel-agent-runtime--fsms nil nil #'equal)
                  child)
            (setf (mevedel-session-background-agents session)
                  '("explorer--A"))
            (setf (mevedel-session-messages session) nil)
            (mevedel-agent-runtime--bwait-watchdog-expire parent)
            (should (eq 'WAIT (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))
            (should (= 1 (length (mevedel-session-messages session))))
            (let ((body (plist-get (car (mevedel-session-messages session))
                                   :body)))
              (should (string-match-p "became stranded" body)))
            (should (null mevedel-agent-runtime--fsms))))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (kill-buffer buf)))

  :doc "does not synthesize duplicate result already in parent transcript"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-wd-transcript-dupe*"))
         (mevedel-agent-background-timeout 600))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (insert (mevedel-agent-runtime--agent-result-format
                   "explorer--A" "explorer" "survey" "real result")
                  "\n")
          (setf (mevedel-session-background-agents session)
                '("explorer--A"))
          (setf (mevedel-session-messages session) nil)
          (let ((parent (gptel-make-fsm :info (list :buffer buf)
                                        :handlers nil :state 'BWAIT)))
            (mevedel-agent-runtime--bwait-watchdog-expire parent)
            (should (eq 'DONE (gptel-fsm-state parent)))
            (should (null (mevedel-session-background-agents session)))
            (should (null (mevedel-session-messages session)))))
      (kill-buffer buf)))

  :doc "no-op when FSM has already left BWAIT"
  (let ((parent (gptel-make-fsm :handlers nil :state 'DONE)))
    (mevedel-agent-runtime--bwait-watchdog-expire parent)
    (should (eq 'DONE (gptel-fsm-state parent)))))

(mevedel-deftest mevedel-agent-runtime-dispatch-bg-callback-hardening
  (:before-each (progn (mevedel-tool-clear-registry)
                       (mevedel-agent-runtime-test--register-agent-tools)
                       (setq mevedel-agent--registry
                             (copy-sequence
                              mevedel-agent-runtime-test--agent-registry)))
   :after-each (progn (mevedel-workspace-clear-registry)
                      (setq mevedel-agent--registry nil)
                      (mevedel-tool-clear-registry)))
  ,test
  (test)

  :doc "bg callback removes agent from tracking even when push-message throws"
  (let* ((session (mevedel-agent-runtime-test--make-session))
         (buf (generate-new-buffer " *mt-bg-harden*"))
         (captured-cb nil)
         (push-called 0)
         (remove-called 0))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--session session)
          (setq-local mevedel-agent-runtime--fsms nil)
          (cl-letf* ((ov (progn (insert "x")
                                (make-overlay (point-min) (point-max))))
                     (fake-fsm (gptel-make-fsm
                                :info (list :context ov :buffer buf)))
                     ((symbol-function 'mevedel-agent-exec--run)
                      (lambda (cb _type _desc _prompt &rest _)
                        (setq captured-cb cb)
                        fake-fsm)))
            (let ((mevedel-tools--current-fsm nil))
              (mevedel-agent-runtime-dispatch
               #'ignore (mevedel-agent-get "explorer") "survey" "go"
               :background t))
            (should (= 1 (length (mevedel-session-background-agents session))))
            ;; Break push-message so the bg callback's push branch raises;
            ;; remove-background-agent MUST still run so the parent isn't
            ;; stranded in BWAIT.
	            (cl-letf (((symbol-function 'mevedel-agent-runtime--ctx-push-message)
	                       (lambda (&rest _)
	                         (cl-incf push-called)
	                         (error "Simulated push failure")))
                      ((symbol-function 'mevedel-agent-runtime--ctx-remove-background-agent)
                       (lambda (_ctx _id)
                         (cl-incf remove-called))))
              (funcall captured-cb "child result"))
            (should (= 1 push-called))
            (should (= 1 remove-called))))
      (kill-buffer buf))))

(mevedel-deftest mevedel-agent-runtime--prune-stale-agents-fsm
  ()
  ,test
  (test)

  :doc "prunes terminal-state FSMs"
  (let ((buf (generate-new-buffer " *mt-prune-done*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel-agent-runtime--fsms nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv-a (mevedel-agent-invocation-create agent))
                 (inv-b (mevedel-agent-invocation-create agent))
                 (_live-buffers
                  (setf (mevedel-agent-invocation-buffer inv-a) buf
                        (mevedel-agent-invocation-buffer inv-b) buf))
                 (alive (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv-a)
                         :handlers nil :state 'WAIT))
                 (done  (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv-b)
                         :handlers nil :state 'DONE)))
            (setf (alist-get "alive" mevedel-agent-runtime--fsms nil nil #'equal) alive)
            (setf (alist-get "done"  mevedel-agent-runtime--fsms nil nil #'equal) done)
            (mevedel-agent-runtime--prune-stale-agents-fsm)
            (should (assoc "alive" mevedel-agent-runtime--fsms))
            (should-not (assoc "done" mevedel-agent-runtime--fsms))))
      (kill-buffer buf)))

  :doc "prunes TOOL-state FSMs that no longer carry an invocation"
  (let ((buf (generate-new-buffer " *mt-prune-tool*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel-agent-runtime--fsms nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv (mevedel-agent-invocation-create agent))
                 (_live-buffer
                  (setf (mevedel-agent-invocation-buffer inv) buf))
                 (good (gptel-make-fsm
                        :info (list :mevedel-agent-invocation inv)
                       :handlers nil :state 'TOOL))
                 (bad  (gptel-make-fsm :info nil
                       :handlers nil :state 'TOOL)))
            (setf (alist-get "good" mevedel-agent-runtime--fsms nil nil #'equal) good)
            (setf (alist-get "bad"  mevedel-agent-runtime--fsms nil nil #'equal) bad)
            (mevedel-agent-runtime--prune-stale-agents-fsm)
            (should (assoc "good" mevedel-agent-runtime--fsms))
            (should-not (assoc "bad" mevedel-agent-runtime--fsms))))
      (kill-buffer buf)))

  :doc "prunes FSMs whose invocation transcript status is terminal"
  (let ((buf (generate-new-buffer " *mt-prune-terminal-inv*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel-agent-runtime--fsms nil)
          (let* ((agent (mevedel-agent--create :name "explorer"))
                 (inv-a (mevedel-agent-invocation-create agent))
                 (inv-b (mevedel-agent-invocation-create agent))
                 (_live-buffers
                  (setf (mevedel-agent-invocation-buffer inv-a) buf
                        (mevedel-agent-invocation-buffer inv-b) buf))
                 (alive (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv-a)
                         :handlers nil :state 'WAIT))
                 (done  (gptel-make-fsm
                         :info (list :mevedel-agent-invocation inv-b)
                         :handlers nil :state 'WAIT)))
            (setf (mevedel-agent-invocation-transcript-status inv-a)
                  'running)
            (setf (mevedel-agent-invocation-transcript-status inv-b)
                  'error)
            (setf (alist-get "alive" mevedel-agent-runtime--fsms
                             nil nil #'equal)
                  alive)
            (setf (alist-get "done" mevedel-agent-runtime--fsms
                             nil nil #'equal)
                  done)
            (mevedel-agent-runtime--prune-stale-agents-fsm)
            (should (assoc "alive" mevedel-agent-runtime--fsms))
            (should-not (assoc "done" mevedel-agent-runtime--fsms))))
      (kill-buffer buf))))

(mevedel-deftest mevedel-agent-runtime-display-label
  (:doc "derives <type>--<idshort> from <type>--<32-char-md5>")
  ,test
  (test)

  :doc "extracts first 8 chars of suffix after the `--' separator"
  (should (equal "explorer--abc7f3d2"
                 (mevedel-agent-runtime-display-label
                  "explorer--abc7f3d2deadbeefcafe1234567890ab")))

  :doc "preserves type prefix verbatim"
  (should (equal "verifier--12345678"
                 (mevedel-agent-runtime-display-label
                  "verifier--1234567890abcdefdeadbeefcafefeed")))

  :doc "returns input unchanged when no `--' separator present"
  (should (equal "main"
                 (mevedel-agent-runtime-display-label "main")))

  :doc "returns nil for nil input"
  (should-not (mevedel-agent-runtime-display-label nil))

  :doc "handles short suffixes without crashing"
  (should (equal "x--abc"
                 (mevedel-agent-runtime-display-label "x--abc"))))

(mevedel-deftest mevedel-agent-runtime--handle-wait-inject
  (:after-each (setq mevedel-agent--registry nil))
  ,test
  (test)

  :doc "prepends a user-role message block built from firing reminders on first turn"
  (let* ((r (mevedel-reminder-create
             :type 'note
             :trigger (lambda (_) t)
             :content (lambda (_) "REMIND-ME")))
         (_ (mevedel-define-agent wait-agent
              :description "WAIT inject test"
              :tools nil
              :reminders (list r)))
         (agent (mevedel-agent-get "wait-agent"))
         (inv (mevedel-agent-invocation-create agent))
         (ov-buf (generate-new-buffer " *mevedel-test-ov*")))
    (unwind-protect
        (let* ((ov (with-current-buffer ov-buf
                     (insert "x")
                     (make-overlay (point-min) (point-max))))
               (data (list :messages (vector (list :role "user"
                                                   :content "original"))))
               (fsm (gptel-make-fsm
                     :info (list :context ov
                                 :backend nil
                                 :data data))))
          (setf (gptel-fsm-info fsm)
                (plist-put (gptel-fsm-info fsm)
                           :mevedel-agent-invocation inv))
          (mevedel-agent-runtime--handle-wait-inject fsm)
          ;; Turn count incremented
          (should (equal 1 (mevedel-agent-invocation-turn-count inv)))
          ;; Message vector grew by 1 user-role block.  On the first
          ;; WAIT cycle the reminder is injected ahead of the task
          ;; prompt (audit-log-friendly order).
          (let ((msgs (plist-get data :messages)))
            (should (equal 2 (length msgs)))
            (should (equal "user" (plist-get (aref msgs 0) :role)))
            (should (string-match-p "REMIND-ME"
                                    (plist-get (aref msgs 0) :content)))
            (should (equal "original" (plist-get (aref msgs 1) :content)))))
      (kill-buffer ov-buf)))

  :doc "advances turn count even when no reminders fire"
  (let* ((_ (mevedel-define-agent wait-quiet-agent
              :description "No reminders"
              :tools nil))
         (agent (mevedel-agent-get "wait-quiet-agent"))
         (inv (mevedel-agent-invocation-create agent))
         (ov-buf (generate-new-buffer " *mevedel-test-ov*")))
    (unwind-protect
        (let* ((ov (with-current-buffer ov-buf
                     (insert "x")
                     (make-overlay (point-min) (point-max))))
               (data (list :messages (vector)))
               (fsm (gptel-make-fsm
                     :info (list :context ov
                                 :backend nil
                                 :data data))))
          (setf (gptel-fsm-info fsm)
                (plist-put (gptel-fsm-info fsm)
                           :mevedel-agent-invocation inv))
          (mevedel-agent-runtime--handle-wait-inject fsm)
          (should (equal 1 (mevedel-agent-invocation-turn-count inv)))
          ;; Messages untouched
          (should (equal 0 (length (plist-get data :messages)))))
      (kill-buffer ov-buf)))

  :doc "is a no-op when FSM has no agent invocation"
  (let* ((data (list :messages (vector (list :role "user" :content "x"))))
         (fsm (gptel-make-fsm :info (list :data data))))
    (mevedel-agent-runtime--handle-wait-inject fsm)
    (should (equal 1 (length (plist-get data :messages)))))

  :doc "respects interval throttling across multiple WAIT cycles"
  (let* ((r (mevedel-reminder-create
             :type 'throttled
             :trigger (lambda (_) t)
             :content (lambda (_) "TICK")
             :interval 2))
         (_ (mevedel-define-agent wait-throttle-agent
              :description "Interval test"
              :tools nil
              :reminders (list r)))
         (agent (mevedel-agent-get "wait-throttle-agent"))
         (inv (mevedel-agent-invocation-create agent))
         (ov-buf (generate-new-buffer " *mevedel-test-ov*")))
    (unwind-protect
        (let* ((ov (with-current-buffer ov-buf
                     (insert "x")
                     (make-overlay (point-min) (point-max))))
               (data (list :messages (vector)))
               (fsm (gptel-make-fsm
                     :info (list :context ov
                                 :backend nil
                                 :data data))))
          (setf (gptel-fsm-info fsm)
                (plist-put (gptel-fsm-info fsm)
                           :mevedel-agent-invocation inv))
          ;; 4 cycles at interval 2: fires on 0 and 2 (2 injections).
          (dotimes (_ 4) (mevedel-agent-runtime--handle-wait-inject fsm))
          (should (equal 4 (mevedel-agent-invocation-turn-count inv)))
          (should (equal 2 (length (plist-get data :messages)))))
      (kill-buffer ov-buf))))

(mevedel-deftest mevedel-agent-runtime--augment-agent-handlers
  ()
  ,test
  (test)

  :doc "prepend merges new handlers at the head of matching state entries"
  (let* ((h (lambda (_fsm) 'extra))
         (base `((WAIT ,#'ignore)
                 (DONE ,#'car)
                 (ERRS ,#'cdr)))
         (result (mevedel-agent-runtime--augment-agent-handlers
                  base :prepend `((WAIT . (,h))))))
    (should (equal (list h #'ignore) (cdr (assq 'WAIT result))))
    (should (equal (list #'car) (cdr (assq 'DONE result))))
    (should (equal (list #'cdr) (cdr (assq 'ERRS result))))
    ;; original alist not mutated
    (should (equal (list #'ignore) (cdr (assq 'WAIT base)))))

  :doc "append merges new handlers at the tail of matching state entries"
  (let* ((h (lambda (_fsm) 'extra))
         (base `((WAIT ,#'ignore)
                 (DONE ,#'car)))
         (result (mevedel-agent-runtime--augment-agent-handlers
                  base :append `((DONE . (,h)) (ERRS . (,h))))))
    (should (equal (list #'car h) (cdr (assq 'DONE result))))
    (should (equal (list h) (cdr (assq 'ERRS result))))
    (should (equal (list #'ignore) (cdr (assq 'WAIT result)))))

  :doc "creates missing state entries for both prepend and append"
  (let* ((h (lambda (_fsm) 'extra))
         (base `((WAIT ,#'ignore)))
         (result (mevedel-agent-runtime--augment-agent-handlers
                  base
                  :prepend `((TOOL . (,h)))
                  :append `((DONE . (,h))))))
    (should (equal (list h) (cdr (assq 'TOOL result))))
    (should (equal (list h) (cdr (assq 'DONE result))))))

(mevedel-deftest mevedel-agent-runtime--finalize ()
		 ,test
		 (test)

		 :doc "stops every execution owned by the terminating agent"
		 (let* ((session (mevedel-session--create :name "test"))
			(agent-id "explorer--owned")
			(inv (mevedel-agent-invocation--create
			      :agent-id agent-id
			      :parent-session session
			      :transcript-status 'running))
			stopped)
		   (cl-letf (((symbol-function 'mevedel-execution-stop-owner)
			      (lambda (owner-session owner)
				(setq stopped (list owner-session owner))))
			     ((symbol-function
			       'mevedel-session-persistence--update-transcript-entry)
			      #'ignore)
			     ((symbol-function 'mevedel-agent-exec--flush-transcript-save)
			      #'ignore)
			     ((symbol-function 'mevedel-agent-exec--record-activity)
			      #'ignore)
			     ((symbol-function 'mevedel-agent-exec--handle-update) #'ignore)
			     ((symbol-function 'mevedel-agent-exec--run-stop-hook) #'ignore)
			     ((symbol-function
			       'mevedel-view-agent-live-transcript-finalize)
			      (lambda (_invocation) nil)))
		     (mevedel-agent-runtime--finalize inv 'aborted))
		   (should (equal (list session agent-id) stopped)))

		 :doc "writes sidecar after terminal activity is promoted to full history"
		 (let* ((parent-buf (generate-new-buffer " *mev-agent-finalize-parent*"))
			(agent (mevedel-agent--create :name "explorer"))
			(agent-id "explorer--finalize")
			(activity (cl-loop for i below 6
					   collect (list :type 'tool-finish
							 :tool-name "Read"
							 :summary (format "tool-%d" i))))
			(session (mevedel-session--create
				  :name "test"
				  :agent-transcripts
				  (list (cons agent-id
					      (list :status 'running
						    :activity (last activity 5))))))
			(inv (mevedel-agent-invocation--create
			      :agent agent
			      :agent-id agent-id
			      :parent-session session
			      :parent-data-buffer parent-buf
			      :background-p t
			      :activity activity))
			(written-entry nil))
		   (unwind-protect
		       (cl-letf (((symbol-function
				   'mevedel-agent-exec--save-transcript-buffer)
				  (lambda (_invocation) t))
				 ((symbol-function 'mevedel-agent-exec--handle-update)
				  (lambda (_invocation) t))
				 ((symbol-function
				   'mevedel-agent-exec--run-stop-hook)
				  (lambda (_invocation _status) t))
				 ((symbol-function
				   'mevedel-view-agent-live-transcript-finalize)
				  (lambda (_invocation) nil))
				 ((symbol-function
				   'mevedel-session-persistence--write-sidecar-now)
				  (lambda (write-session _buffer)
				    (setq written-entry
					  (copy-tree
					   (alist-get agent-id
						      (mevedel-session-agent-transcripts
						       write-session)
						      nil nil #'equal))))))
			 (mevedel-agent-runtime--finalize inv 'completed)
			 (should written-entry)
			 (should (eq 'completed (plist-get written-entry :status)))
			 (let ((written-activity (plist-get written-entry :activity)))
			   (should (= 6 (length written-activity)))
			   (should (equal "tool-0"
					  (plist-get (car written-activity) :summary)))
			   (should-not
			    (seq-some (lambda (item)
					(eq (plist-get item :type) 'status))
				      written-activity))))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

:doc "completed finalization persists cleanup of agent-owned tasks"
  (let* ((parent-buf (generate-new-buffer " *mev-agent-task-finalize-parent*"))
         (agent (mevedel-agent--create :name "explorer"))
         (agent-id "explorer--0123456789abcdef0123456789abcdef")
         (session (mevedel-session--create
                   :name "test"
                   :tasks (list
                           (mevedel-task--create
                            :id 1 :subject "agent open"
                            :status 'pending :owner agent-id)
                           (mevedel-task--create
                            :id 2 :subject "main open"
                            :status 'pending))
                   :task-status-notes
                   (list (cons agent-id
                               '(:note "Inspecting"
                                 :updated-turn 1
                                 :updated-at "now")))
                   :agent-transcripts
                   (list (cons agent-id
                               (list :status 'running)))))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id agent-id
               :parent-session session
               :parent-data-buffer parent-buf
               :background-p t))
         (written-tasks nil)
         (written-notes :unset)
         (written-turn :unset))
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-agent-exec--save-transcript-buffer)
                   (lambda (_invocation) t))
                  ((symbol-function 'mevedel-agent-exec--handle-update)
                   (lambda (_invocation) t))
                  ((symbol-function
                    'mevedel-agent-exec--run-stop-hook)
                   (lambda (_invocation _status) t))
                  ((symbol-function
                    'mevedel-view-agent-live-transcript-finalize)
                   (lambda (_invocation) nil))
                  ((symbol-function 'mevedel-tool-task--refresh-display)
                   (lambda () t))
                  ((symbol-function
                    'mevedel-session-persistence--write-sidecar-now)
                   (lambda (write-session _buffer)
                     (setq written-tasks
                           (mapcar #'copy-mevedel-task
                                   (mevedel-session-tasks
                                    write-session)))
                     (setq written-notes
                           (copy-tree
                            (mevedel-session-task-status-notes
                             write-session)))
                     (setq written-turn
                           (mevedel-session-last-task-write-turn
                            write-session)))))
          (mevedel-agent-runtime--finalize inv 'completed)
          (should written-tasks)
          (should (eq 'completed
                      (mevedel-task-status (car written-tasks))))
          (should (eq 'pending
                      (mevedel-task-status (cadr written-tasks))))
          (should (= 1 written-turn))
          (should (null written-notes)))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

:doc "marks status terminal and is idempotent"
  (cl-destructuring-bind (workspace . tempdir)
      (mevedel-agent-runtime-test--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (agent (mevedel-agent--create :name "explorer"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent)))
          (setf (mevedel-agent-invocation-agent-id inv) "explorer--fin")
          (setf (mevedel-agent-invocation-parent-session inv) session)
          (setf (mevedel-agent-invocation-transcript-status inv) 'running)
          ;; Seed the session slot so finalize has somewhere to update.
          (setf (mevedel-session-agent-transcripts session)
                (list (cons "explorer--fin"
                            (list :status 'running
                                  :path "agents/explorer--fin.chat.org"
                                  :parent-turn 1))))
          (mevedel-agent-runtime--finalize inv 'completed)
          (should (eq (mevedel-agent-invocation-transcript-status inv)
                      'completed))
          (should (eq (plist-get (cdr (assoc "explorer--fin"
                                             (mevedel-session-agent-transcripts
                                              session)))
                                 :status)
                      'completed))
          ;; Idempotent: a second call with a different status doesn't
          ;; flip the terminal one.
          (mevedel-agent-runtime--finalize inv 'aborted)
          (should (eq (mevedel-agent-invocation-transcript-status inv)
                      'completed)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "keeps live transcript data buffer while its rendered view is open"
  (cl-destructuring-bind (workspace . tempdir)
      (mevedel-agent-runtime-test--make-workspace)
    (let ((agent-buf (generate-new-buffer " *mevedel-live-finalize-agent*"))
          (view-buf (generate-new-buffer " *mevedel-live-finalize-view*")))
      (unwind-protect
          (let* ((session (mevedel-session-create "main" workspace))
                 (agent (mevedel-agent--create :name "explorer"
                                               :system-prompt "stub"
                                               :tools nil
                                               :reminders nil))
                 (inv (mevedel-agent-invocation-create agent))
                 (agent-id "explorer--livefin"))
            (setf (mevedel-agent-invocation-agent-id inv) agent-id)
            (setf (mevedel-agent-invocation-parent-session inv) session)
            (setf (mevedel-agent-invocation-buffer inv) agent-buf)
            (setf (mevedel-agent-invocation-transcript-status inv) 'running)
            (setf (mevedel-agent-invocation-call-count inv) 3)
            (setf (mevedel-session-agent-transcripts session)
                  (list (cons agent-id
                              (list :status 'running
                                    :path "agents/explorer--livefin.chat.org"
                                    :parent-turn 1))))
            (with-current-buffer agent-buf
              (org-mode)
              (setq-local mevedel--session session)
              (setq-local mevedel--agent-invocation inv)
              (insert "*** Live transcript\n"))
            (mevedel-view--setup
             view-buf agent-buf
             (list :agent-transcript-p t
                   :agent-id agent-id
                   :preserve-data-view-buffer t
                   :transcript-info
                   (list :agent-id agent-id
                         :status 'running
                         :buffer agent-buf
                         :live-buffer t
                         :calls 3
                         :session session)))
            (mevedel-agent-runtime--finalize inv 'completed)
            (should (buffer-live-p agent-buf))
            (should (buffer-live-p view-buf))
            (with-current-buffer view-buf
              (should-not (plist-get mevedel-view--agent-transcript-info
                                     :live-buffer))
              (should (eq (plist-get mevedel-view--agent-transcript-info
                                     :status)
                          'completed))
              (mevedel-view-close-agent-transcript))
            (should-not (buffer-live-p view-buf))
            (should-not (buffer-live-p agent-buf)))
        (when (buffer-live-p view-buf) (kill-buffer view-buf))
        (when (buffer-live-p agent-buf)
          (with-current-buffer agent-buf
            (setq kill-buffer-hook nil))
          (kill-buffer agent-buf))
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry))))

  :doc "copies final background activity into sidecar entry and render-data"
  (cl-destructuring-bind (workspace . tempdir)
      (mevedel-agent-runtime-test--make-workspace)
    (let ((parent (generate-new-buffer " *mevedel-finalize-parent*")))
      (unwind-protect
          (let* ((session (mevedel-session-create "main" workspace))
                 (agent (mevedel-agent--create :name "explorer"
                                               :system-prompt "stub"
                                               :tools nil
                                               :reminders nil))
                 (inv (mevedel-agent-invocation-create agent))
                 (agent-id "explorer--bgfin"))
            (setf (mevedel-agent-invocation-agent-id inv) agent-id)
            (setf (mevedel-agent-invocation-parent-session inv) session)
            (setf (mevedel-agent-invocation-parent-data-buffer inv) parent)
            (setf (mevedel-agent-invocation-transcript-status inv) 'running)
            (setf (mevedel-agent-invocation-background-p inv) t)
            (setf (mevedel-agent-invocation-activity inv)
                  '((:type message :from "main")
                    (:type waiting)
                    (:type tool-start :tool-name "SendMessage")
                    (:type tool-finish :tool-name "SendMessage")
                    (:type waiting)
                    (:type tool-start :tool-name "Read")
                    (:type tool-finish :tool-name "Read")
                    (:type tool-start :tool-name "Grep")))
            (setf (mevedel-session-agent-transcripts session)
                  (list (cons agent-id
                              (list :status 'running
                                    :path "agents/explorer--bgfin.chat.org"
                                    :parent-turn 1))))
            (with-current-buffer parent
              (insert "launch"
                      (mevedel-pipeline--format-render-data-block
                       (list :kind 'agent-transcript
                             :agent-id agent-id
                             :background t
                             :status 'running
                             :calls 0))))
            (mevedel-agent-runtime--finalize inv 'completed)
            (let* ((entry (cdr (assoc agent-id
                                      (mevedel-session-agent-transcripts
                                       session))))
              (activity (plist-get entry :activity)))
              (should (eq (plist-get entry :status) 'completed))
              (should (= 8 (length activity)))
              (should (equal "main" (plist-get (car activity) :from)))
              (should (eq 'tool-start
                          (plist-get (car (last activity)) :type))))
            (with-current-buffer parent
              (let* ((bounds
                      (mevedel-pipeline--find-render-data-block-by-agent-id
                       agent-id))
                     (raw (buffer-substring-no-properties
                           (car bounds) (cdr bounds)))
                     (rd (cdr (mevedel-pipeline-extract-render-data raw))))
                (should (eq (plist-get rd :status) 'completed))
                (should (= 8 (length (plist-get rd :activity)))))))
        (when (buffer-live-p parent) (kill-buffer parent))
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry))))

:doc "finalize leaves a displayed agent buffer alive"
  (cl-destructuring-bind (workspace . tempdir)
      (mevedel-agent-runtime-test--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (agent (mevedel-agent--create :name "explorer"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent))
               (parent-buf (generate-new-buffer "*spec21-fin-parent*"))
               (agent-buf nil))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (setf (mevedel-agent-invocation-agent-id inv) "explorer--keepit")
          (setf (mevedel-agent-invocation-parent-session inv) session)
          (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buf)
          (setf (mevedel-agent-invocation-transcript-status inv) 'running)
          (setf (mevedel-session-agent-transcripts session)
                (list (cons "explorer--keepit"
                            (list :status 'running
                                  :path "agents/x.chat.org"
                                  :parent-turn 1))))
          (setq agent-buf (mevedel-agent-exec--allocate-agent-buffer
                           inv parent-buf))
          (setf (mevedel-agent-invocation-buffer inv) agent-buf)
          ;; Display the buffer in a window.
          (let ((win (display-buffer agent-buf)))
            (mevedel-agent-runtime--finalize inv 'completed)
            (should (buffer-live-p agent-buf))
            (delete-window win))
          (when (buffer-live-p agent-buf)
            (with-current-buffer agent-buf
              (set-buffer-modified-p nil)
              (setq kill-buffer-hook nil))
            (kill-buffer agent-buf))
          (kill-buffer parent-buf))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))







(mevedel-deftest mevedel-agent-runtime-dispatch--wrap-foreground-response ()
  ,test
  (test)

  :doc "wraps with render-data when transcript path is set"
  (let* ((agent (mevedel-agent--create :name "explorer"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent)))
    (setf (mevedel-agent-invocation-agent-id inv) "explorer--frw")
    (setf (mevedel-agent-invocation-transcript-relative-path inv)
          "agents/explorer--frw.chat.org")
    (setf (mevedel-agent-invocation-transcript-status inv) 'completed)
    (let ((result
           (mevedel-agent-runtime-dispatch--wrap-foreground-response
            "the response text" inv)))
      (should (consp result))
      (should (equal (plist-get result :result) "the response text"))
      (let ((rd (plist-get result :render-data)))
        (should (eq (plist-get rd :kind) 'agent-transcript))
        (should (equal (plist-get rd :agent-id) "explorer--frw"))
        (should (equal (plist-get rd :transcript-relative-path)
                       "agents/explorer--frw.chat.org")))))

  :doc "passes through when no transcript path"
  (let* ((agent (mevedel-agent--create :name "explorer"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent)))
    (setf (mevedel-agent-invocation-agent-id inv) "explorer--nopath")
    (let ((result (mevedel-agent-runtime-dispatch--wrap-foreground-response
                   "raw" inv)))
      (should (equal result "raw"))))

  :doc "passes through non-string responses unchanged"
  (let* ((agent (mevedel-agent--create :name "explorer"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent)))
    (setf (mevedel-agent-invocation-agent-id inv) "explorer--ns")
    (setf (mevedel-agent-invocation-transcript-relative-path inv)
          "agents/explorer--ns.chat.org")
    (let ((result (mevedel-agent-runtime-dispatch--wrap-foreground-response
                   nil inv)))
      (should (eq result nil)))))

(mevedel-deftest mevedel-agent-runtime--verifier-verdict ()
  ,test
  (test)
  :doc "parses literal verifier verdict lines only for verifier agents"
  (let* ((agent (mevedel-agent--create :name "verifier"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent)))
    (should (eq 'fail
                (mevedel-agent-runtime--record-verifier-verdict
                 "### Check\nok\nVERDICT: FAIL" inv)))
    (should (eq 'fail (mevedel-agent-invocation-verdict inv))))
  :doc "uses the final verdict line instead of earlier verdict-looking evidence"
  (let* ((agent (mevedel-agent--create :name "verifier"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent)))
    (should (eq 'pass
                (mevedel-agent-runtime--record-verifier-verdict
                 (concat "### Check\n"
                         "**Output observed:**\n"
                         "  VERDICT: FAIL\n\n"
                         "**Result: PASS**\n\n"
                         "VERDICT: PASS\n")
                 inv)))
    (should (eq 'pass (mevedel-agent-invocation-verdict inv))))
  (let* ((agent (mevedel-agent--create :name "explorer"
                                       :system-prompt "stub"
                                       :tools nil
                                       :reminders nil))
         (inv (mevedel-agent-invocation-create agent)))
    (should-not
     (mevedel-agent-runtime--record-verifier-verdict
      "VERDICT: FAIL" inv))))

(mevedel-deftest mevedel-agent-runtime-dispatch--path-collision ()
  ,test
  (test)

  :doc "appends -2 when basename already exists"
  (cl-destructuring-bind (workspace . tempdir)
      (mevedel-agent-runtime-test--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (parent-buf (generate-new-buffer "*spec21-collision-parent*"))
               (agent (mevedel-agent--create :name "explorer"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent)))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (setf (mevedel-agent-invocation-agent-id inv)
                "explorer--abcdef0123456789abcdef0123456789")
          (setf (mevedel-agent-invocation-parent-session inv) session)
          (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buf)
          (setf (mevedel-agent-invocation-parent-turn inv) 1)
          ;; Materialize so we have a save-path under which to plant
          ;; the colliding file.
          (mevedel-session-persistence--shallow-ensure-files session parent-buf)
          (let* ((save-path (mevedel-session-save-path session))
                 (timestamp (format-time-string "%FT%H-%M-%S"))
                 (suffix "abcdef01")
                 (basename (format "explorer--%s--%s.chat.org"
                                   timestamp suffix))
                 (collide (file-name-concat save-path "agents" basename))
                 (agent-buf (mevedel-agent-exec--allocate-agent-buffer
                             inv parent-buf)))
            (setf (mevedel-agent-invocation-buffer inv) agent-buf)
            (with-temp-file collide (insert "preexisting"))
            (cl-letf (((symbol-function 'format-time-string)
                       (lambda (&rest _) timestamp)))
              (mevedel-agent-runtime-dispatch--setup-transcript inv agent-buf))
            (let ((rel (mevedel-agent-invocation-transcript-relative-path
                        inv)))
              (should rel)
              (should (string-match-p "-2\\.chat\\.org\\'" rel)))
            (when (buffer-live-p agent-buf)
              (with-current-buffer agent-buf
                (set-buffer-modified-p nil)
                (setq kill-buffer-hook nil))
              (kill-buffer agent-buf))
            (mevedel-agent-runtime-test--release-and-kill parent-buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-agent-runtime-dispatch--mark-start-blocked ()
  ,test
  (test)
  :doc "marks a pre-start blocked transcript as terminal"
  (cl-destructuring-bind (workspace . tempdir)
      (mevedel-agent-runtime-test--make-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (parent-buf (generate-new-buffer "*spec21-block-parent*"))
               (agent (mevedel-agent--create :name "explorer"
                                             :system-prompt "stub"
                                             :tools nil
                                             :reminders nil))
               (inv (mevedel-agent-invocation-create agent))
               (agent-buf nil))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (setf (mevedel-agent-invocation-agent-id inv)
                "explorer--blocked")
          (setf (mevedel-agent-invocation-parent-session inv) session)
          (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buf)
          (setf (mevedel-agent-invocation-parent-turn inv) 1)
          (mevedel-session-persistence--shallow-ensure-files session parent-buf)
          (setq agent-buf (mevedel-agent-exec--allocate-agent-buffer
                           inv parent-buf))
          (setf (mevedel-agent-invocation-buffer inv) agent-buf)
          (mevedel-agent-runtime-dispatch--setup-transcript inv agent-buf)
          (setf (mevedel-agent-invocation-transcript-status inv) 'running)
          (should (eq 'running
                      (mevedel-agent-invocation-transcript-status inv)))
          (mevedel-agent-runtime-dispatch--mark-start-blocked inv "blocked")
          (should (eq 'error
                      (mevedel-agent-invocation-transcript-status inv)))
          (should (equal "blocked"
                         (mevedel-agent-invocation-terminal-reason inv)))
          (let ((entry (cdr (assoc "explorer--blocked"
                                   (mevedel-session-agent-transcripts
                                    session)))))
            (should (eq 'error (plist-get entry :status)))
            (should (equal "blocked" (plist-get entry :reason))))
          (when (buffer-live-p agent-buf)
            (with-current-buffer agent-buf
              (set-buffer-modified-p nil)
              (setq kill-buffer-hook nil))
            (kill-buffer agent-buf))
          (mevedel-agent-runtime-test--release-and-kill parent-buf session))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))



(mevedel-deftest mevedel-agent-runtime--queue-background-status-reminder ()
  ,test
  (test)

  :doc "queues running and terminal background status details"
  (cl-destructuring-bind (workspace . tempdir)
      (mevedel-agent-runtime-test--make-workspace)
    (unwind-protect
        (let ((session (mevedel-session-create "main" workspace)))
          (mevedel-agent-runtime--queue-background-status-reminder
           session "worker--1" "worker" "patch tests" 'running
           "agents/worker--1.org")
          (mevedel-agent-runtime--queue-background-status-reminder
           session "worker--1" "worker" "patch tests" 'completed
           "agents/worker--1.org" "All checks pass\nmore detail" nil)
          (let ((body (string-join
                       (mevedel-session-pending-reminders session)
                       "\n")))
            (should (string-match-p "worker--1" body))
            (should (string-match-p "running" body))
            (should (string-match-p "completed" body))
            (should (string-match-p "Task: patch tests" body))
            (should (string-match-p "Transcript: agents/worker--1.org" body))
            (should (string-match-p "Latest summary: All checks pass" body))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-agent-runtime)
;;; test-mevedel-agent-runtime.el ends here
