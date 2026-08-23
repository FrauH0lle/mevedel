;;; test-mevedel-tool-exec.el -- Tests for Bash and Eval execution tools -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for Bash and Eval execution tools.

;;; Code:

(require 'mevedel-tool-exec)
(require 'cl-lib)
(require 'seq)
(require 'mevedel-bash-analysis)
(require 'mevedel-structs)
(require 'mevedel-tool-registry)
(require 'mevedel-execution)
(require 'mevedel-execution-target)
(require 'mevedel-pipeline)
(require 'mevedel-sandbox)
(require 'mevedel-telemetry)
(require 'mevedel-workspace)
(require 'mevedel-permission-rules)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(declare-function gptel-tool-args "gptel" (tool))
(declare-function gptel-tool-function "gptel" (tool))

(defun mevedel-tool-exec-test--handler-result (envelope)
  "Return the result from a canonical handler ENVELOPE."
  (should (mevedel-pipeline--handler-return-p envelope))
  (plist-get envelope :result))

(defun mevedel-tool-exec-test--call-bash (callback args)
  "Call the Bash handler with CALLBACK and ARGS in a test session."
  (let ((mevedel--session
         (or mevedel--session
             (mevedel-skills-test--make-session "bash"))))
    (mevedel-tool-exec--bash callback args)))

(mevedel-deftest mevedel-tool-exec--bash-yield-time-ms ()
  ,test
  (test)
  :doc "defaults to ten seconds and passes declared-range values through"
  (should (= 10000 (mevedel-tool-exec--bash-yield-time-ms nil)))
  (should (= 250 (mevedel-tool-exec--bash-yield-time-ms
                  (list :yield_time_ms 250))))
  (should (= 30000 (mevedel-tool-exec--bash-yield-time-ms
                    (list :yield_time_ms 30000))))
  (should (= 10000 (mevedel-tool-exec--bash-yield-time-ms
                    (list :yield_time_ms nil)))))

(mevedel-deftest mevedel-tool-exec--write-wait-time-ms ()
  ,test
  (test)
  :doc "uses distinct poll and input defaults"
  (should (= 5000 (mevedel-tool-exec--write-wait-time-ms nil "")))
  (should (= 250 (mevedel-tool-exec--write-wait-time-ms nil "x")))
  :doc "clamps positive short poll waits to five seconds"
  (dolist (value '(1 250 1000 4999))
    (should (= 5000
               (mevedel-tool-exec--write-wait-time-ms
                (list :yield_time_ms value) ""))))
  :doc "accepts the inclusive poll and input bounds"
  (dolist (case '((5000 "") (300000 "") (250 "x") (30000 "x")))
    (should (= (car case)
               (mevedel-tool-exec--write-wait-time-ms
                (list :yield_time_ms (car case)) (cadr case)))))
  :doc "clamps to the distinct poll and input ranges without erroring"
  (dolist (case '((-1 . 5000) (0 . 5000) (300001 . 300000)
                  (1.5 . 5000) ("5000" . 5000) ("abc" . 5000)))
    (should (= (cdr case)
               (mevedel-tool-exec--write-wait-time-ms
                (list :yield_time_ms (car case)) ""))))
  (should (= 250 (mevedel-tool-exec--write-wait-time-ms
                  '(:yield_time_ms 249) "x")))
  (should (= 30000 (mevedel-tool-exec--write-wait-time-ms
                    '(:yield_time_ms 30001) "x"))))

(mevedel-deftest mevedel-tool-exec--execution-artifact-directory ()
  ,test
  (test)
  :doc "places execution artifacts below the session tool-results directory"
  (let* ((target (mevedel-execution-target-create default-directory))
         (session (mevedel-session--create :authority-mode 'pid-lock :execution-target target)))
    (cl-letf (((symbol-function 'mevedel-pipeline-tool-results-dir)
               (lambda (_session _buffer) "/tmp/tool-results")))
      (should (equal "/tmp/tool-results/executions"
                     (mevedel-tool-exec--execution-artifact-directory
                      session)))))
  :doc "remote sessions use the execution module's local temporary spool"
  (let* ((target (mevedel-execution-target-create
                  "/ssh:user@host:/srv/project/"))
         (session (mevedel-session--create :authority-mode 'pid-lock :execution-target target)))
    (cl-letf (((symbol-function 'mevedel-pipeline-tool-results-dir)
               (lambda (&rest _)
                 (ert-fail "Remote spool consulted the target store"))))
      (should-not
       (mevedel-tool-exec--execution-artifact-directory session))))
  :doc "ephemeral Bash requests do not materialize retained artifacts"
  (let* ((root (make-temp-file "mevedel-ephemeral-bash-" t))
         (workspace (mevedel-workspace--create
                     :type 'project :id "bash" :root root :name "bash"))
         (session (mevedel-session--create
                   :name "side" :workspace workspace))
         (request (mevedel-request--create
                   :session session :ephemeral-p t)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--current-request request)
          (should-not
           (mevedel-tool-exec--execution-artifact-directory session))
          (should-not (mevedel-session-save-path session))
          (should-not (file-exists-p
                       (file-name-concat root ".mevedel"))))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-exec--execution-facts-xml ()
  ,test
  (test)
  :doc "serializes canonical facts without a chunk id"
  (let ((xml
         (mevedel-tool-exec--execution-facts-xml
          '(:execution-id "exec-1" :state running
                          :command "printf \"x\" & wait"
                          :wall-time-seconds 1.25 :output-bytes 4 :output-lines 1
                          :omitted-output-bytes 0 :tty nil
                          :output-path "/tmp/a&b"))))
    (should (string-match-p "execution_id=\"exec-1\"" xml))
    (should (string-match-p
             "command=\"printf &quot;x&quot; &amp; wait\"" xml))
    (should (string-match-p "tty=\"false\"" xml))
    (should (string-match-p "output_path=\"/tmp/a&amp;b\"" xml))
    (should-not (string-match-p "chunk" xml))))

(mevedel-deftest mevedel-tool-exec-format-execution-metadata ()
  ,test
  (test)
  :doc "formats shared live and terminal row metadata"
  (should
   (equal "running · 2.5s · 3 lines · 42 bytes · exec-1"
          (mevedel-tool-exec-format-execution-metadata
           '(:state running :wall-time-seconds 2.5
                    :output-lines 3 :output-bytes 42 :execution-id "exec-1")))))

(mevedel-deftest mevedel-tool-exec--observation-envelope ()
  ,test
  (test)
  :doc "keeps output raw while status and facts remain structured"
  (let* ((envelope
          (mevedel-tool-exec--observation-envelope
           '(:output "failure text"
                     :facts (:state completed :termination exited :exit-code 7
                                    :outcome failure :wall-time-seconds 0.1
                                    :output-bytes 12 :output-lines 1
                                    :omitted-output-bytes 0 :tty nil))))
         (result (plist-get envelope :result)))
    (should (eq 'error (plist-get envelope :status)))
    (should (string-prefix-p "failure text\n\n<bash-execution" result))
    (should (string-match-p "state=\"completed\"" result))
    (should-not (string-match-p "Command failed" result))
    (should (eq 'completed
                (plist-get (plist-get envelope :render-data) :state)))
    (should (= 7 (plist-get (plist-get envelope :render-data)
                            :exit-code))))
  :doc "a process that never started is an error, not a success"
  ;; Status came from facts alone: with no completed state to judge,
  ;; a start failure was reported successful with a "Failed to start
  ;; process" body.
  (let ((envelope
         (mevedel-tool-exec--observation-envelope
          '(:error "spawning child process: no such file"
                   :facts (:state failed)))))
    (should (eq 'error (plist-get envelope :status)))
    (should (string-prefix-p "Failed to start process"
                             (plist-get envelope :result))))

  :doc "keeps trusted injection output clean while retaining hidden facts"
  (let ((envelope
         (mevedel-tool-exec--observation-envelope
          '(:output "injected"
                    :facts (:state completed :termination exited :exit-code 0
                                   :outcome success :wall-time-seconds 0.1
                                   :output-bytes 8 :output-lines 1
                                   :omitted-output-bytes 0 :tty nil))
          t)))
    (should (equal "injected" (plist-get envelope :result)))
    (should (eq 'success (plist-get envelope :status)))
    (should (= 0 (plist-get (plist-get envelope :render-data)
                            :exit-code))))
  :doc "adds recovery guidance only to failed confined commands"
  (let* ((guidance
          "This command ran with network/path confinement. If confinement caused the failure, retry with `with_additional_permissions` and request only the required network or exact path capability. Use `require_escalated` only when additive permissions cannot represent the requirement.")
         (failed
          (mevedel-tool-exec--observation-envelope
           '(:output "network failed"
                     :facts (:state completed :termination exited :exit-code 1
                                    :outcome failure :wall-time-seconds 0.1
                                    :output-bytes 14 :output-lines 1
                                    :omitted-output-bytes 0 :tty nil)
                     :sandbox-facts
                     (:sandbox bubblewrap :filesystem workspace-write
                               :network isolated))))
         (semantic
          (mevedel-tool-exec--observation-envelope
           '(:output ""
                     :facts (:state completed :termination exited :exit-code 1
                                    :outcome no-match :wall-time-seconds 0.1
                                    :output-bytes 0 :output-lines 0
                                    :omitted-output-bytes 0 :tty nil)
                     :sandbox-facts
                     (:sandbox bubblewrap :filesystem workspace-write
                               :network isolated)))))
    (should (string-search guidance (plist-get failed :result)))
    (should-not (string-search guidance (plist-get semantic :result))))
  :doc "directs a required-mode refusal to a new full escalation request"
  (let ((result
         (plist-get
          (mevedel-tool-exec--observation-envelope
           '(:output ""
                     :error (error "Bubblewrap unavailable")
                     :facts (:state completed :termination spawn-failed :exit-code -1
                                    :outcome failure :wall-time-seconds 0.0
                                    :output-bytes 0 :output-lines 0
                                    :omitted-output-bytes 0 :tty nil)
                     :sandbox-facts
                     (:sandbox unavailable :filesystem unrestricted
                               :network unrestricted :refused t)))
          :result)))
    (should
     (string-search
      "Only a new invocation with `require_escalated`"
      result))
    (should-not
     (string-search "`with_additional_permissions`" result))))

(mevedel-deftest mevedel-tool-exec--bash-outcome ()
  ,test
  (test)
  :doc "derives supported outcomes and conservatively falls back"
  (dolist (case '(("rg needle" 1 exited no-match)
                  ("diff one two" 1 exited different)
                  ("test 1 = 2" 1 exited false)
                  ("[ 1 = 2 ]" 1 exited false)
                  ("grep needle" 2 exited failure)
                  ("rg needle" 2 exited failure)
                  ("diff one two" 2 exited failure)
                  ("test 1 = 2" 2 exited failure)
                  ("grep needle" 0 exited success)
                  ("true && grep needle" 1 exited failure)
                  ("grep needle >out" 1 exited failure)
                  ("/bin/grep needle" 1 exited failure)
                  ("false" 1 exited failure)
                  ("grep needle" 1 stopped failure)))
    (pcase-let ((`(,command ,exit-code ,termination ,expected) case))
      (should
       (eq expected
           (mevedel-tool-exec--bash-outcome
            (mevedel-bash-analysis-analyze command)
            exit-code termination))))))

(mevedel-deftest mevedel-tool-exec--write-stdin ()
  ,test
  (test)
  :doc "polls through the captured session and canonical owner"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
        captured events input result)
    (let ((mevedel--session session)
          (mevedel--current-request nil)
          (mevedel--agent-invocation nil))
      (setq input (plist-put (list :execution_id "exec-1")
                             :yield_time_ms 30000))
      (cl-letf (((symbol-function 'mevedel-execution-observe)
                 (lambda (&rest args)
                   (setq captured args)
                   (funcall
                    (nth 3 args)
                    '(:output "delta"
                              :facts (:execution-id "exec-1" :state running
                                                    :wall-time-seconds 1.0 :output-bytes 5
                                                    :output-lines 1 :omitted-output-bytes 0
                                                    :tty nil)))))
                ((symbol-function 'mevedel-telemetry-record)
                 (lambda (_session event &rest props)
                   (push (cons event props) events))))
        (mevedel-tool-exec--write-stdin
         (lambda (value) (setq result value))
         input)))
    (should (eq session (nth 0 captured)))
    (should (equal "/root" (nth 1 captured)))
    (should (equal "exec-1" (nth 2 captured)))
    (should (= 30000 (plist-get (nthcdr 4 captured) :wait-ms)))
    (should
     (equal
      '(execution-observe-requested
        :execution-id "exec-1" :owner "/root" :input-p nil
        :requested-yield-time-ms 30000 :effective-wait-ms 30000)
      (car events)))
    (should (string-prefix-p "delta" (plist-get result :result)))
    (should (eq 'poll
                (plist-get (plist-get result :render-data)
                           :execution-control)))
    (should (plist-get (plist-get result :render-data)
                       :observation-output-p)))
  :doc "side execution observations use the durable audit target"
  (let* ((parent (mevedel-session--create :authority-mode 'pid-lock :name "parent"))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "side" :audit-session parent))
         captured)
    (let ((mevedel--session session)
          (mevedel--current-request nil)
          (mevedel--agent-invocation nil))
      (cl-letf (((symbol-function 'mevedel-execution-observe)
                 (lambda (_session _owner _execution-id callback &rest _)
                   (funcall
                    callback
                    '(:output ""
                              :facts (:execution-id "exec-1" :state running
                                                    :wall-time-seconds 1.0 :output-bytes 0
                                                    :output-lines 0 :omitted-output-bytes 0
                                                    :tty nil)))))
                ((symbol-function 'mevedel-telemetry-record)
                 (lambda (target event &rest props)
                   (setq captured (list target event props)))))
        (mevedel-tool-exec--write-stdin
         #'ignore '(:execution_id "exec-1" :chars "private input"))))
    (should (eq parent (car captured)))
    (should (eq 'execution-observe-requested (cadr captured)))
    (should (eq 'btw
                (plist-get (nth 2 captured) :conversation-scope)))
    (should-not
     (string-match-p "private input" (prin1-to-string (nth 2 captured)))))
  :doc "marks input writes separately from empty output polls"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
        result)
    (let ((mevedel--session session)
          (mevedel--current-request nil)
          (mevedel--agent-invocation nil))
      (cl-letf (((symbol-function 'mevedel-execution-observe)
                 (lambda (_session _owner _execution-id callback &rest _)
                   (funcall
                    callback
                    '(:output ""
                              :facts (:execution-id "exec-1" :state running
                                                    :wall-time-seconds 1.0 :output-bytes 0
                                                    :output-lines 0 :omitted-output-bytes 0
                                                    :tty t)))))
                ((symbol-function 'mevedel-telemetry-record) #'ignore))
        (mevedel-tool-exec--write-stdin
         (lambda (value) (setq result value))
         '(:execution_id "exec-1" :chars "yes\n"))))
    (should (eq 'input
                (plist-get (plist-get result :render-data)
                           :execution-control)))
    (should-not (plist-get (plist-get result :render-data)
                           :observation-output-p))))

(mevedel-deftest mevedel-tool-exec--list-executions ()
  ,test
  (test)
  :doc "lists only facts returned by the owner-filtered execution API"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
        captured)
    (let ((mevedel--session session)
          (mevedel--current-request nil)
          (mevedel--agent-invocation nil))
      (cl-letf (((symbol-function 'mevedel-execution-list)
                 (lambda (&rest args)
                   (setq captured args)
                   '((:execution-id "exec-1" :state running
                                    :wall-time-seconds 1.0 :output-bytes 0
                                    :output-lines 0 :omitted-output-bytes 0 :tty nil)))))
        (let ((envelope (mevedel-tool-exec--list-executions nil)))
          (should (string-match-p "execution_id=\"exec-1\""
                                  (plist-get envelope :result)))
          (should-not (plist-member envelope :render-data)))))
    (should (equal (list session "/root") captured)))
  :doc "registered dispatch completes through the full pipeline"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
        result)
    (require 'mevedel-tools)
    (mevedel-tool-exec--register)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-execution-list)
                 (lambda (&rest _)
                   '((:execution-id "exec-1" :state running
                                    :wall-time-seconds 1.0 :output-bytes 0
                                    :output-lines 0 :omitted-output-bytes 0 :tty nil)))))
        (let* ((tool (mevedel-tool-get "ListExecutions" "mevedel"))
               (fn (gptel-tool-function (mevedel-tool-gptel-tool tool))))
          (funcall fn (lambda (value) (setq result value))))))
    (should (string-match-p "execution_id=\"exec-1\"" result))
    (should-not (string-match-p "Error:" result))))

(mevedel-deftest mevedel-tool-exec--stop-execution ()
  ,test
  (test)
  :doc "stops through the owner-filtered API and reports tool success"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
        captured result)
    (let ((mevedel--session session)
          (mevedel--current-request nil)
          (mevedel--agent-invocation nil))
      (cl-letf (((symbol-function 'mevedel-execution-stop)
                 (lambda (&rest args)
                   (setq captured args)
                   (funcall
                    (nth 3 args)
                    '(:output "partial"
                              :facts (:execution-id "exec-1" :state completed
                                                    :termination stopped :exit-code 15
                                                    :outcome failure :wall-time-seconds 1.0
                                                    :output-bytes 7 :output-lines 1
                                                    :omitted-output-bytes 0 :tty nil))))))
        (mevedel-tool-exec--stop-execution
         (lambda (value) (setq result value))
         '(:execution_id "exec-1"))))
    (should (equal (list session "/root" "exec-1")
                   (butlast captured)))
    (should (eq 'success (plist-get result :status)))))

(mevedel-deftest mevedel-tool-exec-handle-execution-event ()
  ,test
  (test)
  :doc "queues unread output and final facts for an independent completion"
  (require 'mevedel-agent-control)
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test")))
    (should
     (mevedel-tool-exec-handle-execution-event
      (list :type 'terminal :delivery 'mailbox
            :session session :owner "/root"
            :tool-args '(:command "printf done")
            :observation
            '(:output "done"
                      :facts (:execution-id "exec-1" :state completed
                                            :termination exited :exit-code 0 :outcome success
                                            :wall-time-seconds 0.1 :output-bytes 4
                                            :output-lines 1 :omitted-output-bytes 0 :tty nil)))
      session))
    (let ((messages (mevedel-agent-control-context-mailbox session)))
      (should (= 1 (length messages)))
      (should (eq 'EXECUTION (plist-get (car messages) :type)))
      (should (string-match-p "done" (plist-get (car messages) :payload)))
      (should (string-match-p
               "execution_id=\\\"exec-1\\\""
               (plist-get (car messages) :payload)))))
  :doc "ignores model-claimed terminal events"
  (should-not
   (mevedel-tool-exec-handle-execution-event
    '(:type terminal :delivery model) nil)))

(mevedel-deftest mevedel-tool-exec--bash
  (:quiet t)
  ,test
  (test)
  :doc "errors on missing command"
  (should-error
   (mevedel-tool-exec-test--call-bash #'ignore (list))
   :type 'error)
  :doc "rejects shell-native backgrounding"
  (should-error
   (mevedel-tool-exec-test--call-bash #'ignore '(:command "sleep 1 &"))
   :type 'error)
  :doc "forwards the yield the repair pipeline already range-checked"
  (let (captured)
    (cl-letf (((symbol-function 'mevedel-execution-start-bash)
               (lambda (&rest args) (setq captured args))))
      (mevedel-tool-exec-test--call-bash
       #'ignore (list :command "sleep 1" :yield_time_ms 250)))
    (should (= 250 (plist-get (cdr captured) :yield-time-ms))))
  :doc "trusted internal waits disable yielding"
  (let (captured)
    (cl-letf (((symbol-function 'mevedel-execution-start-bash)
               (lambda (&rest args) (setq captured args))))
      (mevedel-tool-exec-test--call-bash
       #'ignore '(:command "printf done" :wait-for-completion-p t)))
    (should-not (plist-get (cdr captured) :yield-time-ms)))
  :doc "forwards original arguments and durable tool-use identity"
  (let ((mevedel-pipeline--active-tool-use-id "call-bash-7")
        captured)
    (cl-letf (((symbol-function 'mevedel-execution-start-bash)
               (lambda (&rest keys) (setq captured keys))))
      (mevedel-tool-exec-test--call-bash
       #'ignore '(:command "printf identity" :yield_time_ms 250)))
    (should (equal "call-bash-7"
                   (plist-get (cdr captured) :tool-use-id)))
    (should (equal '(:command "printf identity" :yield_time_ms 250)
                   (plist-get (cdr captured) :tool-args))))
  :doc "launches a default call with its matching remembered profile"
  (let* ((session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "profile"
           :permission-rules
           '(("Bash" :pattern "npx test" :network t :action allow))))
         (mevedel--session session)
         captured)
    (cl-letf (((symbol-function 'mevedel-execution-start-bash)
               (lambda (&rest args) (setq captured args))))
      (mevedel-tool-exec--bash #'ignore '(:command "npx test")))
    (should
     (equal '(:network t)
            (plist-get (cdr captured) :additional-permissions))))
  :doc "forwards only proven read-only analysis to scheduler admission"
  (let (read-only unknown)
    (cl-letf (((symbol-function 'mevedel-execution-start-bash)
               (lambda (&rest args)
                 (if read-only
                     (setq unknown args)
                   (setq read-only args)))))
      (mevedel-tool-exec-test--call-bash #'ignore '(:command "pwd"))
      (mevedel-tool-exec-test--call-bash
       #'ignore '(:command "touch scheduler-test")))
    (should (eq t (plist-get (cdr read-only) :read-only-p)))
    (should-not (plist-get (cdr unknown) :read-only-p)))
  :doc "shares one special outcome across handler status, XML, and render data"
  (let (envelope)
    (cl-letf (((symbol-function 'mevedel-execution-start-bash)
               (lambda (&rest args)
                 (let ((outcome-function
                        (plist-get (cdr args) :outcome-function)))
                   (funcall
                    (car args)
                    (list
                     :output "raw"
                     :facts
                     (list
                      :state 'completed :termination 'exited :exit-code 1
                      :outcome (funcall outcome-function 1 'exited)
                      :wall-time-seconds 0.1 :output-bytes 3
                      :output-lines 1 :omitted-output-bytes 0 :tty nil)))))))
      (mevedel-tool-exec-test--call-bash
       (lambda (value) (setq envelope value))
       '(:command "diff one two")))
    (let ((facts (plist-get envelope :render-data)))
      (should (eq 'different (plist-get facts :outcome)))
      (should (eq 'success (plist-get envelope :status)))
      (should (= 1 (plist-get facts :exit-code)))
      (should (string-prefix-p "raw\n\n<bash-execution"
                               (plist-get envelope :result)))
      (should (string-match-p "outcome=\"different\""
                              (plist-get envelope :result)))))
  :doc "passes explicit PTY mode without changing execution authority"
  (let (captured)
    (cl-letf (((symbol-function 'mevedel-execution-start-bash)
               (lambda (&rest args) (setq captured args))))
      (mevedel-tool-exec-test--call-bash
       #'ignore '(:command "printf prompt" :tty t)))
    (should (eq t (plist-get (cdr captured) :tty)))
    (should-not (plist-get (cdr captured) :additional-permissions))
    (should-not (plist-get (cdr captured) :sandbox-permissions)))
  :doc "normalizes JSON false PTY mode and rejects non-booleans"
  (let (captured)
    (cl-letf (((symbol-function 'mevedel-execution-start-bash)
               (lambda (&rest args) (setq captured args))))
      (mevedel-tool-exec-test--call-bash
       #'ignore '(:command "printf plain" :tty :json-false)))
    (should-not (plist-get (cdr captured) :tty))
    (should-error
     (mevedel-tool-exec-test--call-bash
      #'ignore '(:command "printf invalid" :tty "yes"))
     :type 'error))
  :doc "passes approved network authority to the child launcher"
  (let (captured result)
    (cl-letf (((symbol-function
                'mevedel-execution-start-bash)
               (lambda (&rest args)
                 (setq captured args)
                 (funcall (car args)
                          '(:output "ok"
                                    :facts (:state completed :termination exited
                                                   :exit-code 0 :outcome success
                                                   :wall-time-seconds 0.1 :output-bytes 2
                                                   :output-lines 1 :omitted-output-bytes 0
                                                   :tty nil)
                                    :sandbox-facts
                                    (:sandbox bubblewrap
                                              :filesystem workspace-write
                                              :network unrestricted))))))
      (mevedel-tool-exec-test--call-bash
       (lambda (envelope)
         (setq result (mevedel-tool-exec-test--handler-result envelope)))
       '(:command "curl https://example.test"
                  :sandbox_permissions "with_additional_permissions"
                  :additional_permissions (:network t)
                  :justification "Download the requested page?")))
    (should (equal '(:network t)
                   (plist-get (cdr captured) :additional-permissions)))
    (should (string-match-p "network: unrestricted" result)))
  :doc "passes approved full escalation to the child launcher"
  (let (captured result)
    (cl-letf (((symbol-function
                'mevedel-execution-start-bash)
               (lambda (&rest args)
                 (setq captured args)
                 (funcall (car args)
                          '(:output "ok"
                                    :facts (:state completed :termination exited
                                                   :exit-code 0 :outcome success
                                                   :wall-time-seconds 0.1 :output-bytes 2
                                                   :output-lines 1 :omitted-output-bytes 0
                                                   :tty nil)
                                    :sandbox-facts
                                    (:sandbox escalated
                                              :filesystem unrestricted
                                              :network unrestricted))))))
      (mevedel-tool-exec-test--call-bash
       (lambda (envelope)
         (setq result (mevedel-tool-exec-test--handler-result envelope)))
       '(:command "emacs --batch -Q"
                  :sandbox_permissions "require_escalated"
                  :justification "Run without confinement?")))
    (should-not (plist-get (cdr captured) :additional-permissions))
    (should (eq 'require-escalated
                (plist-get (cdr captured) :sandbox-permissions)))
    (should (string-match-p "sandbox: escalated" result)))
  :doc "passes large output intact to the shared pipeline"
  (let ((output (concat (make-string 550000 ?h)
                        (make-string 50000 ?t)))
        result)
    (cl-letf (((symbol-function
                'mevedel-execution-start-bash)
               (lambda (&rest args)
                 (funcall (car args)
                          (list :output output
                                :facts
                                '(:state completed :termination exited
                                         :exit-code 0 :outcome success
                                         :wall-time-seconds 0.1
                                         :output-bytes 600000 :output-lines 1
                                         :omitted-output-bytes 0 :tty nil)
                                :sandbox-facts
                                '(:sandbox bubblewrap
                                           :filesystem workspace-write
                                           :network isolated))))))
      (mevedel-tool-exec-test--call-bash
       (lambda (envelope)
         (setq result (mevedel-tool-exec-test--handler-result envelope)))
       '(:command "noisy-command")))
    (should (string-prefix-p (make-string 100 ?h) result))
    (should (string-search (make-string 100 ?t) result))
    (should-not (string-search "Output truncated" result)))
  :doc "executes simple command and returns output"
  (let ((result nil)
        (done nil))
    (mevedel-tool-exec-test--call-bash
     (lambda (r)
       (setq result (mevedel-tool-exec-test--handler-result r)
             done t))
     (list :command "echo hello"))
    ;; Wait for async process
    (with-timeout (5 (error "Timed out"))
      (while (not done)
        (accept-process-output nil 0.1)))
    (should (string-match-p "hello" result)))
  :doc "reports raw exit code structurally without rewriting output"
  (let ((result nil)
        (done nil))
    (mevedel-tool-exec-test--call-bash
     (lambda (r)
       (setq result (mevedel-tool-exec-test--handler-result r)
             done t))
     (list :command "exit 42"))
    (with-timeout (5 (error "Timed out"))
      (while (not done)
        (accept-process-output nil 0.1)))
    (should (string-match-p "exit_code=\"42\"" result))
    (should-not (string-match-p "Command failed" result)))
  :doc "discloses automatic unrestricted fallback"
  (let ((mevedel-sandbox-mode 'best-effort)
        (mevedel-sandbox--probe-cache
         '((nil . (:available nil
                              :reason "test confinement unavailable"))))
        result done)
    (mevedel-tool-exec-test--call-bash
     (lambda (r)
       (setq result (mevedel-tool-exec-test--handler-result r)
             done t))
     (list :command "printf fallback"))
    (with-timeout (5 (error "Timed out"))
      (while (not done)
        (accept-process-output nil 0.1)))
    (should (string-prefix-p "fallback" result))
    (should (string-match-p "sandbox: unavailable" result))
    (should (string-match-p "filesystem: unrestricted" result))
    (should (string-match-p "network: unrestricted" result))
    (should (string-match-p "test confinement unavailable" result)))
  :doc "loads Bash login initialization from an isolated home"
  (let* ((home (make-temp-file "mevedel-bash-login-" t))
         (profile (file-name-concat home ".bash_profile"))
         (process-environment (copy-sequence process-environment))
         result done)
    (unwind-protect
        (progn
          (with-temp-file profile
            (insert "export MEVEDEL_LOGIN_MARKER=loaded\n"))
          (setenv "HOME" home)
          (mevedel-tool-exec-test--call-bash
           (lambda (r)
             (setq result (mevedel-tool-exec-test--handler-result r)
                   done t))
           (list :command "printf %s \"$MEVEDEL_LOGIN_MARKER\""))
          (with-timeout (5 (error "Timed out"))
            (while (not done)
              (accept-process-output nil 0.1)))
          (should (string-prefix-p "loaded\n" result))
          (should (string-match-p "\n\\[sandbox: " result)))
      (delete-directory home t)))
  :doc "runs from the session working directory when current buffer is elsewhere"
  (let* ((root (make-temp-file "mevedel-bash-cwd-" t))
         (module-dir (file-name-concat root "packages" "api"))
         (agent-dir (file-name-concat root ".mevedel" "sessions"
                                      "main" "agents"))
         (workspace (mevedel-workspace-get-or-create
                     'file root root "test"))
         (session (mevedel-session-create "main" workspace module-dir))
         (mevedel--session session)
         (default-directory (file-name-as-directory agent-dir))
         result done)
    (make-directory module-dir t)
    (make-directory agent-dir t)
    (unwind-protect
        (progn
          (mevedel-tool-exec-test--call-bash
           (lambda (r)
             (setq result (mevedel-tool-exec-test--handler-result r)
                   done t))
           (list :command (if (eq system-type 'windows-nt)
                              "pwd -W"
                            "pwd")))
          (with-timeout (5 (error "Timed out"))
            (while (not done)
              (accept-process-output nil 0.1)))
          (should
           (file-equal-p module-dir
                         (car (split-string result "\n" t)))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-tool-exec--sandbox-writable-roots ()
  ,test
  (test)
  :doc "sandbox root fallback:
`mevedel-tool-exec--sandbox-writable-roots' includes work and temp directories"
  (let ((roots
         (mevedel-tool-exec--sandbox-writable-roots default-directory)))
    (should
     (cl-some (lambda (root)
                (or (string-equal
                     (file-truename root) (file-truename default-directory))
                    (file-in-directory-p default-directory root)))
              roots))
    (should (member (file-name-as-directory
                     (expand-file-name temporary-file-directory))
                    roots)))
  :doc "uses the target temporary directory for remote confinement"
  (let* ((workdir "/ssh:user@host:/srv/project/")
         (target (mevedel-execution-target-create workdir))
         (workspace (mevedel-workspace--create :root workdir))
         (session (mevedel-session--create
                   :workspace workspace
                   :execution-target target
                   :working-directory workdir))
         (temporary-file-directory "/client/private/tmp/"))
    (setf (mevedel-execution-target-environment target)
          '(("HOME" . "/home/user") ("TMPDIR" . "/var/tmp/mevedel")))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (let ((roots (mevedel-tool-exec--sandbox-writable-roots workdir)))
        (should (member "/ssh:user@host:/var/tmp/mevedel/" roots))
        (should-not (member "/client/private/tmp/" roots))
        (should (cl-every #'file-remote-p roots))))))

(mevedel-deftest mevedel-tool-exec--sandbox-disclosure ()
  ,test
  (test)
  :doc "normal disclosure:
`mevedel-tool-exec--sandbox-disclosure' appends active execution boundaries"
  (should
   (equal
    (mevedel-tool-exec--sandbox-disclosure
     "ok" '(:sandbox-facts
            (:sandbox bubblewrap :filesystem workspace-write :network isolated)))
    "ok\n\n[sandbox: bubblewrap; filesystem: workspace-write; network: isolated]"))
  :doc "first fallback disclosure:
`mevedel-tool-exec--sandbox-disclosure' includes one model-visible note"
  (should
   (string-search
    "Confinement was unavailable, so this invocation ran directly."
    (mevedel-tool-exec--sandbox-disclosure
     "ok" '(:sandbox-facts
            (:sandbox unavailable :filesystem unrestricted
                      :network unrestricted :first-direct-fallback t)))))
  :doc "suppressed unrestricted disclosure:
`mevedel-tool-exec--sandbox-disclosure' warns without contaminating substitution"
  (let (warning)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional _level _buffer-name)
                 (setq warning message))))
      (should
       (equal
        (mevedel-tool-exec--sandbox-disclosure
         "literal"
         '(:sandbox-facts
           (:sandbox off :filesystem unrestricted
                     :network unrestricted :reason "disabled"))
         t)
        "literal"))
      (should (string-match-p "without confinement" warning))
      (should (string-match-p "network: unrestricted" warning))))
  :doc "suppressed fallback disclosure:
the execution boundary owns the session's single unavailable warning"
  (let (warning)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest _) (setq warning t))))
      (should
       (equal
        "literal"
        (mevedel-tool-exec--sandbox-disclosure
         "literal"
         '(:sandbox-facts
           (:sandbox unavailable :filesystem unrestricted
                     :network unrestricted :first-direct-fallback t))
         t)))
      (should-not warning))))

(mevedel-deftest mevedel-tool-exec--register ()
  ,test
  (test)
  :doc "registers the shared execution escalation vocabulary for Bash"
  (mevedel-tool-exec--register)
  (let* ((tool (mevedel-tool-get "Bash"))
         (args (gptel-tool-args (mevedel-tool-gptel-tool tool)))
         (sandbox
          (seq-find (lambda (arg)
                      (equal "sandbox_permissions" (plist-get arg :name)))
                    args))
         (additional
          (seq-find (lambda (arg)
                      (equal "additional_permissions" (plist-get arg :name)))
                    args))
         (justification
          (seq-find (lambda (arg)
                      (equal "justification" (plist-get arg :name)))
                    args))
         (filesystem
          (plist-get (plist-get additional :properties) :file_system)))
    (should (equal ["use_default" "with_additional_permissions"
                    "require_escalated"]
                   (plist-get sandbox :enum)))
    (should (equal "boolean"
                   (plist-get (plist-get (plist-get additional :properties)
                                         :network)
                              :type)))
    (dolist (access '(:read :write))
      (let ((schema (plist-get (plist-get filesystem :properties) access)))
        (should (equal "array" (plist-get schema :type)))
        (should (equal "string"
                       (plist-get (plist-get schema :items) :type)))))
    (should (equal "string" (plist-get justification :type)))
    (should (plist-get justification :optional)))
  :doc "registers managed Bash lifecycle tools and yield schema"
  (mevedel-tool-exec--register)
  (let* ((bash (mevedel-tool-get "Bash"))
         (args (gptel-tool-args (mevedel-tool-gptel-tool bash)))
         (yield (seq-find (lambda (arg)
                            (equal "yield_time_ms" (plist-get arg :name)))
                          args))
         (timeout (seq-find (lambda (arg)
                              (equal "timeout_seconds"
                                     (plist-get arg :name)))
                            args))
         (tty (seq-find (lambda (arg)
                          (equal "tty" (plist-get arg :name)))
                        args)))
    (should (equal "integer" (plist-get yield :type)))
    (should-not timeout)
    (should (equal "boolean" (plist-get tty :type)))
    (should (plist-get tty :optional))
    (dolist (name '("WriteStdin" "ListExecutions" "StopExecution"))
      (should (mevedel-tool-get name))))
  :doc "registered WriteStdin dispatch preserves the underscored schema key"
  (progn
    (mevedel-tool-exec--register)
    (let* ((tool (mevedel-tool-get "WriteStdin" "mevedel"))
           (fn (gptel-tool-function (mevedel-tool-gptel-tool tool)))
           captured)
      (should (eq 'yield_time_ms
                  (car (nth 2 (mevedel-tool-args tool)))))
      (cl-letf (((symbol-function 'mevedel-pipeline-run-tool)
                 (lambda (_tool _callback args)
                   (setq captured args))))
        (funcall fn #'ignore "exec-1" "" 12345))
      (should (equal "exec-1" (plist-get captured :execution_id)))
      (should (equal ":yield_time_ms" (symbol-name (nth 4 captured))))
      (should (= 12345 (nth 5 captured)))
      (should-not (plist-member captured :yield-time-ms))))
  :doc "execution control inherits authority without becoming read-only"
  (mevedel-tool-exec--register)
  (dolist (name '("WriteStdin" "StopExecution"))
    (let ((tool (mevedel-tool-get name)))
      (should-not (mevedel-tool-read-only-p tool))
      (dolist (mode '(ask edits full-auto))
        (should
         (eq 'allow
             (mevedel-check-permission
              name :tool-struct tool :content nil :mode mode))))
      (let ((mevedel-permission-rules `((,name :action deny))))
        (should
         (eq 'deny
             (mevedel-check-permission
              name :tool-struct tool :content nil :mode 'full-auto))))))
  :doc "one-shot execution control asks for PTY input but not containment"
  (mevedel-tool-exec--register)
  (let ((write-stdin (mevedel-tool-get "WriteStdin"))
        (stop (mevedel-tool-get "StopExecution"))
        (mevedel-permission-rules nil)
        (mevedel-protected-paths nil))
    (should
     (eq 'ask
         (mevedel-check-permission
          "WriteStdin" :tool-struct write-stdin
          :content '(:execution_id "exec-1" :chars "yes\n")
          :mode 'full-auto :one-shot-mutations-p t)))
    (should
     (eq 'allow
         (mevedel-check-permission
          "StopExecution" :tool-struct stop
          :content '(:execution_id "exec-1")
          :mode 'full-auto :one-shot-mutations-p t)))
    (dolist (chars (list nil "" "\C-c"))
      (should
       (eq 'allow
           (mevedel-check-permission
            "WriteStdin" :tool-struct write-stdin
            :content `(:execution_id "exec-1" :chars ,chars)
            :mode 'full-auto :one-shot-mutations-p t))))
    (dolist (name '("WriteStdin" "StopExecution"))
      (let ((mevedel-permission-rules `((,name :action deny))))
        (should
         (eq 'deny
             (mevedel-check-permission
              name :tool-struct (mevedel-tool-get name)
              :content '(:execution_id "exec-1" :chars "\C-c")
              :mode 'full-auto :one-shot-mutations-p t))))))
  :doc "registers Eval mode and preserve_ui optional arguments"
  (mevedel-tool-exec--register)
  (let* ((tool (mevedel-tool-get "Eval"))
         (args (gptel-tool-args (mevedel-tool-gptel-tool tool)))
         (mode (seq-find (lambda (arg)
                           (equal "mode" (plist-get arg :name)))
                         args))
         (preserve-ui (seq-find (lambda (arg)
                                  (equal "preserve_ui"
                                         (plist-get arg :name)))
                                args))
         (sandbox (seq-find (lambda (arg)
                              (equal "sandbox_permissions"
                                     (plist-get arg :name)))
                            args))
         (additional (seq-find (lambda (arg)
                                 (equal "additional_permissions"
                                        (plist-get arg :name)))
                               args))
         (filesystem
          (plist-get (plist-get additional :properties) :file_system)))
    (should (equal "string" (plist-get mode :type)))
    (should (equal ["live" "batch"] (plist-get mode :enum)))
    (should (plist-get mode :optional))
    (should (equal "boolean" (plist-get preserve-ui :type)))
    (should (plist-get preserve-ui :optional))
    (should (equal ["use_default" "with_additional_permissions"
                    "require_escalated"]
                   (plist-get sandbox :enum)))
    (should (equal "boolean"
                   (plist-get (plist-get (plist-get additional :properties)
                                         :network)
                              :type)))
    (dolist (access '(:read :write))
      (let ((schema (plist-get (plist-get filesystem :properties) access)))
        (should (equal "array" (plist-get schema :type)))
        (should (equal "string"
                       (plist-get (plist-get schema :items) :type)))))
    (should
     (eq 'invalid-enum
         (plist-get
          (car (mevedel-tool-repair-validate
                tool '(:expression "(+ 1 2)" :mode "bogus")))
          :kind)))))

(mevedel-deftest mevedel-tool-exec--eval ()
  ,test
  (test)
  :doc "errors on missing expression"
  (should-error
   (mevedel-tool-exec--eval #'ignore (list))
   :type 'error)
  :doc "passes approved network authority only to batch Eval"
  (let (captured)
    (cl-letf (((symbol-function 'mevedel-tool-exec--eval-batch)
               (lambda (_callback expression result-format additional
                                  &optional sandbox-permissions)
                 (setq captured (list expression result-format additional
                                      sandbox-permissions)))))
      (mevedel-tool-exec--eval
       #'ignore
       '(:expression "(+ 1 2)"
                     :mode "batch"
                     :sandbox_permissions "with_additional_permissions"
                     :additional_permissions (:network t)
                     :justification "Fetch package metadata?")))
    (should (equal '("(+ 1 2)" nil (:network t) nil) captured)))
  :doc "passes approved full escalation only to batch Eval"
  (let (captured)
    (cl-letf (((symbol-function 'mevedel-tool-exec--eval-batch)
               (lambda (_callback expression _result-format additional
                                  &optional sandbox-permissions)
                 (setq captured (list expression additional
                                      sandbox-permissions)))))
      (mevedel-tool-exec--eval
       #'ignore
       '(:expression "(+ 1 2)"
                     :mode "batch"
                     :sandbox_permissions "require_escalated"
                     :justification "Run batch Eval without confinement?")))
    (should (equal '("(+ 1 2)" nil require-escalated) captured)))
  :doc "evaluates simple expression"
  (let (result)
    (mevedel-tool-exec--eval
     (lambda (r)
       (setq result (mevedel-tool-exec-test--handler-result r)))
     (list :expression "(+ 1 2 3)"))
    (should (string-match-p "Result:\n6" result))
    (should (string-match-p
             "Live Eval ran inside Emacs without child-process confinement"
             result)))
  :doc "captures printed output"
  (let (result)
    (mevedel-tool-exec--eval
     (lambda (r)
       (setq result (mevedel-tool-exec-test--handler-result r)))
     (list :expression "(princ \"hello world\")"))
    (should (string-match-p "STDOUT:\nhello world" result)))
  :doc "passes large live output intact to the shared pipeline"
  (let (result)
    (mevedel-tool-exec--eval
     (lambda (r)
       (setq result (mevedel-tool-exec-test--handler-result r)))
     (list :expression
           "(progn (princ (make-string 550000 ?h)) (princ (make-string 50000 ?t)) 42)"
           :mode "live"))
    (should (string-search (make-string 100 ?t) result))
    (should-not (string-search "Output truncated" result)))
  :doc "reports eval errors"
  (let (result)
    (mevedel-tool-exec--eval
     (lambda (r)
       (setq result (mevedel-tool-exec-test--handler-result r)))
     (list :expression "(error \"test error\")"))
    (should (string-match-p "Error:.*test error" result)))
  :doc "returns string results with %S formatting"
  (let (result)
    (mevedel-tool-exec--eval
     (lambda (r)
       (setq result (mevedel-tool-exec-test--handler-result r)))
     (list :expression "\"hello\""))
    (should (string-match-p "Result:\n\"hello\"" result)))
  :doc "evaluates with the session working directory bound"
  (let* ((root (make-temp-file "mevedel-eval-cwd-" t))
         (module-dir (file-name-concat root "packages" "api"))
         (workspace (mevedel-workspace-get-or-create
                     'file root root "test"))
         (session (mevedel-session-create "main" workspace module-dir))
         (mevedel--session session)
         result)
    (make-directory module-dir t)
    (unwind-protect
        (progn
          (mevedel-tool-exec--eval
           (lambda (r)
             (setq result (mevedel-tool-exec-test--handler-result r)))
           (list :expression "default-directory"))
          (should (string-match-p
                   (regexp-quote
                    (format "Result:\n%S"
                            (file-name-as-directory module-dir)))
                   result)))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "preserves window configuration by default in live mode"
  (let ((original (current-window-configuration))
        result)
    (unwind-protect
        (progn
          (delete-other-windows)
          (split-window-right)
          (should (= 2 (length (window-list))))
          (mevedel-tool-exec--eval
           (lambda (r)
             (setq result (mevedel-tool-exec-test--handler-result r))
             (should (= 2 (length (window-list)))))
           (list :expression "(delete-other-windows)"))
          (should (string-match-p "Result:" result))
          (should (= 2 (length (window-list)))))
      (set-window-configuration original)))
  :doc "allows live mode window changes when preserve_ui is false"
  (let ((original (current-window-configuration))
        result)
    (unwind-protect
        (progn
          (delete-other-windows)
          (split-window-right)
          (should (= 2 (length (window-list))))
          (mevedel-tool-exec--eval
           (lambda (r)
             (setq result (mevedel-tool-exec-test--handler-result r)))
           (list :expression "(delete-other-windows)"
                 :preserve_ui :json-false))
          (should (string-match-p "Result:" result))
          (should (= 1 (length (window-list)))))
      (set-window-configuration original)))
  :doc "evaluates simple expressions in batch mode"
  (let ((original-start
         (symbol-function 'mevedel-execution-start-one-shot))
        (child-starts 0)
        result)
    (cl-letf (((symbol-function 'mevedel-execution-start-one-shot)
               (lambda (&rest args)
                 (cl-incf child-starts)
                 (apply original-start args))))
      (mevedel-tool-exec--eval
       (lambda (r)
         (setq result (mevedel-tool-exec-test--handler-result r)))
       (list :expression "(+ 4 5)" :mode "batch"))
      (while (null result)
        (accept-process-output nil 0.1)))
    (should (= 1 child-starts))
    (should (string-match-p "Result:\n9" result))
    (should-not (string-match-p "without child-process confinement" result)))
  :doc "failed confined batch mode includes explicit retry guidance"
  (let (result)
    (cl-letf (((symbol-function 'mevedel-tool-exec--eval-read-batch-result)
               (lambda (_file)
                 '(:status error :text "Error: batch failure")))
              ((symbol-function 'mevedel-execution-start-one-shot)
               (lambda (callback &rest _args)
                 (funcall
                  callback
                  '(:exit-code 1 :output "" :error nil
                               :sandbox-facts
                               (:sandbox bubblewrap :filesystem workspace-write
                                         :network isolated))))))
      (mevedel-tool-exec--eval
       (lambda (envelope)
         (setq result (mevedel-tool-exec-test--handler-result envelope)))
       (list :expression "(error \"batch failure\")" :mode "batch")))
    (should
     (string-search
      "retry with `with_additional_permissions`"
      result)))
  :doc "pipeline settles both batch launch failure forms as errors"
  (progn
    (require 'mevedel-tools)
    (let* ((workspace (mevedel-workspace--create :root default-directory))
           (mevedel--session
            (mevedel-session--create :name "main" :workspace workspace)))
      (dolist (failure '(child-result start-signal))
        (let ((tool (mevedel-tool--create
                     :name "Eval"
                     :handler #'mevedel-tool-exec--eval
                     :args '((expression string :required "Expression")
                             (mode string :optional "Mode"
                                   :enum ["live" "batch"]))
                     :async-p t
                     :read-only-p t))
              events result)
          (cl-letf (((symbol-function 'mevedel-execution-start-one-shot)
                     (lambda (callback &rest _)
                       (if (eq failure 'child-result)
                           (funcall callback
                                    '(:exit-code nil :output ""
                                                 :error "launch failed"))
                         (error "Launch failed"))))
                    ((symbol-function 'mevedel-hooks-run-event)
                     (lambda (event _payload callback &rest _)
                       (push event events)
                       (funcall callback nil))))
            (mevedel-pipeline-run-tool
             tool (lambda (value) (setq result value))
             '(:expression "(+ 1 2)" :mode "batch")))
          (should
           (string-match-p "Failed to start Eval batch process" result))
          (should
           (equal '(PreToolUse PostToolUseFailure) (nreverse events)))))))
  :doc "live mode does not use the child-process seam"
  (let ((child-starts 0)
        result)
    (cl-letf (((symbol-function 'mevedel-execution-start-one-shot)
               (lambda (&rest _args)
                 (cl-incf child-starts))))
      (mevedel-tool-exec--eval
       (lambda (r)
         (setq result (mevedel-tool-exec-test--handler-result r)))
       (list :expression "(+ 2 3)" :mode "live")))
    (should (= 0 child-starts))
    (should (string-match-p "Result:\n5" result)))
  :doc "batch mode removes its temporary script and result files"
  (let* ((temp-dir (make-temp-file "mevedel-eval-cleanup-" t))
         (temporary-file-directory (file-name-as-directory temp-dir))
         result)
    (unwind-protect
        (progn
          (mevedel-tool-exec--eval
           (lambda (r)
             (setq result (mevedel-tool-exec-test--handler-result r)))
           (list :expression "(error \"cleanup\")" :mode "batch"))
          (with-timeout (5 (error "Timed out"))
            (while (null result)
              (accept-process-output nil 0.1)))
          (should (string-prefix-p "Error:" result))
          (should-not
           (directory-files temp-dir nil directory-files-no-dot-files-regexp)))
      (delete-directory temp-dir t)))
  :doc "captures printed output in batch mode"
  (let (result)
    (mevedel-tool-exec--eval
     (lambda (r)
       (setq result (mevedel-tool-exec-test--handler-result r)))
     (list :expression "(princ \"batch hello\")" :mode "batch"))
    (while (null result)
      (accept-process-output nil 0.1))
    (should (string-match-p "STDOUT:\nbatch hello" result)))
  :doc "batch mode does not mutate parent variables"
  (let (result)
    (makunbound 'mevedel-test-batch-parent-mutation)
    (mevedel-tool-exec--eval
     (lambda (r)
       (setq result (mevedel-tool-exec-test--handler-result r)))
     (list :expression "(setq mevedel-test-batch-parent-mutation 99)"
           :mode "batch"))
    (while (null result)
      (accept-process-output nil 0.1))
    (should (string-match-p "Result:\n99" result))
    (should-not (boundp 'mevedel-test-batch-parent-mutation)))
  :doc "batch mode does not expose bootstrap locals"
  (let (result)
    (mevedel-tool-exec--eval
     (lambda (r)
       (setq result (mevedel-tool-exec-test--handler-result r)))
     (list :expression
           "(list (boundp 'result-file) (boundp 'stdout-buffer) (boundp 'max-output-bytes))"
           :mode "batch"))
    (while (null result)
      (accept-process-output nil 0.1))
    (should (string-match-p "Result:\n(nil nil nil)" result)))
  :doc "batch mode bootstrap locals cannot be corrupted by evaluated code"
  (let (result)
    (mevedel-tool-exec--eval
     (lambda (r)
       (setq result (mevedel-tool-exec-test--handler-result r)))
     (list :expression "(progn (setq result-file nil) 42)"
           :mode "batch"))
    (while (null result)
      (accept-process-output nil 0.1))
    (should (string-match-p "Result:\n42" result)))
  :doc "batch mode uses the session working directory"
  (let* ((root (make-temp-file "mevedel-eval-batch-cwd-" t))
         (module-dir (file-name-concat root "packages" "api"))
         (workspace (mevedel-workspace-get-or-create
                     'file root root "test"))
         (session (mevedel-session-create "main" workspace module-dir))
         (mevedel--session session)
         result)
    (make-directory module-dir t)
    (unwind-protect
        (progn
          (mevedel-tool-exec--eval
           (lambda (r)
             (setq result (mevedel-tool-exec-test--handler-result r)))
           (list :expression "default-directory" :mode "batch"))
          (while (null result)
            (accept-process-output nil 0.1))
          (should (string-match-p
                   (regexp-quote
                    (format "Result:\n%S"
                            (file-name-as-directory module-dir)))
                   result)))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "batch mode belongs to its agent and cleans temporary files on teardown"
  (let* ((root (make-temp-file "mevedel-eval-batch-owner-" t))
         (temporary-file-directory (file-name-as-directory root))
         (workspace (mevedel-workspace-get-or-create
                     'test root root "eval-owner"))
         (session (mevedel-session-create "main" workspace root))
         (mevedel--session session)
         (mevedel--current-request
          (mevedel-request--create
           :origin "/root/agent_eval" :session session))
         (mevedel-sandbox-mode 'off)
         callback-result record)
    (unwind-protect
        (progn
          (mevedel-tool-exec--eval
           (lambda (result) (setq callback-result result))
           (list :expression "(progn (sleep-for 30) 1)" :mode "batch"))
          (with-timeout (2 (error "Batch Eval did not start"))
            (while
                (progn
                  (setq record
                        (car
                         (mevedel-execution--state-record-list
                          (mevedel-session-execution-state session))))
                  (null record))
              (accept-process-output nil 0.02)))
          (should (equal "/root/agent_eval"
                         (mevedel-execution--origin-owner
                          (mevedel-execution--record-origin record))))
          (should (= 1 (mevedel-execution-stop-owner
                        session "/root/agent_eval")))
          (should-not callback-result)
          (should-not
           (directory-files root nil directory-files-no-dot-files-regexp)))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "preserves large batch error output for shared pipeline persistence"
  (let (result)
    (mevedel-tool-exec--eval
     (lambda (r)
       (setq result (mevedel-tool-exec-test--handler-result r)))
     (list :expression
           "(progn (princ (make-string 550000 ?h)) (princ (make-string 50000 ?t)) (error \"boom\"))"
           :mode "batch"))
    (while (null result)
      (accept-process-output nil 0.1))
    (should (string-prefix-p "Error:" result))
    (should (string-search (make-string 100 ?t) result))
    (should-not (string-search "Output truncated" result))))

(mevedel-deftest mevedel-tool-exec--render-bash ()
  ,test
  (test)
  :doc "returns nil for non-string result"
  (should (null (mevedel-tool-exec--render-bash
                 "Bash" '(:command "ls") nil nil)))

  :doc "header shows first command line and expanded body keeps the full command"
  (let* ((body "file1\nfile2\n")
         (plist (mevedel-tool-exec--render-bash
                 "Bash" '(:command "ls -la\n# more") body nil)))
    (should (equal "Bash: ls -la" (plist-get plist :header)))
    (should (equal (concat "$ ls -la\n# more\n\n" body)
                   (plist-get plist :body)))
    (should (eq 'sh-mode (plist-get plist :body-mode))))

  :doc "truncates long headers without truncating the expanded command"
  (let* ((command (concat "git add -- " (make-string 90 ?x)))
         (plist (mevedel-tool-exec--render-bash
                 "Bash" (list :command command) "done\n" nil)))
    (should (string-prefix-p "Bash: git add -- " (plist-get plist :header)))
    (should (string-suffix-p "..." (plist-get plist :header)))
    (should (<= (string-width (string-remove-prefix
                               "Bash: " (plist-get plist :header)))
                60))
    (should (equal (concat "$ " command "\n\ndone\n")
                   (plist-get plist :body))))

  :doc "hides the model-only execution envelope from expanded bodies"
  (let ((plist
         (mevedel-tool-exec--render-bash
          "WriteStdin" nil
          (concat "Hello, Ada\n\n"
                  "<bash-execution execution_id=\"exec-1\" state=\"completed\"/>")
          '(:status success :state completed))))
    (should (equal "Hello, Ada" (plist-get plist :body))))

  :doc "labels polls and input as background-process interactions"
  (let ((poll
         (mevedel-tool-exec--render-bash
          "WriteStdin" '(:execution_id "exec-1")
          "<bash-execution execution_id=\"exec-1\" state=\"completed\"/>"
          '(:status success :state completed :execution-id "exec-1"
                    :execution-control poll :observation-output-p nil)))
        (input
         (mevedel-tool-exec--render-bash
          "WriteStdin" '(:execution_id "exec-1" :chars "yes\n")
          "<bash-execution execution_id=\"exec-1\" state=\"running\"/>"
          '(:status success :state running :execution-id "exec-1"
                    :execution-control input :observation-output-p nil))))
    (should (equal "WriteStdin: polled background process (completed · exec-1)"
                   (plist-get poll :header)))
    (should (equal "WriteStdin:exec-1"
                   (plist-get poll :coalesce-key)))
    (should
     (equal "WriteStdin: sent input to background process (running · exec-1)"
            (plist-get input :header)))
    (should-not (plist-get input :coalesce-key)))

  :doc "coalesces only successful output-free polls"
  (dolist (render-data
           '((:status success :state completed :execution-id "exec-1"
                      :execution-control poll :observation-output-p t)
             (:status error :state completed :execution-id "exec-1"
                      :execution-control poll :observation-output-p nil)
             (:status success :state completed :execution-id "exec-1"
                      :execution-control input :observation-output-p nil)))
    (should-not
     (plist-get
      (mevedel-tool-exec--render-bash
       "WriteStdin" '(:execution_id "exec-1") "result" render-data)
      :coalesce-key)))

  :doc "hides only successful empty polls while execution remains running"
  (let ((hidden
         (mevedel-tool-exec--render-bash
          "WriteStdin" nil
          "<bash-execution execution_id=\"exec-1\" state=\"running\"/>"
          '(:status success :state running
                    :execution-control poll :observation-output-p nil)))
        (terminal
         (mevedel-tool-exec--render-bash
          "WriteStdin" nil
          "<bash-execution execution_id=\"exec-1\" state=\"completed\"/>"
          '(:status success :state completed
                    :execution-control poll :observation-output-p nil)))
        (input
         (mevedel-tool-exec--render-bash
          "WriteStdin" nil
          "<bash-execution execution_id=\"exec-1\" state=\"running\"/>"
          '(:status success :state running
                    :execution-control input :observation-output-p nil))))
    (should (eq t (plist-get hidden :hidden-p)))
    (should-not (plist-get terminal :hidden-p))
    (should-not (plist-get input :hidden-p))))

(provide 'test-mevedel-tool-exec)

;;; test-mevedel-tool-exec.el ends here
