;;; test-niwa-tool-exec.el -- Tests for Bash and Eval execution tools -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for Bash and Eval execution tools.

;;; Code:

(require 'niwa-tool-exec-registration)

(require 'niwa-tool-exec)
(require 'cl-lib)
(require 'seq)
(require 'niwa-agent-control)
(require 'niwa-bash-analysis)
(require 'niwa-structs)
(require 'niwa-tool-registry)
(require 'niwa-tools)
(require 'niwa-execution)
(require 'niwa-execution-telemetry)
(require 'niwa-execution-transcript)
(require 'niwa-execution-target)
(require 'niwa-hooks)
(require 'niwa-pipeline)
(require 'niwa-ptc-driver)
(require 'niwa-sandbox)
(require 'niwa-sleep)
(require 'niwa-session-rewind)
(require 'niwa-session-control-transfer)
(require 'niwa-telemetry)
(require 'niwa-workspace)
(require 'niwa-permission-rules)
(require 'niwa-pending-inputs)
(require 'niwa-view-audit)
(require 'niwa-view-render)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(declare-function gptel-tool-args "gptel" (tool))
(declare-function gptel-tool-function "gptel" (tool))

(defun niwa-tool-exec-test--handler-result (envelope)
  "Return the result from a canonical handler ENVELOPE."
  (should (niwa-pipeline--handler-return-p envelope))
  (plist-get envelope :result))

(defvar niwa-tool-exec-test--acks nil
  "Acknowledgements (SUCCESS ERROR) the last handled event produced.")

(defun niwa-tool-exec-test--handle (event context)
  "Deliver EVENT for CONTEXT and return the handler's result.
A synchronous result must come with exactly one matching acknowledgement."
  (setq niwa-tool-exec-test--acks nil)
  (let ((result (niwa-tool-exec-handle-execution-event
                 event context
                 (lambda (success error)
                   (push (list success error) niwa-tool-exec-test--acks)))))
    (should-not (eq result 'pending))
    (should (= 1 (length niwa-tool-exec-test--acks)))
    (should (eq (and result t) (car (car niwa-tool-exec-test--acks))))
    result))

(defun niwa-tool-exec-test--attempt (event context)
  "Deliver EVENT for CONTEXT; return (RESULT ERROR ACKS) without signalling."
  (setq niwa-tool-exec-test--acks nil)
  (let (result error)
    (condition-case err
        (setq result (niwa-tool-exec-handle-execution-event
                      event context
                      (lambda (success reason)
                        (push (list success reason)
                              niwa-tool-exec-test--acks))))
      (error (setq error err)))
    (list result error (reverse niwa-tool-exec-test--acks))))

(defun niwa-tool-exec-test--call-bash (callback args)
  "Call the Bash handler with CALLBACK and ARGS in a test session."
  (let ((niwa--session
         (or niwa--session
             (niwa-skills-test--make-session "bash"))))
    (niwa-tool-exec--bash callback args)))

(niwa-deftest niwa-tool-exec--bash-yield-time-ms ()
  ,test
  (test)
  :doc "defaults to ten seconds and passes declared-range values through"
  (should (= 10000 (niwa-tool-exec--bash-yield-time-ms nil)))
  (should (= 250 (niwa-tool-exec--bash-yield-time-ms
                  (list :yield_time_ms 250))))
  (should (= 30000 (niwa-tool-exec--bash-yield-time-ms
                    (list :yield_time_ms 30000))))
  (should (= 10000 (niwa-tool-exec--bash-yield-time-ms
                    (list :yield_time_ms nil)))))

(niwa-deftest niwa-tool-exec--write-wait-time-ms ()
  ,test
  (test)
  :doc "uses distinct poll and input defaults"
  (should (= 5000 (niwa-tool-exec--write-wait-time-ms nil "")))
  (should (= 250 (niwa-tool-exec--write-wait-time-ms nil "x")))
  :doc "clamps positive short poll waits to five seconds"
  (dolist (value '(1 250 1000 4999))
    (should (= 5000
               (niwa-tool-exec--write-wait-time-ms
                (list :yield_time_ms value) ""))))
  :doc "accepts the inclusive poll and input bounds"
  (dolist (case '((5000 "") (300000 "") (250 "x") (30000 "x")))
    (should (= (car case)
               (niwa-tool-exec--write-wait-time-ms
                (list :yield_time_ms (car case)) (cadr case)))))
  :doc "clamps to the distinct poll and input ranges without erroring"
  (dolist (case '((-1 . 5000) (0 . 5000) (300001 . 300000)
                  (1.5 . 5000) ("5000" . 5000) ("abc" . 5000)))
    (should (= (cdr case)
               (niwa-tool-exec--write-wait-time-ms
                (list :yield_time_ms (car case)) ""))))
  (should (= 250 (niwa-tool-exec--write-wait-time-ms
                  '(:yield_time_ms 249) "x")))
  (should (= 30000 (niwa-tool-exec--write-wait-time-ms
                    '(:yield_time_ms 30001) "x"))))

(niwa-deftest niwa-tool-exec--execution-artifact-directory ()
  ,test
  (test)
  :doc "places execution artifacts below the session tool-results directory"
  (let* ((target (niwa-execution-target-create default-directory))
         (session (niwa-session--create :execution-target target)))
    (cl-letf (((symbol-function 'niwa-pipeline-tool-results-dir)
               (lambda (_session _buffer _request existing-only-p)
                 (should-not existing-only-p)
                 "/tmp/tool-results")))
      (should (equal "/tmp/tool-results/executions"
                     (niwa-tool-exec--execution-artifact-directory
                      session nil)))))
  :doc "remote sessions use the execution module's local temporary spool"
  (let* ((target (niwa-execution-target-create
                  "/ssh:user@host:/srv/project/"))
         (session (niwa-session--create :execution-target target)))
    (cl-letf (((symbol-function 'niwa-pipeline-tool-results-dir)
               (lambda (&rest _)
                 (ert-fail "Remote spool consulted the target store"))))
      (should-not
       (niwa-tool-exec--execution-artifact-directory session nil))))
  :doc "ephemeral Bash requests do not materialize retained artifacts"
  (let* ((root (make-temp-file "niwa-ephemeral-bash-" t))
         (workspace (niwa-workspace--create
                     :type 'project :id "bash" :root root :name "bash"))
         (session (niwa-session--create
                   :name "side" :workspace workspace))
         (request (niwa-request--create
                   :session session :ephemeral-p t)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--current-request request)
          (should-not
           (niwa-tool-exec--execution-artifact-directory session nil))
          (should-not (niwa-session-save-path session))
          (should-not (file-exists-p
                       (file-name-concat root ".niwa"))))
      (delete-directory root t)))
  :doc "readers use existing storage or temporary spools without metadata writes"
  (let* ((root (make-temp-file "niwa-reader-spool-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace)))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-workspace-ensure-generated-state-ignored)
                   (lambda (&rest _) (ert-fail "Reader wrote Git exclusions")))
                  ((symbol-function 'niwa-session-persistence-shallow-ensure-files)
                   (lambda (&rest _) (ert-fail "Reader initialized storage"))))
          (should-not (niwa-tool-exec--execution-artifact-directory session t))
          (should-not (niwa-session-save-path session))
          (should-not (directory-files root nil "^[^.].*"))
          (setf (niwa-session-save-path session) root)
          (should (equal (file-name-concat root "tool-results" "executions")
                         (niwa-tool-exec--execution-artifact-directory session t)))
          (with-temp-buffer
            (setq-local niwa--current-request
                        (niwa-request--create :session session :ephemeral-p t))
            (should-not
             (niwa-tool-exec--execution-artifact-directory session t))))
      (delete-directory root t))))

(niwa-deftest niwa-tool-exec--execution-facts-xml ()
  ,test
  (test)
  :doc "serializes canonical facts without a chunk id"
  (let ((xml
         (niwa-tool-exec--execution-facts-xml
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

(niwa-deftest niwa-tool-exec-format-execution-metadata ()
  ,test
  (test)
  :doc "formats shared live and terminal row metadata"
  (should
   (equal "running · 2s · 3 lines · 42 bytes · exec-1"
          (niwa-tool-exec-format-execution-metadata
           '(:state running :wall-time-seconds 2.5
                    :output-lines 3 :output-bytes 42 :execution-id "exec-1"))))
  :doc "keeps the working directory in expanded details"
  (should
   (string-match-p
    "cwd /tmp/project"
    (niwa-tool-exec-format-execution-metadata
     '(:state completed :workdir "/tmp/project")))))

(niwa-deftest niwa-tool-exec--observation-envelope ()
  ,test
  (test)
  :doc "keeps output raw while status and facts remain structured"
  (let* ((envelope
          (niwa-tool-exec--observation-envelope
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
    (should (plist-get (plist-get envelope :render-data)
                       :model-result-envelope-p))
    (should (eq 'completed
                (plist-get (plist-get envelope :render-data) :state)))
    (should (= 7 (plist-get (plist-get envelope :render-data)
                            :exit-code))))
  :doc "a process that never started is an error, not a success"
  ;; Status came from facts alone: with no completed state to judge,
  ;; a start failure was reported successful with a "Failed to start
  ;; process" body.
  (let ((envelope
         (niwa-tool-exec--observation-envelope
          '(:error "spawning child process: no such file"
                   :facts (:state failed)))))
    (should (eq 'error (plist-get envelope :status)))
    (should (string-prefix-p "Failed to start process"
                             (plist-get envelope :result))))
  :doc "a collected command failure succeeds but a failed control does not"
  (should
   (eq 'success
       (plist-get
        (niwa-tool-exec--observation-envelope
         '(:facts (:state completed :outcome failure :exit-code 7)) nil t)
        :status)))
  (should
   (eq 'error
       (plist-get
        (niwa-tool-exec--observation-envelope
         '(:error "invalid handle" :facts (:state failed)) nil t)
        :status)))

  :doc "keeps trusted injection output clean while retaining hidden facts"
  (let ((envelope
         (niwa-tool-exec--observation-envelope
          '(:output "injected"
                    :facts (:state completed :termination exited :exit-code 0
                                   :outcome success :wall-time-seconds 0.1
                                   :output-bytes 8 :output-lines 1
                                   :omitted-output-bytes 0 :tty nil))
          t)))
    (should (equal "injected" (plist-get envelope :result)))
    (should (eq 'success (plist-get envelope :status)))
    (should-not (plist-get (plist-get envelope :render-data)
                           :model-result-envelope-p))
    (should (= 0 (plist-get (plist-get envelope :render-data)
                            :exit-code))))
  :doc "adds recovery guidance only to failed confined commands"
  (let* ((guidance
          "This command ran with network/path confinement. If confinement caused the failure, retry with `with_additional_permissions` and request only the required network or exact path capability. Use `require_escalated` only when additive permissions cannot represent the requirement.")
         (failed
          (niwa-tool-exec--observation-envelope
           '(:output "network failed"
                     :facts (:state completed :termination exited :exit-code 1
                                    :outcome failure :wall-time-seconds 0.1
                                    :output-bytes 14 :output-lines 1
                                    :omitted-output-bytes 0 :tty nil)
                     :sandbox-facts
                     (:sandbox bubblewrap :filesystem workspace-write
                               :network isolated))))
         (semantic
          (niwa-tool-exec--observation-envelope
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
          (niwa-tool-exec--observation-envelope
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

(niwa-deftest niwa-tool-exec--bash-outcome ()
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
           (niwa-tool-exec--bash-outcome
            (niwa-bash-analysis-analyze command)
            exit-code termination))))))

(niwa-deftest niwa-tool-exec-completion ()
  ,test
  (test)
  :doc "the full precedence table, over symbols and tag strings alike"
  (dolist (case '(;; 1. stop terminations beat every outcome
                  (stopped nil stopped "stopped")
                  (interrupted failure stopped "stopped")
                  (cancelled success stopped "stopped")
                  (owner-stopped errored stopped "stopped")
                  ;; 2. timeouts, next
                  (timeout nil timed-out "timed out")
                  (timed-out failure timed-out "timed out")
                  (expired success timed-out "timed out")
                  ;; 3. a lost termination or outcome is lost: its end was never seen
                  (lost nil lost "lost")
                  (lost success lost "lost")
                  (nil lost lost "lost")
                  ;; 4. failure outcomes
                  (exited failure failed "failed")
                  (exited errored failed "failed")
                  (signaled failure failed "failed")
                  ;; 5. non-failure Bash outcomes
                  (exited success completed "completed")
                  (exited no-match completed "completed")
                  (exited different completed "completed")
                  (exited false completed "completed")
                  (nil success completed "completed")
                  ;; 6. no outcome, or an unknown one, claims nothing
                  (exited nil ended "ended")
                  (nil nil ended "ended")
                  (flood nil ended "ended")
                  (exited unheard-of ended "ended")
                  (exited interrupted ended "ended")))
    (pcase-let ((`(,termination ,outcome ,status ,verb) case))
      (dolist (form (list (list termination outcome)
                          (list (and termination (symbol-name termination))
                                (and outcome (symbol-name outcome)))))
        (should (equal (cons status verb)
                       (apply #'niwa-tool-exec-completion form))))))
  :doc "tag strings are compared by name and never interned"
  (let ((name (format "never-seen-%d" (random 1000000))))
    (should (equal '(ended . "ended") (niwa-tool-exec-completion name name)))
    (should-not (intern-soft name))))

(niwa-deftest niwa-tool-exec--write-stdin ()
  ,test
  (test)
  :doc "a poll that reads the terminal result cancels its mailbox wake"
  (dolist (state '(running completed))
    (let ((session (niwa-session--create :name "test")))
      (niwa-agent-control-enqueue-execution-result
       session "/root" "done" "exec-1" t)
      (let ((niwa--session session)
            (niwa--current-request nil)
            (niwa--agent-invocation nil))
        (cl-letf (((symbol-function 'niwa-execution-observe)
                   (lambda (&rest args)
                     (funcall (nth 3 args)
                              (list :output "done"
                                    :facts (list :execution-id "exec-1"
                                                 :state state)))))
                  ((symbol-function 'niwa-telemetry-record) #'ignore))
          (niwa-tool-exec--write-stdin #'ignore '(:execution_id "exec-1"))))
      (should (eq (eq state 'running)
                  (niwa-agent-control-wake-pending-p session)))))
  :doc "polls through the captured session and canonical owner"
  (let ((session (niwa-session--create :name "test"))
        captured events input result)
    (let ((niwa--session session)
          (niwa--current-request nil)
          (niwa--agent-invocation nil))
      (setq input (plist-put (list :execution_id "exec-1")
                             :yield_time_ms 30000))
      (cl-letf (((symbol-function 'niwa-execution-observe)
                 (lambda (&rest args)
                   (setq captured args)
                   (funcall
                    (nth 3 args)
                    '(:output "delta"
                              :facts (:execution-id "exec-1" :state running
                                                    :wall-time-seconds 1.0 :output-bytes 5
                                                    :output-lines 1 :omitted-output-bytes 0
                                                    :tty nil)))))
                ((symbol-function 'niwa-telemetry-record)
                 (lambda (_session event &rest props)
                   (push (cons event props) events))))
        (niwa-tool-exec--write-stdin
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
                       :control-succeeded-p))
    (should (plist-get (plist-get result :render-data)
                       :observation-output-p)))
  :doc "retains model-visible failure while marking a successful observation"
  (let ((session (niwa-session--create
                                          :name "failed-command"))
        result)
    (let ((niwa--session session)
          (niwa--agent-invocation nil))
      (cl-letf (((symbol-function 'niwa-execution-observe)
                 (lambda (_session _owner _id callback &rest _)
                   (funcall callback
                            '(:output "failure text"
                                      :facts (:execution-id "exec-1"
                                            :state completed :outcome failure
                                            :exit-code 7)))))
                ((symbol-function 'niwa-telemetry-record) #'ignore))
        (niwa-tool-exec--write-stdin
         (lambda (value) (setq result value))
         '(:execution_id "exec-1" :chars ""))))
    (should (eq 'error (plist-get result :status)))
    (should (string-prefix-p "failure text" (plist-get result :result)))
    (should (plist-get (plist-get result :render-data)
                       :control-succeeded-p))
    (should (plist-get (niwa-tool-exec--render-bash
                        "WriteStdin" '(:chars "")
                        (plist-get result :result)
                        (plist-get result :render-data))
                       :hidden-p)))
  :doc "accepted input collecting a failed command is a successful interaction"
  (let ((session (niwa-session--create
                                          :name "failed-after-input"))
        result)
    (let ((niwa--session session)
          (niwa--agent-invocation nil))
      (cl-letf (((symbol-function 'niwa-execution-observe)
                 (lambda (_session _owner _id callback &rest _)
                   (funcall callback
                            '(:output "failed command"
                                      :facts (:execution-id "exec-1"
                                            :state completed :termination exited
                                            :outcome failure :exit-code 2)
                                      :sandbox-facts
                                      (:sandbox bubblewrap :filesystem workspace-write
                                                :network isolated)))))
                ((symbol-function 'niwa-telemetry-record) #'ignore))
        (niwa-tool-exec--write-stdin
         (lambda (value) (setq result value))
         '(:execution_id "exec-1" :chars "answer\n" :yield_time_ms 1000))))
    (should (string-prefix-p "failed command" (plist-get result :result)))
    (should (string-search niwa-tool-exec--sandbox-recovery-guidance
                           (plist-get result :result)))
    (should (eq 'error (plist-get result :status)))
    (should (plist-get (plist-get result :render-data) :control-succeeded-p))
    (should (eq 'failure (plist-get (plist-get result :render-data) :outcome)))
    (cl-letf (((symbol-function 'niwa-ptc-intern)
               (lambda (_state _symbol) :error))
              ((symbol-function 'niwa-ptc-check-value) #'ignore))
      (pcase-let ((`(,status ,text (:error ,value))
                   (niwa-ptc-driver--classify-outcome nil result)))
        (should (eq 'error status))
        (should (equal text value))))
    (let* ((data (plist-put (copy-sequence (plist-get result :render-data))
                            :status (plist-get result :status)))
           (row (niwa-tool-exec--render-bash
                 "WriteStdin" '(:chars "answer\n")
                 (plist-get result :result) data)))
      (should (eq 'success (plist-get row :status)))
      (should (equal "WriteStdin: sent input" (plist-get row :header)))))
  :doc "side execution observations use the durable audit target"
  (let* ((parent (niwa-session--create :name "parent"))
         (session (niwa-session--create
                   :name "side" :audit-session parent))
         captured)
    (let ((niwa--session session)
          (niwa--current-request nil)
          (niwa--agent-invocation nil))
      (cl-letf (((symbol-function 'niwa-execution-observe)
                 (lambda (_session _owner _execution-id callback &rest _)
                   (funcall
                    callback
                    '(:output ""
                              :facts (:execution-id "exec-1" :state running
                                                    :wall-time-seconds 1.0 :output-bytes 0
                                                    :output-lines 0 :omitted-output-bytes 0
                                                    :tty nil)))))
                ((symbol-function 'niwa-telemetry-record)
                 (lambda (target event &rest props)
                   (setq captured (list target event props)))))
        (niwa-tool-exec--write-stdin
         #'ignore '(:execution_id "exec-1" :chars "private input"))))
    (should (eq parent (car captured)))
    (should (eq 'execution-observe-requested (cadr captured)))
    (should (eq 'btw
                (plist-get (nth 2 captured) :conversation-scope)))
    (should-not
     (string-match-p "private input" (prin1-to-string (nth 2 captured)))))
  :doc "marks input writes separately from empty output polls"
  (let ((session (niwa-session--create :name "test"))
        result)
    (let ((niwa--session session)
          (niwa--current-request nil)
          (niwa--agent-invocation nil))
      (cl-letf (((symbol-function 'niwa-execution-observe)
                 (lambda (_session _owner _execution-id callback &rest _)
                   (funcall
                    callback
                    '(:output ""
                              :facts (:execution-id "exec-1" :state running
                                                    :wall-time-seconds 1.0 :output-bytes 0
                                                    :output-lines 0 :omitted-output-bytes 0
                                                    :tty t)))))
                ((symbol-function 'niwa-telemetry-record) #'ignore))
        (niwa-tool-exec--write-stdin
         (lambda (value) (setq result value))
         '(:execution_id "exec-1" :chars "yes\n"))))
    (should (eq 'input
                (plist-get (plist-get result :render-data)
                           :execution-control)))
    (should-not (plist-get (plist-get result :render-data)
                           :observation-output-p))))

(niwa-deftest niwa-tool-exec--list-executions ()
  ,test
  (test)
  :doc "lists only facts returned by the owner-filtered execution API"
  (let ((session (niwa-session--create :name "test"))
        captured)
    (let ((niwa--session session)
          (niwa--current-request nil)
          (niwa--agent-invocation nil))
      (cl-letf (((symbol-function 'niwa-execution-list)
                 (lambda (&rest args)
                   (setq captured args)
                   '((:execution-id "exec-1" :state running
                                    :wall-time-seconds 1.0 :output-bytes 0
                                    :output-lines 0 :omitted-output-bytes 0 :tty nil)))))
        (let ((envelope (niwa-tool-exec--list-executions nil)))
          (should (string-match-p "execution_id=\"exec-1\""
                                  (plist-get envelope :result)))
          (should-not (plist-member envelope :render-data)))))
    (should (equal (list session "/root") captured)))
  :doc "registered dispatch completes through the full pipeline"
  (let ((session (niwa-session--create :name "test"))
        result)
    (niwa-tool-exec--register)
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-execution-list)
                 (lambda (&rest _)
                   '((:execution-id "exec-1" :state running
                                    :wall-time-seconds 1.0 :output-bytes 0
                                    :output-lines 0 :omitted-output-bytes 0 :tty nil)))))
        (let* ((tool (niwa-tool-get "ListExecutions" "niwa"))
               (fn (gptel-tool-function (niwa-tool-gptel-tool tool))))
          (funcall fn (lambda (value) (setq result value))))))
    (should (string-match-p "execution_id=\"exec-1\"" result))
    (should-not (string-match-p "Error:" result))))

(niwa-deftest niwa-tool-exec--stop-execution ()
  ,test
  (test)
  :doc "stops through the owner-filtered API and reports tool success"
  (let ((session (niwa-session--create :name "test"))
        captured result)
    (let ((niwa--session session)
          (niwa--current-request nil)
          (niwa--agent-invocation nil))
      (cl-letf (((symbol-function 'niwa-execution-stop)
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
        (niwa-tool-exec--stop-execution
         (lambda (value) (setq result value))
         '(:execution_id "exec-1"))))
    (should (equal (list session "/root" "exec-1")
                   (butlast captured)))
    (should (eq 'success (plist-get result :status)))))

(niwa-deftest niwa-tool-exec-handle-execution-event ()
  ,test
  (test)
  :doc "queues unread output and final facts for an independent completion"
  (let ((session (niwa-session--create :name "test")))
    (should
     (niwa-tool-exec-test--handle
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
    (let ((messages (niwa-agent-control-context-mailbox session)))
      (should (= 1 (length messages)))
      (should (eq 'EXECUTION (plist-get (car messages) :type)))
      (should (string-match-p "done" (plist-get (car messages) :payload)))
      (should (string-match-p
               "execution_id=\\\"exec-1\\\""
               (plist-get (car messages) :payload)))))
  :doc "ignores model-claimed terminal events"
  (should-not
   (niwa-tool-exec-test--handle
    '(:type terminal :delivery model) nil))
  :doc "a root command that ended on its own wakes the idle root; stops do not"
  (dolist (case '((exited . t) (signaled . t) (stopped . nil)))
    (let* ((session (niwa-session--create :name "test"))
           (root (generate-new-buffer " *wake-root*"))
           offered)
      (unwind-protect
          (progn
            (niwa-session-set-root-buffer session root)
            (cl-letf (((symbol-function 'niwa-pending-inputs-offer-idle-work)
                       (lambda (buffer) (push buffer offered))))
              (should
               (niwa-tool-exec-test--handle
                (list :type 'terminal :delivery 'mailbox :owner "/root"
                      :tool-args '(:command "make")
                      :facts (list :execution-id "exec-1" :state 'completed
                                   :termination (car case))
                      :observation '(:output "done"))
                session)))
            (let ((message (car (niwa-agent-control-context-mailbox session))))
              (should (equal "exec-1" (plist-get message :execution-id)))
              (should (eq (cdr case) (plist-get message :wake))))
            (should (equal (and (cdr case) (list root)) offered)))
        (kill-buffer root))))
  :doc "a fresh root without saved artifacts wakes through the event's data buffer"
  (dolist (type '(terminal monitor-batch))
    (let ((session (niwa-session--create :name "fresh"))
          offered)
      (with-temp-buffer
        (setq-local niwa--session session)
        (should-not (niwa-session-root-buffer session))
        (cl-letf (((symbol-function 'niwa-pending-inputs-offer-idle-work)
                   (lambda (buffer) (push buffer offered))))
          (should
           (niwa-tool-exec-test--handle
            (list :type type :delivery 'mailbox :owner "/root"
                  :data-buffer (current-buffer)
                  :facts '(:execution-id "exec-1" :termination exited)
                  :monitor-lines '("event") :observation '(:output "done"))
            session)))
        (should (equal (list (current-buffer)) offered))
        (should (niwa-agent-control-wake-pending-p session)))))
  :doc "an agent's completion is queued unchanged and never wakes the root"
  (let ((session (niwa-session--create :name "test"))
        queued offered)
    (cl-letf (((symbol-function 'niwa-agent-runtime-queue-execution-completion)
               (lambda (&rest args)
                 (setq queued args)
                 (funcall (nth 4 args) t nil)
                 t))
              ((symbol-function 'niwa-pending-inputs-offer-idle-work)
               (lambda (buffer) (push buffer offered))))
      (should
       (niwa-tool-exec-test--handle
        (list :type 'terminal :delivery 'mailbox :owner "/root/worker"
              :facts '(:execution-id "exec-1" :termination exited)
              :observation '(:output "done"))
        'invocation)))
    (should (equal '(invocation "/root/worker") (seq-take queued 2)))
    (should (string-prefix-p "done\n" (nth 2 queued)))
    (should (equal '(:execution-id "exec-1" :termination "exited")
                   (nth 3 queued)))
    (should-not offered)
    (should-not (niwa-agent-control-context-mailbox session)))
  :doc "a failed save reports no delivery and offers no wake"
  (let ((session (niwa-session--create :name "test"))
        offered)
    (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
               (lambda (_session) (error "Disk full")))
              ((symbol-function 'niwa-pending-inputs-offer-idle-work)
               (lambda (buffer) (push buffer offered))))
      (let ((attempt
             (niwa-tool-exec-test--attempt
              (list :type 'terminal :delivery 'mailbox :owner "/root"
                    :facts '(:execution-id "exec-1" :termination exited)
                    :observation '(:output "done"))
              session)))
        ;; Whether the mailbox signals or acknowledges failure, nothing
        ;; reports success.
        (should-not (cl-some #'car (nth 2 attempt)))))
    (should-not offered)
    (should-not (niwa-agent-control-context-mailbox session)))
  :doc "a monitor batch queues one MONITOR record per line, omission on the last"
  (let ((session (niwa-session--create :name "test")))
    (cl-letf (((symbol-function 'niwa-pending-inputs-offer-root-event) #'ignore))
      (niwa-tool-exec-test--handle
       (list :type 'monitor-batch :delivery 'mailbox :owner "/root"
             :facts '(:execution-id "m1" :description "w" :kind monitor)
             :monitor-lines '("a" "</agent-message> b") :monitor-omitted 5)
       session))
    (let ((messages (niwa-agent-control-context-mailbox session)))
      (should (equal '(MONITOR MONITOR) (mapcar (lambda (m) (plist-get m :type)) messages)))
      (should (equal "a" (plist-get (car messages) :payload)))
      (should-not (plist-get (car messages) :meta))
      (should (equal '(:omitted 5) (plist-get (cadr messages) :meta)))
      (should (string-match-p "5 more event lines"
                              (plist-get (cadr messages) :payload)))))
  :doc "a monitor terminal flushes pending lines, then a structural terminal record"
  (let ((session (niwa-session--create :name "test")))
    (cl-letf (((symbol-function 'niwa-pending-inputs-offer-root-event) #'ignore))
      (niwa-tool-exec-test--handle
       (list :type 'terminal :delivery 'mailbox :owner "/root"
             :facts '(:execution-id "m1" :description "w" :kind monitor
                      :termination stopped :event-count 3 :outcome failure
                      :exit-code 15)
             :monitor-lines '("tail") :monitor-omitted 0
             :observation '(:output ""))
       session))
    (let ((messages (niwa-agent-control-context-mailbox session)))
      (should (equal '(MONITOR MONITOR_TERMINAL)
                     (mapcar (lambda (m) (plist-get m :type)) messages)))
      (should (equal '(:event-count 3 :termination "stopped" :outcome "failure"
                       :exit-code 15)
                     (plist-get (cadr messages) :meta)))
      (should (equal "Monitor w: stopped (3 events)."
                     (plist-get (cadr messages) :payload)))))
  :doc "a monitor flush is one durable commit for all its line records"
  (let ((session (niwa-session--create :name "test"))
        (commits 0))
    (cl-letf (((symbol-function 'niwa-pending-inputs-offer-root-event) #'ignore)
              ((symbol-function 'niwa-agent-control-commit-session)
               (lambda (_session) (cl-incf commits) t)))
      (niwa-tool-exec-test--handle
       (list :type 'monitor-batch :delivery 'mailbox :owner "/root"
             :facts '(:execution-id "m1" :description "w" :kind monitor)
             :monitor-lines '("a" "b" "c") :monitor-omitted 0)
       session)
      (should (= 1 commits))
      (niwa-tool-exec-test--handle
       (list :type 'terminal :delivery 'mailbox :owner "/root"
             :facts '(:execution-id "m1" :description "w" :kind monitor
                      :termination exited :event-count 4)
             :monitor-lines '("d") :monitor-omitted 0
             :observation '(:output ""))
       session)
      (should (= 2 commits)))
    (should (equal '(MONITOR MONITOR MONITOR MONITOR MONITOR_TERMINAL)
                   (mapcar (lambda (m) (plist-get m :type))
                           (niwa-agent-control-context-mailbox session)))))
  :doc "a failed monitor commit queues nothing and a retry does not duplicate"
  (dolist (kind '(monitor-batch terminal))
    (let ((session (niwa-session--create :name "test"))
          (fail t) (attempts 0) err
          (unrelated (list :type 'MAIL :sender "/root/a" :recipient "/root"
                           :payload "unrelated" :timestamp '(1 0 0 0)))
          (event (if (eq kind 'terminal)
                     (list :type 'terminal :delivery 'mailbox :owner "/root"
                           :facts '(:execution-id "m1" :description "w"
                                    :kind monitor :termination exited
                                    :event-count 3)
                           :monitor-lines '("a" "b") :monitor-omitted 0
                           :observation '(:output ""))
                   (list :type 'monitor-batch :delivery 'mailbox :owner "/root"
                         :facts '(:execution-id "m1" :description "w"
                                  :kind monitor)
                         :monitor-lines '("a" "b" "c") :monitor-omitted 0))))
      (niwa-session--set-messages session (list unrelated))
      (cl-letf (((symbol-function 'niwa-pending-inputs-offer-root-event) #'ignore)
                ((symbol-function 'niwa-agent-control-commit-session)
                 (lambda (_session)
                   (cl-incf attempts)
                   (when fail (error "Injected commit failure")) t)))
        (setq err (niwa-tool-exec-test--attempt event session))
        ;; The injected failure, not some earlier error, reached us, and
        ;; nothing reports success.
        (should-not (cl-some #'car (nth 2 err)))
        (should (or (equal '(error "Injected commit failure") (nth 1 err))
                    (equal '(nil) (mapcar #'car (nth 2 err)))))
        (should (= 1 attempts))
        (should (equal (list unrelated)
                       (niwa-agent-control-context-mailbox session)))
        (setq fail nil)
        (niwa-tool-exec-test--handle event session)
        (should (= 2 attempts)))
      (let ((messages (niwa-agent-control-context-mailbox session)))
        (should (equal (if (eq kind 'terminal)
                           '(MAIL MONITOR MONITOR MONITOR_TERMINAL)
                         '(MAIL MONITOR MONITOR MONITOR))
                       (mapcar (lambda (m) (plist-get m :type)) messages)))
        (should (equal (if (eq kind 'terminal)
                           (list "unrelated" "a" "b"
                                 (niwa-tool-exec--monitor-terminal-body event))
                         '("unrelated" "a" "b" "c"))
                       (mapcar (lambda (m) (plist-get m :payload)) messages))))))
  :doc "an execution result carries its terminal facts as metadata"
  (let ((session (niwa-session--create :name "test")))
    (niwa-tool-exec-test--handle
     (list :type 'terminal :delivery 'mailbox :owner "/root"
           :tool-args '(:command "make")
           :facts '(:execution-id "e1" :description "build" :command "make"
                    :state completed :termination exited :exit-code 2
                    :outcome failure :output-path "artifact://executions/e1.log"
                    :output-bytes 42 :omitted-output-bytes 3)
           :observation '(:output "boom" :facts (:execution-id "e1" :state completed)))
     session)
    (should (equal '(:execution-id "e1" :description "build" :command "make"
                     :termination "exited" :outcome "failure" :exit-code 2
                     :output-path "artifact://executions/e1.log"
                     :output-bytes 42 :omitted-output-bytes 3)
                   (plist-get (car (niwa-agent-control-context-mailbox session))
                              :meta))))
  :doc "a failure while offering the wake keeps the delivered waking mail"
  (let* ((session (niwa-session--create :name "test"))
         (root (generate-new-buffer " *wake-root*")))
    (unwind-protect
        (progn
          (niwa-session-set-root-buffer session root)
          (cl-letf (((symbol-function 'niwa-pending-inputs-offer-idle-work)
                     (lambda (_buffer) (error "No timers"))))
            (niwa-test--with-captured-diagnostics nil
              (should
               (niwa-tool-exec-test--handle
                (list :type 'terminal :delivery 'mailbox :owner "/root"
                      :facts '(:execution-id "exec-1" :termination exited)
                      :observation '(:output "done"))
                session))))
          (should (niwa-agent-control-wake-pending-p session)))
      (kill-buffer root))))

(niwa-deftest niwa-tool-exec--bash
  (:quiet t)
  ,test
  (test)
  :doc "errors on missing command"
  (should-error
   (niwa-tool-exec-test--call-bash #'ignore (list))
   :type 'error)
  :doc "rejects shell-native backgrounding"
  (should-error
   (niwa-tool-exec-test--call-bash #'ignore '(:command "sleep 1 &"))
   :type 'error)
  :doc "launches literal heredoc ampersands without granting read-only authority"
  (let* ((source "cat <<'EOF'\ntext & more\n$(sleep 1 &)\nEOF")
         captured)
    (cl-letf (((symbol-function 'niwa-execution-start-bash)
               (lambda (&rest args) (setq captured (cdr args)))))
      (niwa-tool-exec-test--call-bash #'ignore (list :command source)))
    (should captured)
    (should-not (plist-get captured :read-only-p))
    (should (equal '("bash" "-lc") (butlast (plist-get captured :command))))
    (with-temp-buffer
      (should (zerop (apply #'process-file (car (plist-get captured :command))
                           nil t nil (cdr (plist-get captured :command)))))
      (should (string-prefix-p "text & more\n$(sleep 1 &)\n" (buffer-string)))))
  :doc "still rejects executable backgrounding within expanding heredocs"
  (should-error
   (niwa-tool-exec-test--call-bash
    #'ignore '(:command "cat <<EOF\n$(sleep 1 &)\nEOF"))
   :type 'error)
  :doc "forwards the yield the repair pipeline already range-checked"
  (let (captured)
    (cl-letf (((symbol-function 'niwa-execution-start-bash)
               (lambda (&rest args) (setq captured args))))
      (niwa-tool-exec-test--call-bash
       #'ignore (list :command "sleep 1" :yield_time_ms 250)))
    (should (= 250 (plist-get (cdr captured) :yield-time-ms))))
  :doc "trusted internal waits disable yielding"
  (let (captured)
    (cl-letf (((symbol-function 'niwa-execution-start-bash)
               (lambda (&rest args) (setq captured args))))
      (niwa-tool-exec-test--call-bash
       #'ignore '(:command "printf done" :wait-for-completion-p t)))
    (should-not (plist-get (cdr captured) :yield-time-ms)))
  :doc "forwards the Bash description to the execution"
  (let (captured)
    (cl-letf (((symbol-function 'niwa-execution-start-bash)
               (lambda (&rest args) (setq captured (cdr args)))))
      (niwa-tool-exec-test--call-bash
       #'ignore '(:command "make" :description "Build it")))
    (should (equal "Build it" (plist-get captured :description)))
    (should (eq 'bash (plist-get captured :kind))))
  :doc "forwards original arguments and durable tool-use identity"
  (let ((niwa-pipeline--active-tool-use-id "call-bash-7")
        captured)
    (cl-letf (((symbol-function 'niwa-execution-start-bash)
               (lambda (&rest keys) (setq captured keys))))
      (niwa-tool-exec-test--call-bash
       #'ignore '(:command "printf identity" :yield_time_ms 250)))
    (should (equal "call-bash-7"
                   (plist-get (cdr captured) :tool-use-id)))
    (should (equal '(:command "printf identity" :yield_time_ms 250)
                   (plist-get (cdr captured) :tool-args))))
  :doc "keeps authored facts while launching hardened read-only commands"
  (let* ((source "git status --short && git diff --stat")
         (args (list :command source))
         captured)
    (cl-letf (((symbol-function 'niwa-execution-start-bash)
               (lambda (&rest keys) (setq captured (cdr keys)))))
      (niwa-tool-exec-test--call-bash #'ignore args))
    (should (equal args (plist-get captured :tool-args)))
    (should (plist-get captured :read-only-p))
    (should (equal '("bash" "--noprofile" "--norc" "-p" "-c")
                   (butlast (plist-get captured :command))))
    (should-not (equal source (car (last (plist-get captured :command)))))
    (should (string-match-p "--no-optional-locks"
                            (car (last (plist-get captured :command))))))
  :doc "standalone assignments launch intact rather than failing read-only preparation"
  (progn
    (skip-unless (treesit-language-available-p 'bash))
    (let* ((source "IFS=,; false | cat") captured)
      (cl-letf (((symbol-function 'niwa-execution-start-bash)
                 (lambda (&rest keys) (setq captured (cdr keys)))))
        (niwa-tool-exec-test--call-bash #'ignore (list :command source)))
      (should captured)
      (should-not (plist-get captured :read-only-p))
      (should (equal '("bash" "-lc") (butlast (plist-get captured :command))))
      (should (string-prefix-p source (car (last (plist-get captured :command)))))
      (with-temp-buffer
        (should (zerop (apply #'process-file (car (plist-get captured :command))
                             nil t nil (cdr (plist-get captured :command))))))
      ;; Removing the assignment restores the ordinary read-only launch path.
      (cl-letf (((symbol-function 'niwa-execution-start-bash)
                 (lambda (&rest keys) (setq captured (cdr keys)))))
        (niwa-tool-exec-test--call-bash #'ignore '(:command "false | cat")))
      (should (plist-get captured :read-only-p))))
  :doc "launches a default call with its matching remembered profile"
  (let* ((session
          (niwa-session--create
           :name "profile"
           :permission-rules
           '(("Bash" :pattern "npx test" :network t :action allow))))
         (niwa--session session)
         captured)
    (cl-letf (((symbol-function 'niwa-execution-start-bash)
               (lambda (&rest args) (setq captured args))))
      (niwa-tool-exec--bash #'ignore '(:command "npx test")))
    (should
     (equal '(:network t)
            (plist-get (cdr captured) :additional-permissions))))
  :doc "forwards only proven read-only analysis to scheduler admission"
  (let (read-only unknown)
    (cl-letf (((symbol-function 'niwa-execution-start-bash)
               (lambda (&rest args)
                 (if read-only
                     (setq unknown args)
                   (setq read-only args)))))
      (niwa-tool-exec-test--call-bash #'ignore '(:command "pwd"))
      (niwa-tool-exec-test--call-bash
       #'ignore '(:command "touch scheduler-test")))
    (should (eq t (plist-get (cdr read-only) :read-only-p)))
    (should-not (plist-get (cdr unknown) :read-only-p)))
  :doc "shares one special outcome across handler status, XML, and render data"
  (let (envelope)
    (cl-letf (((symbol-function 'niwa-execution-start-bash)
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
      (niwa-tool-exec-test--call-bash
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
    (cl-letf (((symbol-function 'niwa-execution-start-bash)
               (lambda (&rest args) (setq captured args))))
      (niwa-tool-exec-test--call-bash
       #'ignore '(:command "printf prompt" :tty t)))
    (should (eq t (plist-get (cdr captured) :tty)))
    (should-not (plist-get (cdr captured) :additional-permissions))
    (should-not (plist-get (cdr captured) :sandbox-permissions)))
  :doc "normalizes JSON false PTY mode and rejects non-booleans"
  (let (captured)
    (cl-letf (((symbol-function 'niwa-execution-start-bash)
               (lambda (&rest args) (setq captured args))))
      (niwa-tool-exec-test--call-bash
       #'ignore '(:command "printf plain" :tty :json-false)))
    (should-not (plist-get (cdr captured) :tty))
    (should-error
     (niwa-tool-exec-test--call-bash
      #'ignore '(:command "printf invalid" :tty "yes"))
     :type 'error))
  :doc "passes approved network authority to the child launcher"
  (let (captured result)
    (cl-letf (((symbol-function
                'niwa-execution-start-bash)
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
      (niwa-tool-exec-test--call-bash
       (lambda (envelope)
         (setq result (niwa-tool-exec-test--handler-result envelope)))
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
                'niwa-execution-start-bash)
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
      (niwa-tool-exec-test--call-bash
       (lambda (envelope)
         (setq result (niwa-tool-exec-test--handler-result envelope)))
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
                'niwa-execution-start-bash)
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
      (niwa-tool-exec-test--call-bash
       (lambda (envelope)
         (setq result (niwa-tool-exec-test--handler-result envelope)))
       '(:command "noisy-command")))
    (should (string-prefix-p (make-string 100 ?h) result))
    (should (string-search (make-string 100 ?t) result))
    (should-not (string-search "Output truncated" result)))
  :doc "executes simple command and returns output"
  (let ((result nil)
        (done nil))
    (niwa-tool-exec-test--call-bash
     (lambda (r)
       (setq result (niwa-tool-exec-test--handler-result r)
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
    (niwa-tool-exec-test--call-bash
     (lambda (r)
       (setq result (niwa-tool-exec-test--handler-result r)
             done t))
     (list :command "exit 42"))
    (with-timeout (5 (error "Timed out"))
      (while (not done)
        (accept-process-output nil 0.1)))
    (should (string-match-p "exit_code=\"42\"" result))
    (should-not (string-match-p "Command failed" result)))
  :doc "discloses automatic unrestricted fallback"
  (let ((niwa-sandbox-mode 'best-effort)
        (niwa-sandbox--probe-cache
         '((nil . (:available nil
                              :reason "test confinement unavailable"))))
        result done)
    (niwa-tool-exec-test--call-bash
     (lambda (r)
       (setq result (niwa-tool-exec-test--handler-result r)
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
  :doc "read-only launch ignores startup files and inherited shell functions"
  (let* ((home (make-temp-file "niwa-bash-reading-" t))
         (startup (file-name-concat home "startup"))
         (marker (file-name-concat home "helper-ran"))
         (process-environment (copy-sequence process-environment))
         result done)
    (unwind-protect
        (progn
          (with-temp-file startup
            (insert "touch " (shell-quote-argument marker) "\n"))
          (copy-file startup (file-name-concat home ".bash_profile"))
          (setenv "HOME" home)
          (setenv "BASH_ENV" startup)
          (setenv "BASH_FUNC_echo%%"
                  (concat "() { touch " (shell-quote-argument marker) "; }"))
          (niwa-tool-exec-test--call-bash
           (lambda (r)
             (setq result (niwa-tool-exec-test--handler-result r) done t))
           '(:command "echo inspected"))
          (with-timeout (5 (error "Timed out"))
            (while (not done) (accept-process-output nil 0.1)))
          (should (string-prefix-p "inspected\n" result))
          (should-not (file-exists-p marker)))
      (delete-directory home t)))
  :doc "loads Bash login initialization from an isolated home"
  (let* ((home (make-temp-file "niwa-bash-login-" t))
         (profile (file-name-concat home ".bash_profile"))
         (process-environment (copy-sequence process-environment))
         result done)
    (unwind-protect
        (progn
          (with-temp-file profile
            (insert "export NIWA_LOGIN_MARKER=loaded\n"))
          (setenv "HOME" home)
          (niwa-tool-exec-test--call-bash
           (lambda (r)
             (setq result (niwa-tool-exec-test--handler-result r)
                   done t))
           (list :command "printf %s \"$NIWA_LOGIN_MARKER\""))
          (with-timeout (5 (error "Timed out"))
            (while (not done)
              (accept-process-output nil 0.1)))
          (should (string-prefix-p "loaded\n" result))
          (should (string-match-p "\n\\[sandbox: " result)))
      (delete-directory home t)))
  :doc "runs from the session working directory when current buffer is elsewhere"
  (let* ((root (make-temp-file "niwa-bash-cwd-" t))
         (module-dir (file-name-concat root "packages" "api"))
         (agent-dir (file-name-concat root ".niwa" "sessions"
                                      "main" "agents"))
         (workspace (niwa-workspace-get-or-create
                     'file root root "test"))
         (session (niwa-session-create "main" workspace module-dir))
         (niwa--session session)
         (default-directory (file-name-as-directory agent-dir))
         result done)
    (make-directory module-dir t)
    (make-directory agent-dir t)
    (unwind-protect
        (progn
          (niwa-tool-exec-test--call-bash
           (lambda (r)
             (setq result (niwa-tool-exec-test--handler-result r)
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
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-tool-exec--sandbox-writable-roots ()
  ,test
  (test)
  :doc "sandbox root fallback:
`niwa-tool-exec--sandbox-writable-roots' includes work and temp directories"
  (let ((roots
         (niwa-tool-exec--sandbox-writable-roots default-directory)))
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
         (target (niwa-execution-target-create workdir))
         (workspace (niwa-workspace--create :root workdir))
         (session (niwa-session--create
                   :workspace workspace
                   :execution-target target
                   :working-directory workdir))
         (temporary-file-directory "/client/private/tmp/"))
    (setf (niwa-execution-target-environment target)
          '(("HOME" . "/home/user") ("TMPDIR" . "/var/tmp/niwa")))
    (with-temp-buffer
      (setq-local niwa--session session)
      (let ((roots (niwa-tool-exec--sandbox-writable-roots workdir)))
        (should (member "/ssh:user@host:/var/tmp/niwa/" roots))
        (should-not (member "/client/private/tmp/" roots))
        (should (cl-every #'file-remote-p roots))))))

(niwa-deftest niwa-tool-exec--sandbox-disclosure ()
  ,test
  (test)
  :doc "normal disclosure:
`niwa-tool-exec--sandbox-disclosure' appends active execution boundaries"
  (should
   (equal
    (niwa-tool-exec--sandbox-disclosure
     "ok" '(:sandbox-facts
            (:sandbox bubblewrap :filesystem workspace-write :network isolated)))
    "ok\n\n[sandbox: bubblewrap; filesystem: workspace-write; network: isolated]"))
  :doc "system SSH ownership failure: explains namespace mapping and an approved fresh retry"
  (dolist (path '("/etc/ssh/ssh_config"
                  "/etc/ssh/ssh_config.d/20-systemd-ssh-proxy.conf"))
    (let* ((output (format "Bad owner or permissions on %s\n" path))
           (text (niwa-tool-exec--sandbox-disclosure
                  output '(:sandbox-facts (:sandbox bubblewrap)) nil t)))
      (should (string-prefix-p output text))
      (should (string-search "overflow UID (often 65534)" text))
      (should (string-search "does not prove a namespace cause" text))
      (should (string-search "`ssh -G`" text))
      (should (string-search "grants do not change UID mapping" text))
      (should (string-search "fetch-only transport" text))
      (should (string-search "`require_escalated`" text))
      (should (string-search "invocation-only approval" text))
      (should (string-search "no retry is automatic" text))
      (should (string-search "Keep SSH ownership" text))
      (should-not (string-search niwa-tool-exec--sandbox-recovery-guidance text))))
  :doc "system SSH ownership hint: stays scoped to a failed confined system-config diagnostic"
  (dolist (case '(("Bad owner or permissions on /home/user/.ssh/config\n" bubblewrap t nil)
                  ("Bad owner or permissions on /etc/ssh/ssh_config-other\n" bubblewrap t nil)
                  ("quoted: Bad owner or permissions on /etc/ssh/ssh_config\n" bubblewrap t nil)
                  ("Permission denied (publickey).\n" bubblewrap t nil)
                  ("Bad owner or permissions on /etc/ssh/ssh_config\n" escalated t nil)
                  ("Bad owner or permissions on /etc/ssh/ssh_config\n" bubblewrap nil nil)
                  ("Bad owner or permissions on /etc/ssh/ssh_config\n" bubblewrap t t)))
    (pcase-let ((`(,output ,sandbox ,failed ,suppressed) case))
      (let ((text (niwa-tool-exec--sandbox-disclosure
                   output `(:sandbox-facts (:sandbox ,sandbox)) suppressed failed)))
        (should (string-prefix-p output text))
        (should-not (string-search "overflow UID" text)))))
  :doc "hidden session bus failure: names keyring tools and an approved fresh escalation"
  (let* ((output (concat "could not read the token: failed to read \"token\" "
                         "from the operating system keyring: dial unix "
                         "/run/user/1000/bus: connect: no such file or directory\n"))
         (text (niwa-tool-exec--sandbox-disclosure
                output '(:sandbox-facts (:sandbox bubblewrap)) nil t)))
    (should (string-prefix-p output text))
    (should (string-search "session D-Bus is hidden" text))
    (should (string-search "`glab`" text))
    (should (string-search "`/run/user` are refused" text))
    (should (string-search "`require_escalated`" text))
    (should (string-search "no retry is automatic" text))
    (should-not (string-search niwa-tool-exec--sandbox-recovery-guidance text)))
  :doc "hidden session bus hint: stays scoped to a failed confined bus diagnostic"
  (dolist (case '(("dial unix /run/user/1000/bus: connect\n" escalated t nil)
                  ("dial unix /run/user/1000/bus: connect\n" bubblewrap nil nil)
                  ("dial unix /run/user/1000/bus: connect\n" bubblewrap t t)
                  ("dial unix /run/user/1000/bus-proxy: connect\n" bubblewrap t nil)
                  ("dial unix /run/user/1000/pulse/native: connect\n" bubblewrap t nil)))
    (pcase-let ((`(,output ,sandbox ,failed ,suppressed) case))
      (let ((text (niwa-tool-exec--sandbox-disclosure
                   output `(:sandbox-facts (:sandbox ,sandbox)) suppressed failed)))
        (should (string-prefix-p output text))
        (should-not (string-search "session D-Bus is hidden" text)))))
  :doc "authority preparation refusal:
invalid grants retain the specific reason and do not prescribe full escalation"
  (let ((text
         (niwa-tool-exec--sandbox-disclosure
          "Exact directory access cannot be confined: /example/cache"
          '(:sandbox-facts (:sandbox refused :refused t :filesystem none
                                    :network none
                                    :reason "Exact directory access cannot be confined: /example/cache"))
          nil t)))
    (should (string-match-p "Exact directory access cannot be confined" text))
    (should-not (string-match-p "unavailable" text))
    (should-not (string-match-p "require_escalated" text)))
  :doc "first fallback disclosure:
`niwa-tool-exec--sandbox-disclosure' includes one model-visible note"
  (should
   (string-search
    "Confinement was unavailable, so this invocation ran directly."
    (niwa-tool-exec--sandbox-disclosure
     "ok" '(:sandbox-facts
            (:sandbox unavailable :filesystem unrestricted
                      :network unrestricted :first-direct-fallback t)))))
  :doc "suppressed unrestricted disclosure:
`niwa-tool-exec--sandbox-disclosure' warns without contaminating substitution"
  (let (warning)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional _level _buffer-name)
                 (setq warning message))))
      (should
       (equal
        (niwa-tool-exec--sandbox-disclosure
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
        (niwa-tool-exec--sandbox-disclosure
         "literal"
         '(:sandbox-facts
           (:sandbox unavailable :filesystem unrestricted
                     :network unrestricted :first-direct-fallback t))
         t)))
      (should-not warning))))

(niwa-deftest niwa-tool-exec--register
  (:before-each (niwa-tool-exec--register))
  ,test
  (test)
  :doc "registers the shared execution escalation vocabulary for Bash"
  (let* ((tool (niwa-tool-get "Bash"))
         (args (gptel-tool-args (niwa-tool-gptel-tool tool)))
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
  (let* ((bash (niwa-tool-get "Bash"))
         (args (gptel-tool-args (niwa-tool-gptel-tool bash)))
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
      (should (niwa-tool-get name))))
  :doc "registered WriteStdin dispatch preserves the underscored schema key"
  (let* ((tool (niwa-tool-get "WriteStdin" "niwa"))
         (fn (gptel-tool-function (niwa-tool-gptel-tool tool)))
         captured)
    (should (eq 'yield_time_ms
                (car (nth 2 (niwa-tool-args tool)))))
    (cl-letf (((symbol-function 'niwa-pipeline-run-tool)
               (lambda (_tool _callback args)
                 (setq captured args))))
      (funcall fn #'ignore "exec-1" "" 12345))
    (should (equal "exec-1" (plist-get captured :execution_id)))
    (should (equal ":yield_time_ms" (symbol-name (nth 4 captured))))
    (should (= 12345 (nth 5 captured)))
    (should-not (plist-member captured :yield-time-ms)))
  :doc "execution control inherits authority without becoming read-only"
  (dolist (name '("WriteStdin" "StopExecution"))
    (let ((tool (niwa-tool-get name)))
      (should-not (niwa-tool-read-only-p tool))
      (dolist (mode '(ask edits full-auto))
        (should
         (eq 'allow
             (niwa-check-permission
              name :tool-struct tool :content nil :mode mode))))
      (let ((niwa-permission-rules `((,name :action deny))))
        (should
         (eq 'deny
             (niwa-check-permission
              name :tool-struct tool :content nil :mode 'full-auto))))))
  :doc "one-shot execution control asks for PTY input but not containment"
  (let ((write-stdin (niwa-tool-get "WriteStdin"))
        (stop (niwa-tool-get "StopExecution"))
        (niwa-permission-rules nil)
        (niwa-protected-paths nil))
    (should
     (eq 'ask
         (niwa-check-permission
          "WriteStdin" :tool-struct write-stdin
          :content '(:execution_id "exec-1" :chars "yes\n")
          :mode 'edits :one-shot-mutations-p t)))
    (should
     (eq 'allow
         (niwa-check-permission
          "StopExecution" :tool-struct stop
          :content '(:execution_id "exec-1")
          :mode 'edits :one-shot-mutations-p t)))
    (dolist (chars (list nil "" "\C-c"))
      (should
       (eq 'allow
           (niwa-check-permission
            "WriteStdin" :tool-struct write-stdin
            :content `(:execution_id "exec-1" :chars ,chars)
            :mode 'edits :one-shot-mutations-p t))))
    (dolist (name '("WriteStdin" "StopExecution"))
      (let ((niwa-permission-rules `((,name :action deny))))
        (should
         (eq 'deny
             (niwa-check-permission
              name :tool-struct (niwa-tool-get name)
              :content '(:execution_id "exec-1" :chars "\C-c")
              :mode 'edits :one-shot-mutations-p t))))))
  :doc "registers Eval mode and preserve_ui optional arguments"
  (let* ((tool (niwa-tool-get "Eval"))
         (args (gptel-tool-args (niwa-tool-gptel-tool tool)))
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
    (should (equal ["live" "fresh" "sandbox"] (plist-get mode :enum)))
    (should (plist-get mode :optional))
    (let ((yield (seq-find (lambda (arg) (equal "yield_time_ms" (plist-get arg :name))) args)))
      (should (equal "integer" (plist-get yield :type)))
      (should (= 250 (plist-get yield :minimum)))
      (should (= 30000 (plist-get yield :maximum)))
      (should (plist-get yield :optional)))
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
          (car (niwa-tool-repair-validate
                tool '(:expression "(+ 1 2)" :mode "bogus")))
          :kind)))))

(defun niwa-tool-exec-test--sandbox-call (expression &rest args)
  "Run EXPRESSION with sandbox tool ARGS and await its result text."
  (let (reply)
    (funcall (niwa-tool-handler (niwa-tool-get "Eval"))
             (lambda (value) (setq reply value))
             (append (list :mode "sandbox" :expression expression) args))
    (with-timeout (10 (error "Sandbox Eval timed out"))
      (while (not reply) (accept-process-output nil 0.02)))
    (plist-get reply :result)))

(niwa-deftest niwa-tool-exec--eval-sandbox
  (:vars* ((root (make-temp-file "sandbox-tool-" t))
           (default-directory (file-name-as-directory root))
           (niwa--session (niwa-session--create
                             :name "sandbox" :working-directory default-directory
                             :workspace (niwa-workspace--create :root default-directory)))
           (niwa-eval-sandbox--sessions (make-hash-table :test #'eq))
           (niwa-eval-sandbox-idle-timeout nil))
   :before-each (niwa-tool-exec--register)
   :after-each (progn (niwa-execution-teardown-session niwa--session)
                      (delete-directory root t)))
  (progn
    (unless (plist-get (niwa-sandbox-probe) :available) (ert-skip "No confinement backend"))
    ,test)
  (test)
  :doc "registered Eval persists definitions, resets and isolates agent owners"
  (progn
    (should (string-search "Sandbox started; features:"
                           (niwa-tool-exec-test--sandbox-call "(defun sandbox-answer () 42)")))
    (let ((text (niwa-tool-exec-test--sandbox-call "(sandbox-answer)")))
      (should (string-search "Result:\n42" text))
      (should (string-search "Sandbox reused; features:" text)))
    (let ((niwa--current-request
           (niwa-request--create :session niwa--session :origin "/root/agent")))
      (should (string-search "Result:\nnil"
                             (niwa-tool-exec-test--sandbox-call "(fboundp 'sandbox-answer)"))))
    (let ((text (niwa-tool-exec-test--sandbox-call "(fboundp 'sandbox-answer)" :reset t)))
      (should (string-search "Result:\nnil" text))
      (should (string-search "Earlier definitions are gone" text))))
  :doc "captures child output, messages and bounded signal-time backtraces"
  (let ((text (niwa-tool-exec-test--sandbox-call
               "(progn (princ \"hello\") (message \"note\") (error \"boom\"))")))
    (dolist (section '("STDOUT:\nhello" "MESSAGES:\nnote" "BACKTRACE:" "boom"))
      (should (string-search section text))))
  :doc "capture locals do not collide with evaluated definitions"
  (should (string-search "Result:\n42"
                         (niwa-tool-exec-test--sandbox-call
                          "(progn (defvar stdout 42) (defvar messages nil) stdout)")))
  :doc "large values and output are truncated in the child, not protocol failures"
  (let ((text (niwa-tool-exec-test--sandbox-call
               "(progn (princ (make-string 1100000 ?x)) (make-string 1100000 ?y))")))
    (should (string-prefix-p "Result:" text))
    (should (string-search "[Output truncated]" text))
    (should (< (length text) 33000)))
  :doc "busy calls refuse and owning request cancellation kills the service"
  (progn
    (niwa-tool-exec-test--sandbox-call "t")
    (let ((niwa--current-request
           (niwa-request--create :session niwa--session :origin "/root")) reply)
      (niwa-tool-exec--eval (lambda (value) (setq reply value))
                              '(:mode "sandbox" :expression "(sleep-for 30)"))
      (should (string-search "busy" (error-message-string
                                     (should-error (niwa-tool-exec-test--sandbox-call "t")))))
      (niwa-request-cancel niwa--current-request)
      (should (string-search "Earlier definitions are gone" (plist-get reply :result)))
      (should-not (niwa-execution-session-live-p niwa--session))
      (should-not (plist-get (niwa-eval-sandbox-status niwa--session "/root") :features))))
  :doc "backend refusal is a clean Eval error with fresh guidance and no invented loss"
  (cl-letf (((symbol-function 'niwa-sandbox-probe)
             (lambda (&optional _) '(:available nil :reason "No backend"))))
    (let ((text (niwa-tool-exec-test--sandbox-call "t")))
      (should (string-prefix-p "Error: Eval failed" text))
      (should (string-search "Use fresh Eval" text))
      (should-not (string-search "Sandbox started" text))
      (should-not (string-search "Earlier definitions are gone" text))))
  :doc "failed restart reports definition loss exactly once"
  (progn
    (niwa-tool-exec-test--sandbox-call "t")
    (cl-letf (((symbol-function 'niwa-sandbox-probe)
               (lambda (&optional _) '(:available nil :reason "No backend"))))
      (let ((text (niwa-tool-exec-test--sandbox-call "t" :reset t)))
        (should (string-prefix-p "Error: Eval failed" text))
        (should (= 2 (length (split-string text "Earlier definitions are gone")))))))
  :doc "yielded calls poll the inline result, retain activity and arm idle expiry after settlement"
  (let* ((niwa-eval-sandbox-idle-timeout 60)
         (niwa-execution-mailbox-delivery-function nil)
         (expression "(progn (sleep-for 0.7) (princ \"printed\") (message \"note\") 42)")
         initial final events
         (niwa-execution-event-functions (list (lambda (event) (push event events)))))
    (niwa-tool-exec-test--sandbox-call "t")
    (let ((inline (niwa-tool-exec-test--sandbox-call expression))
          (entry (niwa-eval-sandbox--entry niwa--session "/root")))
      (niwa-tool-exec--eval (lambda (value) (setq initial value))
                              (list :mode "sandbox" :expression expression :yield_time_ms 250))
      (with-timeout (10 (error "Sandbox did not yield"))
        (while (not initial) (accept-process-output nil 0.02)))
      (let ((id (plist-get (plist-get initial :render-data) :execution-id)))
        (should (stringp id))
        (should (string-search id (plist-get (niwa-tool-exec--list-executions nil) :result)))
        (should-not (niwa-eval-sandbox--entry-timer entry))
        (should (niwa-execution-working-p))
        (should (niwa-execution-owner-live-p niwa--session "/root"))
        (should (niwa-execution-session-live-p niwa--session))
        (dolist (reset '(nil t))
          (should (string-search id (error-message-string
                                    (should-error
                                     (niwa-tool-exec-test--sandbox-call "t" :reset reset))))))
        (dolist (input '("text" "\C-c"))
          (should-error (niwa-tool-exec--write-stdin #'ignore
                        (list :execution_id id :chars input))
                        :type 'niwa-execution-input-error))
        (niwa-tool-exec--write-stdin (lambda (value) (setq final value))
                                       (list :execution_id id :yield_time_ms 5000))
        (with-timeout (10 (error "Sandbox poll timed out"))
          (while (not final) (accept-process-output nil 0.02)))
        (should (equal inline (plist-get final :result)))
        (let ((summary (plist-get
                        (plist-get (seq-find (lambda (e) (eq (plist-get e :type) 'terminal))
                                              events) :observation) :sandbox-summary)))
          (should (= 1 (plist-get summary :attempt-count) (plist-get summary :started-count)))
          (should-not (niwa-execution-telemetry-sandbox-summary-class summary))
          (should-not (plist-get (plist-get final :render-data) :sandbox-summary)))
        (should (timerp (niwa-eval-sandbox--entry-timer entry)))
        (should-not (niwa-execution-working-p))
        (should-not (niwa-execution-owner-live-p niwa--session "/root"))
        (should-not (niwa-execution-session-live-p niwa--session))
        (let ((facts (plist-get (seq-find (lambda (e) (eq (plist-get e :type) 'terminal))
                                          events) :facts)))
          (should (eq 'sandbox (plist-get facts :eval-mode)))
          (should (string-search "Background Eval completed"
                                 (niwa-view-audit--breadcrumb-label (list :facts facts))))))))
  :doc "yield survives request cancellation; stop kills the sandbox and reports loss"
  (let ((niwa--current-request (niwa-request--create :session niwa--session :origin "/root"))
        (niwa-execution-mailbox-delivery-function nil) initial stopped)
    (niwa-tool-exec--eval (lambda (value) (setq initial value))
                            '(:mode "sandbox" :expression "(sleep-for 30)" :yield_time_ms 250))
    (with-timeout (10 (error "Sandbox did not yield"))
      (while (not initial) (accept-process-output nil 0.02)))
    (let* ((entry (niwa-eval-sandbox--entry niwa--session "/root"))
           (service (niwa-eval-sandbox--entry-service entry))
           (id (plist-get (plist-get initial :render-data) :execution-id)))
      (niwa-request-cancel niwa--current-request)
      (should (niwa-execution-service-live-p service))
      (niwa-tool-exec--stop-execution (lambda (value) (setq stopped value))
                                        (list :execution_id id))
      (with-timeout (10 (error "Sandbox did not stop"))
        (while (not stopped) (accept-process-output nil 0.02)))
      (should (string-search "Earlier definitions are gone" (plist-get stopped :result)))
      (should (eq 'stopped (plist-get (plist-get stopped :render-data) :termination)))
      (should-not (niwa-execution-service-live-p service))
      (should-not (niwa-eval-sandbox--entry-timer entry))
      (let ((niwa--current-request nil))
        (should (string-search "Earlier definitions are gone"
                               (niwa-tool-exec-test--sandbox-call "t"))))))
  :doc "yielded sandbox holds sleep and blocks history/transfer until user Stop All"
  (let ((niwa-execution-mailbox-delivery-function nil)
        (niwa-execution-state-change-hook '(niwa-sleep-update))
        (niwa-sleep-inhibit t)
        (niwa-sleep-inhibit-command '("cat"))
        (niwa-sleep--poll-interval 0.05)
        (niwa-sleep--timer nil) (niwa-sleep--process nil)
        (niwa-sleep--held-reasons nil) (niwa-sleep--failed nil)
        initial final held)
    (unwind-protect
        (progn
          (niwa-tool-exec-test--sandbox-call "t")
          ;; Idle services alone do not obstruct either boundary.
          (should-not (niwa-session-rewind-assert-stable-source
                       niwa--session (current-buffer) "rewind"))
          (should (niwa-session-control-transfer-drained-p niwa--session))
          (should-not (member "command running" (niwa-sleep--reasons)))
          (niwa-tool-exec--eval
           (lambda (value) (setq initial value))
           '(:mode "sandbox" :expression "(sleep-for 30)" :yield_time_ms 250))
          (with-timeout (5 (error "Sandbox did not yield and hold sleep"))
            (while (not (and initial (process-live-p niwa-sleep--process)))
              (accept-process-output nil 0.02)))
          (setq held niwa-sleep--process)
          (should (equal '("command running") niwa-sleep--held-reasons))
          (should (string-search
                   "Stop live executions"
                   (error-message-string
                    (should-error
                     (niwa-session-rewind-assert-stable-source
                      niwa--session (current-buffer) "rewind") :type 'user-error))))
          (should (equal "a live execution"
                         (niwa-session-control-transfer-drain-blocker niwa--session)))
          (should-not (niwa-session-control-transfer-drained-p niwa--session))
          (should (> (niwa-execution-stop-all-user niwa--session) 0))
          (niwa-tool-exec--write-stdin
           (lambda (value) (setq final value))
           (list :execution_id (plist-get (plist-get initial :render-data) :execution-id)))
          (with-timeout (5 (error "Stop All did not settle and release sleep"))
            (while (not (and final (not (process-live-p held))))
              (accept-process-output nil 0.02)))
          (should (eq 'stopped (plist-get (plist-get final :render-data) :termination)))
          (should (string-search "Earlier definitions are gone" (plist-get final :result)))
          (should-not (plist-get (niwa-eval-sandbox-status niwa--session "/root") :features))
          (should-not (niwa-execution-working-p))
          (should-not niwa-sleep--process)
          (should-not niwa-sleep--held-reasons)
          (should-not niwa-sleep--timer)
          (should-not (niwa-session-rewind-assert-stable-source
                       niwa--session (current-buffer) "rewind"))
          (should (niwa-session-control-transfer-drained-p niwa--session)))
      (niwa-sleep-stop)))
  :doc "queued calls reserve the owner and cancel without launching a child"
  (let* ((state (niwa-execution--state-for-session niwa--session))
         (lease (niwa-execution-scheduler-submit
                 (niwa-execution--state-scheduler state) 'exclusive #'ignore))
         (niwa--current-request (niwa-request--create :session niwa--session :origin "/root"))
         reply)
    (unwind-protect
        (progn
          (niwa-tool-exec--eval (lambda (value) (setq reply value))
                                  '(:mode "sandbox" :expression "42"))
          (should-not reply)
          (should-not (plist-get (niwa-eval-sandbox-status niwa--session "/root") :age-seconds))
          (let ((message (error-message-string
                          (should-error (niwa-tool-exec-test--sandbox-call "t")))))
            (should (string-search "foreground" message))
            (should-not (string-search "execution_id" message)))
          (niwa-request-cancel niwa--current-request)
          (should reply)
          (should-not (niwa-execution-session-live-p niwa--session)))
      (niwa-execution-scheduler-release lease)))
  :doc "large yielded replies are bounded in polling, mailbox and transcript; completion offers root wake"
  (dolist (delivery '(poll mailbox))
    (let* ((niwa-execution-inline-output-limit 1024)
           (niwa-execution-mailbox-delivery-function
            (and (eq delivery 'mailbox) #'niwa-tool-exec-handle-execution-event))
           initial final events (wakes 0)
           (niwa-execution-event-functions (list (lambda (event) (push event events)))))
      (cl-letf (((symbol-function 'niwa-pending-inputs-offer-idle-work)
                 (lambda (&rest _) (cl-incf wakes))))
        (niwa-tool-exec--eval (lambda (value) (setq initial value))
                                '(:mode "sandbox" :expression "(progn (sleep-for 0.7) (make-string 10000 ?x))"
                                  :yield_time_ms 250))
        (with-timeout (10 (error "Sandbox did not yield"))
          (while (not initial) (accept-process-output nil 0.02)))
        (when (eq delivery 'poll)
          (niwa-tool-exec--write-stdin (lambda (value) (setq final value))
                                         (list :execution_id (plist-get (plist-get initial :render-data) :execution-id)
                                               :yield_time_ms 5000)))
        (with-timeout (10 (error "Sandbox did not settle"))
          (while (not (seq-find (lambda (e) (eq (plist-get e :type) 'terminal)) events))
            (accept-process-output nil 0.02)))
        (let* ((event (seq-find (lambda (e) (eq (plist-get e :type) 'terminal)) events))
               (preview (plist-get (plist-get event :observation) :result)))
          (should (plist-get event :whole-output-truncated-p))
          (should (string-search "tool output truncated" preview))
          (should (< (length preview) 1200))
          (should (equal preview (plist-get (niwa-execution-transcript-terminal-render-data event)
                                            :execution-output)))
          (should (string-search preview
                                 (if (eq delivery 'poll) (plist-get final :result)
                                   (plist-get (car (niwa-agent-control-context-mailbox niwa--session))
                                              :payload))))
          (should (= wakes (if (eq delivery 'mailbox) 1 0))))))))

(niwa-deftest niwa-tool-exec--eval
  (:vars* ((eval-directory (make-temp-file "niwa-eval-test-" t))
           (default-directory (file-name-as-directory eval-directory))
           (niwa--session
            (niwa-session--create
             :name "eval" :working-directory default-directory
             :workspace (niwa-workspace--create :root default-directory))))
   :after-each (progn
                 (niwa-execution-teardown-session niwa--session)
                 (delete-directory eval-directory t)))
  ,test
  (test)
  :doc "errors on missing expression"
  (should-error
   (niwa-tool-exec--eval #'ignore (list))
   :type 'error)
  :doc "live Eval runs inline even when the caller supplies child defaults"
  (let (reply)
    (niwa-tool-exec--eval
     (lambda (result) (setq reply result))
     '(:expression "(+ 1 2)" :mode "live" :reset :json-false
       :preserve_ui t :yield_time_ms 10000 :sandbox_permissions "use_default"
       :additional_permissions (:network :json-false :file_system (:read [] :write []))))
    (should (string-search "Result:\n3" (plist-get reply :result)))
    (should-not (plist-get (plist-get reply :render-data) :execution-id)))
  :doc "rejects fresh TTY"
  (should-error (niwa-tool-exec--eval #'ignore '(:expression "1" :mode "fresh" :tty t)))
  :doc "fresh yielding polls the same result as inline completion and labels Eval"
  (let* ((niwa-sandbox-mode 'off)
         (temporary-file-directory default-directory)
         (niwa-execution-mailbox-delivery-function nil)
         events inline yielded final listed
         (niwa-execution-event-functions
          (list (lambda (event) (push event events))))
         (expression "(progn (sleep-for 0.7) (princ \"printed\") (message \"diagnostic\") 42)"))
    (niwa-tool-exec--eval
     (lambda (result) (setq inline result))
     (list :expression expression :mode "fresh" :yield_time_ms 10000))
    (with-timeout (15 (error "Inline Eval timed out"))
      (while (null inline) (accept-process-output nil 0.02)))
    (should-not (plist-get (plist-get inline :render-data) :execution-id))
    (setq events nil)
    (niwa-tool-exec--eval
     (lambda (result) (setq yielded result))
     (list :expression expression :mode "fresh" :yield_time_ms 250))
    (with-timeout (10 (error "Eval did not yield"))
      (while (null yielded) (accept-process-output nil 0.02)))
    (let* ((id (plist-get (plist-get yielded :render-data) :execution-id))
           (record (niwa-execution--owned-yielded-record niwa--session "/root" id)))
      (should (stringp id))
      (should (string-search "<eval-execution" (plist-get yielded :result)))
      (should (niwa-execution--record-mutating-p record))
      (should-not (niwa-execution--record-pipeline-marker record))
      (setq listed (niwa-tool-exec--list-executions nil))
      (should (string-search id (plist-get listed :result)))
      (dolist (chars '("input" "\C-c"))
        (should-error (niwa-tool-exec--write-stdin #'ignore
                      (list :execution_id id :chars chars))
                      :type 'niwa-execution-input-error))
      (niwa-tool-exec--write-stdin
       (lambda (result) (setq final result))
       (list :execution_id id :chars "" :yield_time_ms 5000))
      (with-timeout (10 (error "Eval poll timed out"))
        (while (null final) (accept-process-output nil 0.02)))
      (should (equal (plist-get inline :result) (plist-get final :result)))
      (should (string-search "STDOUT:\nprinted\n\nMESSAGES:\ndiagnostic"
                             (plist-get final :result)))
      (let* ((event (seq-find (lambda (event) (eq (plist-get event :type) 'terminal)) events))
             (facts (plist-get event :facts)))
        (should (eq 'success (plist-get facts :outcome)))
        (should-not (plist-get event :whole-output-truncated-p))
        (let* ((render-data (niwa-execution-transcript-terminal-render-data event))
               (rendered (niwa-tool-exec--render-eval
                          "Eval" (list :mode "fresh" :expression expression)
                          (plist-get yielded :result) render-data)))
          (should (equal (plist-get inline :result) (plist-get rendered :body)))
          (should (eq 'success (plist-get rendered :status))))
        (should (string-search "Background Eval completed"
                               (niwa-view-audit--breadcrumb-label (list :facts facts))))
        (should (string-search "Background Eval completed"
                               (car (niwa-view--mail-notice
                                     "<agent-message type=\"EXECUTION\" kind=\"eval\" termination=\"exited\" outcome=\"success\">"
                                     nil "/root/child" "")))))
      (should-not (niwa-execution--record-teardown-function record))
      (should-not (directory-files default-directory nil "\\`niwa-eval-fresh-"))))
  :doc "bounds large yielded values in polls, mail and durable output, but not inline"
  (dolist (delivery '(inline poll mailbox))
    (let* ((niwa-sandbox-mode 'off)
           (niwa-execution-inline-output-limit 1024)
           (niwa-execution-mailbox-delivery-function
            (and (eq delivery 'mailbox) #'niwa-tool-exec-handle-execution-event))
           events initial final
           (niwa-execution-event-functions
            (list (lambda (event) (push event events)))))
      (cl-letf (((symbol-function 'niwa-pending-inputs-offer-idle-work) #'ignore))
        (niwa-tool-exec--eval
         (lambda (result) (setq initial result))
         (list :expression "(progn (sleep-for 0.7) (make-string 2000000 ?x))"
               :mode "fresh" :yield_time_ms (if (eq delivery 'inline) 10000 250)))
        (with-timeout (15 (error "Large Eval did not return or yield"))
          (while (null initial) (accept-process-output nil 0.02)))
        (if (eq delivery 'inline)
            (should (> (length (plist-get initial :result)) 2000000))
          (when (eq delivery 'poll)
            (niwa-tool-exec--write-stdin
             (lambda (result) (setq final result))
             (list :execution_id (plist-get (plist-get initial :render-data) :execution-id)
                   :chars "" :yield_time_ms 5000)))
          (with-timeout (15 (error "Large Eval did not settle"))
            (while (not (seq-find (lambda (event) (eq (plist-get event :type) 'terminal)) events))
              (accept-process-output nil 0.02)))
          (let* ((event (seq-find (lambda (event) (eq (plist-get event :type) 'terminal)) events))
                 (preview (plist-get (plist-get event :observation) :result))
                 (render-data (niwa-execution-transcript-terminal-render-data event)))
            (should (< (length preview) (+ niwa-execution-inline-output-limit 100)))
            (should (string-prefix-p "Result:\n\"xxx" preview))
            (should (string-suffix-p "xxx\"" preview))
            (should (string-search "tool output truncated" preview))
            (should (plist-get event :whole-output-truncated-p))
            (should (equal preview (plist-get render-data :execution-output)))
            (let ((payload
                   (if (eq delivery 'poll)
                       (plist-get final :result)
                     (plist-get
                      (car (niwa-agent-control-context-mailbox niwa--session))
                      :payload))))
              ;; Model delivery may append a sandbox disclosure to the preview.
              (should (string-search preview payload))
              (should (< (length payload) (+ niwa-execution-inline-output-limit 1000)))))))))
  :doc "yielded fresh calls stream messages into a running poll"
  (let ((niwa-sandbox-mode 'off)
        (niwa-execution-mailbox-delivery-function nil)
        yielded polled)
    (niwa-tool-exec--eval
     (lambda (result) (setq yielded result))
     '(:expression "(progn (while (not (file-exists-p \"emit-message\")) (sleep-for 0.02)) (message \"poll progress\") (sleep-for 30))"
       :mode "fresh" :yield_time_ms 250))
    (with-timeout (10 (error "Eval did not yield"))
      (while (null yielded) (accept-process-output nil 0.02)))
    (with-temp-file "emit-message" (insert "ready"))
    (niwa-tool-exec--write-stdin
     (lambda (result) (setq polled result))
     (list :execution_id (plist-get (plist-get yielded :render-data) :execution-id)
           :chars "" :yield_time_ms 5000))
    (with-timeout (10 (error "Eval poll did not return"))
      (while (null polled) (accept-process-output nil 0.02)))
    (should (string-search "poll progress" (plist-get polled :result)))
    (should (eq 'running (plist-get (plist-get polled :render-data) :state))))
  :doc "fresh stop retains streamed messages and teardown removes temporary files"
  (dolist (action '(stop teardown))
    (let* ((temporary-file-directory default-directory)
           (niwa-sandbox-mode 'off)
           (niwa-execution-mailbox-delivery-function nil)
           yielded stopped)
      (niwa-tool-exec--eval
       (lambda (result) (setq yielded result))
       '(:expression "(progn (message \"stop progress\") (sleep-for 30))" :mode "fresh" :yield_time_ms 250))
      (with-timeout (10 (error "Eval did not yield"))
        (while (null yielded) (accept-process-output nil 0.02)))
      (let* ((id (plist-get (plist-get yielded :render-data) :execution-id))
             (record (niwa-execution--owned-yielded-record niwa--session "/root" id))
             (child (niwa-execution--record-child record)))
        (with-timeout (10 (error "Fresh message did not reach the spool"))
          (while (not (string-search "stop progress" (niwa-execution--read-output record)))
            (accept-process-output nil 0.02)))
        (should (= 2 (length (directory-files default-directory nil "\\`niwa-eval-fresh-"))))
        (if (eq action 'stop)
            (progn
              (niwa-tool-exec--stop-execution
               (lambda (result) (setq stopped result)) (list :execution_id id))
              (with-timeout (10 (error "Eval did not stop"))
                (while (null stopped) (accept-process-output nil 0.02)))
              (should (string-prefix-p "Eval stopped" (plist-get stopped :result)))
              (should (string-search "stop progress" (plist-get stopped :result)))
              (should (eq 'stopped (plist-get (plist-get stopped :render-data) :termination)))
              (should (equal '(stopped . "stopped")
                             (niwa-tool-exec-completion 'stopped 'failure))))
          (niwa-execution-teardown-session niwa--session))
        (should-not (niwa-execution-process-live-p child))
        (should-not (directory-files default-directory nil "\\`niwa-eval-fresh-")))))
  :doc "fresh independent completion delivers its formatted result and offers a root wake"
  (let* ((niwa-sandbox-mode 'off)
         (niwa-execution-mailbox-delivery-function #'niwa-tool-exec-handle-execution-event)
         yielded offered)
    (cl-letf (((symbol-function 'niwa-pending-inputs-offer-idle-work)
               (lambda (buffer) (push buffer offered))))
      (niwa-tool-exec--eval
       (lambda (result) (setq yielded result))
       '(:expression "(progn (sleep-for 0.7) (message \"mail diagnostic\") 42)"
         :mode "fresh" :yield_time_ms 250))
      (with-timeout (15 (error "Eval mailbox completion timed out"))
        (while (null offered) (accept-process-output nil 0.02))))
    (should (plist-get (plist-get yielded :render-data) :execution-id))
    (let ((message (car (niwa-agent-control-context-mailbox niwa--session))))
      (should (plist-get message :wake))
      (should (equal "eval" (plist-get (plist-get message :meta) :kind)))
      (should (string-search "Result:\n42\n\nMESSAGES:\nmail diagnostic"
                             (plist-get message :payload)))))
  :doc "passes approved network authority only to fresh Eval"
  (let (captured)
    (cl-letf (((symbol-function 'niwa-tool-exec--eval-fresh)
               (lambda (_callback expression result-format additional
                                  &optional sandbox-permissions _args)
                 (setq captured (list expression result-format additional
                                      sandbox-permissions)))))
      (niwa-tool-exec--eval
       #'ignore
       '(:expression "(+ 1 2)"
                     :mode "fresh"
                     :sandbox_permissions "with_additional_permissions"
                     :additional_permissions (:network t)
                     :justification "Fetch package metadata?")))
    (should (equal '("(+ 1 2)" nil (:network t) nil) captured)))
  :doc "passes approved full escalation only to fresh Eval"
  (let (captured)
    (cl-letf (((symbol-function 'niwa-tool-exec--eval-fresh)
               (lambda (_callback expression _result-format additional
                                  &optional sandbox-permissions _args)
                 (setq captured (list expression additional
                                      sandbox-permissions)))))
      (niwa-tool-exec--eval
       #'ignore
       '(:expression "(+ 1 2)"
                     :mode "fresh"
                     :sandbox_permissions "require_escalated"
                     :justification "Run fresh Eval without confinement?")))
    (should (equal '("(+ 1 2)" nil require-escalated) captured)))
  :doc "evaluates simple expression"
  (let (result)
    (niwa-tool-exec--eval
     (lambda (r)
       (setq result (niwa-tool-exec-test--handler-result r)))
     (list :expression "(+ 1 2 3)"))
    (should (string-match-p "Result:\n6" result))
    (should (string-match-p
             "Live Eval ran inside Emacs without child-process confinement"
             result)))
  :doc "captures printed output"
  (let (result)
    (niwa-tool-exec--eval
     (lambda (r)
       (setq result (niwa-tool-exec-test--handler-result r)))
     (list :expression "(princ \"hello world\")"))
    (should (string-match-p "STDOUT:\nhello world" result)))
  :doc "captures live messages without suppressing downstream handlers or logging"
  (let* ((result-file (make-temp-file "niwa-eval-live-messages-"))
         (process-environment (cons "TERM=xterm" process-environment))
         (form
          `(progn
             (setq load-path ',load-path native-comp-jit-compilation nil)
             (require 'niwa-tool-exec)
             (let* ((default-directory ,default-directory)
                    downstream response
                    (set-message-functions
                     (list (lambda (text) (push text downstream) nil))))
               (niwa-tool-exec--eval-live
                (lambda (value) (setq response value))
                "(progn (message \"live hello\") (message \"live again\") 42)"
                nil t)
               (with-temp-file ,result-file
                 (prin1 (list response (nreverse downstream)
                              (with-current-buffer "*Messages*" (buffer-string)))
                        (current-buffer))))
             (kill-emacs 0)))
         (child (make-process
                 :name "niwa-test-live-messages" :buffer nil :noquery t
                 :connection-type 'pty
                 :command (list (expand-file-name invocation-name invocation-directory)
                                "-Q" "-nw" "--eval" (prin1-to-string form)))))
    ;; Echo-area hooks do not run in batch Emacs, even with noninteractive bound.
    (unwind-protect
        (progn
          (with-timeout (30 (error "Live message fixture timed out"))
            (while (process-live-p child) (accept-process-output child 0.05)))
          (should (zerop (process-exit-status child)))
          (let* ((data (niwa-tool-exec--eval-read-fresh-result result-file))
                 (result (plist-get (car data) :result)))
            (should (string-search "Result:\n42" result))
            (should (string-search "MESSAGES:\nlive hello\nlive again" result))
            (should (equal '("live hello" "live again") (cadr data)))
            (should (string-search "live hello\nlive again" (caddr data)))))
      (when (process-live-p child) (delete-process child))
      (delete-file result-file)))
  :doc "returns fresh messages on success"
  (let (result)
    (niwa-tool-exec--eval
     (lambda (value) (setq result (plist-get value :result)))
     '(:expression "(progn (message \"fresh hello\") 42)" :mode "fresh"))
    (with-timeout (30 (error "Fresh messages timed out"))
      (while (null result) (accept-process-output nil 0.05)))
    (should (string-search "Result:\n42" result))
    (should (string-search "MESSAGES:\nfresh hello" result)))
  :doc "keeps injection results compact in both modes"
  (dolist (mode '("live" "fresh"))
    (let (result)
      (niwa-tool-exec--eval
       (lambda (value) (setq result (plist-get value :result)))
       (list :expression "(progn (princ \"printed\") 42)"
             :result-format "injection" :mode mode))
      (with-timeout (30 (error "Injection result timed out"))
        (while (null result) (accept-process-output nil 0.05)))
      (should (string-prefix-p "42\n\nSTDOUT:\nprinted" result))
      (should-not (string-search "Result:" result))
      (should-not (string-search "BACKTRACE:" result))
      (should-not (string-search "MESSAGES:" result))))
  :doc "returns pre-error output and a bounded failing frame in both modes"
  (dolist (mode '("live" "fresh"))
    (let (result)
      (niwa-tool-exec--eval
       (lambda (value) (setq result (plist-get value :result)))
       (list :expression "(progn (princ \"before failure\") (error \"failing frame\"))"
             :mode mode))
      (with-timeout (30 (error "Error result timed out"))
        (while (null result) (accept-process-output nil 0.05)))
      (should (string-prefix-p "Error:" result))
      (should (string-search "STDOUT:\nbefore failure" result))
      (let ((trace (cadr (split-string result "BACKTRACE:\n"))))
        (should trace)
        (setq trace (car (split-string trace "\n\n\\(?:Execution:\\|\\[sandbox:\\)")))
        (should (string-search "error(\"failing frame\")" trace))
        (should (<= (length trace) niwa-tool-exec--eval-backtrace-chars)))))
  :doc "limits the number of frames passed to the backtrace formatter"
  (let ((frames (make-list (1+ niwa-tool-exec--eval-backtrace-frames)
                           (car (backtrace-get-frames))))
        rendered)
    (cl-letf (((symbol-function 'backtrace-get-frames) (lambda () frames))
              ((symbol-function 'backtrace-to-string)
               (lambda (frames) (setq rendered frames) "trace")))
      (should (string-suffix-p niwa-tool-exec--eval-backtrace-marker
                               (niwa-tool-exec--eval-capture-backtrace))))
    (should (equal rendered (seq-take frames niwa-tool-exec--eval-backtrace-frames))))
  :doc "visibly truncates deep and overlong backtraces in both modes"
  (dolist (mode '("live" "fresh"))
    (dolist (expression
             '("(funcall (letrec ((recur (lambda (n) (if (zerop n) (error \"deep\") (funcall recur (1- n)))))) recur) 50)"
               "(error \"%s\" (make-string 10000 ?x))"))
      (let (result)
        (niwa-tool-exec--eval
         (lambda (value) (setq result (plist-get value :result)))
         (list :expression expression :mode mode))
        (with-timeout (30 (error "Truncated result timed out"))
          (while (null result) (accept-process-output nil 0.05)))
        (let ((trace (cadr (split-string result "BACKTRACE:\n"))))
          (should trace)
          (setq trace (car (split-string trace "\n\n\\(?:Execution:\\|\\[sandbox:\\)")))
          (should (<= (length trace) niwa-tool-exec--eval-backtrace-chars))
          (should (string-suffix-p niwa-tool-exec--eval-backtrace-marker trace))))))
  :doc "returns fresh errors with unreadable error data"
  (let (result envelope)
    (niwa-tool-exec--eval
     (lambda (value) (setq envelope value result (plist-get value :result)))
     '(:expression "(signal 'wrong-type-argument (list 'numberp (current-buffer)))"
       :mode "fresh"))
    (with-timeout (30 (error "Unreadable error timed out"))
      (while (null result) (accept-process-output nil 0.05)))
    (should (string-prefix-p "Error: Eval failed with error wrong-type-argument" result))
    (should (eq 'error (plist-get envelope :status)))
    (should (eq 'failure (plist-get (plist-get envelope :render-data) :outcome)))
    (should (string-search "#<buffer" result))
    (should (string-search "BACKTRACE:" result)))
  :doc "passes large live output intact to the shared pipeline"
  (let (result)
    (niwa-tool-exec--eval
     (lambda (r)
       (setq result (niwa-tool-exec-test--handler-result r)))
     (list :expression
           "(progn (princ (make-string 550000 ?h)) (princ (make-string 50000 ?t)) 42)"
           :mode "live"))
    (should (string-search (make-string 100 ?t) result))
    (should-not (string-search "Output truncated" result)))
  :doc "prints shared structure once in live and fresh results"
  (dolist (mode '("live" "fresh"))
    (let (result)
      (niwa-tool-exec--eval
       (lambda (r)
         (setq result (niwa-tool-exec-test--handler-result r)))
       ;; Without shared-structure notation this prints 2^22 leaves.
       (list :expression "(let ((x 1)) (dotimes (_ 22) (setq x (cons x x))) x)"
             :mode mode))
      (let ((deadline (+ (float-time) 30)))
        (while (and (not result) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (string-search "#1=" result))
      (should (< (length result) 4096))))
  :doc "reports eval errors"
  (let (result)
    (niwa-tool-exec--eval
     (lambda (r)
       (setq result (niwa-tool-exec-test--handler-result r)))
     (list :expression "(error \"test error\")"))
    (should (string-match-p "Error:.*test error" result)))
  :doc "returns string results with %S formatting"
  (let (result)
    (niwa-tool-exec--eval
     (lambda (r)
       (setq result (niwa-tool-exec-test--handler-result r)))
     (list :expression "\"hello\""))
    (should (string-match-p "Result:\n\"hello\"" result)))
  :doc "evaluates with the session working directory bound"
  (let* ((root (make-temp-file "niwa-eval-cwd-" t))
         (module-dir (file-name-concat root "packages" "api"))
         (workspace (niwa-workspace-get-or-create
                     'file root root "test"))
         (session (niwa-session-create "main" workspace module-dir))
         (niwa--session session)
         result)
    (make-directory module-dir t)
    (unwind-protect
        (progn
          (niwa-tool-exec--eval
           (lambda (r)
             (setq result (niwa-tool-exec-test--handler-result r)))
           (list :expression "default-directory"))
          (should (string-match-p
                   (regexp-quote
                    (format "Result:\n%S"
                            (file-name-as-directory module-dir)))
                   result)))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "preserves window configuration by default in live mode"
  (let ((original (current-window-configuration))
        result)
    (unwind-protect
        (progn
          (delete-other-windows)
          (split-window-right)
          (should (= 2 (length (window-list))))
          (niwa-tool-exec--eval
           (lambda (r)
             (setq result (niwa-tool-exec-test--handler-result r))
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
          (niwa-tool-exec--eval
           (lambda (r)
             (setq result (niwa-tool-exec-test--handler-result r)))
           (list :expression "(delete-other-windows)"
                 :preserve_ui :json-false))
          (should (string-match-p "Result:" result))
          (should (= 1 (length (window-list)))))
      (set-window-configuration original)))
  :doc "evaluates simple expressions in fresh mode"
  (let ((original-start
         (symbol-function 'niwa-execution-start-bash))
        (child-starts 0)
        result)
    (cl-letf (((symbol-function 'niwa-execution-start-bash)
               (lambda (&rest args)
                 (cl-incf child-starts)
                 (let ((workdir (plist-get (cdr args) :workdir)))
                   (should (file-directory-p workdir))
                   ;; `file-in-directory-p' stats the shared temporary root
                   ;; twice; parallel workers can change its metadata between
                   ;; those reads.  Check the exact canonical fixture instead.
                   (should
                    (equal (file-name-as-directory (file-truename workdir))
                           (file-name-as-directory
                            (file-truename eval-directory)))))
                 (apply original-start args))))
      (niwa-tool-exec--eval
       (lambda (r)
         (setq result (niwa-tool-exec-test--handler-result r)))
       (list :expression "(+ 4 5)" :mode "fresh"))
      (while (null result)
        (accept-process-output nil 0.1)))
    (should (= 1 child-starts))
    (should (string-match-p "Result:\n9" result))
    (should-not (string-match-p "without child-process confinement" result)))
  :doc "failed confined fresh mode includes explicit retry guidance"
  (let (result)
    (cl-letf (((symbol-function 'niwa-tool-exec--eval-read-fresh-result)
               (lambda (_file)
                 '(:status error :error "error: (\"Fresh failure\")")))
              ((symbol-function 'niwa-execution-start-bash)
               (lambda (callback &rest args)
                 (let ((result (funcall (plist-get args :result-function)
                                        '(:exit-code 1 :output "" :termination exited))))
                   (funcall (plist-get args :teardown-function))
                 (funcall
                  callback
                  (list :result (plist-get result :output)
                        :facts '(:kind eval :state completed :outcome failure)
                        :sandbox-facts
                        '(:sandbox bubblewrap :filesystem workspace-write
                                   :network isolated)))))))
      (niwa-tool-exec--eval
       (lambda (envelope)
         (setq result (niwa-tool-exec-test--handler-result envelope)))
       (list :expression "(error \"fresh failure\")" :mode "fresh")))
    (should
     (string-search
      "retry with `with_additional_permissions`"
      result)))
  :doc "pipeline settles both fresh launch failure forms as errors"
  (progn
    (let* ((workspace (niwa-workspace--create :root default-directory))
           (niwa--session
            (niwa-session--create :name "main" :workspace workspace)))
      (dolist (failure '(child-result start-signal))
        (let ((tool (niwa-tool--create
                     :name "Eval"
                     :handler #'niwa-tool-exec--eval
                     :args '((expression string :required "Expression")
                             (mode string :optional "Mode"
                                   :enum ["live" "fresh"]))
                     :async-p t
                     :read-only-p t))
              events result)
          (cl-letf (((symbol-function 'niwa-execution-start-bash)
                     (lambda (callback &rest args)
                       (if (eq failure 'child-result)
                           (let ((result (funcall (plist-get args :result-function)
                                                  '(:exit-code nil :output ""
                                                    :error "launch failed"))))
                             (funcall (plist-get args :teardown-function))
                             (funcall callback
                                      (list :result (plist-get result :output)
                                            :facts '(:kind eval :state completed :outcome failure))))
                         (error "Launch failed"))))
                    ((symbol-function 'niwa-hooks-run-event)
                     (lambda (event _payload callback &rest _)
                       (push event events)
                       (funcall callback nil))))
            (niwa-pipeline-run-tool
             tool (lambda (value) (setq result value))
             '(:expression "(+ 1 2)" :mode "fresh")))
          (should
           (string-match-p "Failed to start Eval fresh process" result))
          (should
           (equal '(PreToolUse PostToolUseFailure) (nreverse events)))))))
  :doc "live mode does not use the child-process seam"
  (let ((child-starts 0)
        result)
    (cl-letf (((symbol-function 'niwa-execution-start-bash)
               (lambda (&rest _args)
                 (cl-incf child-starts))))
      (niwa-tool-exec--eval
       (lambda (r)
         (setq result (niwa-tool-exec-test--handler-result r)))
       (list :expression "(+ 2 3)" :mode "live")))
    (should (= 0 child-starts))
    (should (string-match-p "Result:\n5" result)))
  :doc "fresh mode removes its temporary script and result files"
  (let* ((temp-dir (make-temp-file "niwa-eval-cleanup-" t))
         (temporary-file-directory (file-name-as-directory temp-dir))
         result)
    (unwind-protect
        (progn
          (niwa-tool-exec--eval
           (lambda (r)
             (setq result (niwa-tool-exec-test--handler-result r)))
           (list :expression "(error \"cleanup\")" :mode "fresh"))
          (with-timeout (5 (error "Timed out"))
            (while (null result)
              (accept-process-output nil 0.1)))
          (should (string-prefix-p "Error:" result))
          (should-not
           (directory-files temp-dir nil "\\`niwa-eval-fresh-"))
          (should-not
           (directory-files (file-name-concat temp-dir ".niwa-pending-executions")
                            nil directory-files-no-dot-files-regexp)))
      (delete-directory temp-dir t)))
  :doc "fresh capture preserves definitions and captures output and messages"
  (let (result)
    (niwa-tool-exec--eval
     (lambda (r)
       (setq result (niwa-tool-exec-test--handler-result r)))
     (list :expression "(progn (defvar stdout 42) (defvar messages nil) (message \"note\") (princ \"fresh hello\") stdout)" :mode "fresh"))
    (while (null result)
      (accept-process-output nil 0.1))
    (should (string-match-p "STDOUT:\nfresh hello" result))
    (should (string-match-p "MESSAGES:\nnote" result))
    (should (string-match-p "Result:\n42" result)))
  :doc "fresh final messages are bounded even without a result file"
  (dolist (tail '("42" "(kill-emacs 42)"))
    (let ((niwa-sandbox-mode 'off) result)
      (niwa-tool-exec--eval
       (lambda (reply) (setq result (plist-get reply :result)))
       (list :mode "fresh" :expression
             (format "(progn (message \"lost diagnostic %%s\" (make-string 50000 ?x)) %s)" tail)))
      (with-timeout (10 (error "Fresh message result timed out"))
        (while (null result) (accept-process-output nil 0.02)))
      (should (string-search "lost diagnostic" result))
      (should (string-search "[Output truncated]" result))
      (should (< (length result) 17000))))
  :doc "fresh mode does not mutate parent variables"
  (let (result)
    (makunbound 'niwa-test-fresh-parent-mutation)
    (niwa-tool-exec--eval
     (lambda (r)
       (setq result (niwa-tool-exec-test--handler-result r)))
     (list :expression "(setq niwa-test-fresh-parent-mutation 99)"
           :mode "fresh"))
    (while (null result)
      (accept-process-output nil 0.1))
    (should (string-match-p "Result:\n99" result))
    (should-not (boundp 'niwa-test-fresh-parent-mutation)))
  :doc "fresh mode does not expose bootstrap locals"
  (let (result)
    (niwa-tool-exec--eval
     (lambda (r)
       (setq result (niwa-tool-exec-test--handler-result r)))
     (list :expression
           "(list (boundp 'result-file) (boundp 'stdout-buffer) (boundp 'max-output-bytes))"
           :mode "fresh"))
    (while (null result)
      (accept-process-output nil 0.1))
    (should (string-match-p "Result:\n(nil nil nil)" result)))
  :doc "fresh mode bootstrap locals cannot be corrupted by evaluated code"
  (let (result)
    (niwa-tool-exec--eval
     (lambda (r)
       (setq result (niwa-tool-exec-test--handler-result r)))
     (list :expression "(progn (setq result-file nil) 42)"
           :mode "fresh"))
    (while (null result)
      (accept-process-output nil 0.1))
    (should (string-match-p "Result:\n42" result)))
  :doc "fresh mode uses the session working directory"
  (let* ((root (make-temp-file "niwa-eval-fresh-cwd-" t))
         (module-dir (file-name-concat root "packages" "api"))
         (workspace (niwa-workspace-get-or-create
                     'file root root "test"))
         (session (niwa-session-create "main" workspace module-dir))
         (niwa--session session)
         result)
    (make-directory module-dir t)
    (unwind-protect
        (progn
          (niwa-tool-exec--eval
           (lambda (r)
             (setq result (niwa-tool-exec-test--handler-result r)))
           (list :expression "default-directory" :mode "fresh"))
          (while (null result)
            (accept-process-output nil 0.1))
          (should (string-match-p
                   (regexp-quote
                    (format "Result:\n%S"
                            (file-name-as-directory module-dir)))
                   result)))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "fresh mode belongs to its agent and cleans temporary files on teardown"
  (let* ((root (make-temp-file "niwa-eval-fresh-owner-" t))
         (temporary-file-directory (file-name-as-directory root))
         (workspace (niwa-workspace-get-or-create
                     'test root root "eval-owner"))
         (session (niwa-session-create "main" workspace root))
         (niwa--session session)
         (niwa--current-request
          (niwa-request--create
           :origin "/root/agent_eval" :session session))
         (niwa-sandbox-mode 'off)
         callback-result record)
    (unwind-protect
        (progn
          (niwa-tool-exec--eval
           (lambda (result) (setq callback-result result))
           (list :expression "(progn (sleep-for 30) 1)" :mode "fresh"))
          (with-timeout (2 (error "Fresh Eval did not start"))
            (while
                (progn
                  (setq record
                        (car
                         (niwa-execution--state-record-list
                          (niwa-session-execution-state session))))
                  (null record))
              (accept-process-output nil 0.02)))
          (should (equal "/root/agent_eval"
                         (niwa-execution--origin-owner
                          (niwa-execution--record-origin record))))
          (should (= 1 (niwa-execution-stop-owner
                        session "/root/agent_eval")))
          (should (eq 'error (plist-get callback-result :status)))
          (should-not
           (directory-files root nil "\\`niwa-eval-fresh-")))
      (niwa-execution-teardown-session session)
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "preserves large fresh error output for shared pipeline persistence"
  (let (result)
    (niwa-tool-exec--eval
     (lambda (r)
       (setq result (niwa-tool-exec-test--handler-result r)))
     (list :expression
           "(progn (princ (make-string 550000 ?h)) (princ (make-string 50000 ?t)) (error \"boom\"))"
           :mode "fresh"))
    (while (null result)
      (accept-process-output nil 0.1))
    (should (string-prefix-p "Error:" result))
    (should (string-search (make-string 100 ?t) result))
    (should-not (string-search "Output truncated" result))))

(niwa-deftest niwa-tool-exec--render-bash ()
  ,test
  (test)
  :doc "collapsed summaries skip output formatting, even while running"
  (let* ((output (concat (make-string 100000 ?x)
                         "\n<bash-execution state=\"completed\"/>"))
         (args '(:command "printf output"))
         (data '(:status error :state completed :outcome failure :exit-code 1))
         (full (niwa-tool-exec--render-bash "Bash" args output data))
         (replace (symbol-function 'replace-regexp-in-string))
         summary)
    (let ((niwa-tool-render-summary-only t))
      (cl-letf (((symbol-function 'replace-regexp-in-string)
                 (lambda (regexp replacement string &rest rest)
                   (should-not (eq string output))
                   (apply replace regexp replacement string rest))))
        (setq summary (niwa-tool-exec--render-bash "Bash" args output data))))
    (should-not (plist-get summary :body))
    (should (equal (plist-put (copy-sequence full) :body nil) summary))
    (let* ((niwa-tool-render-summary-only t)
           (live (niwa-tool-exec--render-bash
                  "Bash" args output (append '(:live-execution-p t) data))))
      (should-not (plist-get live :body))
      (should-not (plist-get live :force-expanded-p))
      (should (plist-get live :initially-collapsed-p))))
  :doc "returns nil for non-string result"
  (should (null (niwa-tool-exec--render-bash
                 "Bash" '(:command "ls") nil nil)))

  :doc "discloses a truncated cumulative preview even when polls omitted nothing"
  (let ((row (niwa-tool-exec--render-bash
              "Bash" '(:command "produce-output") "bounded preview"
              '(:status success :state completed :outcome success
                        :omitted-output-bytes 0
                        :output-preview-truncated-p t))))
    (should (plist-get row :initially-collapsed-p))
    (should (string-match-p "output truncated" (plist-get row :header))))

  :doc "preserves launch failure diagnostics without substituting an envelope for stdout"
  (let ((row (niwa-tool-exec--render-bash
              "Bash" '(:command "printf hello") "actual output"
              '(:status error :state completed :outcome failure
                        :termination spawn-failed
                        :execution-error "spawning child process: no such file: bash"))))
    (should (string-match-p "Failed to start process: spawning child process"
                            (plist-get row :body)))
    (should (string-match-p "actual output" (plist-get row :body))))
  :doc "a post-launch failure does not claim the process failed to start"
  (let ((row (niwa-tool-exec--render-bash
              "Bash" '(:command "remote-write") ""
              '(:status error :state lost :termination unknown
                        :execution-error "remote process-group identity lost"))))
    (should (string-match-p "Execution error: remote process-group identity lost"
                            (plist-get row :body)))
    (should-not (string-match-p "Failed to start process"
                                (plist-get row :body))))

  :doc "header shows first command line and expanded body keeps the full command"
  (let* ((body "file1\nfile2\n")
         (plist (niwa-tool-exec--render-bash
                 "Bash" '(:command "ls -la\n# more") body nil)))
    (should (equal "Bash: ls -la" (plist-get plist :header)))
    (should (equal (concat "$ ls -la\n# more\n\n" body)
                   (plist-get plist :body)))
    (should (eq 'sh-mode (plist-get plist :body-mode))))

  :doc "truncates long headers without truncating the expanded command"
  (let* ((command (concat "git add -- " (make-string 90 ?x)))
         (plist (niwa-tool-exec--render-bash
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
         (niwa-tool-exec--render-bash
          "WriteStdin" nil
          (concat "Hello, Ada\n\n"
                  "<bash-execution execution_id=\"exec-1\" state=\"completed\"/>")
          '(:status success :state completed :model-result-envelope-p t))))
    (should (equal "Hello, Ada\n\nDetails: completed"
                   (plist-get plist :body))))
  (let ((plist
         (niwa-tool-exec--render-bash
          "Bash" '(:command "printf marker")
          (concat "<bash-execution marker=\"USER_OUTPUT\"/>\n\n"
                  "<bash-execution execution_id=\"exec-1\" state=\"completed\"/>")
          '(:status success :state completed :model-result-envelope-p t))))
    (should (string-match-p "<bash-execution marker=\"USER_OUTPUT\"/>"
                            (plist-get plist :body)))
    (should-not (string-match-p "execution_id=\"exec-1\""
                                (plist-get plist :body))))

  :doc "keeps marker-shaped stdout from canonical terminal and progress evidence"
  (let* ((output "<bash-execution marker=\"USER_OUTPUT\"/>\n")
         (terminal (niwa-tool-exec--render-bash
                    "Bash" '(:command "printf marker") output
                    (list :status 'success :state 'completed
                          :outcome 'success :exit-code 0
                          :execution-output output)))
         (progress (niwa-tool-exec--render-bash
                    "Bash" '(:command "printf marker") output
                    '(:status success :state running :live-execution-p t))))
    (should (string-match-p (regexp-quote output)
                            (plist-get terminal :body)))
    (should (string-match-p (regexp-quote output)
                            (plist-get progress :body)))
    (should (string-match-p
             (regexp-quote output)
             (plist-get
              (niwa-tool-exec--render-bash
               "Bash" '(:command "printf marker") output
               '(:status success :state completed :outcome success :exit-code 0))
              :body))))

  :doc "labels polls and input as background-process interactions"
  (let ((poll
         (niwa-tool-exec--render-bash
          "WriteStdin" '(:execution_id "exec-1")
          "<bash-execution execution_id=\"exec-1\" state=\"completed\"/>"
          '(:status success :state completed :execution-id "exec-1"
                    :execution-control poll :observation-output-p nil)))
        (input
         (niwa-tool-exec--render-bash
          "WriteStdin" '(:execution_id "exec-1" :chars "yes\n")
          "<bash-execution execution_id=\"exec-1\" state=\"running\"/>"
          '(:status success :state running :execution-id "exec-1"
                    :execution-control input :observation-output-p nil))))
    (should (equal "WriteStdin: observation finished"
                   (plist-get poll :header)))
    (should (plist-get poll :hidden-p))
    (should (equal "WriteStdin: sent input" (plist-get input :header)))
    (should (string-match-p "yes" (plist-get input :body)))
    (should-not (plist-get input :coalesce-key)))

  :doc "hides successful polls with or without output, not failed controls"
  (dolist (render-data
           '((:status success :state completed :execution-id "exec-1"
                      :execution-control poll :observation-output-p t)
             (:status error :state completed :execution-id "exec-1"
                      :execution-control poll :observation-output-p nil)
             (:status success :state completed :execution-id "exec-1"
                      :execution-control input :observation-output-p nil)))
    (let ((row (niwa-tool-exec--render-bash
                "WriteStdin" '(:execution_id "exec-1") "result" render-data)))
      (should (eq (and (plist-get row :hidden-p) t)
                  (and (eq (plist-get render-data :status) 'success)
                       (eq (plist-get render-data :execution-control) 'poll))))))

  :doc "hides only successful empty polls while execution remains running"
  (let ((hidden
         (niwa-tool-exec--render-bash
          "WriteStdin" nil
          "<bash-execution execution_id=\"exec-1\" state=\"running\"/>"
          '(:status success :state running
                    :execution-control poll :observation-output-p nil)))
        (terminal
         (niwa-tool-exec--render-bash
          "WriteStdin" nil
          "<bash-execution execution_id=\"exec-1\" state=\"completed\"/>"
          '(:status success :state completed
                    :execution-control poll :observation-output-p nil)))
        (input
         (niwa-tool-exec--render-bash
          "WriteStdin" nil
          "<bash-execution execution_id=\"exec-1\" state=\"running\"/>"
          '(:status success :state running
                    :execution-control input :observation-output-p nil))))
    (should (eq t (plist-get hidden :hidden-p)))
    (should (plist-get terminal :hidden-p))
    (should-not (plist-get input :hidden-p)))
  :doc "distinguishes interrupted input and failed control labels"
  (let ((interrupt
         (niwa-tool-exec--render-bash
          "WriteStdin" '(:chars "\C-c") "" '(:status success)))
        (failed
         (niwa-tool-exec--render-bash
          "WriteStdin" '(:chars "") "invalid handle" '(:status error))))
    (should (string-match-p "interrupted process"
                            (plist-get interrupt :header)))
    (should (equal "WriteStdin: observation failed"
                   (plist-get failed :header)))
    (should-not (plist-get failed :hidden-p)))
  :doc "a collected command failure does not display as failed input"
  (let ((row (niwa-tool-exec--render-bash
              "WriteStdin" '(:chars "yes\n") "failed command"
              '(:status error :outcome failure :state completed
                        :execution-control input :control-succeeded-p t))))
    (should (equal "WriteStdin: sent input" (plist-get row :header))))
  :doc "marks stopped and lost executions explicitly"
  (let ((row (niwa-tool-exec--render-bash
              "Bash" '(:command "sleep 10") ""
              '(:state completed :termination stopped :outcome failure
                :status error))))
    (should (string-match-p " · stopped" (plist-get row :header)))
    (should (eq 'cancelled (plist-get row :status))))
  (should (string-match-p
           "timed out"
           (plist-get
            (niwa-tool-exec--render-bash
             "Bash" '(:command "tail -f x") ""
             '(:state completed :termination timed-out :outcome failure
               :status error))
            :header)))
  (should (eq 'error
              (plist-get
               (niwa-tool-exec--render-bash
                "Bash" '(:command "false") ""
                '(:state completed :termination exited :exit-code 1
                  :outcome failure :status error))
               :status)))
  (should (eq 'cancelled
              (plist-get (niwa-execution-transcript-terminal-render-data
                          '(:facts (:termination stopped :outcome failure)))
                         :status)))
  (should (eq 'failure
              (plist-get (niwa-execution-transcript-terminal-render-data
                          '(:facts (:termination stopped :outcome failure)))
                         :outcome)))
  (should (string-match-p
           "signaled"
           (plist-get
            (niwa-tool-exec--render-bash
             "Bash" '(:command "sleep 10") ""
             '(:state completed :termination signaled :exit-code 13
                      :outcome failure))
            :header)))
  (let ((metadata (niwa-tool-exec-format-execution-metadata
                   '(:state completed :termination signaled :exit-code 13
                            :outcome failure))))
    (should (string-match-p "signal 13" metadata))
    (should-not (string-match-p "exit 13" metadata)))
  (let ((metadata (niwa-tool-exec-format-execution-metadata
                   '(:state completed :termination interrupted :exit-code 2
                            :outcome failure))))
    (should (string-match-p "signal 2" metadata))
    (should-not (string-match-p "exit 2" metadata)))
  (let ((metadata (niwa-tool-exec-format-execution-metadata
                   '(:state completed :termination stopped :exit-code 15
                            :outcome failure))))
    (should-not (string-match-p "exit 15" metadata))
    (should-not (string-match-p "signal 15" metadata)))
  (should (string-match-p
           "lost execution"
           (plist-get
            (niwa-tool-exec--render-bash
             "Bash" '(:command "sleep 10") "" '(:state lost :status error))
            :header))))

(provide 'test-niwa-tool-exec)

;;; test-niwa-tool-exec.el ends here
