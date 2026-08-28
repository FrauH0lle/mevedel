;;; mevedel-tool-exec.el --- Bash and Eval tool adapters -*- lexical-binding: t -*-

;;; Commentary:

;; Bash and Eval schemas, lifecycle adapters, result formatting, rendering,
;; and registration.  `mevedel-execution' owns their operating-system children;
;; `mevedel-tool-exec-permission' and `mevedel-bash-policy' own authorization.

;;; Code:

;; `mevedel-utilities'
(declare-function mevedel--truncate-display
                  "mevedel-utilities" (text width &optional ellipsis))
(autoload 'mevedel--truncate-display "mevedel-utilities")

(eval-when-compile
  (require 'mevedel-tool-registry))

(require 'cl-lib)
(require 'mevedel-agent-control)
(require 'mevedel-agent-runtime)
(require 'mevedel-bash-analysis)
(require 'mevedel-bash-policy)
(require 'mevedel-execution)
(require 'mevedel-pipeline)
(require 'mevedel-sandbox)
(require 'mevedel-telemetry)
(require 'mevedel-tool-exec-permission)
(require 'mevedel-turn)
(require 'mevedel-workspace)
(require 'subr-x)
(require 'xml)

;; `gptel'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))
(defvar read-eval)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-enqueue-execution-result
                  "mevedel-agent-control" (session owner body))

;; `mevedel-agent-runtime'
(declare-function mevedel-agent-runtime-queue-execution-completion
                  "mevedel-agent-runtime"
                  (context owner body))

;; `mevedel-agents'
(defvar mevedel--agent-invocation)

;; `mevedel-bash-analysis'
(declare-function mevedel-bash-analysis-analyze
                  "mevedel-bash-analysis" (source))

;; `mevedel-execution'
(declare-function mevedel-execution-list
                  "mevedel-execution" (session owner))
(declare-function mevedel-execution-observe
                  "mevedel-execution"
                  (session owner execution-id callback &rest keys))
(declare-function mevedel-execution-start-bash
                  "mevedel-execution" (callback &rest keys))
(declare-function mevedel-execution-start-one-shot
                  "mevedel-execution" (callback &rest keys))
(declare-function mevedel-execution-stop
                  "mevedel-execution"
                  (session owner execution-id callback))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-environment
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-expand-path
                  "mevedel-execution-target" (target path &optional directory))
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt-attribution-line
                  "mevedel-interaction-prompt" (origin))

(autoload 'mevedel--prompt-attribution-line "mevedel-interaction-prompt")

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-active-tool-use-id
                  "mevedel-pipeline" ())
(declare-function mevedel-pipeline-tool-results-dir
                  "mevedel-pipeline" (session buffer &optional request))

;; `mevedel-sandbox'
(declare-function mevedel-sandbox-status-text "mevedel-sandbox" (facts))

;; `mevedel-structs'
(declare-function mevedel-request-p "mevedel-structs" (cl-x))
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-p "mevedel-structs" (cl-x))
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record-audit
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-tool-exec-permission'
(declare-function mevedel-tool-exec-permission-check-bash-async
                  "mevedel-tool-exec-permission" (tool-struct input cont))
(declare-function mevedel-tool-exec-permission-check-eval-async
                  "mevedel-tool-exec-permission" (tool-struct input cont))
(declare-function mevedel-tool-exec-permission-current-context
                  "mevedel-tool-exec-permission"
                  (tool-name args &optional session))
(declare-function mevedel-tool-exec-permission-default-directory
                  "mevedel-tool-exec-permission" ())
(declare-function mevedel-tool-exec-permission-effective-sandbox-request
                  "mevedel-tool-exec-permission"
                  (input tool-name detail &optional eval-mode
                         permission-context))
(declare-function mevedel-tool-exec-permission-eval-mode
                  "mevedel-tool-exec-permission" (args))
(declare-function mevedel-tool-exec-permission-eval-preserve-ui-p
                  "mevedel-tool-exec-permission" (args))
(declare-function mevedel-tool-exec-permission-session
                  "mevedel-tool-exec-permission" ())

;; `mevedel-turn'
(declare-function mevedel-current-origin "mevedel-turn" ())

;; `mevedel-utilities'
(declare-function mevedel--clamped-integer
                  "mevedel-utilities" (value default minimum maximum))
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))

;; `mevedel-workspace'
(declare-function mevedel--all-allowed-roots
                  "mevedel-workspace" (&optional buffer))

;; `xml'
(declare-function xml-escape-string "xml" (string))


;;

;;; Bash

(defun mevedel-tool-exec--bash-yield-time-ms (input)
  "Return the Bash yield time in milliseconds from INPUT.
The declared schema range is enforced by the input-repair pipeline
before the handler runs, so only the omitted-argument default remains
handler policy."
  (or (plist-get input :yield_time_ms) 10000))

(defun mevedel-tool-exec--write-wait-time-ms (input chars)
  "Return the observation wait from INPUT and CHARS.
The input-repair pipeline enforces the declared union range.  The
mode-dependent bounds stay here because they depend on CHARS: input
writes use 250-30000ms and pure polls use 5000-300000ms."
  (let ((input-p (and (stringp chars) (not (string-empty-p chars)))))
    (mevedel--clamped-integer (plist-get input :yield_time_ms)
                              (if input-p 250 5000)
                              (if input-p 250 5000)
                              (if input-p 30000 300000))))

(defun mevedel-tool-exec--execution-artifact-directory (session)
  "Return SESSION's retained execution artifact directory, if available."
  (unless (and (mevedel-session-execution-target session)
               (mevedel-execution-target-remote-p
                (mevedel-session-execution-target session)))
    (when-let* ((root
                 (mevedel-pipeline-tool-results-dir
                  session
                  (or (and (boundp 'mevedel--data-buffer)
                           mevedel--data-buffer)
                      (current-buffer)))))
      (file-name-concat root "executions"))))

(defun mevedel-tool-exec--sandbox-writable-roots (workdir)
  "Return writable child-confinement roots for WORKDIR."
  (let* ((session (and (boundp 'mevedel--session) mevedel--session))
         (target (and session (mevedel-session-execution-target session)))
         (remote (and target
                      (mevedel-execution-target-remote-p target)))
         (temporary-root
          (if remote
              (mevedel-execution-target-expand-path
               target
               (or (cdr (assoc "TMPDIR"
                               (mevedel-execution-target-environment target)))
                   "/tmp")
               workdir)
            temporary-file-directory))
         (roots
         (condition-case nil
              (mevedel--all-allowed-roots (current-buffer))
            (error nil))))
    (when remote
      (setq roots
            (cl-remove-if-not
             (lambda (root)
               (equal (file-remote-p root)
                      (file-remote-p workdir)))
             roots)))
    (delete-dups
     (append (or roots
                 (list (file-name-as-directory (expand-file-name workdir))))
             (list (file-name-as-directory
                    (expand-file-name temporary-root)))))))

(defconst mevedel-tool-exec--sandbox-recovery-guidance
  (concat
   "This command ran with network/path confinement. If confinement caused "
   "the failure, retry with `with_additional_permissions` and request only "
   "the required network or exact path capability. Use `require_escalated` "
   "only when additive permissions cannot represent the requirement.")
  "Model guidance appended to failed confined child results.")

(defun mevedel-tool-exec--sandbox-disclosure
    (text child-result &optional suppress-p failed-p)
  "Append CHILD-RESULT confinement facts and recovery guidance to TEXT.
SUPPRESS-P hides model-facing disclosure.  FAILED-P identifies a failed tool
operation rather than a successful or semantic non-error result."
  (let ((facts (plist-get child-result :sandbox-facts)))
    (cond
     ((not facts) text)
     (suppress-p
     (when (and (eq (plist-get facts :filesystem) 'unrestricted)
                (not (eq (plist-get facts :sandbox) 'unavailable)))
        (mevedel--warn-once
         'exec-skill-unconfined
         "Skill shell expansion ran without confinement: %s"
         (mevedel-sandbox-status-text facts)))
     text)
     (t
      (string-join
       (delq
        nil
        (list
         (unless (string-empty-p (or text "")) text)
         (when (plist-get facts :first-direct-fallback)
           "Confinement was unavailable, so this invocation ran directly.")
         (concat "[" (mevedel-sandbox-status-text facts) "]")
         (cond
          ((and failed-p (plist-get facts :refused))
           (concat
            "Confinement is required but unavailable. Only a new invocation "
            "with `require_escalated` and a justification can request direct "
            "execution."))
          ((and failed-p (eq (plist-get facts :sandbox) 'bubblewrap))
           mevedel-tool-exec--sandbox-recovery-guidance))))
       "\n\n")))))

(defun mevedel-tool-exec--execution-facts-xml (facts)
  "Return compact model-visible XML derived from canonical FACTS."
  (let (attributes)
    (dolist (entry '((:execution-id . "execution_id")
                     (:command . "command")
                     (:state . "state")
                     (:termination . "termination")
                     (:exit-code . "exit_code")
                     (:outcome . "outcome")
                     (:wall-time-seconds . "wall_time_seconds")
                     (:output-bytes . "output_bytes")
                     (:output-lines . "output_lines")
                     (:omitted-output-bytes . "omitted_output_bytes")
                     (:tty . "tty")
                     (:output-path . "output_path")))
      (let ((value (plist-get facts (car entry))))
        (when (or value (eq (car entry) :tty))
          (push
           (format "%s=\"%s\"" (cdr entry)
                   (xml-escape-string
                    (cond
                     ((eq value t) "true")
                     ((null value) "false")
                     ((floatp value) (format "%.3f" value))
                     (t (format "%s" value)))))
           attributes))))
    (format "<bash-execution %s/>" (string-join (nreverse attributes) " "))))

(defun mevedel-tool-exec-format-execution-metadata (facts)
  "Return compact UI metadata for execution FACTS."
  (string-join
   (delq nil
         (list
          (when-let* ((state (plist-get facts :state)))
            (symbol-name state))
          (when (plist-member facts :wall-time-seconds)
            (format "%.1fs" (or (plist-get facts :wall-time-seconds) 0)))
          (when (plist-member facts :output-lines)
            (format "%d lines" (or (plist-get facts :output-lines) 0)))
          (when (plist-member facts :output-bytes)
            (format "%d bytes" (or (plist-get facts :output-bytes) 0)))
          (plist-get facts :execution-id)))
   " · "))

(defun mevedel-tool-exec--bash-outcome (analysis exit-code termination)
  "Derive a canonical outcome from ANALYSIS, EXIT-CODE, and TERMINATION."
  (let* ((commands (plist-get analysis :commands))
         (command
          (and (= (length commands) 1)
               (not (memq (plist-get analysis :class) '(complex dangerous)))
               (caar commands)))
         (exit-one-outcome
          (pcase command
            ((or "grep" "rg") 'no-match)
            ("diff" 'different)
            ((or "test" "[") 'false))))
    (cond
     ((not (eq termination 'exited)) 'failure)
     ((and (integerp exit-code) (zerop exit-code)) 'success)
     ((and (equal exit-code 1) exit-one-outcome) exit-one-outcome)
     (t 'failure))))

(defun mevedel-tool-exec--observation-envelope
    (observation &optional suppress-sandbox-disclosure-p force-success-p)
  "Return a handler envelope for managed OBSERVATION.

SUPPRESS-SANDBOX-DISCLOSURE-P preserves trusted internal expansion behavior.
FORCE-SUCCESS-P makes control-tool completion successful independently of the
stopped command's outcome."
  (let* ((facts (plist-get observation :facts))
         (output (or (plist-get observation :output) ""))
         (error-data (plist-get observation :error))
         (failed-p
          (and (not force-success-p)
               (eq (plist-get facts :state) 'completed)
               (eq (plist-get facts :outcome) 'failure)))
         (body (if error-data
                   (format "Failed to start process: %s" error-data)
                 output))
         (with-sandbox
          (mevedel-tool-exec--sandbox-disclosure
           body observation suppress-sandbox-disclosure-p failed-p))
         (result
          (if suppress-sandbox-disclosure-p
              with-sandbox
            (concat with-sandbox
                    (unless (string-empty-p with-sandbox) "\n\n")
                    (mevedel-tool-exec--execution-facts-xml facts))))
         (status
          (cond
           (force-success-p 'success)
           ;; A start failure has no completed state to judge by; facts
           ;; alone would report the command successful with a "Failed
           ;; to start process" body.
           (error-data 'error)
           ((or (not (eq (plist-get facts :state) 'completed))
                (memq (plist-get facts :outcome)
                      '(success no-match different false)))
            'success)
           (t 'error))))
    (list :result result
          :status status
          :render-data
          (append (copy-sequence facts)
                  (list :sandbox-facts
                        (plist-get observation :sandbox-facts))))))

(defun mevedel-tool-exec-handle-execution-event (event owner-context)
  "Secure an independently completed Bash EVENT in its owner mailbox."
  (when (and (eq (plist-get event :type) 'terminal)
             (eq (plist-get event :delivery) 'mailbox))
    (let* ((args (plist-get event :tool-args))
           (observation (plist-get event :observation))
           (envelope
            (mevedel-tool-exec--observation-envelope
             observation
             (plist-get args :suppress-sandbox-disclosure-p))))
      (if (mevedel-session-p owner-context)
          (progn
            (mevedel-agent-control-enqueue-execution-result
             owner-context
             (plist-get event :owner)
             (plist-get envelope :result)))
        (mevedel-agent-runtime-queue-execution-completion
         owner-context
         (plist-get event :owner)
         (plist-get envelope :result))))))

(defun mevedel-tool-exec--bash (callback args)
  "Execute a Bash command and return its output.
CALLBACK receives the result envelope.  ARGS is a plist with :command."
  (let ((command (plist-get args :command))
        (tty (plist-get args :tty)))
    (unless (stringp command)
      (error "Parameter command is required"))
    (unless (memq tty '(nil t :json-false))
      (error "Parameter tty must be a boolean"))
    (let* ((analysis (mevedel-bash-analysis-analyze command))
           (_ (when (plist-get analysis :background-p)
                (error "Shell-native background execution is not supported; use yield_time_ms")))
           (session (mevedel-tool-exec-permission-session))
           (sandbox-request
            (mevedel-tool-exec-permission-effective-sandbox-request
             args "Bash" command nil
             (mevedel-tool-exec-permission-current-context
              "Bash" args session)))
           (invocation
            (and (boundp 'mevedel--agent-invocation)
                 mevedel--agent-invocation))
           (owner (mevedel-current-origin))
           (yield-time-ms
            (unless (plist-get args :wait-for-completion-p)
              (mevedel-tool-exec--bash-yield-time-ms args)))
           (workdir (mevedel-tool-exec-permission-default-directory)))
      (unless session
        (error "Bash requires an active session"))
      (mevedel-execution-start-bash
       (lambda (observation)
         (funcall
          callback
          (mevedel-tool-exec--observation-envelope
           observation (plist-get args :suppress-sandbox-disclosure-p))))
       :session session :data-buffer (current-buffer)
       :owner owner :request mevedel--current-request
       :owner-context (or invocation session)
       :tool-args args
       :tool-use-id (mevedel-pipeline-active-tool-use-id)
       :command (list "bash" "-lc" command)
       :workdir workdir
       :writable-roots (mevedel-tool-exec--sandbox-writable-roots workdir)
       :outcome-function
       (lambda (exit-code termination)
         (mevedel-tool-exec--bash-outcome analysis exit-code termination))
       :read-only-p (eq (plist-get analysis :class) 'read-only)
       :tty (eq tty t)
       :yield-time-ms yield-time-ms
       :artifact-directory
       (mevedel-tool-exec--execution-artifact-directory session)
       :additional-permissions
       (plist-get sandbox-request :additional-permissions)
       :sandbox-permissions
       (plist-get sandbox-request :sandbox-permissions)))))

(defun mevedel-tool-exec--write-stdin (callback args)
  "Poll or write to one owner-scoped yielded execution from ARGS."
  (let* ((execution-id (plist-get args :execution_id))
         (chars (or (plist-get args :chars) ""))
         (requested-yield-time-ms (plist-get args :yield_time_ms))
         (session (mevedel-tool-exec-permission-session))
         (owner (mevedel-current-origin))
         (wait-ms (mevedel-tool-exec--write-wait-time-ms args chars)))
    (unless (and (stringp execution-id) (not (string-empty-p execution-id)))
      (error "Parameter execution_id is required"))
    (unless (stringp chars)
      (error "Parameter chars must be a string"))
    (unless session
      (error "WriteStdin requires an active session"))
    (mevedel-telemetry-record-audit
     session 'execution-observe-requested
     :execution-id execution-id
     :owner owner
     :input-p (not (string-empty-p chars))
     :requested-yield-time-ms requested-yield-time-ms
     :effective-wait-ms wait-ms)
    (mevedel-execution-observe
     session owner execution-id
     (lambda (observation)
       (let* ((envelope
               (mevedel-tool-exec--observation-envelope observation))
              (render-data
               (copy-sequence (plist-get envelope :render-data))))
         (setq render-data
               (plist-put render-data :execution-control
                          (if (string-empty-p chars) 'poll 'input)))
         (setq render-data
               (plist-put
                render-data :observation-output-p
                (not (string-empty-p (or (plist-get observation :output)
                                         "")))))
         (funcall callback
                  (plist-put envelope :render-data render-data))))
     :chars chars :wait-ms wait-ms :request mevedel--current-request)))

(defun mevedel-tool-exec--list-executions (_args)
  "Return yielded executions visible to the current model owner."
  (let ((session (mevedel-tool-exec-permission-session))
        (owner (mevedel-current-origin)))
    (unless session
      (error "ListExecutions requires an active session"))
    (let* ((facts (mevedel-execution-list session owner))
           (result
            (if facts
                (mapconcat #'mevedel-tool-exec--execution-facts-xml facts "\n")
              "No yielded executions.")))
      (list :result result :status 'success))))

(defun mevedel-tool-exec--stop-execution (callback args)
  "Stop one owner-scoped yielded execution named by ARGS."
  (let ((execution-id (plist-get args :execution_id))
        (session (mevedel-tool-exec-permission-session))
        (owner (mevedel-current-origin)))
    (unless (and (stringp execution-id) (not (string-empty-p execution-id)))
      (error "Parameter execution_id is required"))
    (unless session
      (error "StopExecution requires an active session"))
    (mevedel-execution-stop
     session owner execution-id
     (lambda (observation)
       (funcall callback
                (mevedel-tool-exec--observation-envelope
                 observation nil t))))))


;;
;;; Eval

(defun mevedel-tool-exec--eval-format-result
    (result output result-format)
  "Format Eval RESULT and captured OUTPUT for RESULT-FORMAT."
  (if (equal result-format "injection")
      (concat
       (format "%S" result)
       (and (not (string-empty-p (or output "")))
            (format "\n\nSTDOUT:\n%s" output)))
    (concat
     (format "Result:\n%S" result)
     (and (not (string-empty-p (or output "")))
          (format "\n\nSTDOUT:\n%s" output)))))

(defun mevedel-tool-exec--eval-format-error (err output)
  "Format Eval error ERR and captured OUTPUT."
  (concat
   (format "Error: Eval failed with error %S: %S"
           (car err) (cdr err))
   (and (not (string-empty-p (or output "")))
        (format "\n\nSTDOUT:\n%s" output))))

(defun mevedel-tool-exec--eval-live (callback expression result-format preserve-ui)
  "Evaluate EXPRESSION in the live Emacs process.
CALLBACK receives the result envelope.  RESULT-FORMAT controls
the model-facing shape.  PRESERVE-UI restores the selected frame's
window configuration after evaluation."
  (let ((standard-output (generate-new-buffer " *mevedel-eval-elisp*"))
        (window-configuration (and preserve-ui
                                   (current-window-configuration)))
        (result nil) (output nil) response)
    (unwind-protect
        (condition-case err
            (let ((default-directory
                   (mevedel-tool-exec-permission-default-directory)))
              (setq result (eval (read expression) t))
              (when (> (buffer-size standard-output) 0)
                (setq output (with-current-buffer standard-output
                               (buffer-string))))
              (setq response
                    (mevedel-tool-exec--eval-format-result
                     result output result-format)))
          ((error user-error)
           (when (> (buffer-size standard-output) 0)
             (setq output (with-current-buffer standard-output
                            (buffer-string))))
           (setq response
                 (mevedel-tool-exec--eval-format-error err output))))
      (when (window-configuration-p window-configuration)
        (ignore-errors
          (set-window-configuration window-configuration)))
      (kill-buffer standard-output))
    (unless (equal result-format "injection")
      (setq response
            (concat
             response
             "\n\nExecution: Live Eval ran inside Emacs without child-process confinement.")))
    (funcall callback (list :result response))))

(defun mevedel-tool-exec--eval-batch-script
    (expression result-file workdir load-path-value result-format)
  "Return batch Eval source for EXPRESSION writing RESULT-FILE.
WORKDIR, LOAD-PATH-VALUE, and RESULT-FORMAT configure the child Emacs."
  (concat
   ";;; -*- lexical-binding: t -*-\n"
   (prin1-to-string
    `(let ((load-path ',load-path-value)
           (default-directory ,workdir)
           (expression ,expression)
           (result-file ,result-file)
           (result-format ',result-format)
           (stdout-buffer (generate-new-buffer " *mevedel-eval-batch-stdout*"))
           result output)
       (unwind-protect
           (let ((standard-output stdout-buffer))
             (condition-case err
                 (progn
                   (setq result (eval (read expression) t))
                   (setq output
                         (with-current-buffer stdout-buffer
                           (buffer-string)))
                   (with-temp-file result-file
                     (prin1 (list :status 'ok
                                  :text
                                  (if (equal result-format "injection")
                                      (concat
                                       (format "%S" result)
                                       (and (> (length (or output "")) 0)
                                            (format "\n\nSTDOUT:\n%s" output)))
                                    (concat
                                     (format "Result:\n%S" result)
                                     (and (> (length (or output "")) 0)
                                          (format "\n\nSTDOUT:\n%s" output)))))
                            (current-buffer))))
               ((error user-error)
                (setq output
                      (and (buffer-live-p stdout-buffer)
                           (with-current-buffer stdout-buffer
                             (buffer-string))))
                (with-temp-file result-file
                  (prin1 (list :status 'error
                               :text
                               (concat
                                (format "Error: Eval failed with error %S: %S"
                                        (car err) (cdr err))
                                (and (> (length (or output "")) 0)
                                     (format "\n\nSTDOUT:\n%s" output))))
                         (current-buffer))))))
         (when (buffer-live-p stdout-buffer)
           (kill-buffer stdout-buffer)))))))

(defun mevedel-tool-exec--eval-read-batch-result (result-file)
  "Read the batch Eval result plist from RESULT-FILE."
  (when (file-exists-p result-file)
    (with-temp-buffer
      (insert-file-contents result-file)
      (let ((read-eval nil))
        (read (current-buffer))))))

(defun mevedel-tool-exec--eval-batch
    (callback expression result-format additional-permissions
              &optional sandbox-permissions)
  "Evaluate EXPRESSION in a child process and call CALLBACK.
ADDITIONAL-PERMISSIONS is the validated additive execution profile.
SANDBOX-PERMISSIONS may be `require-escalated' after authorization."
  (let* ((workdir (mevedel-tool-exec-permission-default-directory))
         (session (mevedel-tool-exec-permission-session))
         (owner (mevedel-current-origin))
         (script-file (make-temp-file "mevedel-eval-batch-" nil ".el"))
         (result-file (make-temp-file "mevedel-eval-result-" nil ".el"))
         (script (mevedel-tool-exec--eval-batch-script
                  expression result-file workdir load-path result-format))
         cleaned
         (cleanup
          (lambda ()
            (unless cleaned
              (setq cleaned t)
              (ignore-errors (delete-file script-file))
              (ignore-errors (delete-file result-file))))))
    (condition-case err
        (progn
          (with-temp-file script-file
            (insert script))
          (mevedel-execution-start-one-shot
           (lambda (child-result)
             (let* ((exit-code (plist-get child-result :exit-code))
                    (diagnostics (plist-get child-result :output))
                    (payload
                     (condition-case nil
                         (mevedel-tool-exec--eval-read-batch-result
                          result-file)
                       (error nil)))
                    (success-p
                     (and (eq (plist-get payload :status) 'ok)
                          (integerp exit-code)
                          (zerop exit-code))))
               (unwind-protect
                   (funcall
                    callback
                    (list
                     :result
                     (mevedel-tool-exec--sandbox-disclosure
                      (cond
                       ((eq (plist-get payload :status) 'ok)
                        (or (plist-get payload :text) ""))
                       ((eq (plist-get payload :status) 'error)
                        (or (plist-get payload :text) "Error: Eval failed"))
                       ((plist-get child-result :error)
                        (format "Failed to start Eval batch process: %s"
                                (plist-get child-result :error)))
                       (t
                        (format
                         "Error: Eval batch process failed with exit code %d%s"
                         exit-code
                         (if (string-empty-p (or diagnostics ""))
                             ""
                           (format ":\n%s" diagnostics)))))
                      child-result nil
                      (not success-p))
                     :status (if success-p 'success 'error)))
                 (funcall cleanup))))
           :name "mevedel-eval-batch"
           :command
           (list (expand-file-name invocation-name invocation-directory)
                 "-Q" "--batch" "-l" script-file)
           :workdir workdir
           :writable-roots (mevedel-tool-exec--sandbox-writable-roots workdir)
           :additional-permissions additional-permissions
           :sandbox-permissions sandbox-permissions
           :session session
           :owner owner
           :teardown-function cleanup))
      (error
       (funcall cleanup)
       (funcall callback
                (list :result
                      (format "Failed to start Eval batch process: %s" err)
                      :status 'error))
       nil))))

(defun mevedel-tool-exec--eval (callback args)
  "Evaluate an Elisp expression and return the result.
CALLBACK receives the result envelope.  ARGS is a plist with :expression."
  (let ((expression (plist-get args :expression))
        (result-format (plist-get args :result-format))
        (mode (mevedel-tool-exec-permission-eval-mode args)))
    (unless (stringp expression)
      (error "Parameter expression is required"))
    (let* ((session (mevedel-tool-exec-permission-session))
           (request
            (mevedel-tool-exec-permission-effective-sandbox-request
             args "Eval" expression mode
             (mevedel-tool-exec-permission-current-context
              "Eval" args session))))
      (pcase mode
        ('live
         (mevedel-tool-exec--eval-live
          callback expression result-format
          (mevedel-tool-exec-permission-eval-preserve-ui-p args)))
        ('batch
         (mevedel-tool-exec--eval-batch
          callback expression result-format
          (plist-get request :additional-permissions)
          (plist-get request :sandbox-permissions)))))))


;;
;;; Renderers

(defun mevedel-tool-exec--render-bash (name args result render-data)
  "Rendering plist for the Bash tool.
NAME is \"Bash\".  ARGS carries `:command'.  RESULT is stdout/stderr.
Header shows a truncated first line of the command; body fontifies as
`sh-mode'."
  (when (stringp result)
    (let* ((write-stdin-p (equal name "WriteStdin"))
           (cmd (or (plist-get args :command) ""))
           (first-line (car (split-string cmd "\n")))
           (output
            (replace-regexp-in-string
             "\n*<bash-execution [^\n]*/>[ \t\r\n]*\\'" "" result))
           (body
            (if write-stdin-p
                output
              (concat "$ " cmd
                      (unless (string-empty-p output) "\n\n")
                      output)))
           (status (plist-get render-data :status))
           (state (plist-get render-data :state))
           (execution-id
            (or (plist-get render-data :execution-id)
                (plist-get args :execution_id)))
           (control
            (or (plist-get render-data :execution-control)
                (and write-stdin-p
                     (if (string-empty-p
                          (or (plist-get args :chars) ""))
                         'poll
                       'input))))
           (coalesce-key
            (and write-stdin-p
                 (eq status 'success)
                 (eq control 'poll)
                 (not (plist-get render-data :observation-output-p))
                 (stringp execution-id)
                 (format "WriteStdin:%s" execution-id)))
           (metadata
            (and state
                 (mevedel-tool-exec-format-execution-metadata
                  render-data))))
      (let ((rendering
             (list
              :header
              (concat
               (if write-stdin-p
                   (format "%s: %s"
                           (or name "WriteStdin")
                           (if (eq control 'poll)
                               "polled background process"
                             "sent input to background process"))
                 (format "%s: %s"
                         (or name "Bash")
                         (mevedel--truncate-display first-line 60 "...")))
               (and metadata (format " (%s)" metadata)))
              :body body
              :body-mode 'sh-mode
              :status status
              :hidden-p
              (and write-stdin-p
                   (eq status 'success)
                   (eq state 'running)
                   (eq control 'poll)
                   (not (plist-get render-data :observation-output-p)))
              :force-expanded-p
              (and (plist-get render-data :live-execution-p) t)
              :initially-collapsed-p
              (not (plist-get render-data :live-execution-p)))))
        (if coalesce-key
            (plist-put rendering :coalesce-key coalesce-key)
          rendering)))))

(defun mevedel-tool-exec--render-eval (name args result _render-data)
  "Return rendering plist for Eval NAME with ARGS and RESULT."
  (when (stringp result)
    (let* ((expression (or (plist-get args :expression) ""))
           (first-line (car (split-string expression "\n")))
           (mode (let ((raw (plist-get args :mode)))
                   (if (or (null raw)
                           (eq raw :json-false)
                           (and (stringp raw) (string-empty-p raw)))
                       "live"
                     raw)))
           (status (and (string-prefix-p "Error:" result) 'error)))
      (list :header (format "%s: %s %s"
                            (or name "Eval")
                            mode
                            (mevedel--truncate-display first-line 60 "..."))
            :body result
            :body-mode 'emacs-lisp-mode
            :status status
            :initially-collapsed-p t))))


;;
;;; Tool registration

(defun mevedel-tool-exec--register ()
  "Register Bash and Eval tools."
  (mevedel-define-tool
   :name "Bash"
   :description "Execute Bash commands."
   :prompt-file "prompts/tools/bash.md"
   :handler #'mevedel-tool-exec--bash
   :args ((command string :required
                   "The Bash command to execute from the session working directory. Can include pipes and standard shell operators.")
          (yield_time_ms integer :optional
                         "Milliseconds to wait before yielding a still-running command. Defaults to 10000; range 250-30000."
                         :minimum 250 :maximum 30000)
          (tty boolean :optional
               "Allocate a PTY and retain stdin for prompts or REPL input. Defaults to false.")
          (sandbox_permissions string :optional
                               "Child-execution authority: use_default, with_additional_permissions, or require_escalated for a complete confinement bypass."
                               :enum ["use_default"
                                      "with_additional_permissions"
                                      "require_escalated"])
          (additional_permissions object :optional
                                  "Capabilities requested in addition to the default confinement profile."
                                  :properties
                                  (:network
                                   (:type boolean
                                          :description "Allow network access for this invocation.")
                                   :file_system
                                   (:type object
                                          :description "Exact filesystem paths to reopen inside confinement."
                                          :properties
                                          (:read
                                           (:type array
                                                  :items (:type string)
                                                  :description "Absolute paths requiring read access.")
                                           :write
                                           (:type array
                                                  :items (:type string)
                                                  :description "Absolute paths requiring write access.")))))
          (justification string :optional
                         "Concise user-facing reason for a non-default permission request."))
   :async-p t
   :max-result-size 30000
   :groups (eval)
   :check-permission-async #'mevedel-tool-exec-permission-check-bash-async
   :get-pattern (lambda (input) (plist-get input :command))
   :renderer #'mevedel-tool-exec--render-bash)

  (mevedel-define-tool
   :name "WriteStdin"
   :description "Poll unread output or send input to a yielded Bash execution."
   :prompt-file "prompts/tools/writestdin.md"
   :handler #'mevedel-tool-exec--write-stdin
   :args ((execution_id string :required
                        "Opaque execution ID returned by Bash.")
          (chars string :optional
                 "Input to send. Omit or use an empty string to poll. Ordinary input requires a PTY; a single Ctrl-C character interrupts either mode.")
          (yield_time_ms integer :optional
                         "Wait before returning: polls default to 5000ms and clamp positive shorter waits to 5000ms (maximum 300000); input defaults to 250ms (250-30000)."
                         :minimum 250 :maximum 300000))
   :async-p t
   :max-result-size 30000
   :groups (eval)
   :check-permission
   (lambda (_tool args)
     (let ((chars (plist-get args :chars)))
       (if (or (null chars) (equal chars "") (equal chars "\C-c"))
           '(:outcome allow :raw-outcome allow :via execution-control)
         'allow)))
   :renderer #'mevedel-tool-exec--render-bash)

  (mevedel-define-tool
   :name "ListExecutions"
   :description "List yielded Bash executions owned by this agent."
   :prompt-file "prompts/tools/listexecutions.md"
   :handler #'mevedel-tool-exec--list-executions
   :args ()
   :read-only-p t
   :groups (eval))

  (mevedel-define-tool
   :name "StopExecution"
   :description "Stop one yielded Bash execution owned by this agent."
   :prompt-file "prompts/tools/stopexecution.md"
   :handler #'mevedel-tool-exec--stop-execution
   :args ((execution_id string :required
                        "Opaque execution ID returned by Bash."))
   :async-p t
   :max-result-size 30000
   :groups (eval)
   :check-permission
   (lambda (_tool _args)
     '(:outcome allow :raw-outcome allow :via execution-control))
   :renderer #'mevedel-tool-exec--render-bash)

  (mevedel-define-tool
   :name "Eval"
   :description "Evaluate an Elisp expression and return the result."
   :prompt-file "prompts/tools/eval.md"
   :handler #'mevedel-tool-exec--eval
   :args ((expression string :required "A single elisp sexp to evaluate with default-directory set to the session working directory.")
          (mode string :optional "Execution mode: live (default) evaluates in the current Emacs; batch evaluates in a child emacs --batch process."
                :enum ["live" "batch"])
          (preserve_ui boolean :optional "In live mode, restore the current window configuration after evaluation. Defaults to true.")
          (sandbox_permissions string :optional
                               "Batch child-execution authority: use_default, with_additional_permissions, or require_escalated for a complete confinement bypass."
                               :enum ["use_default"
                                      "with_additional_permissions"
                                      "require_escalated"])
          (additional_permissions object :optional
                                  "Capabilities requested in addition to default batch confinement."
                                  :properties
                                  (:network
                                   (:type boolean
                                          :description "Allow network access for this batch invocation.")
                                   :file_system
                                   (:type object
                                          :description "Exact filesystem paths to reopen inside confinement."
                                          :properties
                                          (:read
                                           (:type array
                                                  :items (:type string)
                                                  :description "Absolute paths requiring read access.")
                                           :write
                                           (:type array
                                                  :items (:type string)
                                                  :description "Absolute paths requiring write access.")))))
          (justification string :optional
                         "Concise user-facing reason for a non-default batch permission request."))
   :async-p t
   :max-result-size 30000
   :groups (eval)
   :check-permission-async #'mevedel-tool-exec-permission-check-eval-async
   :renderer #'mevedel-tool-exec--render-eval))

(provide 'mevedel-tool-exec)
;;; mevedel-tool-exec.el ends here
