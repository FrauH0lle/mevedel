;;; mevedel-tool-exec.el --- Bash and Eval tool adapters -*- lexical-binding: t -*-

;;; Commentary:

;; Bash and Eval lifecycle adapters, result formatting, and rendering.
;; Registration lives in `mevedel-tool-exec-registration'.
;; `mevedel-execution' owns their operating-system children;
;; `mevedel-tool-exec-permission' and `mevedel-bash-policy' own authorization.

;;; Code:

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
(declare-function mevedel-agent-control-notify
                  "mevedel-agent-control" (session key function))
(autoload 'mevedel-agent-control-notify "mevedel-agent-control")
(declare-function mevedel-agent-control-enqueue-execution-result
                  "mevedel-agent-control"
                  (session owner body &optional execution-id wake meta
                           acknowledgement))

;; `mevedel-agent-runtime'
(declare-function mevedel-agent-runtime-queue-execution-completion
                  "mevedel-agent-runtime"
                  (context owner body &optional meta acknowledgement))

;; `mevedel-agents'
(defvar mevedel--agent-invocation)

;; `mevedel-bash-analysis'
(declare-function mevedel-bash-analysis-analyze
                  "mevedel-bash-analysis" (source))
(declare-function mevedel-bash-analysis-prepare-read-only
                  "mevedel-bash-analysis" (source analysis &optional temporary-root))

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
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt-attribution-line
                  "mevedel-interaction-prompt" (origin))

(autoload 'mevedel--prompt-attribution-line "mevedel-interaction-prompt")

;; `mevedel-pending-inputs'
(declare-function mevedel-pending-inputs-offer-root-event
                  "mevedel-pending-inputs" (session &optional data-buffer))
(autoload 'mevedel-pending-inputs-offer-root-event "mevedel-pending-inputs")

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-active-permission-approvals
                  "mevedel-pipeline" ())
(declare-function mevedel-pipeline-active-tool-use-id
                  "mevedel-pipeline" ())
(declare-function mevedel-pipeline-tool-results-dir
                  "mevedel-pipeline"
                  (session buffer &optional request existing-only-p))

;; `mevedel-queue'
(declare-function mevedel-queue--current-session "mevedel-queue" ())
(autoload 'mevedel-queue--current-session "mevedel-queue")

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

;; `mevedel-turn'
(declare-function mevedel-current-origin "mevedel-turn" ())

;; `mevedel-utilities'
(declare-function mevedel--duration-label "mevedel-utilities" (seconds))
(autoload 'mevedel--duration-label "mevedel-utilities")
(declare-function mevedel--clamped-integer
                  "mevedel-utilities" (value default minimum maximum))
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))

;; `mevedel-view-composer'
(declare-function mevedel-view-call-preserving-window-configuration
                  "mevedel-view-composer" (thunk))
(autoload 'mevedel-view-call-preserving-window-configuration
  "mevedel-view-composer")

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

(defun mevedel-tool-exec--execution-artifact-directory (session read-only-p)
  "Return SESSION's retained execution artifact directory, if available.
For READ-ONLY-P commands, do not initialize storage or Git exclusions;
unsaved readers use the execution module's temporary spool instead."
  (unless (and (mevedel-session-execution-target session)
               (mevedel-execution-target-remote-p
                (mevedel-session-execution-target session)))
    (when-let* ((root
                 (mevedel-pipeline-tool-results-dir
                  session
                  (or (and (boundp 'mevedel--data-buffer)
                           mevedel--data-buffer)
                      (current-buffer))
                  nil read-only-p)))
      (file-name-concat root "executions"))))

(defun mevedel-tool-exec--sandbox-temporary-root (workdir)
  "Return the execution target's temporary directory for WORKDIR."
  (let* ((session (and (boundp 'mevedel--session) mevedel--session))
         (target (and session (mevedel-session-execution-target session))))
    (if (and target (mevedel-execution-target-remote-p target))
        (mevedel-execution-target-expand-path
         target
         (or (cdr (assoc "TMPDIR"
                         (mevedel-execution-target-environment target)))
             "/tmp")
         workdir)
      temporary-file-directory)))

(defun mevedel-tool-exec--sandbox-writable-roots (workdir)
  "Return writable child-confinement roots for WORKDIR."
  (let* ((session (and (boundp 'mevedel--session) mevedel--session))
         (target (and session (mevedel-session-execution-target session)))
         (remote (and target
                      (mevedel-execution-target-remote-p target)))
         (temporary-root (mevedel-tool-exec--sandbox-temporary-root workdir))
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
         (when (and failed-p (plist-get facts :git-common-directories))
           (format
            (concat "Linked Git shared metadata (objects and refs): %s. "
                    "A grant to only the worktree's index directory does not "
                    "include these locations. This location information does "
                    "not establish the cause of the child failure.")
            (string-join (plist-get facts :git-common-directories) ", ")))
         (cond
          ((and failed-p (eq (plist-get facts :sandbox) 'refused))
           (concat
            "The command did not run. Resolve the reported resource or "
            "grant-preparation problem before submitting a new invocation."))
          ((and failed-p (plist-get facts :refused))
           (concat
            "Confinement is required but unavailable. Only a new invocation "
            "with `require_escalated` and a justification can request direct "
            "execution."))
          ((and failed-p (eq (plist-get facts :sandbox) 'bubblewrap)
                (string-match-p
                 (concat "^Bad owner or permissions on /etc/ssh/ssh_config"
                         "\\(?:\\.d/[^\r\n]+\\)?\r?$")
                 (or text "")))
           (concat
            "OpenSSH rejected system SSH configuration ownership. "
            "Bubblewrap's user namespace can map host root-owned files to "
            "the overflow UID (often 65534); read access does not make that "
            "owner acceptable to OpenSSH. This diagnostic alone does not "
            "prove a namespace cause: compare ownership and a non-connecting "
            "`ssh -G` check on the host. Network or exact-path grants do not "
            "change UID mapping. If confirmed and the operation is still "
            "needed, submit a new Bash invocation with `require_escalated` "
            "and a justification, requesting invocation-only approval to run "
            "on the execution target outside confinement. Command approval "
            "still applies; no retry is automatic. Keep SSH ownership and "
            "configuration checks intact; do not bypass configuration or "
            "change its permissions to work around the namespace."))
          ((and failed-p (eq (plist-get facts :sandbox) 'bubblewrap))
           mevedel-tool-exec--sandbox-recovery-guidance))))
       "\n\n")))))

(defun mevedel-tool-exec--execution-facts-xml (facts)
  "Return compact model-visible XML derived from canonical FACTS."
  (let (attributes)
    (dolist (entry '((:execution-id . "execution_id")
                     (:kind . "kind")
                     (:description . "description")
                     (:expires-at . "expires_at")
                     (:event-count . "event_count")
                     (:command . "command")
                     (:state . "state")
                     (:termination . "termination")
                     (:exit-code . "exit_code")
                     (:pipeline-exit-codes . "pipeline_exit_codes")
                     (:pipeline-failed-stages . "pipeline_failed_stages")
                     (:pipeline-classification . "pipeline_classification")
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
                     ((listp value) (mapconcat #'number-to-string value " "))
                     ;; A model reads a calendar time, not epoch seconds.
                     ((eq (car entry) :expires-at)
                      (format-time-string "%FT%T%z" value))
                     ((floatp value) (format "%.3f" value))
                     (t (format "%s" value)))))
           attributes))))
    (format "<%s-execution %s/>"
            (if (eq (plist-get facts :kind) 'monitor) "monitor" "bash")
            (string-join (nreverse attributes) " "))))

(defun mevedel-tool-exec-format-execution-metadata (facts)
  "Return compact UI metadata for execution FACTS."
  (string-join
   (delq nil
         (list
          (when (eq (plist-get facts :kind) 'monitor)
            (format "Watching: %s" (or (plist-get facts :description) "")))
          (when-let* ((expires (plist-get facts :expires-at)))
            (format "expires %s" (format-time-string "%H:%M:%S" expires)))
          (when (plist-get facts :event-count)
            (format "%d events" (plist-get facts :event-count)))
          (when-let* ((state (plist-get facts :state)))
            (symbol-name (if (eq state 'completed)
                             (or (plist-get facts :outcome) state)
                           state)))
          (when (and (integerp (plist-get facts :exit-code))
                     (memq (plist-get facts :termination)
                           '(nil exited signaled interrupted)))
            (format "%s %d"
                    (if (memq (plist-get facts :termination)
                              '(signaled interrupted))
                        "signal" "exit")
                    (plist-get facts :exit-code)))
          (when-let* ((termination (plist-get facts :termination))
                      ((not (eq termination 'exited))))
            (symbol-name termination))
          (when (and (not (eq (plist-get facts :kind) 'monitor))
                     (plist-member facts :wall-time-seconds))
            (mevedel--duration-label (plist-get facts :wall-time-seconds)))
          (when (plist-member facts :output-lines)
            (format "%d lines" (or (plist-get facts :output-lines) 0)))
          (when (plist-member facts :output-bytes)
            (format "%d bytes" (or (plist-get facts :output-bytes) 0)))
          (when-let* ((workdir (plist-get facts :workdir)))
            (format "cwd %s" workdir))
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

(defun mevedel-tool-exec--pipeline-source (source marker &optional original-source)
  "Append a final outer pipeline snapshot to SOURCE using private MARKER.
No traps or shell options change.  Explicit exit/exec may bypass the snapshot.
Sources that can install EXIT traps are left untouched.  ORIGINAL-SOURCE,
when non-nil, supplies the authored source before trusted Git hardening."
  (if (not (mevedel-bash-analysis-snapshot-safe-p (or original-source source)))
      source
    (let ((variable (concat "__mevedel_" marker)))
      (concat source "\n"
              variable "=$? " variable "_stages=(\"${PIPESTATUS[@]}\")\n"
              "builtin printf -v " variable "_frame '%s ' \"${" variable "_stages[@]}\"\n"
              "builtin printf '\\036" marker ":%s\\037' \"$" variable "_frame\"\n"
              "exit \"$" variable "\""))))

(defun mevedel-tool-exec--pipeline-facts (source codes)
  "Return final pipeline facts for SOURCE and observed CODES.
Expected grep/diff/test statuses retain their ordinary Bash outcomes.
Upstream SIGPIPE is benign when a downstream stage succeeded."
  (let* ((stages (mevedel-bash-analysis-final-pipeline source (length codes)))
         (index 0)
         failures outcome)
    (if (null stages)
        '(:pipeline-classification unavailable)
      (dolist (code codes)
        (setq outcome (mevedel-tool-exec--bash-outcome (pop stages) code 'exited))
        (unless (or (memq outcome '(success no-match different false))
                    (and (= code 141) (memq 0 (nthcdr (1+ index) codes))))
          (push (1+ index) failures))
        (setq index (1+ index)))
      (list :pipeline-failed-stages (nreverse failures)
            :pipeline-outcome (if failures 'failure outcome)
            :pipeline-classification 'observed))))

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
                 (concat output
                         (when-let* ((stages (plist-get facts :pipeline-failed-stages)))
                           (format "\nPipeline failed at stage(s) %s (exit codes: %s)."
                                   (mapconcat #'number-to-string stages ", ")
                                   (mapconcat #'number-to-string
                                              (plist-get facts :pipeline-exit-codes) " "))))))
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
           ;; A start failure has no completed state to judge by; facts
           ;; alone would report the command successful with a "Failed
           ;; to start process" body.
           (error-data 'error)
           (force-success-p 'success)
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
                        (plist-get observation :sandbox-facts)
                        :model-result-envelope-p
                        (not suppress-sandbox-disclosure-p))))))

(defun mevedel-tool-exec--facts-meta (facts &rest keys)
  "Return the mailbox metadata plist of FACTS restricted to KEYS.
Values are plain strings and integers; unknown or nil facts are omitted."
  (let (meta)
    (dolist (key keys)
      (when-let* ((value (plist-get facts key)))
        (setq meta
              (append meta
                      (list key
                            (cond ((memq key '(:termination :outcome))
                                   (symbol-name value))
                                  ((eq key :command)
                                   (truncate-string-to-width value 120 nil nil "…"))
                                  (t value)))))))
    meta))

(defun mevedel-tool-exec--enqueue-monitor-lines
    (owner-context facts lines omitted wake &optional terminal acknowledgement)
  "Queue each framed monitor line of LINES as its own record for OWNER-CONTEXT.
OMITTED lines are summarized on the last record, whose model text carries the
original omission note.  FACTS identify the monitor; WAKE marks every record.
TERMINAL, an entry (BODY META t), follows the lines.  All records share one
durable commit, so a failed flush leaves nothing queued and a retry cannot
duplicate events.  ACKNOWLEDGEMENT, if given, is passed to the mailbox and
runs once when the commit is secured or has failed."
  (let ((entries nil))
    (when (and (null lines) (> omitted 0))
      (setq lines (list "")))
    (while lines
      (let* ((line (pop lines))
             (omit (and (null lines) (> omitted 0))))
        (push (list (if omit
                        (format "%s\n\n[%d more event lines in the output file]"
                                line omitted)
                      line)
                    (and omit (list :omitted omitted)))
              entries)))
    (when terminal
      (push terminal entries))
    (mevedel-agent-control-enqueue-monitor-events
     owner-context (plist-get facts :execution-id)
     (or (plist-get facts :description) "") (nreverse entries) wake
     acknowledgement)))

(defun mevedel-tool-exec--secured-acknowledgement
    (acknowledgement owner-context data-buffer wake)
  "Return an acknowledgement that forwards to ACKNOWLEDGEMENT, then wakes.
On success the producer's record is secured, ACKNOWLEDGEMENT settles the
execution, and only then, when WAKE, is OWNER-CONTEXT's root offered the
event for DATA-BUFFER.  The offer goes through the session's coalesced
notifier, never the mutation preparation queue: it runs at once when nothing
is blocked and otherwise after the gate, and offers for one buffer coalesce.
The notifier contains a failing wake, so it never turns a secured delivery
into a failure.  The facade is always invoked; the offer itself rechecks at
run time and never wakes a session that is stopping."
  (lambda (success error)
    (funcall acknowledgement success error)
    (when (and success wake)
      (mevedel-agent-control-notify
       owner-context (list 'root-wake data-buffer)
       (lambda ()
         (unless (mevedel-session-publication-stopping-p owner-context)
           (mevedel-pending-inputs-offer-root-event
            owner-context data-buffer)))))))

(defun mevedel-tool-exec-handle-execution-event
    (event owner-context acknowledgement)
  "Secure an independently delivered execution EVENT in its owner mailbox.
ACKNOWLEDGEMENT, called with SUCCESS and ERROR exactly once, runs after the
record is durable or has definitively failed.  Return `pending' when it will
run later, else t after it ran.  A yielded root result that ended on its own
wakes an idle model once secured; a stopped one does not."
  (pcase (plist-get event :type)
    ('monitor-batch
     (let ((facts (plist-get event :facts)))
       (mevedel-tool-exec--delivery-result
        (mevedel-tool-exec--enqueue-monitor-lines
         owner-context facts (plist-get event :monitor-lines)
         (or (plist-get event :monitor-omitted) 0) t nil
         (mevedel-tool-exec--secured-acknowledgement
          acknowledgement owner-context (plist-get event :data-buffer) t)))))
    ((and 'terminal (guard (eq (plist-get event :delivery) 'mailbox)))
     (let* ((args (plist-get event :tool-args))
            (facts (plist-get event :facts))
            (observation (plist-get event :observation))
            (meta (mevedel-tool-exec--facts-meta
                   facts :execution-id :description :command :termination
                   :outcome :exit-code :output-path :output-bytes
                   :omitted-output-bytes))
            (root-p (and (mevedel-session-p owner-context)
                         (equal (plist-get event :owner) "/root")))
            (wake (and root-p
                       (not (eq (plist-get facts :termination) 'stopped))))
            (secured
             (mevedel-tool-exec--secured-acknowledgement
              acknowledgement owner-context (plist-get event :data-buffer)
              wake)))
       (mevedel-tool-exec--delivery-result
        (cond
         ((eq (plist-get facts :kind) 'monitor)
          (mevedel-tool-exec--enqueue-monitor-lines
           owner-context facts (plist-get event :monitor-lines)
           (or (plist-get event :monitor-omitted) 0) wake
           (list (mevedel-tool-exec--monitor-terminal-body event)
                 (mevedel-tool-exec--facts-meta
                  facts :event-count :termination :outcome :exit-code)
                 t)
           secured))
         ((mevedel-session-p owner-context)
          (mevedel-agent-control-enqueue-execution-result
           owner-context
           (plist-get event :owner)
           (plist-get
            (mevedel-tool-exec--observation-envelope
             observation (plist-get args :suppress-sandbox-disclosure-p))
            :result)
           (plist-get facts :execution-id) wake meta secured))
         (t
          (mevedel-agent-runtime-queue-execution-completion
           owner-context
           (plist-get event :owner)
           (plist-get
            (mevedel-tool-exec--observation-envelope
             observation (plist-get args :suppress-sandbox-disclosure-p))
            :result)
           meta secured))))))
    (_
     (funcall acknowledgement nil "Unsupported mailbox event")
     nil)))

(defun mevedel-tool-exec--delivery-result (result)
  "Return `pending' when mailbox RESULT awaits acknowledgement, else t."
  (if (eq result 'pending) 'pending t))

(defun mevedel-tool-exec--monitor-terminal-body (event)
  "Return EVENT's terminal notice."
  (let* ((facts (plist-get event :facts))
         (termination (plist-get facts :termination))
         (notice
          (pcase termination
            ('timed-out "timed out; re-arm explicitly to keep watching")
            ('flood "stopped for flooding; restart with a tighter filter")
            ('stopped "stopped")
            ('exited (format "exited with code %s" (plist-get facts :exit-code)))
            (_ (format "ended (%s)" termination)))))
    (format "Monitor %s: %s (%d events)."
            (or (plist-get facts :description) "") notice
            (or (plist-get facts :event-count) 0))))

(defun mevedel-tool-exec--monitor (callback args)
  "Start a root-owned stdout event monitor from ARGS; return via CALLBACK."
  (unless (equal (mevedel-current-origin) "/root")
    (error "Monitor is available only to /root"))
  (when (plist-member args :tty)
    (error "Monitor does not support tty"))
  (mevedel-tool-exec--bash callback args 'monitor))

(defun mevedel-tool-exec--bash (callback args &optional kind)
  "Execute a Bash command and return its output.
CALLBACK receives the envelope.  ARGS has :command; KIND may be `monitor'."
  (let ((command (plist-get args :command))
        (tty (plist-get args :tty)))
    (unless (stringp command)
      (error "Parameter command is required"))
    (unless (memq tty '(nil t :json-false))
      (error "Parameter tty must be a boolean"))
    (let* ((analysis (mevedel-bash-analysis-analyze command))
           (_ (when (plist-get analysis :background-p)
                (error "Shell-native background execution is not supported; use yield_time_ms")))
           (read-only-p (eq (plist-get analysis :class) 'read-only))
           (session (mevedel-queue--current-session))
           (workdir (mevedel-tool-exec-permission-default-directory))
           (temporary-root (mevedel-tool-exec--sandbox-temporary-root workdir))
           (pipeline-marker (make-temp-name "pipeline_"))
           (launch-command
            (if read-only-p
                ;; -p suppresses startup files and inherited shell functions
                ;; and shell-option variables.  It grants no OS authority.
                (list "bash" "--noprofile" "--norc" "-p" "-c"
                      (mevedel-tool-exec--pipeline-source
                       (mevedel-bash-analysis-prepare-read-only
                        command analysis
                        (mevedel-execution-target-native-path
                         (and session (mevedel-session-execution-target session))
                         temporary-root))
                       pipeline-marker command))
              (list "bash" "-lc"
                    (mevedel-tool-exec--pipeline-source command pipeline-marker))))
           (sandbox-request
            (mevedel-tool-exec-permission-effective-sandbox-request
             args "Bash" command nil
             (mevedel-tool-exec-permission-current-context
              "Bash" args session)))
           (invocation
            mevedel--agent-invocation)
           (owner (mevedel-current-origin))
           (yield-time-ms
            (unless (plist-get args :wait-for-completion-p)
              (mevedel-tool-exec--bash-yield-time-ms args))))
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
       :kind (or kind 'bash) :description (plist-get args :description)
       :timeout-ms (or (plist-get args :timeout_ms) 300000)
       :owner-context (or invocation session)
       :tool-args args
       :tool-use-id (mevedel-pipeline-active-tool-use-id)
       :permission-approvals (mevedel-pipeline-active-permission-approvals)
       :command launch-command
       :pipeline-marker pipeline-marker
       :pipeline-facts-function
       (lambda (codes) (mevedel-tool-exec--pipeline-facts command codes))
       :workdir workdir
       :writable-roots (mevedel-tool-exec--sandbox-writable-roots workdir)
       :temporary-root temporary-root
       :outcome-function
       (lambda (exit-code termination)
         (mevedel-tool-exec--bash-outcome analysis exit-code termination))
       :read-only-p read-only-p
       :tty (eq tty t)
       :yield-time-ms yield-time-ms
       :artifact-directory
       (mevedel-tool-exec--execution-artifact-directory session read-only-p)
       :additional-permissions
       (plist-get sandbox-request :additional-permissions)
       :sandbox-permissions
       (plist-get sandbox-request :sandbox-permissions)))))

(defun mevedel-tool-exec--write-stdin (callback args)
  "Poll or write to one owner-scoped yielded execution from ARGS."
  (let* ((execution-id (plist-get args :execution_id))
         (chars (or (plist-get args :chars) ""))
         (requested-yield-time-ms (plist-get args :yield_time_ms))
         (session (mevedel-queue--current-session))
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
       ;; The model now holds the terminal result; its mailbox copy must
       ;; not wake it again.
       (when (eq (plist-get (plist-get observation :facts) :state) 'completed)
         (mevedel-agent-control-clear-wake session owner execution-id))
       (let* ((envelope
               ;; Keep the model-visible tool status unchanged.  Presentation
               ;; separately distinguishes a collected command failure from
               ;; a failed control operation.
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
         (setq render-data
               (plist-put render-data :control-succeeded-p
                          (not (plist-get observation :error))))
         (funcall callback
                  (plist-put envelope :render-data render-data))))
     :chars chars :wait-ms wait-ms :request mevedel--current-request)))

(defun mevedel-tool-exec--list-executions (_args)
  "Return yielded executions visible to the current model owner."
  (let ((session (mevedel-queue--current-session))
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
        (session (mevedel-queue--current-session))
        (owner (mevedel-current-origin)))
    (unless (and (stringp execution-id) (not (string-empty-p execution-id)))
      (error "Parameter execution_id is required"))
    (unless session
      (error "StopExecution requires an active session"))
    (mevedel-execution-stop
     session owner execution-id
     (lambda (observation)
       (mevedel-agent-control-clear-wake session owner execution-id)
       (let ((envelope (mevedel-tool-exec--observation-envelope
                        observation nil t)))
         (funcall callback
                  (plist-put
                   envelope :render-data
                   (plist-put (plist-get envelope :render-data)
                              :execution-control 'stop))))))))


;;
;;; Eval

(defun mevedel-tool-exec--eval-format-result
    (result output result-format)
  "Format Eval RESULT and captured OUTPUT for RESULT-FORMAT.
Shared and circular structure prints once with #N= labels: live objects
such as requests share so much structure that plain printing grows without
bound and exhausted memory."
  (let ((print-circle t))
    (if (equal result-format "injection")
        (concat
         (format "%S" result)
         (and (not (string-empty-p (or output "")))
              (format "\n\nSTDOUT:\n%s" output)))
      (concat
       (format "Result:\n%S" result)
       (and (not (string-empty-p (or output "")))
            (format "\n\nSTDOUT:\n%s" output))))))

(defun mevedel-tool-exec--eval-format-error (err output)
  "Format Eval error ERR and captured OUTPUT."
  (let ((print-circle t))
    (concat
     (format "Error: Eval failed with error %S: %S"
             (car err) (cdr err))
     (and (not (string-empty-p (or output "")))
          (format "\n\nSTDOUT:\n%s" output)))))

(defun mevedel-tool-exec--eval-live (callback expression result-format preserve-ui)
  "Evaluate EXPRESSION in the live Emacs process.
CALLBACK receives the result envelope.  RESULT-FORMAT controls
the model-facing shape.  PRESERVE-UI restores the selected frame's
window configuration after evaluation."
  (let ((standard-output (generate-new-buffer " *mevedel-eval-elisp*"))
        (result nil) (output nil) response)
    (unwind-protect
        (condition-case err
            (let ((default-directory
                   (mevedel-tool-exec-permission-default-directory)))
              (setq result
                    (if preserve-ui
                        (mevedel-view-call-preserving-window-configuration
                         (lambda () (eval (read expression) t)))
                      (eval (read expression) t)))
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
           (print-circle t)
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
         (session (mevedel-queue--current-session))
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
           :temporary-root (mevedel-tool-exec--sandbox-temporary-root workdir)
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
    (let* ((session (mevedel-queue--current-session))
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
Header shows the command and minimal state; the expanded body retains
the full command, bounded output and execution details."
  (when (stringp result)
    (let* ((write-stdin-p (equal name "WriteStdin"))
           (stop-p (equal name "StopExecution"))
           (cmd (or (plist-get args :command) ""))
           (first-line (car (split-string cmd "\n")))
           (control
            (or (plist-get render-data :execution-control)
                (and write-stdin-p
                     (if (string-empty-p (or (plist-get args :chars) ""))
                         'poll 'input))
                (and stop-p 'stop)))
           (status (if (and (memq control '(input poll))
                            (plist-get render-data :control-succeeded-p))
                       'success
                     (plist-get render-data :status)))
           (state (plist-get render-data :state))
           (facts (and state
                       (mevedel-tool-exec-format-execution-metadata
                        render-data)))
           (body
            (unless (bound-and-true-p mevedel-tool-render-summary-only)
              (let ((output
                     (if (and (plist-get render-data :model-result-envelope-p)
                              (not (plist-member render-data :execution-output))
                              (not (plist-get render-data :live-execution-p)))
                         (replace-regexp-in-string
                          "\n*<\\(?:bash\\|monitor\\)-execution [^\n]*/>[ \t\r\n]*\\'" "" result)
                       ;; Canonical output and unmarked results are not tool
                       ;; envelopes, even if the command printed this XML.
                       result)))
                (string-join
                 (delq nil
                       (list (unless (or write-stdin-p stop-p)
                               (concat "$ " cmd))
                             (and (eq control 'input)
                                  (format "Submitted input:\n%s"
                                          (plist-get args :chars)))
                             (when-let* ((execution-error
                                          (plist-get render-data :execution-error)))
                               (format "%s: %s"
                                       (if (eq (plist-get render-data :termination)
                                               'spawn-failed)
                                           "Failed to start process"
                                         "Execution error")
                                       execution-error))
                             (unless (string-empty-p output) output)
                             (and facts (format "Details: %s" facts))
                             (when-let* ((path (plist-get render-data :output-path)))
                               (format "Retained output: %s" path))))
                 "\n\n"))))
           (stopped-p (and (eq state 'completed)
                           (memq (plist-get render-data :termination)
                                 '(stopped owner-stopped interrupted))))
           (label
            (cond
             ((eq state 'lost) "lost execution")
             ((and (eq state 'completed)
                   (eq (plist-get render-data :termination) 'signaled))
              "signaled")
             (stopped-p "stopped")
             ((and (eq state 'completed)
                   (eq (plist-get render-data :termination) 'timed-out))
              "timed out")
             ((eq state 'unknown) "completion unavailable")
             ((eq state 'running)
              (if (eq (plist-get render-data :kind) 'monitor) "watching" "running"))
             ((eq state 'completed)
              (if (eq (plist-get render-data :outcome) 'failure)
                  "failed" "finished"))
             ((eq status 'error) "failed")
             (state (symbol-name state)))))
      (list :header
            (cond
             ((eq control 'poll)
              (format "WriteStdin: observation %s"
                      (if (and (eq status 'error)
                               (not (plist-get render-data :control-succeeded-p)))
                          "failed" (or label "finished"))))
             ((eq control 'input)
              (format "WriteStdin: %s%s"
                      (if (equal (plist-get args :chars) "\C-c")
                          "interrupted process" "sent input")
                      (if (and (eq status 'error)
                               (not (plist-get render-data :control-succeeded-p)))
                          " · failed" "")))
             (stop-p (format "StopExecution: %s"
                             (if (eq status 'error) "failed"
                               (or label "stopped"))))
             (t (concat
                 (if (eq (plist-get render-data :kind) 'monitor)
                     (format "Watching: %s" (or (plist-get render-data :description)
                                               (plist-get args :description) ""))
                   (format "%s: %s" (or name "Bash")
                           (mevedel--truncate-display first-line 60 "...")))
                 (and label (format " · %s" label))
                 (when (and (not (eq (plist-get render-data :kind) 'monitor))
                            (plist-member render-data :wall-time-seconds))
                   (format " · %s" (mevedel--duration-label
                                    (plist-get render-data :wall-time-seconds))))
                 (when (or (plist-get render-data :output-preview-truncated-p)
                           (> (or (plist-get render-data :omitted-output-bytes) 0) 0))
                   " · output truncated"))))
            :body body
            :body-mode 'sh-mode
            ;; The call that started a still-live command succeeded, but the
            ;; command has not: its row must not read as finished.
            ;; A requested stop is neutral, not a failure; the outcome fact
            ;; stays `failure' for the model.
            :status (cond
                     ((and (null control) (eq state 'running)
                           (not (memq status '(error failed denied blocked))))
                      'running)
                     ((and stopped-p (eq status 'error)) 'cancelled)
                     (t status))
            :execution-control-id
            (and (memq control '(input stop))
                 (or (plist-get render-data :execution-id)
                     (plist-get args :execution_id)))
            :hidden-p (and (eq control 'poll)
                           (or (eq status 'success)
                               (plist-get render-data :control-succeeded-p)))
            :initially-collapsed-p t))))

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

(provide 'mevedel-tool-exec)
;;; mevedel-tool-exec.el ends here
