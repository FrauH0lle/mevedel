;;; mevedel-execution-telemetry.el -- Execution telemetry adapter -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the opaque per-execution telemetry context, privacy-safe event
;; projection, sandbox summaries, Eask profiler recognition, and optional GNU
;; time resource capture.  The context retains live ownership provenance and
;; mutable aggregation cells, but never observes live process records.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-sandbox-summary-cell
                  "mevedel-agents" (cl-x) t)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))
(declare-function mevedel-execution-target-prefix
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))

;; `mevedel-structs'
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-finish
                  "mevedel-telemetry" (span &rest props))
(declare-function mevedel-telemetry-forwarded-audit-p
                  "mevedel-telemetry" (session))
(declare-function mevedel-telemetry-profiler-directory
                  "mevedel-telemetry" (session))
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))
(declare-function mevedel-telemetry-record-audit
                  "mevedel-telemetry" (session event &rest props))
(declare-function mevedel-telemetry-start
                  "mevedel-telemetry" (session event &rest props))

(require 'mevedel-telemetry)

(defconst mevedel-execution-telemetry--audit-prop-keys
  '(:additional-read-count :additional-write-count
    :after-confined-launch-failure :cache-identity :chunk-bytes :command-hash
    :duration-ms :exit-code :fallback-offered :fallback-possible :filesystem
    :full-execution-approval-offered :lane :launch-failure-reason-class
    :launch-failure-stage :native-resource-capture
    :native-resource-report-bytes :network :output-bytes :output-limit
    :overlap-count :preparation-state :proc :protected-path-count :queue-depth
    :queue-duration-ms :reason-class :sandbox :termination :test-scope
    :timed-out :tty :workload :yield-time-ms)
  "Execution properties allowed in a side conversation's durable audit.")

(defvar mevedel-execution-telemetry--resource-capture-claims
  (make-hash-table :test #'equal :weakness 'key)
  "Weak set of native resource report paths claimed by live runs.")

(defvar mevedel-execution-telemetry--fallback-sessions
  (make-hash-table :test #'eq :weakness 'key)
  "Weak set of sessions that already reported direct sandbox fallback.")

(defvar mevedel-execution-telemetry-summary-cell nil
  "Dynamically bound cell collecting child confinement for one tool call.")

(cl-defstruct (mevedel-execution-telemetry--context
               (:constructor mevedel-execution-telemetry--context-create))
  "Private telemetry state for one execution."
  agent-summary-cell
  execution-id
  owner
  pipeline-summary-cell
  resource-report-path
  sandbox-summary-cell
  session
  span
  tool-use-id)

(cl-defun mevedel-execution-telemetry-context-create
    (&key session execution-id tool-use-id owner invocation
          pipeline-summary-cell span-event span-properties)
  "Create an opaque telemetry context for one execution."
  (when invocation (require 'mevedel-agents))
  (mevedel-execution-telemetry--context-create
   :agent-summary-cell
   (and invocation
        (mevedel-agent-invocation-p invocation)
        (mevedel-agent-invocation-sandbox-summary-cell invocation))
   :execution-id execution-id
   :owner owner
   :pipeline-summary-cell pipeline-summary-cell
   :sandbox-summary-cell (list nil)
   :session session
   :span (and span-event session
              (apply #'mevedel-telemetry-start
                     session span-event span-properties))
   :tool-use-id tool-use-id))

(defun mevedel-execution-telemetry-context-summary (context)
  "Return a copy of CONTEXT's sandbox summary."
  (copy-tree
   (car (mevedel-execution-telemetry--context-sandbox-summary-cell context))))

(defun mevedel-execution-telemetry-context-properties (context)
  "Return profiler properties currently available from CONTEXT."
  (when-let* ((report
               (mevedel-execution-telemetry--context-resource-report-path
                context)))
    (append
     (list :native-resource-capture t
           :resource-report-relative-path
           (file-name-concat
            "diagnostics"
            (file-name-nondirectory
             (directory-file-name (file-name-directory report)))
            (file-name-nondirectory report)))
     (when (file-readable-p report)
       (list :native-resource-report-bytes
             (file-attribute-size (file-attributes report)))))))

(defun mevedel-execution-telemetry-finish (context &rest properties)
  "Finish CONTEXT's optional telemetry span with PROPERTIES."
  (when-let* ((span (mevedel-execution-telemetry--context-span context)))
    (apply #'mevedel-telemetry-finish span properties)))

(defun mevedel-execution-telemetry-safe-facts (facts)
  "Return the non-sensitive confinement subset of sandbox FACTS."
  (list :sandbox (plist-get facts :sandbox)
        :filesystem (plist-get facts :filesystem)
        :network (plist-get facts :network)
        :proc (plist-get facts :proc)
        :protected-path-count (plist-get facts :protected-paths)
        :additional-read-count (plist-get facts :additional-filesystem-read)
        :additional-write-count (plist-get facts :additional-filesystem-write)))

(defun mevedel-execution-telemetry--merge-sandbox-summary
    (summary facts started-p refused-p)
  "Merge one logical child attempt into SUMMARY.
FACTS contains non-sensitive confinement facts.  STARTED-P records whether the
requested command started, and REFUSED-P records a policy refusal."
  (let* ((attempts (1+ (or (plist-get summary :attempt-count) 0)))
         (started (+ (or (plist-get summary :started-count) 0)
                     (if started-p 1 0)))
         (refused (+ (or (plist-get summary :refused-count) 0)
                     (if refused-p 1 0)))
         (sandbox (plist-get facts :sandbox))
         (filesystem (plist-get facts :filesystem))
         (network (plist-get facts :network))
         (proc (plist-get facts :proc))
         (current-sandbox (plist-get summary :sandbox))
         (current-filesystem (plist-get summary :filesystem))
         (current-network (plist-get summary :network)))
    (list
     :attempt-count attempts
     :started-count started
     :refused-count refused
     :sandbox
     (cond
      ((eq current-sandbox 'refused) current-sandbox)
      ((eq sandbox 'refused) sandbox)
      ((and current-sandbox (not (eq current-sandbox 'bubblewrap)))
       current-sandbox)
      (sandbox sandbox)
      (t current-sandbox))
     :filesystem
     (cond
      ((or (eq current-filesystem 'unrestricted)
           (eq filesystem 'unrestricted))
       'unrestricted)
      ((or (eq current-filesystem 'unavailable)
           (eq filesystem 'unavailable))
       'unavailable)
      (filesystem filesystem)
      (t current-filesystem))
     :network
     (cond
      ((or (eq current-network 'unrestricted)
           (eq network 'unrestricted))
       'unrestricted)
      ((or (eq current-network 'unavailable)
           (eq network 'unavailable))
       'unavailable)
      (network network)
      (t current-network))
     :proc (if (or (eq (plist-get summary :proc) 'host)
                   (eq proc 'host))
               'host
             (or proc (plist-get summary :proc)))
     :additional-read-count
     (+ (or (plist-get summary :additional-read-count) 0)
        (or (plist-get facts :additional-filesystem-read) 0))
     :additional-write-count
     (+ (or (plist-get summary :additional-write-count) 0)
        (or (plist-get facts :additional-filesystem-write) 0)))))

(defun mevedel-execution-telemetry-record-sandbox-attempt
    (context facts started-p refused-p)
  "Record one logical child attempt from FACTS in CONTEXT."
  (let (seen)
    (dolist (cell
             (list
              (mevedel-execution-telemetry--context-sandbox-summary-cell
               context)
              (mevedel-execution-telemetry--context-pipeline-summary-cell
               context)
              (mevedel-execution-telemetry--context-agent-summary-cell
               context)))
      (when (and (consp cell) (not (memq cell seen)))
        (push cell seen)
        (setcar cell
                (mevedel-execution-telemetry--merge-sandbox-summary
                 (car cell) facts started-p refused-p))))))

(defun mevedel-execution-telemetry-sandbox-summary-class (summary)
  "Return `warning' when SUMMARY describes a material deviation."
  (and summary
       (let ((attempts (or (plist-get summary :attempt-count) 0)))
         (when
             (or (> (or (plist-get summary :additional-write-count) 0) 0)
                 (> (or (plist-get summary :refused-count) 0) 0)
                 (< (or (plist-get summary :started-count) 0) attempts)
                 (not (eq (plist-get summary :sandbox) 'bubblewrap))
                 (not (eq (plist-get summary :filesystem) 'workspace-write))
                 (not (eq (plist-get summary :network) 'isolated))
                 (eq (plist-get summary :proc) 'host))
           'warning))))

(defun mevedel-execution-telemetry--eask-command-p (command)
  "Return non-nil when COMMAND invokes Eask directly or through npx."
  (and (stringp command)
       (string-match-p
        (concat
         "\\(?:^\\|[;&|][[:space:]]*\\)"
         "\\(?:[[:alpha:]_][[:alnum:]_]*=[^[:space:]]+[[:space:]]+\\)*"
         "\\(?:npx[ ]+@emacs-eask/cli\\|eask\\)\\(?:[ ]\\|$\\)")
        command)))

(defun mevedel-execution-telemetry--eask-targets (command)
  "Return bounded test file targets named by Eask COMMAND."
  (let ((start 0)
        targets)
    (while (and (< (length targets) 16)
                (string-match "test/[[:alnum:]_./*-]+\\.el" command start))
      (push (match-string 0 command) targets)
      (setq start (match-end 0)))
    (nreverse (delete-dups targets))))

(defun mevedel-execution-telemetry--cache-identity ()
  "Return a hash identifying the parent package-cache environment."
  (secure-hash
   'sha256
   (mapconcat (lambda (name) (or (getenv name) ""))
              '("HOME" "XDG_CACHE_HOME" "XDG_CONFIG_HOME" "EASK_HOME")
              "\0")))

(defun mevedel-execution-telemetry--full-eask-command-p (command)
  "Return non-nil when COMMAND appears to run the full Eask ERT suite."
  (and (mevedel-execution-telemetry--eask-command-p command)
       (string-match-p "\\btest[ ]+ert\\b" command)
       (null (mevedel-execution-telemetry--eask-targets command))))

(defun mevedel-execution-telemetry-command-properties (command)
  "Return non-sensitive profiler properties for COMMAND text."
  (when (mevedel-execution-telemetry--eask-command-p command)
    (let ((targets (mevedel-execution-telemetry--eask-targets command)))
      (list :workload 'eask
            :test-targets targets
            :test-scope (if targets 'focused 'full)
            :cache-identity
            (mevedel-execution-telemetry--cache-identity)))))

(defun mevedel-execution-telemetry-prepare-resource-capture
    (context command-text command)
  "Return an optional profiled COMMAND and record it in CONTEXT."
  (require 'mevedel-execution-target)
  (let* ((session (mevedel-execution-telemetry--context-session context))
         (target (mevedel-session-execution-target session))
         (remote (and target
                      (mevedel-execution-target-remote-p target))))
    (when-let* ((target target)
                (directory (mevedel-telemetry-profiler-directory session))
                ((mevedel-execution-telemetry--full-eask-command-p
                  command-text))
                (time-program
                 (if remote
                     (executable-find
                      "time" (mevedel-execution-target-prefix target))
                   (executable-find "time")))
                ((equal
                  (mevedel-execution-target-native-path
                   target (file-truename time-program))
                  "/usr/bin/time"))
                (report (file-name-concat directory "full-suite-time.txt"))
                ((not (or (file-exists-p report)
                          (gethash
                           report
                           mevedel-execution-telemetry--resource-capture-claims)))))
      (make-directory directory t)
      (puthash report t
               mevedel-execution-telemetry--resource-capture-claims)
      (setf
       (mevedel-execution-telemetry--context-resource-report-path context)
       report)
      (append (list time-program "-v" "-o" report "--") command))))

(defun mevedel-execution-telemetry-mark-direct-fallback (session facts)
  "Mark and warn for SESSION's first direct fallback in FACTS."
  (if (gethash session mevedel-execution-telemetry--fallback-sessions)
      facts
    (puthash session t mevedel-execution-telemetry--fallback-sessions)
    (display-warning
     'mevedel
     "Sandbox unavailable; this session is falling back to direct execution"
     :warning)
    (plist-put (copy-sequence facts) :first-direct-fallback t)))

(defun mevedel-execution-telemetry-record
    (context event props &optional audit)
  "Record execution EVENT and PROPS from CONTEXT.
When AUDIT is non-nil, retain only the safe forwarded-audit property set."
  (let* ((session (mevedel-execution-telemetry--context-session context))
         (props
          (if (not (and audit
                        (mevedel-telemetry-forwarded-audit-p session)))
              props
            (let ((safe nil))
              (dolist (key mevedel-execution-telemetry--audit-prop-keys)
                (when (plist-member props key)
                  (setq safe (plist-put safe key (plist-get props key)))))
              safe))))
    (when session
      (apply (if audit
                 #'mevedel-telemetry-record-audit
               #'mevedel-telemetry-record)
             session event
             (append
              (when audit
                (list
                 :execution-id
                 (mevedel-execution-telemetry--context-execution-id context)
                 :tool-use-id
                 (mevedel-execution-telemetry--context-tool-use-id context)
                 :owner
                 (mevedel-execution-telemetry--context-owner context)))
              props)))))

(provide 'mevedel-execution-telemetry)
;;; mevedel-execution-telemetry.el ends here
