;;; niwa-execution-telemetry.el -- Execution telemetry adapter -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the opaque per-execution telemetry context, privacy-safe event
;; projection, sandbox summaries, Eask profiler recognition, and optional GNU
;; time resource capture.  The context retains live ownership provenance and
;; mutable aggregation cells, but never observes live process records.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `niwa-agents'
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-sandbox-summary-cell
                  "niwa-agents" (cl-x) t)
(autoload 'niwa-agent-invocation-p "niwa-agents")
(autoload 'niwa-agent-invocation-sandbox-summary-cell "niwa-agents")

;; `niwa-execution-target'
(declare-function niwa-execution-target-native-path
                  "niwa-execution-target" (target path))
(declare-function niwa-execution-target-prefix
                  "niwa-execution-target" (cl-x) t)
(declare-function niwa-execution-target-remote-p
                  "niwa-execution-target" (target))
(autoload 'niwa-execution-target-native-path "niwa-execution-target")
(autoload 'niwa-execution-target-prefix "niwa-execution-target")
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")

;; `niwa-structs'
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)

;; `niwa-telemetry'
(declare-function niwa-telemetry-finish
                  "niwa-telemetry" (span &rest props))
(declare-function niwa-telemetry-forwarded-audit-p
                  "niwa-telemetry" (session))
(declare-function niwa-telemetry-profiler-directory
                  "niwa-telemetry" (session))
(declare-function niwa-telemetry-record
                  "niwa-telemetry" (session event &rest props))
(declare-function niwa-telemetry-record-audit
                  "niwa-telemetry" (session event &rest props))
(declare-function niwa-telemetry-start
                  "niwa-telemetry" (session event &rest props))

(require 'niwa-telemetry)
(require 'niwa-utilities)

(defconst niwa-execution-telemetry--audit-prop-keys
  '(:additional-read-count :additional-write-count
    :cache-identity :chunk-bytes :command-hash
    :duration-ms :exit-code :filesystem :lane :native-resource-capture
    :native-resource-report-bytes :network :output-bytes :output-limit
    :overlap-count :preparation-state :proc :protected-path-count :queue-depth
    :queue-duration-ms :reason-class :sandbox :termination :test-scope
    :timed-out :tty :workload :yield-time-ms)
  "Execution properties allowed in a side conversation's durable audit.")

(defvar niwa-execution-telemetry--resource-capture-claims
  (make-hash-table :test #'equal :weakness 'key)
  "Weak set of native resource report paths claimed by live runs.")

(defvar niwa-execution-telemetry--fallback-sessions
  (make-hash-table :test #'eq :weakness 'key)
  "Weak set of sessions that already reported direct sandbox fallback.")

(defvar niwa-execution-telemetry-summary-cell nil
  "Dynamically bound cell collecting child confinement for one tool call.")

(cl-defstruct (niwa-execution-telemetry--context
               (:constructor niwa-execution-telemetry--context-create))
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

(cl-defun niwa-execution-telemetry-context-create
    (&key session execution-id tool-use-id owner invocation
          pipeline-summary-cell span-event span-properties)
  "Create an opaque telemetry context for one execution."
  (niwa-execution-telemetry--context-create
   :agent-summary-cell
   (and invocation
        (niwa-agent-invocation-p invocation)
        (niwa-agent-invocation-sandbox-summary-cell invocation))
   :execution-id execution-id
   :owner owner
   :pipeline-summary-cell pipeline-summary-cell
   :sandbox-summary-cell (list nil)
   :session session
   :span (and span-event session
              (apply #'niwa-telemetry-start
                     session span-event span-properties))
   :tool-use-id tool-use-id))

(defun niwa-execution-telemetry-context-summary (context)
  "Return a copy of CONTEXT's sandbox summary."
  (copy-tree
   (car (niwa-execution-telemetry--context-sandbox-summary-cell context))))

(defun niwa-execution-telemetry-context-properties (context)
  "Return profiler properties currently available from CONTEXT."
  (when-let* ((report
               (niwa-execution-telemetry--context-resource-report-path
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

(defun niwa-execution-telemetry-finish (context &rest properties)
  "Finish CONTEXT's optional telemetry span with PROPERTIES."
  (when-let* ((span (niwa-execution-telemetry--context-span context)))
    (apply #'niwa-telemetry-finish span properties)))

(defun niwa-execution-telemetry-safe-facts (facts)
  "Return the non-sensitive confinement subset of sandbox FACTS."
  (list :sandbox (plist-get facts :sandbox)
        :filesystem (plist-get facts :filesystem)
        :network (plist-get facts :network)
        :proc (plist-get facts :proc)
        :protected-path-count (plist-get facts :protected-paths)
        :additional-read-count (plist-get facts :additional-filesystem-read)
        :additional-write-count (plist-get facts :additional-filesystem-write)))

(defun niwa-execution-telemetry--merge-sandbox-summary
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

(defun niwa-execution-telemetry-record-sandbox-attempt
    (context facts started-p refused-p)
  "Record one logical child attempt from FACTS in CONTEXT."
  (let (seen)
    (dolist (cell
             (list
              (niwa-execution-telemetry--context-sandbox-summary-cell
               context)
              (niwa-execution-telemetry--context-pipeline-summary-cell
               context)
              (niwa-execution-telemetry--context-agent-summary-cell
               context)))
      (when (and (consp cell) (not (memq cell seen)))
        (push cell seen)
        (setcar cell
                (niwa-execution-telemetry--merge-sandbox-summary
                 (car cell) facts started-p refused-p))))))

(defun niwa-execution-telemetry-sandbox-summary-class (summary)
  "Return SUMMARY's disclosure class, or nil for the default boundary.

`warning' marks a call where something the model asked for did not run: a
child was refused, or one never started.  Everything else noteworthy is a
`note' -- a wider boundary than the strictest default is what the session
was configured or granted to run with, so network access, an additional
write path, an escalation, or an unavailable sandbox record the boundary
without claiming anything went wrong."
  (and summary
       (let ((attempts (or (plist-get summary :attempt-count) 0))
             (sandbox (plist-get summary :sandbox)))
         (when
             (or (> (or (plist-get summary :additional-write-count) 0) 0)
                 (> (or (plist-get summary :refused-count) 0) 0)
                 (< (or (plist-get summary :started-count) 0) attempts)
                 (not (eq sandbox 'bubblewrap))
                 (not (eq (plist-get summary :filesystem) 'workspace-write))
                 (not (eq (plist-get summary :network) 'isolated))
                 (eq (plist-get summary :proc) 'host))
           (if (or (> (or (plist-get summary :refused-count) 0) 0)
                   (< (or (plist-get summary :started-count) 0) attempts))
               'warning
             'note)))))

(defun niwa-execution-telemetry--eask-command-p (command)
  "Return non-nil when COMMAND invokes Eask directly or through npx."
  (and (stringp command)
       (string-match-p
        (concat
         "\\(?:^\\|[;&|][[:space:]]*\\)"
         "\\(?:[[:alpha:]_][[:alnum:]_]*=[^[:space:]]+[[:space:]]+\\)*"
         "\\(?:npx[ ]+@emacs-eask/cli\\|eask\\)\\(?:[ ]\\|$\\)")
        command)))

(defun niwa-execution-telemetry--eask-targets (command)
  "Return bounded test file targets named by Eask COMMAND."
  (let ((start 0)
        targets)
    (while (and (< (length targets) 16)
                (string-match "test/[[:alnum:]_./*-]+\\.el" command start))
      (push (match-string 0 command) targets)
      (setq start (match-end 0)))
    (nreverse (delete-dups targets))))

(defun niwa-execution-telemetry--cache-identity ()
  "Return a hash identifying the parent package-cache environment."
  (secure-hash
   'sha256
   (mapconcat (lambda (name) (or (getenv name) ""))
              '("HOME" "XDG_CACHE_HOME" "XDG_CONFIG_HOME" "EASK_HOME")
              "\0")))

(defun niwa-execution-telemetry--full-eask-command-p (command)
  "Return non-nil when COMMAND appears to run the full Eask ERT suite."
  (and (niwa-execution-telemetry--eask-command-p command)
       (string-match-p "\\btest[ ]+ert\\b" command)
       (null (niwa-execution-telemetry--eask-targets command))))

(defun niwa-execution-telemetry--eask-scope (command targets)
  "Classify Eask COMMAND, with its test TARGETS, as a workload scope.
Named test files make it `focused'; an untargeted ERT run or the test
script `full'; otherwise `compile', `clean', or `other'.  Calling every
untargeted command full once made cleanups count as complete suites."
  (cond
   (targets 'focused)
   ((string-match-p "\\btest[ ]+ert\\b\\|\\brun[ ]+script[ ]+test\\b" command) 'full)
   ((string-match-p "\\bcompile\\b" command) 'compile)
   ((string-match-p "\\bclean\\b" command) 'clean)
   (t 'other)))

(defun niwa-execution-telemetry-command-properties (command)
  "Return non-sensitive profiler properties for COMMAND text."
  (when (niwa-execution-telemetry--eask-command-p command)
    (let ((targets (niwa-execution-telemetry--eask-targets command)))
      (list :workload 'eask
            :test-targets targets
            :test-scope (niwa-execution-telemetry--eask-scope command targets)
            :cache-identity
            (niwa-execution-telemetry--cache-identity)))))

(defun niwa-execution-telemetry-prepare-resource-capture
    (context command-text command)
  "Return an optional profiled COMMAND and record it in CONTEXT."
  (let* ((session (niwa-execution-telemetry--context-session context))
         (target (niwa-session-execution-target session))
         (remote (and target
                      (niwa-execution-target-remote-p target))))
    (when-let* ((target target)
                (directory (niwa-telemetry-profiler-directory session))
                ((niwa-execution-telemetry--full-eask-command-p
                  command-text))
                (time-program
                 (if remote
                     (executable-find
                      "time" (niwa-execution-target-prefix target))
                   (executable-find "time")))
                ((equal
                  (niwa-execution-target-native-path
                   target (file-truename time-program))
                  "/usr/bin/time"))
                (report (file-name-concat directory "full-suite-time.txt"))
                ((not (or (file-exists-p report)
                          (gethash
                           report
                           niwa-execution-telemetry--resource-capture-claims)))))
      (make-directory directory t)
      (puthash report t
               niwa-execution-telemetry--resource-capture-claims)
      (setf
       (niwa-execution-telemetry--context-resource-report-path context)
       report)
      (append (list time-program "-v" "-o" report "--") command))))

(defun niwa-execution-telemetry-mark-direct-fallback (session facts)
  "Mark and warn for SESSION's first direct fallback in FACTS.

FACTS are always marked, because an unconfined run is a fact about the
run whether or not a session owns it.  Only the warning is session-scoped:
a nil SESSION is not a session that can be warned about.  Helpers that must
run beside Emacs rather than on the target unbind the session on purpose --
`niwa-generate-diff' is one -- and warning for those announces a
fallback for \"this session\" when no session is in hand, filed under a
hash key that is not a session either, so the one warning the session is
promised arrives twice.  The owning session still warns on its own first
unconfined child."
  (if (and session
           (gethash session niwa-execution-telemetry--fallback-sessions))
      facts
    (when session
      (puthash session t niwa-execution-telemetry--fallback-sessions)
      (niwa-diagnostic-warning
       (niwa-diagnostic-scope session)
       'niwa
       "Sandbox unavailable; this session is falling back to direct execution"
       :warning))
    (plist-put (copy-sequence facts) :first-direct-fallback t)))

(defun niwa-execution-telemetry-record
    (context event props &optional audit)
  "Record execution EVENT and PROPS from CONTEXT.
When AUDIT is non-nil, retain only the safe forwarded-audit property set."
  (let* ((session (niwa-execution-telemetry--context-session context))
         (props
          (if (not (and audit
                        (niwa-telemetry-forwarded-audit-p session)))
              props
            (let ((safe nil))
              (dolist (key niwa-execution-telemetry--audit-prop-keys)
                (when (plist-member props key)
                  (setq safe (plist-put safe key (plist-get props key)))))
              safe))))
    (when session
      (apply (if audit
                 #'niwa-telemetry-record-audit
               #'niwa-telemetry-record)
             session event
             (append
              (when audit
                (list
                 :execution-id
                 (niwa-execution-telemetry--context-execution-id context)
                 :tool-use-id
                 (niwa-execution-telemetry--context-tool-use-id context)
                 :owner
                 (niwa-execution-telemetry--context-owner context)))
              props)))))

(provide 'niwa-execution-telemetry)
;;; niwa-execution-telemetry.el ends here
