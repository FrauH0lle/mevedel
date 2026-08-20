;;; mevedel-execution.el --- Bounded child-process lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Owns model-triggered operating-system processes: stable environments,
;; process groups, optional PTYs, confinement, timeouts, bounded disk spooling,
;; and private process records.  Callers receive result plists and never
;; process objects.  `mevedel-execution-telemetry.el' owns safe event
;; projection, sandbox summaries, and optional profiler resource capture.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'tramp-cache))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-runtime-settled-p
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-sandbox-summary-cell
                  "mevedel-agents" (cl-x) t)
(defvar mevedel--agent-invocation)

;; `mevedel-execution-scheduler'
(declare-function mevedel-execution-scheduler-cancel
                  "mevedel-execution-scheduler" (lease))
(declare-function mevedel-execution-scheduler-create
                  "mevedel-execution-scheduler" ())
(declare-function mevedel-execution-scheduler-release
                  "mevedel-execution-scheduler" (lease))
(declare-function mevedel-execution-scheduler-submit
                  "mevedel-execution-scheduler"
                  (scheduler mode start &optional admit-p reject))

;; `mevedel-execution-telemetry'
(declare-function mevedel-execution-telemetry-agent-summary-cell
                  "mevedel-execution-telemetry" (invocation))
(declare-function mevedel-execution-telemetry-command-properties
                  "mevedel-execution-telemetry" (command))
(declare-function mevedel-execution-telemetry-mark-direct-fallback
                  "mevedel-execution-telemetry" (session facts))
(declare-function mevedel-execution-telemetry-prepare-resource-capture
                  "mevedel-execution-telemetry" (session command-text command))
(declare-function mevedel-execution-telemetry-record
                  "mevedel-execution-telemetry"
                  (session execution-id tool-use-id owner event props))
(declare-function mevedel-execution-telemetry-record-sandbox-attempt
                  "mevedel-execution-telemetry"
                  (facts started-p refused-p &rest cells))
(declare-function mevedel-execution-telemetry-safe-facts
                  "mevedel-execution-telemetry" (facts))
(defvar mevedel-execution-telemetry-summary-cell)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create
                  "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-direct-async-capable-p
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))
(declare-function mevedel-execution-target-prefix
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-workspace-root
                  "mevedel-execution-target" (cl-x) t)

;; `mevedel-resource'
(declare-function mevedel-resource-artifact-address
                  "mevedel-resource" (path session))

;; `mevedel-sandbox'
(declare-function mevedel-sandbox--record-launch-failure
                  "mevedel-sandbox" (child-result &optional workdir))
(declare-function mevedel-sandbox-cleanup "mevedel-sandbox" (preparation))
(declare-function mevedel-sandbox-launch-failed-p
                  "mevedel-sandbox" (preparation child-result))
(declare-function mevedel-sandbox-prepare
                  "mevedel-sandbox"
                  (command workdir writable-roots &optional
                           additional-permissions sandbox-permissions mode))
(declare-function mevedel-sandbox-strip-marker
                  "mevedel-sandbox" (preparation child-result))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-mutation-authority
                  "mevedel-session-artifacts" (session &optional buffer))
(declare-function mevedel-session-artifacts-assert-new-mutation-authority
                  "mevedel-session-artifacts" (session))
(declare-function mevedel-session-artifacts-publish-text
                  "mevedel-session-artifacts"
                  (session path content &optional coding))

;; `mevedel-session-durability'
(declare-function mevedel-session-durability-set-unsettled-mutation
                  "mevedel-session-durability" (session value))
(declare-function mevedel-session-durability-unsettled-mutation-p
                  "mevedel-session-durability" (session))

;; `mevedel-structs'
(declare-function mevedel-session--set-execution-state
                  "mevedel-structs" (session state))
(declare-function mevedel-session-audit-target "mevedel-structs" (session))
(declare-function mevedel-session-execution-state "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-sandbox-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-finish "mevedel-telemetry" (span &rest props))
(declare-function mevedel-telemetry-forwarded-audit-p
                  "mevedel-telemetry" (session))
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))
(declare-function mevedel-telemetry-record-audit
                  "mevedel-telemetry" (session event &rest props))
(declare-function mevedel-telemetry-start
                  "mevedel-telemetry" (session event &rest props))
(declare-function mevedel-telemetry-profiler-directory
                  "mevedel-telemetry" (session))

;; `mevedel-turn'
(declare-function mevedel-request-push-canceller
                  "mevedel-turn" (request canceller))

;; `mevedel-utilities'
(declare-function mevedel--head-tail-preview-parts
                  "mevedel-utilities"
                  (head tail total-length &optional preview-size))

;; `tramp'
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))

;; `tramp-cache'
(declare-function tramp-set-connection-property
                  "tramp-cache" (key property value))


;;
;;; Configuration and private state

(defcustom mevedel-execution-output-limit (* 64 1024 1024)
  "Maximum bytes retained from one child process.

The execution module terminates a child when its merged stdout and stderr
reach this limit.  Bytes already written to its spool remain available in the
terminal result."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-execution-inline-output-limit 2000
  "Maximum unread characters returned inline by one managed observation.

Larger unread ranges use the shared newline-aware head-and-tail preview while
the complete spool remains at the path in the execution facts."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-execution-progress-delay 2
  "Seconds before a managed Bash execution publishes live progress."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-execution-progress-interval 0.25
  "Seconds between managed Bash progress events.
Values below 0.25 are clamped so the UI receives at most four per second."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-execution-remote-direct-async t
  "When non-nil, eligible remote Bash spawns run on a private channel.

An eligible spawn is a non-TTY execution on a single-hop ssh or scp
target whose wrapped command fits the remote pipe buffer.  It runs
over its own connection instead of the shared TRAMP control
connection, so a live execution stops serializing durable
session work behind it and stops being a reentrancy window.  Each
spawn opens its own connection, so authentication must be
non-interactive (an agent, a key, or user-configured connection
sharing).  TTY executions and oversized commands always keep the
classic shared-channel spawn."
  :type 'boolean
  :group 'mevedel)

(defconst mevedel-execution--direct-async-command-limit 3584
  "Largest quoted remote command, in bytes, spawned direct-async.
The remote side receives the command as one string bounded by the
target's pipe buffer, typically 4096 bytes; the margin covers TRAMP's
cd-and-env prefix.  A longer command falls back to the classic spawn.")

(defconst mevedel-execution-live-limit 64
  "Maximum number of live managed Bash processes in one session.")

(defconst mevedel-execution--child-kill-delay 2
  "Seconds to wait before force-killing a stopped child process group.")

(defconst mevedel-execution--remote-control-timeout 5
  "Seconds allowed for one target process-control operation.")

(defconst mevedel-execution--terminal-retention-seconds 60
  "Seconds a completed yielded execution remains pollable.")

(defconst mevedel-execution--environment
  '(("NO_COLOR" . "1")
    ("TERM" . "dumb")
    ("LC_ALL" . "C.UTF-8")
    ("LANG" . "C.UTF-8")
    ("COLORTERM" . "")
    ("PAGER" . "cat")
    ("GIT_PAGER" . "cat")
    ("GH_PAGER" . "cat")
    ("MEVEDEL_EXECUTION" . "1"))
  "Stable defaults for model-triggered child processes.")

(define-error 'mevedel-execution-error "Managed execution error")
(define-error 'mevedel-execution-input-error
  "Managed execution does not accept this input" 'mevedel-execution-error)
(define-error 'mevedel-execution-limit
  "Managed execution limit reached" 'mevedel-execution-error)
(define-error 'mevedel-execution-not-found
  "Managed execution not found" 'mevedel-execution-error)

(cl-defstruct (mevedel-execution--state
               (:constructor mevedel-execution--state-create))
  "Opaque per-session execution state."
  next-id
  records
  scheduler
  unknown-outcome)

(cl-defstruct (mevedel-execution--origin
               (:constructor mevedel-execution--origin-create))
  "Immutable ownership and transcript correlation for one execution."
  data-buffer
  owner
  owner-context
  sandbox-summary-cell
  session
  tool-args
  tool-use-id)

(cl-defstruct (mevedel-execution--record
               (:constructor mevedel-execution--record-create))
  "Private state for one operating-system process."
  callback
  delivery-state
  direct-async-p
  error-data
  execution-id
  exit-code
  finished-p
  force-timer
  group-id
  group-marker
  group-marker-buffer
  group-start-time
  last-byte-newline-p
  launch-attempted-p
  marker
  marker-buffer
  marker-seen-p
  mutating-p
  mutation-armed-p
  newline-count
  observer
  observer-timer
  omitted-output-bytes
  origin
  outcome-function
  output-chars
  output-head
  output-limit-p
  recoverable-output-bytes
  recoverable-output-path
  output-tail
  process
  progress-timer
  read-offset
  resource-report-path
  retained-p
  retire-timer
  sandbox-facts
  sandbox-preparation
  sandbox-summary-cell
  scheduler-lease
  settle-timer
  spool-path
  started-at
  stop-p
  termination
  timed-out-p
  teardown-function
  terminal-observation
  timeout
  timeout-timer
  token
  tty-p
  unread-chars
  unread-head
  unread-tail
  watch-timer
  workdir
  yield-time-ms
  yield-timer
  yielded-p)

(defvar mevedel-execution--orphan-state nil
  "Private state for children that have no chat session owner.")

(defvar mevedel-execution--sessions
  (make-hash-table :test #'eq :weakness 'key)
  "Weak set of sessions that have created execution state.")

(defvar mevedel-execution-event-functions nil
  "Functions notified of immutable managed execution event plists.

Each function receives one event.  Return values are ignored and errors are
contained by the execution module.")

(defvar mevedel-execution-state-change-hook nil
  "Functions notified when a session's live execution set changes.

Each function receives the owning session and originating data buffer.  Errors
are contained by the execution module.")

(defvar mevedel-execution-mailbox-delivery-function nil
  "Function that synchronously secures one terminal event in its owner mailbox.
The function receives an immutable event plist and its private owner context,
then returns non-nil only after durable delivery.  This is deliberately
separate from passive event hooks; the context is never published to them.")

(defconst mevedel-execution--remote-group-script
  (string-join
   '("command=(\"$@\")"
     "read -r stat < /proc/$$/stat || exit 125"
     "rest=${stat##*) }"
     "set -- $rest"
     "printf '%s%s:%s\\n' \"$0\" \"$$\" \"${20}\""
     "trap ':' HUP INT TERM"
     "\"${command[@]}\""
     "status=$?"
     "group_has_members() { for path in /proc/[0-9]*/stat"
     "do read -r member 2>/dev/null < \"$path\" || continue"
     "pid=${member%% *}"
     "tail=${member##*) }"
     "set -- $tail"
     "[ \"$pid\" != \"$$\" ] && [ \"$3\" = \"$$\" ] && [ \"$1\" != \"Z\" ] && return 0"
     "done"
     "return 1"
     "}"
     "while group_has_members; do sleep 0.25; done"
     "exit \"$status\"")
   "; ")
  "Target-side launcher that reports a private process-group identity.")

(defconst mevedel-execution--remote-group-status-script
  (string-join
   '("group=$1"
     "expected=$2"
     "live=0"
     "for path in /proc/[0-9]*/stat"
     "do read -r member 2>/dev/null < \"$path\" || continue"
     "rest=${member##*) }"
     "set -- $rest"
     "[ \"$3\" = \"$group\" ] || continue"
     "[ \"$1\" = \"Z\" ] && continue"
     "live=1"
     "break"
     "done"
     "[ \"$live\" = 1 ] || exit 1"
     "[ -r \"/proc/$group/stat\" ] || exit 2"
     "read -r stat < \"/proc/$group/stat\" || exit 2"
     "rest=${stat##*) }"
     "set -- $rest"
     "[ \"${20}\" = \"$expected\" ] || exit 2")
   "; ")
  "Target-side zombie-aware group liveness and identity probe.
Exit 0: a non-zombie member remains and the leader identity matches.
Exit 1: no non-zombie member remains -- zombies count as settled.
Exit 2: live members without a matching leader identity; never signal
such a group, because the leader PID may have been reused.")


;;
;;; State and environment

(defun mevedel-execution--managed-live-p (record)
  "Return non-nil when RECORD is a user-visible live Bash execution."
  (and (mevedel-execution--record-execution-id record)
       (not (mevedel-execution--record-finished-p record))))

(defun mevedel-execution--new-state ()
  "Return an empty opaque execution state."
  (require 'mevedel-execution-scheduler)
  (mevedel-execution--state-create
   :next-id 0
   :records (make-hash-table :test #'equal)
   :scheduler (mevedel-execution-scheduler-create)))

(defun mevedel-execution--state-for-session (session)
  "Return the private execution state for SESSION.

When SESSION is nil, use the module-owned state for direct non-session calls."
  (if session
      (progn
        (puthash session t mevedel-execution--sessions)
        (or (mevedel-session-execution-state session)
            (let ((state (mevedel-execution--new-state)))
              (mevedel-session--set-execution-state session state)
              state)))
    (or mevedel-execution--orphan-state
        (setq mevedel-execution--orphan-state
              (mevedel-execution--new-state)))))

(defun mevedel-execution--mutation-target (session)
  "Return SESSION's durable mutation-authority target."
  (and session (mevedel-session-audit-target session)))

(defun mevedel-execution--mutation-records (session)
  "Return every process record sharing SESSION's mutation authority."
  (let ((target (mevedel-execution--mutation-target session))
        records)
    (maphash
     (lambda (candidate _present)
       (when (eq target (mevedel-execution--mutation-target candidate))
         (when-let* ((state (mevedel-session-execution-state candidate)))
           (maphash
            (lambda (_key record) (push record records))
            (mevedel-execution--state-records state)))))
     mevedel-execution--sessions)
    records))

(defun mevedel-execution--armed-mutation-records (session)
  "Return SESSION's transient records backed by the durable mutation latch."
  (cl-remove-if-not #'mevedel-execution--record-mutation-armed-p
                    (mevedel-execution--mutation-records session)))

(defun mevedel-execution--durable-mutation-latch-p (session)
  "Return non-nil when SESSION's remote authority has its mutation latch set."
  (let* ((authority (mevedel-execution--mutation-target session))
         (target (and authority
                      (mevedel-session-execution-target authority))))
    (when (and target (mevedel-execution-target-remote-p target))
      (require 'mevedel-session-durability)
      (mevedel-session-durability-unsettled-mutation-p authority))))

(defun mevedel-execution--assert-mutation-authority (record)
  "Assert RECORD's durable remote mutation authority."
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (let* ((origin (mevedel-execution--record-origin record))
         (authority
          (mevedel-execution--mutation-target
           (mevedel-execution--origin-session origin)))
         (data-buffer (mevedel-execution--origin-data-buffer origin)))
    (if (buffer-live-p data-buffer)
        (mevedel-session-artifacts-assert-mutation-authority
         authority data-buffer)
      (with-temp-buffer
        (mevedel-session-artifacts-assert-mutation-authority
         authority (current-buffer))))))

(defun mevedel-execution--arm-mutation (record)
  "Durably arm mutating remote RECORD before its child can start."
  (when (mevedel-execution--record-mutating-p record)
    (let* ((origin (mevedel-execution--record-origin record))
           (session (mevedel-execution--origin-session origin))
           (authority (mevedel-execution--mutation-target session))
           (target (mevedel-session-execution-target authority)))
      (when (mevedel-execution-target-remote-p target)
        (mevedel-execution--assert-mutation-authority record)
        (require 'mevedel-session-durability)
        (unless
            (mevedel-session-durability-set-unsettled-mutation authority t)
          (signal 'mevedel-execution-error
                  (list "Could not arm remote mutation authority")))
        (setf (mevedel-execution--record-mutation-armed-p record) t)))))

(defun mevedel-execution--mark-unknown (record error-data)
  "Record ERROR-DATA when RECORD's target process outcome cannot be proved."
  (let* ((origin (mevedel-execution--record-origin record))
         (session (mevedel-execution--origin-session origin))
         (state (mevedel-execution--state-for-session session))
         (outcome
          (list :group-id (mevedel-execution--record-group-id record)
                :group-start-time
                (mevedel-execution--record-group-start-time record)
                :workdir (mevedel-execution--record-workdir record)
                :error error-data)))
    (setf (mevedel-execution--record-termination record) 'unknown
          (mevedel-execution--record-error-data record) error-data
          (mevedel-execution--state-unknown-outcome state) outcome)
    outcome))

(defun mevedel-execution--settle-mutation (record)
  "Clear RECORD's durable latch after every armed mutation is proved settled."
  (when (and (mevedel-execution--record-mutation-armed-p record)
             (not (eq 'unknown
                      (mevedel-execution--record-termination record))))
    (let* ((origin (mevedel-execution--record-origin record))
           (session (mevedel-execution--origin-session origin))
           (authority (mevedel-execution--mutation-target session)))
      (setf (mevedel-execution--record-mutation-armed-p record) nil)
      (unless (mevedel-execution--armed-mutation-records session)
        (condition-case err
            (progn
              (require 'mevedel-session-durability)
              (unless
                  (mevedel-session-durability-set-unsettled-mutation
                   authority nil)
                (error "Remote mutation authority changed before settlement")))
          (error
           (setf (mevedel-execution--record-mutation-armed-p record) t)
           (mevedel-execution--mark-unknown record err)))))))

(defun mevedel-execution-unknown-outcome (session)
  "Return SESSION's unproved target process outcome, if any."
  (or (mevedel-execution--state-unknown-outcome
       (mevedel-execution--state-for-session session))
      (when (mevedel-execution--durable-mutation-latch-p session)
        (let ((armed (mevedel-execution--armed-mutation-records session)))
          (when (or (null armed)
                    (cl-some #'mevedel-execution--record-finished-p armed))
            '(:durable-unsettled-mutation t))))))

(defun mevedel-execution-unsettled-mutation-p (session)
  "Return non-nil while SESSION has live or unproved remote mutation."
  (or (mevedel-execution--state-unknown-outcome
       (mevedel-execution--state-for-session session))
      (mevedel-execution--durable-mutation-latch-p session)))

(defun mevedel-execution-mutation-blocked-p (session)
  "Return non-nil when SESSION must not start mutating execution."
  (and (mevedel-execution-unknown-outcome session) t))

(defun mevedel-execution-acknowledge-unknown (session)
  "Acknowledge and clear SESSION's unproved target process outcome."
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (let* ((authority (mevedel-execution--mutation-target session))
         (target (and authority
                      (mevedel-session-execution-target authority)))
         (records (mevedel-execution--armed-mutation-records session)))
    (when (cl-some (lambda (record)
                     (not (mevedel-execution--record-finished-p record)))
                   records)
      (signal 'mevedel-execution-error
              (list "Cannot acknowledge while mutating execution is live")))
    (when (and target (mevedel-execution-target-remote-p target))
      (mevedel-session-artifacts-assert-mutation-authority
       authority (current-buffer))
      (require 'mevedel-session-durability)
      (unless
          (mevedel-session-durability-set-unsettled-mutation authority nil)
        (signal 'mevedel-execution-error
                (list "Could not clear remote mutation authority"))))
    (dolist (record records)
      (setf (mevedel-execution--record-mutation-armed-p record) nil))
    (maphash
     (lambda (candidate _present)
       (when (eq authority (mevedel-execution--mutation-target candidate))
         (when-let* ((state (mevedel-session-execution-state candidate)))
           (setf (mevedel-execution--state-unknown-outcome state) nil))))
     mevedel-execution--sessions)
    nil))

(defun mevedel-execution--process-environment (&optional remote)
  "Return a child environment with stable execution defaults.
When REMOTE is non-nil, preserve the target environment instead of forwarding
the client environment."
  (let ((process-environment
         (and (not remote) (copy-sequence process-environment))))
    (dolist (entry mevedel-execution--environment)
      (setenv (car entry) (cdr entry)))
    process-environment))

(defun mevedel-execution--localize-command (record command workdir)
  "Return COMMAND in RECORD's target-native path domain for WORKDIR."
  (require 'mevedel-execution-target)
  (let* ((session
          (mevedel-execution--origin-session
           (mevedel-execution--record-origin record)))
         (target
          (if session
              (mevedel-session-execution-target session)
            (mevedel-execution-target-create workdir))))
    (mapcar (lambda (argument)
              (if (stringp argument)
                  (mevedel-execution-target-native-path target argument)
                argument))
            command)))

(defun mevedel-execution--remote-command (record command)
  "Wrap COMMAND with target-side process-group ownership for RECORD."
  (let ((marker (make-temp-name "MEVEDEL_PROCESS_GROUP_")))
    (setf (mevedel-execution--record-group-marker record) marker
          (mevedel-execution--record-group-marker-buffer record) "")
    (append
     ;; A direct-async spawn carries its environment explicitly: TRAMP
     ;; only transfers the difference against the top-level environment,
     ;; which silently drops any default the user's own environment
     ;; happens to share.
     (when (mevedel-execution--record-direct-async-p record)
       (cons "env"
             (mapcar (lambda (entry)
                       (format "%s=%s" (car entry) (cdr entry)))
                     mevedel-execution--environment)))
     (list "setsid" "-f" "-w" "bash" "-c"
           mevedel-execution--remote-group-script marker)
     command)))

(defun mevedel-execution--with-spawn-channel (remote direct-async thunk)
  "Call THUNK with the spawn channel forced for a REMOTE launch.
DIRECT-ASYNC non-nil forces the private channel; nil forces the shared
classic channel, which keeps a TTY or oversized spawn classic even on
a connection where something else enabled direct-async.  Local spawns
run THUNK untouched."
  (if (and remote (fboundp 'tramp-direct-async-process-p))
      ;; Emacs 30's direct-async spawn still asks for its ssh options
      ;; through `tramp-ssh-controlmaster-options', but TRAMP renamed
      ;; that function to `tramp-ssh-or-plink-options' and the
      ;; compat-funcall silently returns nil -- dropping every option
      ;; the user routed through the same-named variable, including a
      ;; -F config a host alias may need to resolve at all.  Route the
      ;; call for this spawn's extent only; a no-shim spawn rebinds the
      ;; symbol to its current definition, which is a no-op.
      (let ((shim (and direct-async
                       (not (fboundp 'tramp-ssh-controlmaster-options))
                       (fboundp 'tramp-ssh-or-plink-options))))
        (cl-letf (((symbol-function 'tramp-direct-async-process-p)
                   (if direct-async
                       (lambda (&rest _) t)
                     (lambda (&rest _) nil)))
                  ((symbol-function 'tramp-ssh-controlmaster-options)
                   (if shim
                       (symbol-function 'tramp-ssh-or-plink-options)
                     (symbol-function 'tramp-ssh-controlmaster-options))))
          (if direct-async
              (progn
                (require 'tramp-cache)
                (let ((vec (tramp-dissect-file-name remote)))
                  (with-tramp-saved-connection-property vec "direct-async"
                    (tramp-set-connection-property vec "direct-async" t)
                    (funcall thunk))))
            (funcall thunk))))
    (funcall thunk)))

(defun mevedel-execution--direct-async-p (record command workdir)
  "Return non-nil when RECORD's COMMAND below WORKDIR spawns direct-async.
COMMAND is the localized argv before the group wrapper; the length
check adds the wrapper's own size."
  (and mevedel-execution-remote-direct-async
       (not (mevedel-execution--record-tty-p record))
       (fboundp 'tramp-direct-async-process-p)
       (require 'mevedel-execution-target nil t)
       (mevedel-execution-target-direct-async-capable-p
        (mevedel-execution-target-create workdir))
       (< (+ (string-bytes
              (mapconcat #'shell-quote-argument command " "))
             (string-bytes mevedel-execution--remote-group-script)
             256)
          mevedel-execution--direct-async-command-limit)))

(defun mevedel-execution--filter-group-marker (record filter chunk)
  "Strip RECORD's target process-group marker from CHUNK before FILTER."
  (let ((marker (mevedel-execution--record-group-marker record)))
    (if (or (null marker)
            (eq :done
                (mevedel-execution--record-group-marker-buffer record)))
        (funcall filter record chunk)
      (setq chunk
            (concat (mevedel-execution--record-group-marker-buffer record)
                    chunk))
      (if-let* ((newline (string-search "\n" chunk)))
          (let ((line (string-trim-right (substring chunk 0 newline) "\r")))
            (setf (mevedel-execution--record-group-marker-buffer record)
                  :done)
            (if (string-match
                 (concat "\\`" (regexp-quote marker)
                         "\\([0-9]+\\):\\([0-9]+\\)\\'")
                 line)
                (setf (mevedel-execution--record-group-id record)
                      (string-to-number (match-string 1 line))
                      (mevedel-execution--record-group-start-time record)
                      (match-string 2 line))
              (funcall filter record (substring chunk 0 (1+ newline))))
            (let ((rest (substring chunk (1+ newline))))
              (unless (string-empty-p rest)
                (funcall filter record rest))))
        (setf (mevedel-execution--record-group-marker-buffer record) chunk)))))

(defun mevedel-execution--remote-group-status (record)
  "Return RECORD's target group status in one probe round trip.

The result is `live' (a non-zombie member remains and the leader
identity matches -- safe to signal), `dead' (no non-zombie member
remains -- safe to settle; zombies count as settled), or `ambiguous'
\(live members without a matching leader identity, or the record lacks
the data to probe -- never signal).  Transport failures signal."
  (let ((group-id (mevedel-execution--record-group-id record))
        (start-time (mevedel-execution--record-group-start-time record))
        (workdir (mevedel-execution--record-workdir record))
        (process-environment nil))
    (if (not (and (integerp group-id) (> group-id 0)
                  (stringp start-time) (stringp workdir)
                  (file-remote-p workdir)))
        'ambiguous
      (with-timeout
          (mevedel-execution--remote-control-timeout
           (error "Target process status probe timed out"))
        (with-temp-buffer
          (setq default-directory workdir)
          (pcase
              (process-file
               "bash" nil nil nil "-c"
               mevedel-execution--remote-group-status-script
               "mevedel-process-group" (number-to-string group-id)
               start-time)
            (0 'live)
            (1 'dead)
            (_ 'ambiguous)))))))

(defun mevedel-execution--settle-unknown (record)
  "End RECORD's lost transport and schedule unknown-outcome settlement."
  (unless (mevedel-execution--timer-pending-p
           (mevedel-execution--record-settle-timer record))
    (setf (mevedel-execution--record-settle-timer record)
          (run-at-time mevedel-execution--child-kill-delay nil
                       #'mevedel-execution--settle-after-kill record)))
  (when-let* ((process (mevedel-execution--record-process record))
              ((process-live-p process)))
    (ignore-errors (delete-process process))))

(defun mevedel-execution--notify-state-change (record)
  "Notify observers that RECORD's session execution set changed."
  (when (mevedel-execution--record-execution-id record)
    (let* ((origin (mevedel-execution--record-origin record))
           (session (mevedel-execution--origin-session origin))
           (data-buffer (mevedel-execution--origin-data-buffer origin)))
      (when session
        (dolist (function mevedel-execution-state-change-hook)
          (when (functionp function)
            (condition-case err
                (funcall function session data-buffer)
              (error
               (display-warning
                'mevedel
                (format "Execution state consumer failed: %s"
                        (error-message-string err))
                :warning)))))))))


;;
;;; Process groups and output spooling

(defun mevedel-execution--signal-confined-group (record signal)
  "Send SIGNAL to RECORD's foreground process group inside Bubblewrap."
  (let ((outer-group-id (mevedel-execution--record-group-id record))
        (children (make-hash-table :test #'eql))
        (attributes (make-hash-table :test #'eql))
        (pending (list (mevedel-execution--record-group-id record)))
        group-id)
    (dolist (pid (list-system-processes))
      (when-let* ((attrs (process-attributes pid))
                  (parent (alist-get 'ppid attrs)))
        (puthash pid attrs attributes)
        (push pid (gethash parent children))))
    (while (and pending (not group-id))
      (let ((pid (pop pending)))
        (dolist (child (gethash pid children))
          (push child pending)
          (let ((attrs (gethash child attributes)))
            (unless (equal "bwrap" (alist-get 'comm attrs))
              (let ((candidate
                     (let ((foreground (alist-get 'tpgid attrs)))
                       (if (and (integerp foreground)
                                (> foreground 0)
                                (not (eql foreground outer-group-id)))
                           foreground
                         (alist-get 'pgrp attrs)))))
                (when (and (integerp candidate) (> candidate 0))
                  (unless (eql candidate outer-group-id)
                    (setq group-id candidate)))))))))
    (when group-id
      (condition-case nil
          (progn
            (signal-process (- group-id) signal)
            t)
        (error nil)))))

(defun mevedel-execution--signal-record (record signal)
  "Send SIGNAL to RECORD's process group when available."
  (let* ((process (mevedel-execution--record-process record))
         (group-id (mevedel-execution--record-group-id record))
         (workdir (mevedel-execution--record-workdir record))
         (remote (and workdir (file-remote-p workdir)))
         (confined-p
          (eq 'bubblewrap
              (plist-get (mevedel-execution--record-sandbox-facts record)
                         :sandbox))))
    (condition-case err
        (cond
         ((and (not remote)
               confined-p
               (eq signal 'INT)
               (mevedel-execution--signal-confined-group record signal)))
         ((and (not remote)
               (eq system-type 'windows-nt)
               (process-live-p process))
          (kill-process process t))
         ((and (eq signal 'INT)
               (mevedel-execution--record-tty-p record)
               (process-live-p process))
          (process-send-string process (string 3)))
         ((and (or remote (not (eq system-type 'windows-nt)))
               (integerp group-id) (> group-id 0))
          (if remote
              (pcase (mevedel-execution--remote-group-status record)
                ('live
                 (with-timeout
                     (mevedel-execution--remote-control-timeout
                      (error "Target process signal timed out"))
                   (signal-process (- group-id) signal workdir)))
                ('dead -1)
                (status
                 (error "Remote process-group status is %s" status)))
            (signal-process (- group-id) signal)))
         (remote
          (error "Remote process-group identity is unavailable"))
         ((process-live-p process)
          (signal-process process signal)))
      (error
       (if remote
           (progn
             (mevedel-execution--mark-unknown record err)
             (mevedel-execution--settle-unknown record))
         (when (process-live-p process)
           (ignore-errors (signal-process process signal))))))))

(defun mevedel-execution--group-live-p (record)
  "Return non-nil when RECORD's process group still has a live member.

Remote groups are probed zombie-aware: a group whose only remaining
members are zombies reports dead, so settlement does not ride the kill
grace for work that already ended.  An ambiguous probe reports live --
safety over latency -- and keeps the bounded escalation in charge."
  (let* ((workdir (mevedel-execution--record-workdir record))
         (remote (and workdir (file-remote-p workdir)))
         (group-id (mevedel-execution--record-group-id record)))
    (cond
     ((and remote (not group-id))
      (mevedel-execution--mark-unknown
       record '(error "Remote process-group identity was not received"))
      nil)
     (remote
      (condition-case err
          (not (eq 'dead (mevedel-execution--remote-group-status record)))
        (error
         (mevedel-execution--mark-unknown record err)
         nil)))
     ((not (eq system-type 'windows-nt))
      (when group-id
        (condition-case nil
            (zerop (signal-process (- group-id) 0))
          (error nil)))))))

(defun mevedel-execution--append-output (record chunk)
  "Append bounded raw output CHUNK to RECORD's spool."
  (unless (or (mevedel-execution--record-finished-p record)
              (mevedel-execution--record-output-limit-p record))
    (let* ((path (mevedel-execution--record-spool-path record))
           (current (file-attribute-size (file-attributes path)))
           (remaining (- mevedel-execution-output-limit current))
           (length (string-bytes chunk)))
      (when (> remaining 0)
        (let ((coding-system-for-write 'no-conversion))
          (write-region
           (if (> length remaining)
               (substring chunk 0 remaining)
             chunk)
           nil path t 'silent)))
      (when (> length remaining)
        (setf (mevedel-execution--record-output-limit-p record) t)
        (mevedel-execution--begin-stop record 'output-limit)))))

(defun mevedel-execution--read-output (record)
  "Return RECORD's complete decoded spooled output."
  (let ((path (mevedel-execution--record-spool-path record)))
    (if (not (file-readable-p path))
        ""
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (decode-coding-string (buffer-string) 'utf-8-unix t)))))

(defun mevedel-execution--release-runtime (record)
  "Release RECORD's process, timers, and group file."
  (let ((process (mevedel-execution--record-process record)))
    (dolist (timer (list (mevedel-execution--record-timeout-timer record)
                         (mevedel-execution--record-force-timer record)
                         (mevedel-execution--record-observer-timer record)
                         (mevedel-execution--record-progress-timer record)
                         (mevedel-execution--record-retire-timer record)
                         (mevedel-execution--record-settle-timer record)
                         (mevedel-execution--record-watch-timer record)
                         (mevedel-execution--record-yield-timer record)))
      (when (timerp timer)
        (cancel-timer timer)))
    (setf (mevedel-execution--record-process record) nil
          (mevedel-execution--record-group-id record) nil
          (mevedel-execution--record-timeout-timer record) nil
          (mevedel-execution--record-force-timer record) nil
          (mevedel-execution--record-observer-timer record) nil
          (mevedel-execution--record-progress-timer record) nil
          (mevedel-execution--record-retire-timer record) nil
          (mevedel-execution--record-settle-timer record) nil
          (mevedel-execution--record-watch-timer record) nil
          (mevedel-execution--record-yield-timer record) nil)
    (when process
      (set-process-query-on-exit-flag process nil)
      (ignore-errors (delete-process process)))))

(defun mevedel-execution--cleanup-record (record &optional preserve-spool)
  "Release runtime and registry state for RECORD.

Delete its spool unless PRESERVE-SPOOL is non-nil."
  (let ((removed-live-p (mevedel-execution--managed-live-p record)))
    (mevedel-execution--release-runtime record)
    (unless preserve-spool
      (when-let* ((path (mevedel-execution--record-spool-path record)))
        (ignore-errors (delete-file path))))
    (let ((state
           (mevedel-execution--state-for-session
            (mevedel-execution--origin-session
             (mevedel-execution--record-origin record)))))
      (remhash (or (mevedel-execution--record-execution-id record)
                   (mevedel-execution--record-token record))
               (mevedel-execution--state-records state))
      (when removed-live-p
        (mevedel-execution--notify-state-change record)))))

(defun mevedel-execution--retire-terminal-record (record)
  "Retain yielded RECORD briefly for idempotent terminal polling."
  (let ((preserve-spool
         (and (mevedel-execution--record-retained-p record)
              (not (file-remote-p
                    (or (mevedel-execution--record-workdir record) ""))))))
    (if (mevedel-execution--record-yielded-p record)
        (setf (mevedel-execution--record-retire-timer record)
              (run-at-time
               mevedel-execution--terminal-retention-seconds nil
               #'mevedel-execution--cleanup-record record preserve-spool))
      (mevedel-execution--cleanup-record record preserve-spool))))

(defun mevedel-execution--finish-record (record status &optional error-data)
  "Settle RECORD once with STATUS and optional ERROR-DATA."
  (unless (mevedel-execution--record-finished-p record)
    (when-let* ((process (mevedel-execution--record-process record))
                ((memq (process-status process) '(exit signal))))
      (while (accept-process-output process 0.01 nil 1)))
    (setf (mevedel-execution--record-finished-p record) t)
    (let* ((path (mevedel-execution--record-spool-path record))
           (bytes (or (and (file-readable-p path)
                           (file-attribute-size (file-attributes path)))
                      0))
           (output (mevedel-execution--read-output record))
           (callback (mevedel-execution--record-callback record))
           (result
            (list :exit-code status
                  :output output
                  :output-bytes bytes
                  :termination
                  (mevedel-execution--record-termination record)
                  :timed-out-p
                  (mevedel-execution--record-timed-out-p record)
                  :output-limit-p
                  (mevedel-execution--record-output-limit-p record)
                  :wall-time-seconds
                  (- (float-time)
                     (mevedel-execution--record-started-at record))
                  :error error-data)))
      (mevedel-execution--cleanup-record record)
      (funcall callback result))))

(defun mevedel-execution--settle-after-kill (record)
  "Settle stopped RECORD after its final process-group signal."
  (unless (mevedel-execution--record-finished-p record)
    (when (and (file-remote-p
                (or (mevedel-execution--record-workdir record) ""))
               (not (eq 'unknown
                        (mevedel-execution--record-termination record)))
               (mevedel-execution--group-live-p record))
      (mevedel-execution--mark-unknown
       record
       '(mevedel-execution-error
         "Remote process group survived the final KILL signal")))
    (if (mevedel-execution--record-execution-id record)
        (unless (and (mevedel-execution--record-stop-p record)
                     (mevedel-execution--timer-pending-p
                      (mevedel-execution--record-force-timer record)))
          (mevedel-execution--finish-managed record))
      (mevedel-execution--finish-record
       record (or (mevedel-execution--record-exit-code record) -1)))))

(defun mevedel-execution--force-kill (record)
  "Force-kill RECORD's process group and schedule bounded settlement."
  (unless (mevedel-execution--record-finished-p record)
    (setf (mevedel-execution--record-force-timer record) nil)
    (mevedel-execution--signal-record record 'KILL)
    (unless (mevedel-execution--timer-pending-p
             (mevedel-execution--record-settle-timer record))
      (setf (mevedel-execution--record-settle-timer record)
            (run-at-time mevedel-execution--child-kill-delay nil
                         #'mevedel-execution--settle-after-kill record)))))

(defun mevedel-execution--start-stop (record)
  "Send RECORD's first stop signal outside its process filter."
  (setf (mevedel-execution--record-force-timer record) nil)
  (unless (mevedel-execution--record-finished-p record)
    (mevedel-execution--signal-record record 'TERM)
    (unless (eq 'unknown (mevedel-execution--record-termination record))
      (setf (mevedel-execution--record-force-timer record)
            (run-at-time mevedel-execution--child-kill-delay nil
                         #'mevedel-execution--force-kill record)))))

(defun mevedel-execution--begin-stop (record reason)
  "Latch REASON and terminate RECORD with TERM then bounded KILL grace."
  (unless (or (mevedel-execution--record-finished-p record)
              (mevedel-execution--record-stop-p record))
    (setf (mevedel-execution--record-stop-p record) t
          (mevedel-execution--record-termination record) reason)
    (if (and (mevedel-execution--record-workdir record)
             (file-remote-p (mevedel-execution--record-workdir record)))
        (setf (mevedel-execution--record-force-timer record)
              (run-at-time 0 nil #'mevedel-execution--start-stop record))
      (mevedel-execution--signal-record record 'TERM)
      (setf (mevedel-execution--record-force-timer record)
            (run-at-time mevedel-execution--child-kill-delay nil
                         #'mevedel-execution--force-kill record)))
    t))

(defun mevedel-execution--settle-timed-out (record)
  "Forcibly settle timed-out RECORD when its escalation never finished.
A remote child that is still unaccounted for at this point has an
unprovable outcome and is recorded as such before settlement."
  (unless (mevedel-execution--record-finished-p record)
    (when (and (file-remote-p
                (or (mevedel-execution--record-workdir record) ""))
               (not (eq 'unknown
                        (mevedel-execution--record-termination record))))
      (mevedel-execution--mark-unknown
       record
       '(mevedel-execution-error
         "Timed-out child did not settle within the bounded escalation")))
    (mevedel-execution--finish-record
     record (or (mevedel-execution--record-exit-code record) -1)
     (mevedel-execution--record-error-data record))))

(defun mevedel-execution--time-out (record)
  "Mark RECORD timed out and terminate its process group."
  (when (mevedel-execution--begin-stop record 'timed-out)
    (setf (mevedel-execution--record-timed-out-p record) t)
    ;; A raw one-shot caller waits synchronously on this record, so a
    ;; wedged transport inside the TERM/KILL escalation must not leave
    ;; that wait unbounded.  Settle with what is known once the bounded
    ;; escalation window has passed.
    (unless (mevedel-execution--record-execution-id record)
      (run-at-time (+ (* 2 mevedel-execution--child-kill-delay)
                      (* 4 mevedel-execution--remote-control-timeout))
                   nil #'mevedel-execution--settle-timed-out record))))

(defun mevedel-execution--settle-main-exit (record)
  "Clean remaining descendants, or settle RECORD after its main process exits."
  (let ((managed-p (mevedel-execution--record-execution-id record))
        (workdir (mevedel-execution--record-workdir record)))
    (setf (mevedel-execution--record-settle-timer record) nil)
    (if (and (or managed-p
                 (and (not (eq system-type 'windows-nt))
                      (not (file-remote-p (or workdir "")))))
             (mevedel-execution--group-live-p record))
      (mevedel-execution--begin-stop
       record (or (mevedel-execution--record-termination record) 'exited))
      (if managed-p
          (mevedel-execution--finish-managed record)
        (mevedel-execution--finish-record
         record (or (mevedel-execution--record-exit-code record) -1))))))

(defun mevedel-execution--settle-stop-main-exit (record)
  "Finish stopping RECORD early when its remote group already settled.

A probe that reports the group dead cancels the pending kill grace and
settles now.  A live, ambiguous, or failed probe changes nothing: the
bounded escalation stays in charge, and no unknown outcome is latched
here -- a transient transport failure at this moment must not poison a
record the escalation could still settle cleanly."
  (setf (mevedel-execution--record-settle-timer record) nil)
  (unless (mevedel-execution--record-finished-p record)
    (when (eq 'dead (condition-case nil
                        (mevedel-execution--remote-group-status record)
                      (error nil)))
      (let ((timer (mevedel-execution--record-force-timer record)))
        (when (timerp timer)
          (cancel-timer timer))
        (setf (mevedel-execution--record-force-timer record) nil))
      (if (mevedel-execution--record-execution-id record)
          (mevedel-execution--finish-managed record)
        (mevedel-execution--finish-record
         record
         (or (mevedel-execution--record-exit-code record) -1))))))

(defun mevedel-execution--timer-pending-p (timer)
  "Return non-nil when TIMER is armed and still due to fire.

A slot can hold a timer object that will never run: one armed from a
process sentinel that fired inside a TRAMP wait lands on a let-bound
`timer-list' and vanishes when the binding exits.  Treating such a
stranded object as already scheduled would wedge settlement, so the
watchdog checks list membership, not just the slot."
  (and (timerp timer) (memq timer timer-list) t))

(defun mevedel-execution--process-ended (record process)
  "Settle RECORD when PROCESS reaches a terminal state."
  (when (eq process (mevedel-execution--record-process record))
    (let ((status (process-status process)))
      (when (memq status '(exit signal))
        (let ((exit-code (process-exit-status process)))
          (setf (mevedel-execution--record-exit-code record) exit-code)
          (when (and (not (mevedel-execution--record-termination record))
                     (eq status 'signal))
            (setf (mevedel-execution--record-termination record) 'signaled)))
        (cond
         ((mevedel-execution--timer-pending-p
           (mevedel-execution--record-settle-timer record)))
         ((and (mevedel-execution--record-stop-p record)
               (mevedel-execution--timer-pending-p
                (mevedel-execution--record-force-timer record)))
          ;; A stop is escalating.  For a remote record the wrapper only
          ;; exits once its group has drained of non-zombies, so the main
          ;; exit is the moment to confirm with one probe and settle early
          ;; instead of riding the kill grace.
          (when (file-remote-p
                 (or (mevedel-execution--record-workdir record) ""))
            (setf (mevedel-execution--record-settle-timer record)
                  (run-at-time
                   0.02 nil
                   #'mevedel-execution--settle-stop-main-exit record))))
         (t
          (setf (mevedel-execution--record-settle-timer record)
                (run-at-time
                 0.02 nil #'mevedel-execution--settle-main-exit record))))))))

(defun mevedel-execution--launch-record
    (record name command workdir coding filter)
  "Launch COMMAND into RECORD using NAME, WORKDIR, CODING, and FILTER."
  (let* ((remote (file-remote-p workdir))
         (command (mevedel-execution--localize-command
                   record command workdir))
         (direct-async
          (and remote
               (mevedel-execution--direct-async-p record command workdir)))
         (_ (setf (mevedel-execution--record-direct-async-p record)
                  direct-async))
         (command (if remote
                      (mevedel-execution--remote-command record command)
                    command))
         (executable (car-safe command))
         (process-environment
          (mevedel-execution--process-environment remote)))
    ;; `make-process' selects its file handler from the current buffer's
    ;; actual `default-directory', not merely a dynamic binding.
    (with-temp-buffer
      (setq default-directory workdir)
      (unless (and (stringp executable)
                   (if remote
                       (executable-find executable remote)
                     (executable-find executable)))
        (signal 'file-missing (list "Executable not found" executable)))
      (setf (mevedel-execution--record-launch-attempted-p record) t)
      (setf (mevedel-execution--record-process record)
            ;; The spawn channel is decided here, per record, rather than
            ;; through a connection-local profile: a profile flips every
            ;; make-process on the host, including other packages', and a
            ;; TTY spawn on an enabled connection must still be classic.
            ;; The predicate has a single caller inside TRAMP's handler
            ;; and is fboundp-gated in the eligibility check.
            (mevedel-execution--with-spawn-channel remote direct-async
              (lambda ()
                (make-process
                 :name name :buffer nil :command command
                 :coding coding
                 :connection-type
                 (if (mevedel-execution--record-tty-p record) 'pty 'pipe)
                 :file-handler t
                 :filter (lambda (_process chunk)
                           (mevedel-execution--filter-group-marker
                            record filter chunk))
                 :noquery t
                 :sentinel (lambda (process _event)
                             (mevedel-execution--process-ended
                              record process)))))))
    (unless remote
      (setf (mevedel-execution--record-group-id record)
            (process-id (mevedel-execution--record-process record))))
    (when (and (not (mevedel-execution--record-tty-p record))
               (process-live-p
                (mevedel-execution--record-process record)))
      (process-send-eof (mevedel-execution--record-process record)))
    ;; A terminal status can be visible while sentinels are inhibited.
    (setf (mevedel-execution--record-watch-timer record)
          (run-at-time
           0.1 0.1
           (lambda ()
             (unless (mevedel-execution--record-finished-p record)
               (let ((process (mevedel-execution--record-process record)))
                 (when (and (processp process)
                            (memq (process-status process) '(exit signal)))
                   (mevedel-execution--process-ended record process)))))))
    (mevedel-execution--record-process record)))

(defun mevedel-execution--start-process
    (callback name command workdir timeout session owner teardown-function)
  "Start raw COMMAND and call CALLBACK with its bounded terminal result."
  (let* ((record
          (mevedel-execution--record-create
           :callback callback
           :origin (mevedel-execution--origin-create
                    :owner (or owner "/root") :session session)
           :spool-path (make-temp-file "mevedel-execution-output-")
           :started-at (float-time)
           :teardown-function teardown-function
           :token (gensym "execution-process-")
           :workdir workdir))
         (state (mevedel-execution--state-for-session session)))
    (puthash (mevedel-execution--record-token record) record
             (mevedel-execution--state-records state))
    (condition-case err
        (progn
          (mevedel-execution--launch-record
           record name command workdir 'no-conversion
           #'mevedel-execution--append-output)
          (when timeout
            (setf (mevedel-execution--record-timeout-timer record)
                  (run-at-time timeout nil
                               #'mevedel-execution--time-out record)))
          (mevedel-execution--record-process record))
      (error
       (mevedel-execution--finish-record record -1 err)
       nil))))


;;
;;; Managed Bash interface

(defun mevedel-execution--record-output-bytes (record)
  "Return the current spool size for RECORD."
  (let ((path (mevedel-execution--record-spool-path record)))
    (or (and (file-readable-p path)
             (file-attribute-size (file-attributes path)))
        0)))

(defun mevedel-execution--managed-count (state)
  "Return the number of live managed records in STATE."
  (let ((count 0))
    (maphash
     (lambda (_key record)
       (when (mevedel-execution--managed-live-p record)
         (setq count (1+ count))))
     (mevedel-execution--state-records state))
    count))

(defun mevedel-execution--telemetry (record event &rest props)
  "Record safe execution EVENT and PROPS for RECORD."
  (require 'mevedel-execution-telemetry)
  (let* ((origin (mevedel-execution--record-origin record))
         (session (and (mevedel-execution--origin-p origin)
                       (mevedel-execution--origin-session origin))))
    (mevedel-execution-telemetry-record
     session
     (mevedel-execution--record-execution-id record)
     (and origin (mevedel-execution--origin-tool-use-id origin))
     (and origin (mevedel-execution--origin-owner origin))
     event props)))

(defun mevedel-execution--next-id (state)
  "Return the next opaque execution id in STATE."
  (let ((next (1+ (mevedel-execution--state-next-id state))))
    (setf (mevedel-execution--state-next-id state) next)
    (format "exec-%06d" next)))

(defun mevedel-execution--utf8-prefix (text maximum-bytes)
  "Return the longest prefix of TEXT no larger than MAXIMUM-BYTES in UTF-8."
  (if (<= (string-bytes text) maximum-bytes)
      text
    (let ((low 0)
          (high (length text)))
      (while (< low high)
        (let ((middle (/ (+ low high 1) 2)))
          (if (<= (string-bytes (substring text 0 middle)) maximum-bytes)
              (setq low middle)
            (setq high (1- middle)))))
      (substring text 0 low))))

(defun mevedel-execution--retain-output (record text)
  "Retain bounded whole and unread preview TEXT in RECORD."
  (cl-labels
      ((next (chars head tail)
         (let* ((limit mevedel-execution-inline-output-limit)
                (head (concat (or head "") text))
                (tail (concat (or tail "") text)))
           (list (+ (or chars 0) (length text))
                 (substring head 0 (min limit (length head)))
                 (substring tail (max 0 (- (length tail) limit)))))))
    (pcase-let ((`(,chars ,head ,tail)
                 (next (mevedel-execution--record-output-chars record)
                       (mevedel-execution--record-output-head record)
                       (mevedel-execution--record-output-tail record))))
      (setf (mevedel-execution--record-output-chars record) chars
            (mevedel-execution--record-output-head record) head
            (mevedel-execution--record-output-tail record) tail))
    (pcase-let ((`(,chars ,head ,tail)
                 (next (mevedel-execution--record-unread-chars record)
                       (mevedel-execution--record-unread-head record)
                       (mevedel-execution--record-unread-tail record))))
      (setf (mevedel-execution--record-unread-chars record) chars
            (mevedel-execution--record-unread-head record) head
            (mevedel-execution--record-unread-tail record) tail))))

(defun mevedel-execution--write-managed-output (record text)
  "Write a UTF-8-safe bounded prefix of TEXT and update RECORD."
  (let* ((path (mevedel-execution--record-spool-path record))
         (current (mevedel-execution--record-output-bytes record))
         (remaining (max 0 (- mevedel-execution-output-limit current)))
         (written-text (mevedel-execution--utf8-prefix text remaining))
         write-error)
    (unless (string-empty-p written-text)
      (let ((coding-system-for-write 'utf-8-unix))
        (condition-case err
            (write-region written-text nil path t 'silent)
          (file-error (setq write-error err))))
      (unless write-error
        (when (zerop current)
          (mevedel-execution--telemetry
           record 'execution-first-output
           :chunk-bytes (string-bytes written-text)))
        (mevedel-execution--retain-output record written-text)
        (cl-incf (mevedel-execution--record-newline-count record)
                 (cl-count ?\n written-text))
        (setf (mevedel-execution--record-last-byte-newline-p record)
              (eq (aref written-text (1- (length written-text))) ?\n))))
    (if write-error
        (progn
          (setf (mevedel-execution--record-error-data record) write-error)
          (mevedel-execution--begin-stop record 'output-write-failed))
      (when (< (length written-text) (length text))
        (setf (mevedel-execution--record-output-limit-p record) t)
        (mevedel-execution--begin-stop record 'output-limit)))))

(defun mevedel-execution--managed-append (record chunk)
  "Remove RECORD's private sandbox marker and spool CHUNK."
  (unless (mevedel-execution--record-finished-p record)
    (let ((marker (mevedel-execution--record-marker record)))
      (when (and marker
                 (not (eq (mevedel-execution--record-marker-buffer record)
                          :done)))
        (setq chunk
              (concat (or (mevedel-execution--record-marker-buffer record) "")
                      chunk))
        (if-let* ((newline (string-search "\n" chunk)))
            (let ((first (substring chunk 0 newline)))
              (setf (mevedel-execution--record-marker-buffer record) :done)
              (when (equal first marker)
                (setf (mevedel-execution--record-marker-seen-p record) t)
                (setq chunk (substring chunk (1+ newline)))))
          (setf (mevedel-execution--record-marker-buffer record) chunk)
          (setq chunk "")))
      (unless (string-empty-p chunk)
        (mevedel-execution--write-managed-output record chunk)))))

(defun mevedel-execution--managed-lines (record)
  "Return the retained logical line count for RECORD."
  (let ((bytes (mevedel-execution--record-output-bytes record))
        (newlines (or (mevedel-execution--record-newline-count record) 0)))
    (+ newlines
       (if (and (> bytes 0)
                (not (mevedel-execution--record-last-byte-newline-p record)))
           1
         0))))

(defun mevedel-execution--termination (record)
  "Return RECORD's canonical termination symbol, or nil while running."
  (when (mevedel-execution--record-finished-p record)
    (or (mevedel-execution--record-termination record)
        (and (mevedel-execution--record-error-data record) 'spawn-failed)
        'exited)))

(defun mevedel-execution--resolve-outcome
    (outcome-function exit-code termination)
  "Resolve a canonical outcome without risking execution settlement.

OUTCOME-FUNCTION may interpret EXIT-CODE and TERMINATION.  Without one,
preserve the default zero-success/nonzero-failure rule."
  (let ((default-outcome
         (if (and (integerp exit-code) (zerop exit-code))
             'success
           'failure)))
    (if (null outcome-function)
        default-outcome
      (condition-case err
          (let ((outcome (funcall outcome-function exit-code termination)))
            (unless (memq outcome '(success failure no-match different false))
              (error "Invalid execution outcome: %S" outcome))
            outcome)
        (error
         (display-warning
          'mevedel
          (format "Execution outcome resolver failed: %s"
                  (error-message-string err))
          :warning)
         default-outcome)))))

(defun mevedel-execution--lifecycle-state (record)
  "Return RECORD's canonical public lifecycle state."
  (cond
   ((mevedel-execution--record-finished-p record) 'completed)
   ((mevedel-execution--record-stop-p record) 'stopping)
   ((processp (mevedel-execution--record-process record)) 'running)
   (t 'queued)))

(defun mevedel-execution--artifact-address (record)
  "Return RECORD's logical model-visible artifact address, or nil.

A remote execution keeps its recoverable output artifact, so its address
does not depend on the local spool the target never wrote to."
  (when-let* ((path
               (if (file-remote-p
                    (or (mevedel-execution--record-workdir record) ""))
                   (mevedel-execution--record-recoverable-output-path record)
                 (and (mevedel-execution--record-yielded-p record)
                      (mevedel-execution--record-spool-path record)))))
    (require 'mevedel-resource)
    (mevedel-resource-artifact-address
     path
     (mevedel-execution--origin-session
      (mevedel-execution--record-origin record)))))

(defun mevedel-execution--facts (record)
  "Return an immutable public fact snapshot for RECORD."
  (let* ((finished (mevedel-execution--record-finished-p record))
         (origin (mevedel-execution--record-origin record))
         (exit-code (and finished
                         (mevedel-execution--record-exit-code record)))
         (termination (mevedel-execution--termination record))
         (outcome-function
          (mevedel-execution--record-outcome-function record)))
    (list :execution-id
          (and (mevedel-execution--record-yielded-p record)
               (mevedel-execution--record-execution-id record))
          :command
          (plist-get (mevedel-execution--origin-tool-args origin) :command)
          :state (mevedel-execution--lifecycle-state record)
          :termination termination
          :exit-code exit-code
          :outcome (and finished
                        (mevedel-execution--resolve-outcome
                         outcome-function exit-code termination))
          :wall-time-seconds
          (- (float-time) (mevedel-execution--record-started-at record))
          :output-bytes (mevedel-execution--record-output-bytes record)
          :output-lines (mevedel-execution--managed-lines record)
          :omitted-output-bytes
          (or (mevedel-execution--record-omitted-output-bytes record) 0)
          :tty (and (mevedel-execution--record-tty-p record) t)
          :direct-async
          (and (mevedel-execution--record-direct-async-p record) t)
          :group-id (mevedel-execution--record-group-id record)
          :output-path (mevedel-execution--artifact-address record))))

(defun mevedel-execution--event (record type &rest properties)
  "Return an immutable TYPE event for RECORD with PROPERTIES."
  (append
   (list :type type
         :emitted-at (float-time)
         :session (mevedel-execution--origin-session
                   (mevedel-execution--record-origin record))
         :data-buffer (mevedel-execution--origin-data-buffer
                       (mevedel-execution--record-origin record))
         :owner (mevedel-execution--origin-owner
                 (mevedel-execution--record-origin record))
         :tool-args
         (copy-tree
          (mevedel-execution--origin-tool-args
           (mevedel-execution--record-origin record)))
         :tool-use-id (mevedel-execution--origin-tool-use-id
                       (mevedel-execution--record-origin record))
         :facts (mevedel-execution--facts record))
   properties))

(defun mevedel-execution--copy-value (value)
  "Return an isolated copy of VALUE, including mutable string leaves."
  (cl-labels
      ((copy-leaves (value)
         (cond
          ((stringp value) (copy-sequence value))
          ((consp value)
           (cons (copy-leaves (car value))
                 (copy-leaves (cdr value))))
          (t value))))
    (copy-leaves value)))

(defun mevedel-execution--emit-event (event)
  "Publish EVENT to passive consumers, ignoring their return values."
  (dolist (function mevedel-execution-event-functions)
    (when (functionp function)
      (condition-case err
          (funcall function (mevedel-execution--copy-value event))
        (error
         (display-warning
          'mevedel
          (format "Execution event consumer failed: %s"
                  (error-message-string err))
          :warning))))))

(defun mevedel-execution--deliver-mailbox-event (record event)
  "Return non-nil after the mailbox sink secures RECORD's EVENT."
  (when (functionp mevedel-execution-mailbox-delivery-function)
    (condition-case err
        (funcall mevedel-execution-mailbox-delivery-function
                 (mevedel-execution--copy-value event)
                 (mevedel-execution--origin-owner-context
                  (mevedel-execution--record-origin record)))
      (error
       (display-warning
        'mevedel
        (format "Execution mailbox delivery failed: %s"
                (error-message-string err))
        :warning)
       nil))))

(defun mevedel-execution--whole-preview (record)
  "Return RECORD's bounded whole-artifact head-and-tail preview."
  (require 'mevedel-utilities)
  (plist-get
   (mevedel--head-tail-preview-parts
    (or (mevedel-execution--record-output-head record) "")
    (or (mevedel-execution--record-output-tail record) "")
    (or (mevedel-execution--record-output-chars record) 0)
    mevedel-execution-inline-output-limit)
   :text))

(defun mevedel-execution--emit-progress (record)
  "Publish one bounded progress event for live RECORD."
  (unless (mevedel-execution--record-finished-p record)
    (mevedel-execution--emit-event
     (mevedel-execution--event
      record 'progress
      :output-tail (or (mevedel-execution--record-output-tail record) "")))
    (setf (mevedel-execution--record-progress-timer record)
          (run-at-time
           (max 0.25 mevedel-execution-progress-interval) nil
           #'mevedel-execution--emit-progress record))))

(defun mevedel-execution--unread-preview (record unread-bytes)
  "Return RECORD's bounded unread preview for UNREAD-BYTES."
  (require 'mevedel-utilities)
  (let* ((parts
          (mevedel--head-tail-preview-parts
           (or (mevedel-execution--record-unread-head record) "")
           (or (mevedel-execution--record-unread-tail record) "")
           (or (mevedel-execution--record-unread-chars record) 0)
           mevedel-execution-inline-output-limit))
         (visible-bytes
          (+ (string-bytes (plist-get parts :head))
             (string-bytes (plist-get parts :tail)))))
    (list :output (plist-get parts :text)
          :omitted (max 0 (- unread-bytes visible-bytes)))))

(defun mevedel-execution--unread-range (record)
  "Return RECORD's next unread range without consuming it."
  (let* ((start (or (mevedel-execution--record-read-offset record) 0))
         (end (mevedel-execution--record-output-bytes record))
         (preview (mevedel-execution--unread-preview record (- end start))))
    (list :end end
          :omitted (plist-get preview :omitted)
          :output (plist-get preview :output))))

(defun mevedel-execution--publish-remote-output
    (record range &optional terminal-p)
  "Publish RECORD's recoverable remote output for RANGE.

Publish when RANGE omits bytes, after a prior snapshot, or when TERMINAL-P
settles a yielded execution.  The live spool always remains local."
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (let* ((origin (mevedel-execution--record-origin record))
         (session (mevedel-execution--origin-session origin))
         (target (and session (mevedel-session-execution-target session)))
         (save-path (and session (mevedel-session-save-path session)))
         (bytes (mevedel-execution--record-output-bytes record)))
    (when (and target save-path
               (mevedel-execution-target-remote-p target)
               (> bytes 0)
               (or (> (or (plist-get range :omitted) 0) 0)
                   (mevedel-execution--record-recoverable-output-path record)
                   (and terminal-p
                        (mevedel-execution--record-yielded-p record)))
               (not (equal bytes
                           (mevedel-execution--record-recoverable-output-bytes
                            record))))
      (let* ((qualified
              (file-name-concat
               save-path "tool-results" "executions"
               (concat (mevedel-execution--record-execution-id record)
                       ".log")))
             (native
              (mevedel-execution-target-native-path target qualified)))
        (condition-case err
            (progn
              (when
                  (memq
                   (mevedel-session-artifacts-publish-text
                    session qualified
                    (mevedel-execution--read-output record)
                    'utf-8-unix)
                   '(published queued))
                (setf
                 (mevedel-execution--record-recoverable-output-bytes record)
                 bytes
                 (mevedel-execution--record-recoverable-output-path record)
                 native)))
          (error
           (display-warning
            'mevedel
            (format "Could not publish remote execution output: %s"
                    (error-message-string err))
            :warning)))))))

(defun mevedel-execution--consume-unread-range (record range)
  "Commit consumption of RECORD's unread RANGE."
  (let ((omitted (plist-get range :omitted)))
    (setf (mevedel-execution--record-read-offset record)
          (plist-get range :end)
          (mevedel-execution--record-unread-chars record) 0
          (mevedel-execution--record-unread-head record) ""
          (mevedel-execution--record-unread-tail record) ""
          (mevedel-execution--record-omitted-output-bytes record)
          (+ (or (mevedel-execution--record-omitted-output-bytes record) 0)
             omitted))
    (when (> omitted 0)
      (setf (mevedel-execution--record-retained-p record) t))))

(defun mevedel-execution--range-observation
    (record range &optional claimed project-unconsumed)
  "Return an observation of RECORD and unread RANGE.
CLAIMED non-nil marks it as the final model delivery.
PROJECT-UNCONSUMED includes RANGE's omission in facts before commitment."
  (let ((facts (mevedel-execution--facts record)))
    (when project-unconsumed
      (setq facts
            (plist-put
             facts :omitted-output-bytes
             (+ (or (plist-get facts :omitted-output-bytes) 0)
                (plist-get range :omitted)))))
    (list :output (plist-get range :output)
          :facts facts
          :sandbox-facts (mevedel-execution--record-sandbox-facts record)
          :sandbox-summary
          (copy-tree
           (car (mevedel-execution--record-sandbox-summary-cell record)))
          :error (mevedel-execution--record-error-data record)
          :claimed-final-p (and claimed t))))

(defun mevedel-execution--terminal-event (record delivery observation)
  "Return RECORD's terminal event for DELIVERY and OBSERVATION."
  (let ((event
         (mevedel-execution--event
          record 'terminal
          :delivery delivery
          :observation observation
          :whole-output (mevedel-execution--whole-preview record))))
    (plist-put event :facts (plist-get observation :facts))))

(defun mevedel-execution--observation (record &optional claim-final)
  "Return RECORD's next unread observation.

When CLAIM-FINAL is non-nil, publish the first terminal observation and cache
it briefly so repeated owner polls return the same result."
  (if-let* ((terminal
             (and claim-final
                  (mevedel-execution--record-terminal-observation record))))
      (mevedel-execution--copy-value terminal)
    (when (and claim-final
               (mevedel-execution--record-delivery-state record))
      (signal 'mevedel-execution-not-found
              (list "Execution terminal result is already claimed")))
    (when claim-final
      (setf (mevedel-execution--record-delivery-state record) 'model))
    (let ((range (mevedel-execution--unread-range record)))
      (mevedel-execution--publish-remote-output record range claim-final)
      (mevedel-execution--consume-unread-range record range)
      (let ((observation
             (mevedel-execution--range-observation record range claim-final)))
        (when claim-final
          (setf (mevedel-execution--record-terminal-observation record)
                (mevedel-execution--copy-value observation))
          (mevedel-execution--emit-event
           (mevedel-execution--terminal-event record 'model observation))
          (mevedel-execution--retire-terminal-record record))
        observation))))

(defun mevedel-execution--deliver-independent (record)
  "Secure RECORD's unread terminal result in its owner mailbox."
  (unless (mevedel-execution--record-delivery-state record)
    (setf (mevedel-execution--record-delivery-state record) 'mailbox)
    (let* ((range (mevedel-execution--unread-range record))
           (_ (mevedel-execution--publish-remote-output record range t))
           (observation
            (mevedel-execution--range-observation record range t t))
           (event
            (mevedel-execution--terminal-event record 'mailbox observation)))
      (let ((delivered-p
             (mevedel-execution--deliver-mailbox-event record event)))
        (mevedel-execution--emit-event event)
        (if delivered-p
          (progn
            (setf (mevedel-execution--record-terminal-observation record)
                  (mevedel-execution--copy-value observation))
            (mevedel-execution--consume-unread-range record range)
            (mevedel-execution--retire-terminal-record record)
            t)
          (display-warning
           'mevedel
           (format "Execution %s completion has no mailbox consumer"
                   (mevedel-execution--record-execution-id record))
           :warning)
          (setf (mevedel-execution--record-delivery-state record) nil
                (mevedel-execution--record-terminal-observation record) nil)
          nil)))))

(defun mevedel-execution--deliver-observer (record)
  "Deliver RECORD's terminal observation to its waiting observer."
  (when-let* ((callback (mevedel-execution--record-observer record)))
    (setf (mevedel-execution--record-observer record) nil
          (mevedel-execution--record-observer-timer record) nil)
    (funcall callback (mevedel-execution--observation record t))))

(defun mevedel-execution--cancel-observer (record callback)
  "Detach CALLBACK from RECORD without consuming unread output."
  (when (eq callback (mevedel-execution--record-observer record))
    (when-let* ((timer (mevedel-execution--record-observer-timer record)))
      (cancel-timer timer))
    (setf (mevedel-execution--record-observer record) nil
          (mevedel-execution--record-observer-timer record) nil)))

(defun mevedel-execution--flush-observer (record)
  "Complete RECORD's pending poll before a control operation takes over."
  (when-let* ((callback (mevedel-execution--record-observer record)))
    (when-let* ((timer (mevedel-execution--record-observer-timer record)))
      (cancel-timer timer))
    (setf (mevedel-execution--record-observer record) nil
          (mevedel-execution--record-observer-timer record) nil)
    (condition-case err
        (funcall callback (mevedel-execution--observation record))
      (error
       (display-warning
        'mevedel
        (format "Execution poll callback failed: %s"
                (error-message-string err))
        :warning)))))

(defun mevedel-execution--poll-expired (record)
  "Deliver a running observation when RECORD's poll wait expires."
  (when-let* ((callback (mevedel-execution--record-observer record)))
    (setf (mevedel-execution--record-observer record) nil
          (mevedel-execution--record-observer-timer record) nil)
    (funcall callback (mevedel-execution--observation record))))

(defun mevedel-execution--sandbox-launch-result (record)
  "Return the terminal child-result subset used for sandbox launch checks."
  (list :exit-code (mevedel-execution--record-exit-code record)
        :output (mevedel-execution--read-output record)
        :termination (mevedel-execution--termination record)
        :timed-out-p (mevedel-execution--record-timed-out-p record)
        :error (mevedel-execution--record-error-data record)))

(defun mevedel-execution--restart-unconfined (record facts)
  "Restart RECORD's original command without confinement using FACTS."
  (let* ((preparation
          (mevedel-execution--record-sandbox-preparation record))
         (session
          (mevedel-execution--origin-session
           (mevedel-execution--record-origin record)))
         (facts (mevedel-execution-telemetry-mark-direct-fallback session facts)))
    (apply #'mevedel-execution--telemetry
           record 'sandbox-fallback
           :launch-failure-stage 'before-command-start
           :launch-failure-reason-class 'sandbox-launch-failure
           :fallback-offered t
           :full-execution-approval-offered nil
           (mevedel-execution-telemetry-safe-facts facts))
    (apply #'mevedel-execution--telemetry
           record 'execution-unrestricted
           :reason-class 'sandbox-launch-failure
           :after-confined-launch-failure t
           (mevedel-execution-telemetry-safe-facts facts))
    (mevedel-execution--release-runtime record)
    (mevedel-sandbox-cleanup preparation)
    (let ((coding-system-for-write 'no-conversion))
      (write-region "" nil (mevedel-execution--record-spool-path record)
                    nil 'silent))
    (setf (mevedel-execution--record-exit-code record) nil
          (mevedel-execution--record-error-data record) nil
          (mevedel-execution--record-marker record) nil
          (mevedel-execution--record-marker-buffer record) :done
          (mevedel-execution--record-marker-seen-p record) nil
          (mevedel-execution--record-last-byte-newline-p record) nil
          (mevedel-execution--record-newline-count record) 0
          (mevedel-execution--record-output-chars record) 0
          (mevedel-execution--record-output-head record) ""
          (mevedel-execution--record-output-limit-p record) nil
          (mevedel-execution--record-output-tail record) ""
          (mevedel-execution--record-read-offset record) 0
          (mevedel-execution--record-sandbox-facts record) facts
          (mevedel-execution--record-sandbox-preparation record) nil
          (mevedel-execution--record-stop-p record) nil
          (mevedel-execution--record-termination record) nil
          (mevedel-execution--record-timed-out-p record) nil
          (mevedel-execution--record-unread-chars record) 0
          (mevedel-execution--record-unread-head record) ""
          (mevedel-execution--record-unread-tail record) "")
    (mevedel-execution--launch-managed
     record (plist-get preparation :original-command))
    (unless (mevedel-execution--record-finished-p record)
      (mevedel-execution--arm-managed-timers record))))

(defun mevedel-execution--release-scheduler (record)
  "Release RECORD's scheduler lease exactly once."
  (when-let* ((lease (mevedel-execution--record-scheduler-lease record)))
    (setf (mevedel-execution--record-scheduler-lease record) nil)
    (mevedel-execution-scheduler-release lease)))

(defun mevedel-execution--finish-managed (record)
  "Settle managed RECORD without delivering unsolicited model output."
  (unless (mevedel-execution--record-finished-p record)
    (let* ((preparation
            (mevedel-execution--record-sandbox-preparation record))
           (child-result (and (eq (plist-get preparation :state) 'confined)
                              (not (mevedel-execution--record-marker-seen-p
                                    record))
                              (mevedel-execution--sandbox-launch-result record)))
           (launch-failed
            (and child-result
                 (mevedel-sandbox-launch-failed-p preparation child-result))))
      (if (and launch-failed
               (plist-get preparation :fallback-p)
               (not (mevedel-execution--record-stop-p record))
               (not (mevedel-execution--record-yielded-p record)))
          (mevedel-execution--restart-unconfined
           record (mevedel-sandbox--record-launch-failure
                   child-result (mevedel-execution--record-workdir record)))
        (when launch-failed
          (let ((facts
                 (mevedel-sandbox--record-launch-failure
                  child-result (mevedel-execution--record-workdir record))))
            (unless (plist-get preparation :fallback-p)
              (setq facts
                    (plist-put (copy-sequence facts) :refused t)))
            (setf (mevedel-execution--record-sandbox-facts record) facts)))
        (when preparation
          (mevedel-sandbox-cleanup preparation))
        (setf (mevedel-execution--record-finished-p record) t)
        (mevedel-execution--settle-mutation record)
        (let* ((facts (mevedel-execution--record-sandbox-facts record))
               (refused-p (plist-get facts :refused))
               (started-p
                (and (not refused-p)
                     (not launch-failed)
                     (processp (mevedel-execution--record-process record))))
               (origin (mevedel-execution--record-origin record)))
          (mevedel-execution-telemetry-record-sandbox-attempt
           facts started-p refused-p
           (mevedel-execution--record-sandbox-summary-cell record)
           (mevedel-execution--origin-sandbox-summary-cell origin)
           (mevedel-execution-telemetry-agent-summary-cell
            (mevedel-execution--origin-owner-context origin))))
        (apply #'mevedel-execution--telemetry
               record 'execution-finished
               :exit-code (mevedel-execution--record-exit-code record)
               :termination (mevedel-execution--termination record)
               :duration-ms
               (round (* 1000.0
                         (- (float-time)
                            (mevedel-execution--record-started-at record))))
               :output-bytes (mevedel-execution--record-output-bytes record)
               :output-limit (and (mevedel-execution--record-output-limit-p
                                   record)
                                  t)
               :timed-out (and (mevedel-execution--record-timed-out-p record)
                               t)
               :native-resource-report-bytes
               (when-let* ((report
                            (mevedel-execution--record-resource-report-path
                             record))
                           ((file-readable-p report)))
                 (file-attribute-size (file-attributes report)))
               (mevedel-execution-telemetry-safe-facts
                (mevedel-execution--record-sandbox-facts record)))
        (mevedel-execution--notify-state-change record)
        (mevedel-execution--release-runtime record)
        (mevedel-execution--release-scheduler record)
        (if (mevedel-execution--record-yielded-p record)
            (if (mevedel-execution--record-observer record)
                (mevedel-execution--deliver-observer record)
              (when mevedel-execution-mailbox-delivery-function
                (mevedel-execution--deliver-independent record)))
          (let ((callback (mevedel-execution--record-callback record)))
            (unless (eq (mevedel-execution--record-delivery-state record)
                        'discarded)
              (funcall callback
                       (mevedel-execution--observation record t)))))))))

(defun mevedel-execution--publish-yielded-artifact (record)
  "Publish RECORD's spool without moving a file still being appended.

Foreground output stays under a hidden pending directory until the first
yield.  The initial bytes are copied into the retained directory, then future
process-filter appends use the published path."
  (let* ((path (mevedel-execution--record-spool-path record))
         (private-directory (and path (file-name-directory path)))
         (private-name (and private-directory
                            (file-name-nondirectory
                             (directory-file-name private-directory)))))
    (when (and path
               (equal private-name ".mevedel-pending-executions"))
      (let* ((public-directory
              (file-name-directory (directory-file-name private-directory)))
             (target (file-name-concat
                      public-directory (file-name-nondirectory path))))
        (condition-case nil
            (progn
              (make-directory public-directory t)
              (copy-file path target t)
              (setf (mevedel-execution--record-spool-path record) target)
              (delete-file path))
          (error nil))))))

(defun mevedel-execution--yield-managed (record)
  "Deliver RECORD's initial running observation and detach it from request."
  (unless (or (mevedel-execution--record-finished-p record)
              (mevedel-execution--record-yielded-p record))
    (let ((process (mevedel-execution--record-process record)))
      (if (and (processp process)
               (memq (process-status process) '(exit signal)))
          (mevedel-execution--process-ended record process)
        (mevedel-execution--publish-yielded-artifact record)
        (setf (mevedel-execution--record-yielded-p record) t
              (mevedel-execution--record-retained-p record) t)
        (mevedel-execution--release-scheduler record)
        (mevedel-execution--emit-event
         (mevedel-execution--event record 'yield))
        (funcall (mevedel-execution--record-callback record)
                 (mevedel-execution--observation record))))))

(defun mevedel-execution--arm-managed-timers (record)
  "Arm RECORD's progress and yield clocks."
  (when (and mevedel-execution-event-functions
             (mevedel-execution--origin-tool-use-id
              (mevedel-execution--record-origin record)))
    (setf (mevedel-execution--record-progress-timer record)
          (run-at-time
           (max 0 mevedel-execution-progress-delay)
           nil
           #'mevedel-execution--emit-progress record)))
  (when-let* ((yield-time-ms
               (mevedel-execution--record-yield-time-ms record)))
    (setf (mevedel-execution--record-yield-timer record)
          (run-at-time
           (/ yield-time-ms 1000.0)
           nil #'mevedel-execution--yield-managed record))))

(defun mevedel-execution--launch-managed (record command)
  "Launch managed RECORD with raw COMMAND."
  (condition-case err
      (progn
        (setf (mevedel-execution--record-launch-attempted-p record) nil)
        (mevedel-execution--launch-record
         record "mevedel-bash" command
         (mevedel-execution--record-workdir record)
         'utf-8-unix #'mevedel-execution--managed-append)
        (when (process-live-p (mevedel-execution--record-process record))
          (apply #'mevedel-execution--telemetry
                 record 'execution-started
                 (mevedel-execution-telemetry-safe-facts
                  (mevedel-execution--record-sandbox-facts record)))))
    (error
     (if (and (mevedel-execution--record-mutation-armed-p record)
              (mevedel-execution--record-launch-attempted-p record))
         (mevedel-execution--mark-unknown record err)
       (setf (mevedel-execution--record-error-data record) err
             (mevedel-execution--record-termination record) 'spawn-failed))
     (setf (mevedel-execution--record-exit-code record) -1)
     (mevedel-execution--finish-managed record))))

(defun mevedel-execution--start-admitted (record preparation)
  "Start scheduler-admitted RECORD from PREPARATION."
  (when (and (eq (plist-get preparation :state) 'unrestricted)
             (eq (plist-get (plist-get preparation :facts) :sandbox)
                 'unavailable))
    (plist-put
     preparation :facts
     (mevedel-execution-telemetry-mark-direct-fallback
      (mevedel-execution--origin-session
       (mevedel-execution--record-origin record))
      (plist-get preparation :facts))))
  (apply #'mevedel-execution--telemetry
         record 'execution-admitted
         :queue-duration-ms
         (round (* 1000.0
                   (- (float-time)
                      (mevedel-execution--record-started-at record))))
         :preparation-state (plist-get preparation :state)
         :fallback-possible (and (plist-get preparation :fallback-p) t)
         (mevedel-execution-telemetry-safe-facts
          (plist-get preparation :facts)))
  (when (eq (plist-get preparation :state) 'unrestricted)
    (apply #'mevedel-execution--telemetry
           record 'execution-unrestricted
           :reason-class (plist-get (plist-get preparation :facts) :sandbox)
           :after-confined-launch-failure nil
           (mevedel-execution-telemetry-safe-facts
            (plist-get preparation :facts))))
  (pcase (plist-get preparation :state)
    ('refused
     (setf (mevedel-execution--record-error-data record)
           (list 'error (plist-get preparation :error))
           (mevedel-execution--record-exit-code record) -1
           (mevedel-execution--record-sandbox-facts record)
           (plist-get preparation :facts)
           (mevedel-execution--record-termination record) 'spawn-failed)
     (mevedel-execution--finish-managed record))
    ((or 'unrestricted 'confined)
     (setf (mevedel-execution--record-marker record)
           (plist-get preparation :marker)
           (mevedel-execution--record-marker-buffer record)
           (unless (plist-get preparation :marker) :done)
           (mevedel-execution--record-sandbox-facts record)
           (plist-get preparation :facts)
           (mevedel-execution--record-sandbox-preparation record)
           preparation)
     (mevedel-execution--arm-mutation record)
     (mevedel-execution--launch-managed
      record (plist-get preparation :command))
     (unless (mevedel-execution--record-finished-p record)
       (mevedel-execution--arm-managed-timers record)))
    (_
     (mevedel-execution--cleanup-record record)
     (error "Unknown sandbox preparation state: %s"
            (plist-get preparation :state)))))

(defun mevedel-execution--owner-admissible-p (owner-context)
  "Return non-nil when OWNER-CONTEXT may still start queued execution work."
  (not (and owner-context
            (fboundp 'mevedel-agent-invocation-p)
            (mevedel-agent-invocation-p owner-context)
            (mevedel-agent-invocation-runtime-settled-p owner-context))))

(defun mevedel-execution--reject-owner-record (record lease)
  "Settle RECORD rejected from LEASE because its owner is terminal."
  (setf (mevedel-execution--record-scheduler-lease record) lease
        (mevedel-execution--record-error-data record)
        '(mevedel-execution-error "Execution owner is terminal")
        (mevedel-execution--record-exit-code record) -1
        (mevedel-execution--record-termination record) 'owner-stopped)
  (mevedel-execution--finish-managed record))

(defun mevedel-execution--abort-request-record (record)
  "Abort queued or foreground RECORD for its originating request."
  (unless (or (mevedel-execution--record-finished-p record)
              (mevedel-execution--record-yielded-p record))
    (if (mevedel-execution-scheduler-cancel
         (mevedel-execution--record-scheduler-lease record))
        (progn
          (setf (mevedel-execution--record-exit-code record) -1
                (mevedel-execution--record-termination record) 'aborted)
          (mevedel-execution--finish-managed record))
      (mevedel-execution--begin-stop record 'aborted))))

(cl-defun mevedel-execution-start-bash
    (callback &key session data-buffer owner owner-context request
              command workdir
              writable-roots
              additional-permissions sandbox-permissions artifact-directory
              outcome-function read-only-p tool-args tool-use-id tty
              (yield-time-ms 10000))
  "Start managed Bash COMMAND and call CALLBACK at terminal or yield.

SESSION, canonical OWNER, and OWNER-CONTEXT fix the control boundary.
OWNER-CONTEXT is the durable mailbox object captured at spawn.
REQUEST owns the foreground lifetime only.  Remaining confinement arguments
match the one-shot interface.  ARTIFACT-DIRECTORY owns the spool after yield.
OUTCOME-FUNCTION derives canonical outcome from exit code and termination.
READ-ONLY-P selects the overlapping reader lane; all other calls are exclusive.
DATA-BUFFER and TOOL-USE-ID identify the authoritative transcript row.
TOOL-ARGS correlates immutable events with the original call.
TTY non-nil explicitly allocates a terminal and retains writable stdin.
YIELD-TIME-MS may be nil only for trusted internal callers that must wait for
terminal settlement."
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (require 'mevedel-execution-telemetry)
  (require 'mevedel-sandbox)
  (require 'mevedel-turn)
  (unless session
    (signal 'mevedel-execution-error
            (list "Managed Bash requires an active session")))
  (when (and tty (eq system-type 'windows-nt))
    (signal 'mevedel-execution-input-error
            (list "PTY execution is unavailable on Windows")))
  (let* ((state (mevedel-execution--state-for-session session))
         (authority (mevedel-execution--mutation-target session)))
    (unless read-only-p
      (mevedel-session-artifacts-assert-new-mutation-authority authority))
    (when (and (not read-only-p)
               (mevedel-execution-mutation-blocked-p session))
      (signal
       'mevedel-execution-error
       (list "Mutating execution is blocked by an unknown remote outcome")))
    (when (>= (mevedel-execution--managed-count state)
              mevedel-execution-live-limit)
      (signal 'mevedel-execution-limit
              (list "A session may have at most 64 live Bash processes")))
    (let* ((raw-command (plist-get tool-args :command))
           (command-text (and (stringp raw-command) raw-command))
           (command-properties
            (and command-text
                 (mevedel-execution-telemetry-command-properties
                  command-text)))
           (resource-capture
            (and command-text
                 (listp command)
                 (mevedel-execution-telemetry-prepare-resource-capture
                  session command-text command)))
           (command (or (plist-get resource-capture :command) command))
           (id (mevedel-execution--next-id state))
           (artifact-directory
            (or artifact-directory temporary-file-directory))
           (pending-artifact-directory
            (file-name-concat artifact-directory
                              ".mevedel-pending-executions"))
           (_ (make-directory pending-artifact-directory t))
           (record
            (mevedel-execution--record-create
             :callback callback :execution-id id
             :marker-buffer nil :newline-count 0
             :origin
             (mevedel-execution--origin-create
              :data-buffer data-buffer
              :owner (or owner "/root")
              :owner-context owner-context
              :sandbox-summary-cell mevedel-execution-telemetry-summary-cell
              :session session
              :tool-args tool-args
              :tool-use-id tool-use-id)
             :outcome-function outcome-function
             :mutating-p (not read-only-p)
             :output-chars 0 :output-head "" :output-tail ""
             :read-offset 0
             :resource-report-path (plist-get resource-capture :report)
             :sandbox-summary-cell (list nil)
             :spool-path
             (make-temp-file
              (file-name-concat pending-artifact-directory "execution-")
              nil ".log")
             :started-at (float-time) :token id
             :tty-p (and tty t) :workdir workdir
             :yield-time-ms yield-time-ms)))
      (puthash id record (mevedel-execution--state-records state))
      (apply #'mevedel-execution--telemetry
             record 'execution-enqueued
             :lane (if read-only-p 'read 'exclusive)
             :queue-depth (mevedel-execution--managed-count state)
             :overlap-count
             (max 0 (1- (mevedel-execution--managed-count state)))
             :native-resource-capture (and resource-capture t)
             :resource-report-relative-path
             (and resource-capture
                  (file-name-concat
                   "diagnostics"
                   (file-name-nondirectory
                    (directory-file-name
                     (file-name-directory
                      (plist-get resource-capture :report))))
                   "full-suite-time.txt"))
             :command-hash (and command-text
                                (secure-hash 'sha256 command-text))
             :tty (and tty t)
             :yield-time-ms yield-time-ms
             command-properties)
      (mevedel-execution--notify-state-change record)
      (let ((lease
             (mevedel-execution-scheduler-submit
              (mevedel-execution--state-scheduler state)
              (if read-only-p 'read 'exclusive)
              (lambda (admitted-lease)
                (setf (mevedel-execution--record-scheduler-lease record)
                      admitted-lease)
                (condition-case err
                    (mevedel-execution--start-admitted
                     record
                     (with-current-buffer (or data-buffer (current-buffer))
                       (mevedel-sandbox-prepare
                        command workdir writable-roots additional-permissions
                        sandbox-permissions
                        (mevedel-session-sandbox-mode session))))
                  (error
                   (setf (mevedel-execution--record-error-data record) err
                         (mevedel-execution--record-exit-code record) -1
                         (mevedel-execution--record-termination record)
                         'spawn-failed)
                   (mevedel-execution--finish-managed record))))
              (lambda ()
                (and
                 (mevedel-execution--owner-admissible-p owner-context)
                 (or read-only-p
                     (null
                      (mevedel-execution-mutation-blocked-p session)))))
              (lambda (rejected-lease)
                (if (and (not read-only-p)
                         (mevedel-execution-mutation-blocked-p session))
                    (progn
                      (setf
                       (mevedel-execution--record-scheduler-lease record)
                       rejected-lease
                       (mevedel-execution--record-error-data record)
                       '(mevedel-execution-error
                         "Mutating execution is blocked by an unknown remote outcome")
                       (mevedel-execution--record-exit-code record) -1
                       (mevedel-execution--record-termination record)
                       'unknown)
                      (mevedel-execution--finish-managed record))
                  (mevedel-execution--reject-owner-record
                   record rejected-lease))))))
        (unless (or (mevedel-execution--record-finished-p record)
                    (mevedel-execution--record-scheduler-lease record))
          (setf (mevedel-execution--record-scheduler-lease record) lease)))
      (when (and request
                 (not (mevedel-execution--record-finished-p record))
                 (not (mevedel-execution--record-yielded-p record)))
        (mevedel-request-push-canceller
         request
         (lambda () (mevedel-execution--abort-request-record record))))
      nil)))

(defun mevedel-execution--owned-yielded-record (session owner execution-id)
  "Return yielded EXECUTION-ID owned by OWNER in SESSION, or signal."
  (let* ((state (mevedel-execution--state-for-session session))
         (record (gethash execution-id
                          (mevedel-execution--state-records state))))
    (unless (and record
                 (mevedel-execution--record-execution-id record)
                 (mevedel-execution--record-yielded-p record)
                 (equal (mevedel-execution--origin-owner
                         (mevedel-execution--record-origin record))
                        (or owner "/root")))
      (signal 'mevedel-execution-not-found
              (list "No yielded execution with that id")))
    record))

(cl-defun mevedel-execution-observe
    (session owner execution-id callback &key chars (wait-ms 0) request)
  "Observe unread output from owner-scoped yielded EXECUTION-ID.

Ordinary non-empty CHARS require a PTY; a single Ctrl-C character interrupts
either process mode.  CALLBACK receives immediately for terminal state or
after WAIT-MS while the process remains live."
  (require 'mevedel-turn)
  (let* ((record
          (mevedel-execution--owned-yielded-record
           session owner execution-id))
         (input-p (and (stringp chars) (not (string-empty-p chars)))))
    (unless (or (null chars) (stringp chars))
      (signal 'mevedel-execution-input-error
              (list "Execution input must be a string")))
    (when (and input-p (mevedel-execution--record-observer record))
      (mevedel-execution--flush-observer record))
    (when (mevedel-execution--record-observer record)
      (signal 'mevedel-execution-error
              (list "An observation is already waiting")))
    (when input-p
      (cond
       ((equal chars (string 3))
        (when (eq system-type 'windows-nt)
          (signal
           'mevedel-execution-input-error
           (list "Interrupting managed execution is unavailable on Windows")))
        (unless (or (mevedel-execution--record-finished-p record)
                    (mevedel-execution--record-stop-p record)
                    (not (process-live-p
                          (mevedel-execution--record-process record))))
          (mevedel-execution--signal-record record 'INT)))
       ((not (mevedel-execution--record-tty-p record))
        (signal 'mevedel-execution-input-error
                (list "Pipe-mode Bash stdin is closed")))
       ((or (mevedel-execution--record-finished-p record)
            (mevedel-execution--record-stop-p record)
            (not (process-live-p
                  (mevedel-execution--record-process record))))
        (signal 'mevedel-execution-input-error
                (list "Execution is no longer running")))
       (t
        (condition-case nil
            (process-send-string
             (mevedel-execution--record-process record) chars)
          (error
           (signal 'mevedel-execution-input-error
                   (list "Execution is no longer running")))))))
    (cond
     ((mevedel-execution--record-finished-p record)
      (funcall callback (mevedel-execution--observation record t)))
     ((<= wait-ms 0)
      (funcall callback (mevedel-execution--observation record)))
     (t
      (setf (mevedel-execution--record-observer record) callback
            (mevedel-execution--record-observer-timer record)
            (run-at-time (/ wait-ms 1000.0) nil
                         #'mevedel-execution--poll-expired record))
      (when request
        (mevedel-request-push-canceller
         request
         (lambda ()
           (mevedel-execution--cancel-observer record callback))))))
    nil))

(defun mevedel-execution-list (session owner)
  "Return immutable fact snapshots for OWNER's yielded SESSION executions."
  (let ((state (mevedel-execution--state-for-session session))
        facts)
    (maphash
     (lambda (_key record)
       (when (and (mevedel-execution--record-execution-id record)
                  (mevedel-execution--record-yielded-p record)
                  (not (mevedel-execution--record-finished-p record))
                  (equal (mevedel-execution--origin-owner
                          (mevedel-execution--record-origin record))
                         (or owner "/root")))
         (push (mevedel-execution--facts record) facts)))
     (mevedel-execution--state-records state))
    (nreverse facts)))

(defun mevedel-execution--user-snapshot (record)
  "Return an immutable user-authority snapshot for live RECORD."
  (let* ((origin (mevedel-execution--record-origin record))
         (sandbox-facts (mevedel-execution--record-sandbox-facts record))
         (facts
          (plist-put
           (mevedel-execution--facts record)
           :execution-id
           (mevedel-execution--record-execution-id record))))
    (mevedel-execution--copy-value
     (append
      facts
      (list :owner (mevedel-execution--origin-owner origin)
            :yielded (and (mevedel-execution--record-yielded-p record) t)
            :started-at (mevedel-execution--record-started-at record)
            :output-tail
            (or (mevedel-execution--record-output-tail record) "")
            :artifact-path (mevedel-execution--record-spool-path record)
            :sandbox-state (or (plist-get sandbox-facts :sandbox) 'pending)
            :sandbox-facts sandbox-facts)))))

(defun mevedel-execution-list-user (session)
  "Return immutable snapshots for every live execution in SESSION."
  (when-let* ((state (and session
                          (mevedel-session-execution-state session))))
    (let (snapshots)
      (maphash
       (lambda (_key record)
         (when (mevedel-execution--managed-live-p record)
           (push (mevedel-execution--user-snapshot record) snapshots)))
       (mevedel-execution--state-records state))
      (sort snapshots
            (lambda (left right)
              (< (plist-get left :started-at)
                 (plist-get right :started-at)))))))

(defun mevedel-execution-count-user (session)
  "Return the number of live executions in SESSION."
  (if-let* ((state (and session
                        (mevedel-session-execution-state session))))
      (mevedel-execution--managed-count state)
    0))

(defun mevedel-execution--user-live-record (session execution-id)
  "Return live EXECUTION-ID in SESSION with user authority, or signal."
  (let* ((state (and session
                     (mevedel-session-execution-state session)))
         (record (and state
                      (gethash execution-id
                               (mevedel-execution--state-records state)))))
    (unless (and record
                 (not (mevedel-execution--record-finished-p record)))
      (signal 'mevedel-execution-not-found
              (list "No live execution with that id")))
    record))

(defun mevedel-execution-write-user (session execution-id chars)
  "Send CHARS to PTY EXECUTION-ID in SESSION with user authority."
  (unless (and (stringp chars) (not (string-empty-p chars)))
    (signal 'mevedel-execution-input-error
            (list "Execution input must be a non-empty string")))
  (let* ((record (mevedel-execution--user-live-record session execution-id))
         (process (mevedel-execution--record-process record)))
    (unless (mevedel-execution--record-tty-p record)
      (signal 'mevedel-execution-input-error
              (list "Pipe-mode Bash stdin is closed")))
    (unless (process-live-p process)
      (signal 'mevedel-execution-input-error
              (list "Execution is no longer running")))
    (process-send-string process chars)
    t))

(defun mevedel-execution-interrupt-user (session execution-id)
  "Interrupt live EXECUTION-ID in SESSION with user authority."
  (when (eq system-type 'windows-nt)
    (signal 'mevedel-execution-input-error
            (list "Interrupting managed execution is unavailable on Windows")))
  (let* ((record
          (mevedel-execution--user-live-record session execution-id))
         (process (mevedel-execution--record-process record)))
    (unless process
      (signal 'mevedel-execution-input-error
              (list "Execution has not started")))
    (unless (or (process-live-p process)
                (mevedel-execution--group-live-p record))
      (signal 'mevedel-execution-not-found
              (list "Execution is no longer running")))
    (mevedel-execution--signal-record record 'INT)
    t))

(defun mevedel-execution--state-record-list (state)
  "Return a snapshot list of every private record in STATE."
  (let (records)
    (when state
      (maphash (lambda (_key record) (push record records))
               (mevedel-execution--state-records state)))
    records))

(defun mevedel-execution-owner-live-p (session owner)
  "Return non-nil when OWNER has an unsettled execution in SESSION."
  (when-let* ((state (and session
                          (mevedel-session-execution-state session))))
    (let (live-p)
      (dolist (record (mevedel-execution--state-record-list state))
         (when (and (not (mevedel-execution--record-delivery-state record))
                    (equal (mevedel-execution--origin-owner
                            (mevedel-execution--record-origin record))
                           owner))
           (setq live-p t)))
      live-p)))

(defun mevedel-execution-session-live-p (session)
  "Return non-nil when SESSION owns any unsettled child process."
  (when-let* ((state (and session
                          (mevedel-session-execution-state session))))
    (let (live-p)
      (dolist (record (mevedel-execution--state-record-list state))
         (unless (mevedel-execution--record-finished-p record)
           (setq live-p t)))
      live-p)))

(defun mevedel-execution--finalize-discarded-record
    (record managed-live-p)
  "Finish and remove discarded RECORD.
MANAGED-LIVE-P records whether RECORD was user-visible before teardown."
  (setf (mevedel-execution--record-settle-timer record) nil)
  (unless (mevedel-execution--record-finished-p record)
    (when (and (mevedel-execution--record-mutation-armed-p record)
               (not (eq 'unknown
                        (mevedel-execution--record-termination record)))
               (file-remote-p
                (or (mevedel-execution--record-workdir record) ""))
               (mevedel-execution--group-live-p record))
      (mevedel-execution--mark-unknown
       record
       '(mevedel-execution-error
         "Remote process group remained live during lifecycle teardown")))
    (if managed-live-p
        (mevedel-execution--finish-managed record)
      (setf (mevedel-execution--record-finished-p record) t)
      (when-let* ((preparation
                   (mevedel-execution--record-sandbox-preparation record)))
        (mevedel-sandbox-cleanup preparation))))
  (mevedel-execution--release-scheduler record)
  (mevedel-execution--cleanup-record record)
  (when-let* ((function
               (mevedel-execution--record-teardown-function record)))
    (setf (mevedel-execution--record-teardown-function record) nil)
    (ignore-errors (funcall function)))
  (when managed-live-p
    (mevedel-execution--notify-state-change record)))

(defun mevedel-execution--discard-record (record reason)
  "Kill and forget RECORD because of lifecycle REASON."
  (let ((managed-live-p (mevedel-execution--managed-live-p record))
        defer-p)
    (unless (mevedel-execution--record-finished-p record)
      (setf (mevedel-execution--record-stop-p record) t
            (mevedel-execution--record-termination record) reason)
      (when managed-live-p
        (setf (mevedel-execution--record-error-data record)
              '(mevedel-execution-error "Execution owner was stopped")
              (mevedel-execution--record-exit-code record) -1)
        (when (or (mevedel-execution--record-yielded-p record)
                  (eq reason 'session-ended))
          (setf (mevedel-execution--record-delivery-state record) 'discarded)))
      (when-let* ((lease
                   (mevedel-execution--record-scheduler-lease record)))
        (mevedel-execution-scheduler-cancel lease))
      (mevedel-execution--signal-record record 'KILL)
      (if (and managed-live-p
               (mevedel-execution--record-mutation-armed-p record)
               (file-remote-p
                (or (mevedel-execution--record-workdir record) "")))
          (progn
            (setq defer-p t)
            (when-let* ((timer
                         (mevedel-execution--record-settle-timer record)))
              (cancel-timer timer))
            (setf (mevedel-execution--record-settle-timer record)
                  (run-at-time
                   mevedel-execution--child-kill-delay nil
                   #'mevedel-execution--finalize-discarded-record
                   record managed-live-p))
            (while (timerp
                    (mevedel-execution--record-settle-timer record))
              (accept-process-output nil 0.01)))
        (setq defer-p nil)))
    (unless defer-p
      (mevedel-execution--finalize-discarded-record
       record managed-live-p))))

(defun mevedel-execution-stop-owner (session owner)
  "Discard every execution record belonging to OWNER in SESSION.
Return the number of executions selected."
  (let ((state (and session (mevedel-session-execution-state session)))
        records)
    (when state
      (dolist (record (mevedel-execution--state-record-list state))
         (when (equal owner
                      (mevedel-execution--origin-owner
                       (mevedel-execution--record-origin record)))
           (push record records)))
      (dolist (record records)
        (mevedel-execution--discard-record record 'owner-stopped)))
    (length records)))

(defun mevedel-execution-teardown-session (session)
  "Discard all child records owned by SESSION.
Remote armed mutations wait through their bounded KILL proof.
Return the number selected, including queued one-shot helpers."
  (let ((state (and session (mevedel-session-execution-state session)))
        records)
    (when state
      (setq records (mevedel-execution--state-record-list state))
      (dolist (record records)
        (mevedel-execution--discard-record record 'session-ended)))
    (length records)))

(defun mevedel-execution-teardown-all ()
  "Synchronously discard all session and orphan child records."
  (let ((count 0)
        sessions)
    (maphash (lambda (session _present) (push session sessions))
             mevedel-execution--sessions)
    (dolist (session sessions)
      (cl-incf count (mevedel-execution-teardown-session session)))
    (when mevedel-execution--orphan-state
      (let ((records
             (mevedel-execution--state-record-list
              mevedel-execution--orphan-state)))
        (dolist (record records)
          (mevedel-execution--discard-record record 'session-ended))
        (cl-incf count (length records))))
    count))

(defun mevedel-execution-relocate-artifacts (session old-root new-root)
  "Retarget SESSION execution artifacts moved from OLD-ROOT to NEW-ROOT.
Return the number of live or retained records updated."
  (let ((state (and session (mevedel-session-execution-state session)))
        (target (and session (mevedel-session-execution-target session)))
        (old-prefix (file-name-as-directory (expand-file-name old-root)))
        (new-prefix (file-name-as-directory (expand-file-name new-root)))
        (count 0))
    (when target
      (require 'mevedel-execution-target))
    (let ((old-native-prefix
           (and target
                (mevedel-execution-target-remote-p target)
                (file-name-as-directory
                 (mevedel-execution-target-native-path target old-prefix))))
          (new-native-prefix
           (and target
                (mevedel-execution-target-remote-p target)
                (file-name-as-directory
                 (mevedel-execution-target-native-path target new-prefix)))))
      (when state
        (maphash
         (lambda (_key record)
           (let* ((spool-path
                   (mevedel-execution--record-spool-path record))
                  (expanded-spool (and spool-path
                                       (expand-file-name spool-path)))
                  (new-spool
                   (and expanded-spool
                        (string-prefix-p old-prefix expanded-spool)
                        (concat new-prefix
                                (substring expanded-spool
                                           (length old-prefix)))))
                  (recoverable-path
                   (mevedel-execution--record-recoverable-output-path record))
                  (new-recoverable
                   (and recoverable-path old-native-prefix
                        (string-prefix-p old-native-prefix recoverable-path)
                        (concat new-native-prefix
                                (substring recoverable-path
                                           (length old-native-prefix))))))
             (when (or new-spool new-recoverable)
               (when new-spool
                 (setf (mevedel-execution--record-spool-path record)
                       new-spool))
               (when new-recoverable
                 (setf (mevedel-execution--record-recoverable-output-path record)
                       new-recoverable))
               (cl-incf count))))
         (mevedel-execution--state-records state)))
      count)))

(defun mevedel-execution-stop (session owner execution-id callback)
  "Stop owner-scoped yielded EXECUTION-ID and call CALLBACK at settlement."
  (let ((record
         (mevedel-execution--owned-yielded-record
          session owner execution-id)))
    (if (mevedel-execution--record-finished-p record)
        (funcall callback (mevedel-execution--observation record t))
      (mevedel-execution--flush-observer record)
      (mevedel-execution-observe
       session owner execution-id callback :wait-ms 300000)
      (mevedel-execution--begin-stop record 'stopped))
    nil))

(defun mevedel-execution--stop-user-record (record)
  "Stop live RECORD with user delivery authority."
  (mevedel-execution--flush-observer record)
  (if (and (mevedel-execution--record-scheduler-lease record)
           (mevedel-execution-scheduler-cancel
            (mevedel-execution--record-scheduler-lease record)))
      (progn
        (setf (mevedel-execution--record-exit-code record) -1
              (mevedel-execution--record-stop-p record) t
              (mevedel-execution--record-termination record) 'stopped)
        (mevedel-execution--finish-managed record))
    (mevedel-execution--begin-stop record 'stopped))
  t)

(defun mevedel-execution-stop-user (session execution-id)
  "Stop live EXECUTION-ID in SESSION with user delivery authority.

Unlike the owner-scoped model tool, user control may target every owner and
foreground state.  Yielded terminal output still goes to its owner mailbox."
  (mevedel-execution--stop-user-record
   (mevedel-execution--user-live-record session execution-id)))

(defun mevedel-execution-stop-all-user (session)
  "Stop every live execution in SESSION and return the number selected."
  (let* ((state (and session (mevedel-session-execution-state session)))
         (records
          (cl-delete-if
           #'mevedel-execution--record-finished-p
           (mevedel-execution--state-record-list state))))
    (dolist (record records)
      (mevedel-execution--stop-user-record record))
    (length records)))


;;
;;; Confined one-shot interface

(cl-defun mevedel-execution-start-one-shot
    (callback &key name command workdir writable-roots timeout
              additional-permissions sandbox-permissions session owner
              teardown-function)
  "Start one confined COMMAND and call CALLBACK with terminal facts.

NAME identifies the operating-system process.  WORKDIR and WRITABLE-ROOTS
describe its filesystem boundary.  TIMEOUT is nil or a positive number of
seconds.  ADDITIONAL-PERMISSIONS and SANDBOX-PERMISSIONS are already-authorized
confinement inputs.  SESSION and OWNER fix the transient ownership boundary.
TEARDOWN-FUNCTION releases caller-owned resources when lifecycle destruction
discards the process without invoking CALLBACK."
  (require 'mevedel-execution-telemetry)
  (require 'mevedel-sandbox)
  (let* ((summary-cell (list nil))
         (pipeline-summary-cell mevedel-execution-telemetry-summary-cell)
         (invocation
          (and (boundp 'mevedel--agent-invocation)
               mevedel--agent-invocation))
         (agent-summary-cell
          (mevedel-execution-telemetry-agent-summary-cell invocation))
         (attempt-recorded-p nil)
         (started-p nil)
         (current-facts nil)
         (telemetry-span
          (and session
               (fboundp 'mevedel-telemetry-start)
               (mevedel-telemetry-start
                session 'child-process
                :name name :owner owner
                :command-hash (secure-hash 'sha256 (format "%S" command)))))
         (preparation
          (mevedel-sandbox-prepare
           command workdir writable-roots additional-permissions
           sandbox-permissions
           (and session (mevedel-session-sandbox-mode session))))
         (_
          (when (and (eq (plist-get preparation :state) 'unrestricted)
                     (eq (plist-get (plist-get preparation :facts) :sandbox)
                         'unavailable))
            (plist-put
             preparation :facts
             (mevedel-execution-telemetry-mark-direct-fallback
              session (plist-get preparation :facts))))))
    (cl-labels
        ((record-attempt ()
           (unless attempt-recorded-p
             (setq attempt-recorded-p t)
             (mevedel-execution-telemetry-record-sandbox-attempt
              current-facts started-p
              (plist-get current-facts :refused)
              summary-cell pipeline-summary-cell agent-summary-cell)))
         (finish (child-result facts)
           (setq current-facts facts)
           (record-attempt)
           (when telemetry-span
             (apply #'mevedel-telemetry-finish
                    telemetry-span
                    :outcome (if (zerop (or (plist-get child-result :exit-code)
                                            -1))
                                 'success
                               'error)
                    :exit-code (plist-get child-result :exit-code)
                    :output-bytes (plist-get child-result :output-bytes)
                    :timed-out (and (plist-get child-result :timed-out-p) t)
                    (mevedel-execution-telemetry-safe-facts facts)))
           (funcall callback
                    (let ((result (copy-sequence child-result)))
                      (setq result (plist-put result :sandbox-facts facts))
                      (plist-put result :sandbox-summary
                                 (copy-tree (car summary-cell))))))
         (teardown ()
           (record-attempt)
           (mevedel-sandbox-cleanup preparation)
           (when teardown-function
             (funcall teardown-function))))
      (pcase (plist-get preparation :state)
        ('refused
         (setq current-facts (plist-get preparation :facts))
         (record-attempt)
         (when telemetry-span
           (apply #'mevedel-telemetry-finish
                  telemetry-span :outcome 'refused
                  (mevedel-execution-telemetry-safe-facts
                   (plist-get preparation :facts))))
         (funcall
          callback
          (list :exit-code -1
                :output ""
                :output-bytes 0
                :timed-out-p nil
                :output-limit-p nil
                :wall-time-seconds 0.0
                :error (list 'error (plist-get preparation :error))
                :sandbox-facts (plist-get preparation :facts)
                :sandbox-summary (copy-tree (car summary-cell))))
         nil)
        ('unrestricted
         (setq current-facts (plist-get preparation :facts))
         (when (and session (fboundp 'mevedel-telemetry-record))
           (apply #'mevedel-telemetry-record
                  session 'execution-unrestricted
                  :name name :owner owner
                  :reason-class
                  (plist-get (plist-get preparation :facts) :sandbox)
                  :after-confined-launch-failure nil
                  (mevedel-execution-telemetry-safe-facts
                   (plist-get preparation :facts))))
         (setq started-p
               (processp
                (mevedel-execution--start-process
                 (lambda (child-result)
                   (finish child-result (plist-get preparation :facts)))
                 name (plist-get preparation :command) workdir timeout
                 session owner #'teardown))))
        ('confined
         (setq current-facts (plist-get preparation :facts))
         (setq
          started-p
          (processp
           (mevedel-execution--start-process
            (lambda (child-result)
              (let ((launch-failed
                     (mevedel-sandbox-launch-failed-p
                      preparation child-result)))
                (if (and (plist-get preparation :fallback-p) launch-failed)
                    (let ((facts
                           (mevedel-execution-telemetry-mark-direct-fallback
                            session
                            (mevedel-sandbox--record-launch-failure
                             child-result workdir))))
                      (setq started-p nil
                            current-facts facts)
                      (when (and session
                                 (fboundp 'mevedel-telemetry-record))
                        (apply #'mevedel-telemetry-record
                               session 'sandbox-fallback
                               :name name :owner owner
                               :launch-failure-stage 'before-command-start
                               :launch-failure-reason-class
                               'sandbox-launch-failure
                               :fallback-offered t
                               :full-execution-approval-offered nil
                               (mevedel-execution-telemetry-safe-facts facts)))
                      (when (and session
                                 (fboundp 'mevedel-telemetry-record))
                        (apply #'mevedel-telemetry-record
                               session 'execution-unrestricted
                               :name name :owner owner
                               :reason-class 'sandbox-launch-failure
                               :after-confined-launch-failure t
                               (mevedel-execution-telemetry-safe-facts facts)))
                      (mevedel-sandbox-cleanup preparation)
                      (setq
                       started-p
                       (processp
                        (mevedel-execution--start-process
                         (lambda (fallback-result)
                           (finish fallback-result facts))
                         name (plist-get preparation :original-command)
                         workdir timeout session owner #'teardown))))
                  (let ((facts
                         (if launch-failed
                             (plist-put
                              (copy-sequence
                               (mevedel-sandbox--record-launch-failure
                                child-result workdir))
                              :refused t)
                           (plist-get preparation :facts)))
                        (clean-result
                         (mevedel-sandbox-strip-marker
                          preparation child-result)))
                    (setq started-p (not launch-failed)
                          current-facts facts)
                    (mevedel-sandbox-cleanup preparation)
                    (finish clean-result facts)))))
            name (plist-get preparation :command) workdir timeout session owner
            #'teardown))))
        (_ (error "Unknown sandbox preparation state: %s"
                  (plist-get preparation :state))))
      nil)))

(defun mevedel-execution--owner-teardown-result ()
  "Return structured settlement for a synchronously discarded child."
  '(:exit-code -1 :output "" :output-bytes 0
    :timed-out-p nil :output-limit-p nil :wall-time-seconds 0.0
    :error (error "Execution owner was torn down")))

(cl-defun mevedel-execution-run-one-shot
    (&key name command workdir writable-roots timeout
          additional-permissions sandbox-permissions session owner)
  "Run one confined COMMAND synchronously and return terminal facts.

All keyword arguments follow `mevedel-execution-start-one-shot'."
  (let (done result)
    (mevedel-execution-start-one-shot
     (lambda (child-result)
       (setq result child-result
             done t))
     :name name :command command :workdir workdir
     :writable-roots writable-roots :timeout timeout
     :additional-permissions additional-permissions
     :sandbox-permissions sandbox-permissions :session session :owner owner
     :teardown-function
     (lambda ()
       (setq result (mevedel-execution--owner-teardown-result)
             done t)))
    (while (not done)
      (accept-process-output nil 0.05))
    result))


;;
;;; External helper interface

(cl-defun mevedel-execution-start-helper
    (callback name command read-paths writable-roots
              &key timeout session owner teardown-callback)
  "Start external helper COMMAND and call CALLBACK when it settles.

READ-PATHS are mounted read-only.  WRITABLE-ROOTS are explicit artifact
directories.  A private writable scratch directory is the helper's working
directory and is removed before CALLBACK runs."
  (let* ((target (and session
                      (mevedel-session-execution-target session)))
         (remote-root
          (and target
               (mevedel-execution-target-remote-p target)
               (mevedel-execution-target-workspace-root target)))
         (scratch
          (if remote-root
              (make-nearby-temp-file
               (file-name-concat remote-root ".mevedel-helper-") t)
            (make-temp-file "mevedel-helper-" t)))
         (roots
          (delete-dups
           (mapcar (lambda (path)
                     (file-name-as-directory (expand-file-name path)))
                   (cons scratch writable-roots))))
         (permissions
          (and read-paths
               (list :file-system
                     (mapcar (lambda (path)
                               (list :path (expand-file-name path)
                                     :access 'read))
                             (delete-dups read-paths)))))
         finished
         (cleanup
          (lambda ()
            (unless finished
              (setq finished t)
              (ignore-errors (delete-directory scratch t)))))
         (teardown
          (lambda ()
            (funcall cleanup)
            (when teardown-callback
              (funcall teardown-callback)))))
    (condition-case err
        (mevedel-execution-start-one-shot
         (lambda (child-result)
           (funcall cleanup)
           (funcall callback child-result))
         :name name :command command :workdir scratch
         :writable-roots roots :timeout timeout
         :additional-permissions permissions :session session :owner owner
         :teardown-function teardown)
      (error
       (funcall cleanup)
       (signal (car err) (cdr err))))))

(cl-defun mevedel-execution-run-helper
    (name command read-paths writable-roots &key timeout session owner)
  "Run external helper COMMAND synchronously and return terminal facts.

NAME, READ-PATHS, WRITABLE-ROOTS, TIMEOUT, and SESSION follow
`mevedel-execution-start-helper'."
  (let (done result)
    (mevedel-execution-start-helper
     (lambda (child-result)
       (setq result child-result
             done t))
     name command read-paths writable-roots
     :timeout timeout :session session :owner owner
     :teardown-callback
     (lambda ()
       (setq result (mevedel-execution--owner-teardown-result)
             done t)))
    (while (not done)
      (accept-process-output nil 0.05))
    result))

(provide 'mevedel-execution)
;;; mevedel-execution.el ends here
