;;; niwa-execution.el --- Managed execution lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Owns managed execution admission, per-session records, observations,
;; delivery, and the public execution facade.  The opaque child/process-group
;; and bounded spool lifecycle belongs to `niwa-execution-process.el'.
;; `niwa-execution-telemetry.el' owns safe event projection, sandbox
;; summaries, and optional profiler resource capture.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'niwa-session-durability)
  (require 'subr-x))

(require 'niwa-structs)

;; `niwa-agent-control'
(declare-function niwa-agent-control-monitor-unread-bytes
                  "niwa-agent-control" (session execution-id))
(autoload 'niwa-agent-control-monitor-unread-bytes "niwa-agent-control")

;; `niwa-agents'
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-parent-session
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-runtime-settled-p
                  "niwa-agents" (cl-x) t)
(defvar niwa--agent-invocation)

;; `niwa-execution-process'
(declare-function niwa-execution-process-collect-stderr
                  "niwa-execution-process" (child))
(declare-function niwa-execution-process-create
                  "niwa-execution-process" (&rest keys))
(declare-function niwa-execution-process-discard-stderr
                  "niwa-execution-process" (child))
(declare-function niwa-execution-process-interrupt
                  "niwa-execution-process" (child))
(declare-function niwa-execution-process-launch-attempted-p
                  "niwa-execution-process" (child))
(declare-function niwa-execution-process-live-p
                  "niwa-execution-process" (child))
(declare-function niwa-execution-process-read
                  "niwa-execution-process" (child))
(declare-function niwa-execution-process-rebind-spool
                  "niwa-execution-process" (child path))
(declare-function niwa-execution-process-release
                  "niwa-execution-process" (child &optional preserve-spool))
(declare-function niwa-execution-process-relocate-spool
                  "niwa-execution-process" (child destination))
(declare-function niwa-execution-process-remote-group-status
                  "niwa-execution-process" (group-id start-time workdir))
(declare-function niwa-execution-process-spool-path
                  "niwa-execution-process" (child))
(declare-function niwa-execution-process-start
                  "niwa-execution-process" (child &rest keys))
(declare-function niwa-execution-process-status
                  "niwa-execution-process" (child))
(declare-function niwa-execution-process-stop
                  "niwa-execution-process" (child reason))
(declare-function niwa-execution-process-terminal-p
                  "niwa-execution-process" (child))
(declare-function niwa-execution-process-write
                  "niwa-execution-process" (child input))

;; `niwa-execution-scheduler'
(declare-function niwa-execution-scheduler-cancel
                  "niwa-execution-scheduler" (lease))
(declare-function niwa-execution-scheduler-create
                  "niwa-execution-scheduler" ())
(declare-function niwa-execution-scheduler-release
                  "niwa-execution-scheduler" (lease))
(declare-function niwa-execution-scheduler-submit
                  "niwa-execution-scheduler"
                  (scheduler mode start &optional admit-p reject scope))

;; `niwa-execution-telemetry'
(declare-function niwa-execution-telemetry-command-properties
                  "niwa-execution-telemetry" (command))
(declare-function niwa-execution-telemetry-context-create
                  "niwa-execution-telemetry" (&rest keys))
(declare-function niwa-execution-telemetry-context-properties
                  "niwa-execution-telemetry" (context))
(declare-function niwa-execution-telemetry-context-summary
                  "niwa-execution-telemetry" (context))
(declare-function niwa-execution-telemetry-finish
                  "niwa-execution-telemetry" (context &rest properties))
(declare-function niwa-execution-telemetry-mark-direct-fallback
                  "niwa-execution-telemetry" (session facts))
(declare-function niwa-execution-telemetry-prepare-resource-capture
                  "niwa-execution-telemetry" (context command-text command))
(declare-function niwa-execution-telemetry-record
                  "niwa-execution-telemetry"
                  (context event props &optional audit))
(declare-function niwa-execution-telemetry-record-sandbox-attempt
                  "niwa-execution-telemetry"
                  (context facts started-p refused-p))
(declare-function niwa-execution-telemetry-safe-facts
                  "niwa-execution-telemetry" (facts))
(defvar niwa-execution-telemetry-summary-cell)

;; `niwa-execution-spool'
(declare-function niwa-execution-spool-compress "niwa-execution-spool" (path))
(autoload 'niwa-execution-spool-compress "niwa-execution-spool")

;; `niwa-execution-target'
(declare-function niwa-execution-target-native-path
                  "niwa-execution-target" (target path))
(declare-function niwa-execution-target-remote-p
                  "niwa-execution-target" (target))
(declare-function niwa-execution-target-workspace-root
                  "niwa-execution-target" (cl-x) t)
(autoload 'niwa-execution-target-native-path "niwa-execution-target")
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")
(autoload 'niwa-execution-target-workspace-root "niwa-execution-target")

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-effective
                  "niwa-permission-mode"
                  (&optional session data-buffer surface-buffer request invocation))
(autoload 'niwa-permission-mode-effective "niwa-permission-mode")

;; `niwa-permission-persistence'
(declare-function niwa-permission-persistent-authority
                  "niwa-permission-persistence" (workspace scope))
(autoload 'niwa-permission-persistent-authority "niwa-permission-persistence")

;; `niwa-permission-rules'
(defvar niwa-permission-rules)

;; `niwa-resource'
(declare-function niwa-resource-artifact-address
                  "niwa-resource" (path session))
(autoload 'niwa-resource-artifact-address "niwa-resource")

;; `niwa-sandbox'
(declare-function niwa-sandbox--record-launch-failure
                  "niwa-sandbox" (child-result &optional workdir))
(declare-function niwa-sandbox-cleanup "niwa-sandbox" (preparation))
(declare-function niwa-sandbox-launch-failed-p
                  "niwa-sandbox" (preparation child-result))
(declare-function niwa-sandbox-mode-effective
                  "niwa-sandbox" (&optional session permission-mode))
(declare-function niwa-sandbox-prepare
                  "niwa-sandbox"
                  (command workdir writable-roots &optional
                           additional-permissions sandbox-permissions mode
                           temporary-root candidates scope))
(declare-function niwa-sandbox-prepare-start
                  "niwa-sandbox"
                  (callback start-process command workdir writable-roots
                            &optional additional-permissions sandbox-permissions
                            mode temporary-root scope))
(declare-function niwa-sandbox-strip-marker
                  "niwa-sandbox" (preparation child-result))
(autoload 'niwa-sandbox--record-launch-failure "niwa-sandbox")
(autoload 'niwa-sandbox-cleanup "niwa-sandbox")
(autoload 'niwa-sandbox-launch-failed-p "niwa-sandbox")
(autoload 'niwa-sandbox-mode-effective "niwa-sandbox")
(autoload 'niwa-sandbox-prepare "niwa-sandbox")
(autoload 'niwa-sandbox-prepare-start "niwa-sandbox")
(autoload 'niwa-sandbox-strip-marker "niwa-sandbox")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-assert-mutation-authority
                  "niwa-session-artifacts" (session &optional buffer))
(declare-function niwa-session-artifacts-assert-new-mutation-authority
                  "niwa-session-artifacts" (session))
(declare-function niwa-session-artifacts-publish-text
                  "niwa-session-artifacts"
                  (session path content &optional coding))
(autoload 'niwa-session-artifacts-assert-mutation-authority
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-assert-new-mutation-authority
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-publish-text
  "niwa-session-artifacts")

;; `niwa-session-durability'
(declare-function niwa-session-durability-set-unsettled-mutation
                  "niwa-session-durability" (session value))
(declare-function niwa-session-durability-unsettled-mutation-p
                  "niwa-session-durability" (session))
(autoload 'niwa-session-durability-set-unsettled-mutation
  "niwa-session-durability")
(autoload 'niwa-session-durability-unsettled-mutation-p
  "niwa-session-durability")

;; `niwa-structs'
(declare-function niwa-request-cancelled-p "niwa-structs" (cl-x) t)
(declare-function niwa-request-session "niwa-structs" (cl-x) t)
(declare-function niwa-session--set-execution-state
                  "niwa-structs" (session state))
(declare-function niwa-session-audit-target "niwa-structs" (session))
(declare-function niwa-session-execution-state "niwa-structs" (cl-x) t)
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-rules "niwa-structs" (cl-x) t)
(declare-function niwa-session-resource-grants "niwa-structs" (cl-x) t)
(declare-function niwa-session-sandbox-mode "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--session)

;; `niwa-system'
(defvar niwa-memory-dirs)

;; `niwa-transport'
(declare-function niwa-transport-busy-p
                  "niwa-transport" (&optional path))
(declare-function niwa-transport-cancel-pending
                  "niwa-transport" (&optional key))
(declare-function niwa-transport-run-at-time
                  "niwa-transport" (seconds function &rest args))
(declare-function niwa-transport-run-when-idle
                  "niwa-transport" (key path thunk &optional on-cancel delay))
(autoload 'niwa-transport-busy-p "niwa-transport")
(autoload 'niwa-transport-cancel-pending "niwa-transport")
(autoload 'niwa-transport-run-at-time "niwa-transport")
(autoload 'niwa-transport-run-when-idle "niwa-transport")

;; `niwa-turn'
(declare-function niwa-request-push-canceller
                  "niwa-turn" (request canceller))
(autoload 'niwa-request-push-canceller "niwa-turn")

;; `niwa-utilities'
(declare-function niwa--head-tail-preview-parts
                  "niwa-utilities"
                  (head tail total-length &optional preview-size artifact))
(declare-function niwa--warn-once
                  "niwa-utilities" (scope key format &rest args))

;; `niwa-workspace'
(defvar niwa-workspace-additional-roots)

(require 'niwa-execution-process)
(require 'niwa-execution-scheduler)
(require 'niwa-execution-telemetry)
(require 'niwa-utilities)

;;
;;; Configuration and private state

(defcustom niwa-execution-inline-output-limit 2000
  "Maximum unread characters returned inline by one managed observation.

Larger unread ranges use the shared newline-aware head-and-tail preview while
the complete spool remains at the path in the execution facts.  The 2000
default is deliberate: it bounds what each observation adds to the model's
context.  The truncation marker names the artifact to Read for the rest."
  :type 'integer
  :group 'niwa)

(defcustom niwa-execution-progress-delay 2
  "Seconds before a managed Bash execution publishes live progress."
  :type 'number
  :group 'niwa)

(defcustom niwa-execution-progress-interval 0.25
  "Seconds between managed Bash progress events while output arrives.
Values below 0.25 are clamped so the UI receives at most four per second.
A command printing nothing publishes only its elapsed time, once each
`niwa-execution--quiet-progress-interval', or once each interval when
that is longer."
  :type 'number
  :group 'niwa)

(defconst niwa-execution--quiet-progress-interval 1.0
  "Seconds between progress events of a command whose output has not changed.
Every event redraws the command's row, and on pgtk every wakeup repaints the
whole frame; a silent `sleep' used to publish four unchanged events and four
row redraws a second.")

(defconst niwa-execution-live-limit 64
  "Maximum number of live managed Bash processes in one session.")

(defconst niwa-execution--terminal-retention-seconds 60
  "Seconds a completed yielded execution remains pollable.")

(define-error 'niwa-execution-error "Managed execution error")
(define-error 'niwa-execution-input-error
  "Managed execution does not accept this input" 'niwa-execution-error)
(define-error 'niwa-execution-limit
  "Managed execution limit reached" 'niwa-execution-error)
(define-error 'niwa-execution-not-found
  "Managed execution not found" 'niwa-execution-error)

(cl-defstruct (niwa-execution--state
               (:constructor niwa-execution--state-create))
  "Opaque per-session execution state."
  id-prefix
  next-id
  owed-settlement
  records
  scheduler
  unknown-outcome)

(cl-defstruct (niwa-execution--origin
               (:constructor niwa-execution--origin-create))
  "Immutable ownership and transcript correlation for one execution."
  data-buffer
  owner
  owner-context
  session
  tool-args
  tool-use-id)

(cl-defstruct (niwa-execution--record
               (:constructor niwa-execution--record-create))
  "Private managed lifecycle and one-shot ownership state."
  artifact-address
  callback
  child
  delivery-pending-p
  delivery-state
  description
  error-data
  execution-id
  exit-code
  expires-at
  finished-p
  kind
  last-byte-newline-p
  marker
  marker-buffer
  marker-seen-p
  (monitor-count 0)
  monitor-after
  monitor-inflight
  monitor-lines
  (monitor-bytes 0)
  (monitor-omitted 0)
  (monitor-partial "")
  (monitor-partial-length 0)
  monitor-last-char
  monitor-times
  monitor-timer
  mutating-p
  mutation-armed-p
  newline-count
  observer
  observer-timer
  omitted-output-bytes
  origin
  outcome-function
  result-function
  terminal-result
  output-chars
  output-head
  output-limit-p
  recoverable-output-bytes
  recoverable-output-path
  output-tail
  pipeline-facts-function
  pipeline-marker
  permission-approvals
  preparation-cancel
  progress-timer
  progress-emitted-at
  read-offset
  retained-p
  retire-timer
  sandbox-facts
  sandbox-preparation
  scheduler-lease
  service-active-p
  service-call-p
  service
  active-call
  started-at
  stop-p
  termination
  timed-out-p
  timeout-ms
  teardown-function
  telemetry-context
  terminal-observation
  token
  tty-p
  unread-chars
  unread-head
  unread-tail
  workdir
  yield-time-ms
  yield-timer
  yielded-p)

(defun niwa-execution--diagnostic-scope (record)
  "Return RECORD's originating workspace scope, without ambient lookup."
  (niwa-diagnostic-scope
   (when-let* ((origin (niwa-execution--record-origin record)))
     (niwa-execution--origin-session origin))))

(defvar niwa-execution--orphan-state nil
  "Private state for children that have no chat session owner.")

(defvar niwa-execution--sessions
  (make-hash-table :test #'eq :weakness 'key)
  "Weak set of sessions that have created execution state.")

(defvar niwa-execution-event-functions nil
  "Functions notified of immutable managed execution event plists.

Each function receives one event.  Return values are ignored and errors are
contained by the execution module.")

(defvar niwa-execution-state-change-hook nil
  "Functions notified when a session's live execution set changes.

Each function receives the owning session and originating data buffer.  Errors
are contained by the execution module.")

(defvar niwa-execution-mailbox-delivery-function nil
  "Function that secures one event in its owner mailbox, then acknowledges it.
The function receives an immutable event plist, its private owner context and
an acknowledgement function.  The acknowledgement takes SUCCESS and ERROR and
must run exactly once: SUCCESS non-nil after durable delivery, nil with ERROR
after a definitive failure that left nothing queued.  The function returns
`pending' when the acknowledgement will run later; any other return means it
already ran.  This is deliberately separate from passive event hooks; the
context is never published to them.")

;;
;;; State and environment

(defun niwa-execution--managed-live-p (record)
  "Return non-nil when RECORD is a user-visible live Bash execution."
  (and (niwa-execution--record-execution-id record)
       (not (niwa-execution--record-finished-p record))))

(defun niwa-execution--new-state ()
  "Return an empty opaque execution state."
  (niwa-execution--state-create
   :id-prefix (make-temp-name (make-temp-name "exec-"))
   :next-id 0
   :records (make-hash-table :test #'equal)
   :scheduler (niwa-execution-scheduler-create)))

(defun niwa-execution--state-for-session (session)
  "Return the private execution state for SESSION.

When SESSION is nil, use the module-owned state for direct non-session calls."
  (if session
      (progn
        (puthash session t niwa-execution--sessions)
        (or (niwa-session-execution-state session)
            (let ((state (niwa-execution--new-state)))
              (niwa-session--set-execution-state session state)
              state)))
    (or niwa-execution--orphan-state
        (setq niwa-execution--orphan-state
              (niwa-execution--new-state)))))

(defun niwa-execution--mutation-target (session)
  "Return SESSION's durable mutation-authority target."
  (and session (niwa-session-audit-target session)))

(defun niwa-execution--mutation-records (session)
  "Return every process record sharing SESSION's mutation authority."
  (let ((target (niwa-execution--mutation-target session))
        records)
    (maphash
     (lambda (candidate _present)
       (when (eq target (niwa-execution--mutation-target candidate))
         (when-let* ((state (niwa-session-execution-state candidate)))
           (maphash
            (lambda (_key record) (push record records))
            (niwa-execution--state-records state)))))
     niwa-execution--sessions)
    records))

(defun niwa-execution--armed-mutation-records (session)
  "Return SESSION's transient records backed by the durable mutation latch."
  (cl-remove-if-not #'niwa-execution--record-mutation-armed-p
                    (niwa-execution--mutation-records session)))

(defun niwa-execution--durable-mutation-latch-p (session)
  "Return non-nil when SESSION's remote authority has its mutation latch set."
  (let* ((authority (niwa-execution--mutation-target session))
         (target (and authority
                      (niwa-session-execution-target authority))))
    (when (and target (niwa-execution-target-remote-p target))
      (niwa-session-durability-unsettled-mutation-p authority))))

(defun niwa-execution--assert-mutation-authority (record)
  "Assert RECORD's durable remote mutation authority."
  (let* ((origin (niwa-execution--record-origin record))
         (authority
          (niwa-execution--mutation-target
           (niwa-execution--origin-session origin)))
         (data-buffer (niwa-execution--origin-data-buffer origin)))
    (if (buffer-live-p data-buffer)
        (niwa-session-artifacts-assert-mutation-authority
         authority data-buffer)
      (with-temp-buffer
        (niwa-session-artifacts-assert-mutation-authority
         authority (current-buffer))))))

(defun niwa-execution--arm-mutation (record)
  "Durably arm mutating remote RECORD before its child can start."
  (when (niwa-execution--record-mutating-p record)
    (let* ((origin (niwa-execution--record-origin record))
           (session (niwa-execution--origin-session origin))
           (authority (niwa-execution--mutation-target session))
           (target (niwa-session-execution-target authority)))
      (when (niwa-execution-target-remote-p target)
        ;; The authority check and the arm share one transaction's clock
        ;; and recovery observations.
        (niwa-session-durability-with-transaction
          (niwa-execution--assert-mutation-authority record)
          (unless
              (niwa-session-durability-set-unsettled-mutation authority t)
            (signal 'niwa-execution-error
                    (list "Could not arm remote mutation authority"))))
        (setf (niwa-execution--record-mutation-armed-p record) t)))))

(defun niwa-execution--mark-unknown (record error-data &optional child-status)
  "Record ERROR-DATA when RECORD's target process outcome cannot be proved."
  (let* ((child-status
          (or child-status
              (when-let* ((child (niwa-execution--record-child record)))
                (niwa-execution-process-status child))))
         (origin (niwa-execution--record-origin record))
         (session (niwa-execution--origin-session origin))
         (state (niwa-execution--state-for-session session))
         (outcome
          (list :group-id (plist-get child-status :group-id)
                :group-start-time
                (plist-get child-status :group-start-time)
                :workdir (niwa-execution--record-workdir record)
                :error error-data)))
    (setf (niwa-execution--record-termination record) 'unknown
          (niwa-execution--record-error-data record) error-data
          (niwa-execution--state-unknown-outcome state) outcome)
    outcome))

(defun niwa-execution--write-mutation-settlement (record authority)
  "Clear AUTHORITY\'s unsettled-mutation latch for RECORD.

Settlement is reached from a process filter or a timer, so this write can
come due while the target connection is mid-command.  The control
filesystem refuses to nest rather than let a crossed reply through, and
that refusal is not evidence about the target: the write never ran.
Treating it as evidence would call a proven outcome unprovable and block
the session\'s mutating executions over a nesting that clears itself, so
record the write as owed instead and let the next decision point issue it.
Only a write that ran and failed leaves the outcome unknown."
  (let ((state (niwa-execution--state-for-session authority)))
    (condition-case err
        (progn
          ;; The publication gate may reject nesting before the filesystem does.
          (when (niwa-transport-busy-p (niwa-session-save-path authority))
            (signal 'niwa-session-control-fs-busy
                    (list (niwa-session-save-path authority))))
          (unless (niwa-session-durability-set-unsettled-mutation
                   authority nil)
            (error "Remote mutation authority changed before settlement"))
          (setf (niwa-execution--state-owed-settlement state) nil))
      (niwa-session-control-fs-busy
       (setf (niwa-execution--state-owed-settlement state) authority))
      (error
       (setf (niwa-execution--record-mutation-armed-p record) t)
       (niwa-execution--mark-unknown record err)))))

(defun niwa-execution--flush-owed-mutation-settlement (session)
  "Write a settlement for SESSION that a busy connection refused earlier.

The owed authority is this client\'s own proof that its mutation finished,
so writing it here is not an acknowledgement of somebody else\'s latch.
Here is a decision point rather than a filter or a timer, so the
connection is the caller\'s to use."
  (let* ((authority (niwa-execution--mutation-target session))
         (state (niwa-execution--state-for-session authority)))
    (when-let* ((authority (niwa-execution--state-owed-settlement state)))
      (condition-case nil
          (when (niwa-session-durability-set-unsettled-mutation
                 authority nil)
            (setf (niwa-execution--state-owed-settlement state) nil))
        ;; Still refused or still failing: the write stays owed, and the
        ;; session stays blocked, which is the safe direction.
        (error nil)))))

(defun niwa-execution--settle-mutation (record)
  "Clear RECORD\'s durable latch after every armed mutation is proved settled."
  (when (and (niwa-execution--record-mutation-armed-p record)
             (not (eq 'unknown
                      (niwa-execution--record-termination record))))
    (let* ((origin (niwa-execution--record-origin record))
           (session (niwa-execution--origin-session origin))
           (authority (niwa-execution--mutation-target session)))
      (setf (niwa-execution--record-mutation-armed-p record) nil)
      (unless (niwa-execution--armed-mutation-records session)
        (niwa-execution--write-mutation-settlement record authority)))))

(defun niwa-execution-unknown-outcome (session)
  "Return SESSION's unproved target process outcome, if any."
  (or (niwa-execution--state-unknown-outcome
       (niwa-execution--state-for-session session))
      (when (niwa-execution--durable-mutation-latch-p session)
        (let ((armed (niwa-execution--armed-mutation-records session)))
          (when (or (null armed)
                    (cl-some #'niwa-execution--record-finished-p armed))
            '(:durable-unsettled-mutation t))))))

(defun niwa-execution-unsettled-mutation-p (session)
  "Return non-nil while SESSION has live or unproved remote mutation.
First retry a proved settlement whose durable write was deferred."
  (niwa-execution--flush-owed-mutation-settlement session)
  (or (niwa-execution--state-unknown-outcome
       (niwa-execution--state-for-session session))
      (niwa-execution--durable-mutation-latch-p session)))

(defconst niwa-execution-mutation-blocked-message
  (concat "Mutating execution is blocked by an unknown remote outcome; "
          "run M-x niwa-retry-target-readiness to re-probe the target "
          "and acknowledge it")
  "Refusal text for a mutating request under an unproved remote outcome.
The block never expires on its own, so the message names the command that
clears it rather than leaving the reader to wait for a timeout that does
not exist.")

(defun niwa-execution-mutation-blocked-p (session)
  "Return non-nil when SESSION must not start mutating execution.
This is a cheap predicate safe to poll.  Use
`niwa-execution-mutation-refused-p' at a decision point, where the
recorded outcome is worth re-proving against the target first."
  (and (niwa-execution-unknown-outcome session) t))

(defun niwa-execution-reprove-unknown-outcome (session)
  "Clear SESSION's unknown outcome when its process group is proved dead.

A lost settlement records the target process-group identity, so the
question it left open can be asked again: the group either still exists on
the target or it does not.  An affirmative `dead' is the same proof the
original settlement failed to collect, so it settles the outcome without a
human deciding anything.  Anything else -- still live, unreachable,
ambiguous, or an identity too incomplete to ask about -- leaves the block
in place for `niwa-retry-target-readiness' to resolve.

The durable latch alone, which is what survives a restart, carries no
process identity and can only be acknowledged manually.

Return non-nil when the block was cleared."
  (when-let* ((outcome (niwa-execution--state-unknown-outcome
                        (niwa-execution--state-for-session session)))
              (workdir (plist-get outcome :workdir))
              ((not (niwa-transport-busy-p workdir)))
              ((eq 'dead
                   (condition-case nil
                       (niwa-execution-process-remote-group-status
                        (plist-get outcome :group-id)
                        (plist-get outcome :group-start-time)
                        workdir)
                     (error nil)))))
    (condition-case nil
        (progn
          (niwa-execution-acknowledge-unknown session)
          (not (niwa-execution-mutation-blocked-p session)))
      ;; A live armed record or a lost authority means the block stands;
      ;; refusing the request is always the safe direction.
      (error nil))))

(defun niwa-execution-mutation-refused-p (session)
  "Return non-nil when SESSION must refuse a mutating request now.
Writes a settlement a busy transport refused earlier and re-proves a
recorded unknown outcome against the target before refusing, so a session
whose remote work is long finished recovers on its own."
  (when (niwa-execution-mutation-blocked-p session)
    (niwa-execution--flush-owed-mutation-settlement session)
    (niwa-execution-reprove-unknown-outcome session)
    (niwa-execution-mutation-blocked-p session)))

(defun niwa-execution-acknowledge-unknown (session)
  "Acknowledge and clear SESSION's unproved target process outcome."
  (let* ((authority (niwa-execution--mutation-target session))
         (target (and authority
                      (niwa-session-execution-target authority)))
         (records (niwa-execution--armed-mutation-records session)))
    (when (cl-some (lambda (record)
                     (not (niwa-execution--record-finished-p record)))
                   records)
      (signal 'niwa-execution-error
              (list "Cannot acknowledge while mutating execution is live")))
    (when (and target (niwa-execution-target-remote-p target))
      (niwa-session-artifacts-assert-mutation-authority
       authority (current-buffer))
      (unless
          (niwa-session-durability-set-unsettled-mutation authority nil)
        (signal 'niwa-execution-error
                (list "Could not clear remote mutation authority"))))
    (dolist (record records)
      (setf (niwa-execution--record-mutation-armed-p record) nil))
    (setf (niwa-execution--state-owed-settlement
           (niwa-execution--state-for-session authority))
          nil)
    (maphash
     (lambda (candidate _present)
       (when (eq authority (niwa-execution--mutation-target candidate))
         (when-let* ((state (niwa-session-execution-state candidate)))
           (setf (niwa-execution--state-unknown-outcome state) nil))))
     niwa-execution--sessions)
    nil))

(defun niwa-execution--notify-state-change (record)
  "Notify observers that RECORD's session execution set changed."
  (when (or (niwa-execution--record-execution-id record)
            (eq (niwa-execution--record-kind record) 'service))
    (let* ((origin (niwa-execution--record-origin record))
           (session (niwa-execution--origin-session origin))
           (data-buffer (niwa-execution--origin-data-buffer origin)))
      (when session
        (dolist (function niwa-execution-state-change-hook)
          (when (functionp function)
            (condition-case err
                (funcall function session data-buffer)
              (error
               (niwa--warn-once
                (niwa-diagnostic-scope session)
                'execution-state-consumer
                "Execution state consumer failed: %s"
                (error-message-string err))))))))))


;;
;;; Process lifecycle bridge

(defun niwa-execution--read-output (record)
  "Return RECORD's complete decoded spooled output."
  (if-let* ((child (niwa-execution--record-child record)))
      (niwa-execution-process-read child)
    ""))

(defun niwa-execution--release-runtime (record)
  "Release RECORD's child and managed timers.
The process spool remains owned by RECORD until registry cleanup."
  (dolist (timer (list (niwa-execution--record-observer-timer record)
                       (niwa-execution--record-monitor-timer record)
                       (niwa-execution--record-progress-timer record)
                       (niwa-execution--record-retire-timer record)
                       (niwa-execution--record-yield-timer record)))
    (when (timerp timer)
      (niwa--ui-timer-cancel timer)))
  (setf (niwa-execution--record-observer-timer record) nil
        (niwa-execution--record-monitor-timer record) nil
        (niwa-execution--record-progress-timer record) nil
        (niwa-execution--record-retire-timer record) nil
        (niwa-execution--record-yield-timer record) nil)
  (when-let* ((cancel (niwa-execution--record-preparation-cancel record)))
    (setf (niwa-execution--record-preparation-cancel record) nil)
    (funcall cancel))
  (when-let* ((child (niwa-execution--record-child record)))
    (niwa-execution-process-release child t)))

(defun niwa-execution--cleanup-record (record &optional preserve-spool)
  "Release runtime and registry state for RECORD.

Delete its spool unless PRESERVE-SPOOL is non-nil."
  (let ((removed-live-p (niwa-execution--managed-live-p record)))
    ;; A naturally ended child may still have its delivery queued.  Never
    ;; let that continuation read a released spool or revive a retired owner.
    (niwa-transport-cancel-pending
     (list 'execution-delivery (niwa-execution--record-token record)))
    (niwa-execution--release-runtime record)
    (when-let* ((child (niwa-execution--record-child record)))
      (niwa-execution-process-discard-stderr child))
    (if preserve-spool
        (when (niwa-execution--record-finished-p record)
          (niwa-execution-spool-compress (niwa-execution--spool-path record)))
      (when-let* ((child (niwa-execution--record-child record)))
        (niwa-execution-process-release child)))
    (let ((state
           (niwa-execution--state-for-session
            (niwa-execution--origin-session
             (niwa-execution--record-origin record)))))
      (remhash (or (niwa-execution--record-execution-id record)
                   (niwa-execution--record-token record))
               (niwa-execution--state-records state))
      (when removed-live-p
        (niwa-execution--notify-state-change record)))))

(defun niwa-execution--retire-terminal-record (record)
  "Retain yielded RECORD briefly for idempotent terminal polling."
  (let ((preserve-spool
         (and (niwa-execution--record-retained-p record)
              (not (file-remote-p
                    (or (niwa-execution--record-workdir record) ""))))))
    (if (niwa-execution--record-yielded-p record)
        (setf (niwa-execution--record-retire-timer record)
              ;; Retirement is reached from settlement, which can run
              ;; inside a remote command.
              (niwa-transport-run-at-time
               niwa-execution--terminal-retention-seconds
               #'niwa-execution--cleanup-record record preserve-spool))
      (niwa-execution--cleanup-record record preserve-spool))))

(defun niwa-execution--start-process
    (callback name command workdir timeout session owner teardown-function
              &optional scope)
  "Start raw COMMAND and call CALLBACK with its bounded terminal result.
SCOPE overrides SESSION's diagnostic ownership for sessionless helpers."
  (let* ((record
          (niwa-execution--record-create
           :callback callback
           :origin (niwa-execution--origin-create
                    :owner (or owner "/root") :session session)
           :started-at (float-time)
           :teardown-function teardown-function
           :token (gensym "execution-process-")
           :workdir workdir))
         (state (niwa-execution--state-for-session session))
         child)
    (setq
     child
     (niwa-execution-process-create
      :diagnostic-scope (niwa-diagnostic-scope (or scope session))
      :workdir workdir
      :terminal-function
      (lambda (settled result)
        (when (and (eq settled (niwa-execution--record-child record))
                   (not (niwa-execution--record-finished-p record)))
          (setf (niwa-execution--record-finished-p record) t
                (niwa-execution--record-error-data record)
                (plist-get result :error)
                (niwa-execution--record-exit-code record)
                (plist-get result :exit-code)
                (niwa-execution--record-output-limit-p record)
                (plist-get result :output-limit-p)
                (niwa-execution--record-termination record)
                (plist-get result :termination)
                (niwa-execution--record-timed-out-p record)
                (plist-get result :timed-out-p))
          (when (eq 'unknown (plist-get result :termination))
            (niwa-execution--mark-unknown
             record (plist-get result :error) result))
          (niwa-execution--cleanup-record record)
          (funcall callback result)))))
    (setf (niwa-execution--record-child record) child)
    (puthash (niwa-execution--record-token record) record
             (niwa-execution--state-records state))
    (and (niwa-execution-process-start
          child :name name :command command :coding 'no-conversion
          :timeout timeout)
         child)))


(defun niwa-execution--preparation-authority (session roots &optional request invocation)
  "Snapshot mutable launch authority for SESSION and local discovery ROOTS.
Only scalar policy and path facts are copied, never live runtime objects.
Compare snapshots with `equal'.  A deep copy, unlike printed text, cannot
depend on the printer state of the context that takes it."
  (let* ((policy-session (or session
                             (and request (niwa-request-session request))
                             (and invocation
                                  (niwa-agent-invocation-parent-session invocation))))
         (authority (and session (niwa-execution--mutation-target session)))
         (workspace (and authority (niwa-session-workspace authority))))
    (cl-labels ((snapshot (value)
                  ;; `copy-tree' retains mutable strings, including rule and
                  ;; grant paths.  A later in-place edit must not rewrite the
                  ;; authority captured before asynchronous preparation.
                  (cond ((consp value)
                         (let (items)
                           (while (consp value)
                             (push (snapshot (car value)) items)
                             (setq value (cdr value)))
                           (nconc (nreverse items) (snapshot value))))
                        ((stringp value) (copy-sequence value))
                        ((vectorp value) (vconcat (mapcar #'snapshot value)))
                        (t value))))
      (snapshot
       (list
        (niwa-permission-mode-effective session nil nil request invocation)
        (niwa-sandbox-mode-effective
         policy-session (niwa-permission-mode-effective session nil nil request invocation))
        (and (boundp 'niwa-memory-dirs) niwa-memory-dirs)
        (and (boundp 'niwa-workspace-additional-roots)
             niwa-workspace-additional-roots)
        temporary-file-directory
        (and (boundp 'niwa-permission-rules) niwa-permission-rules)
        (and authority (niwa-session-permission-rules authority))
        (and authority (niwa-session-resource-grants authority))
        (and workspace (niwa-permission-persistent-authority workspace 'global))
        (and workspace (niwa-permission-persistent-authority workspace 'workspace))
        (mapcar (lambda (root)
                  (let ((attributes (file-attributes root)))
                    (list (file-truename root)
                          (file-attribute-inode-number attributes)
                          (file-attribute-device-number attributes))))
                roots))))))

(defun niwa-execution--prepare
    (callback command workdir roots additional permissions session owner
              &optional temporary-root teardown settlement required)
  "Prepare COMMAND under owned, cancellable discovery for SESSION and OWNER.
CALLBACK receives preparation facts.  ROOTS, ADDITIONAL, PERMISSIONS and
TEMPORARY-ROOT describe confinement.  TEARDOWN owns discarded preparation.
SETTLEMENT permits terminal snapshot work after producer cancellation; request
identity, owner liveness, target and policy must still remain unchanged.
REQUIRED forces confinement independently of the permission mode."
  (let* ((buffer (current-buffer))
         (request niwa--current-request)
         (invocation (and (boundp 'niwa--agent-invocation)
                          niwa--agent-invocation))
         ;; A local snapshot can omit the execution SESSION while retaining
         ;; its caller's live policy.  Capture identity, not policy values.
         (policy-session (or session
                             (and request (niwa-request-session request))
                             (and invocation
                                  (niwa-agent-invocation-parent-session invocation))
                             niwa--session))
         (target (and session (niwa-session-execution-target session)))
         snapshot abandoned)
    (cl-labels
        ((valid-p ()
           (condition-case nil
               (and (buffer-live-p buffer)
                    (niwa-execution--owner-admissible-p invocation)
                    (or (null request)
                        (and (or settlement (not (niwa-request-cancelled-p request)))
                             (eq request (buffer-local-value
                                          'niwa--current-request buffer))))
                    (eq target (and session (niwa-session-execution-target session)))
                    (equal snapshot
                           (with-current-buffer buffer
                             (niwa-execution--preparation-authority
                              policy-session (cons workdir roots) request invocation))))
             (error nil))))
      (niwa-sandbox-prepare-start
       (lambda (preparation)
         (unless abandoned
           (if (and snapshot (not (valid-p)))
               (progn
                 (niwa-sandbox-cleanup preparation)
                 (funcall callback
                          '(:state refused :error "Execution authority changed during preparation; submit the operation again"
                            :facts (:sandbox refused :refused t))))
             (with-current-buffer buffer (funcall callback preparation)))))
       (lambda (argv receive)
         (unless snapshot
           (setq snapshot (niwa-execution--preparation-authority
                           policy-session (cons workdir roots) request invocation)))
         (let ((child (niwa-execution--start-process
                       (lambda (result)
                         (if (valid-p)
                             (with-current-buffer buffer (funcall receive result))
                           (funcall receive
                                    '(:exit-code -1 :error (error "Execution authority changed")))))
                       "niwa-protected-discovery" argv workdir 30
                       session owner
                       (lambda ()
                         (setq abandoned t)
                         (when teardown (funcall teardown)))
                       (niwa-diagnostic-scope policy-session))))
           (lambda ()
             (when child (niwa-execution-process-stop child 'aborted)))))
       command workdir roots additional permissions
       (if required 'required
         (niwa-sandbox-mode-effective
          policy-session
          (niwa-permission-mode-effective policy-session nil nil request invocation)))
       temporary-root (niwa-diagnostic-scope policy-session)))))


;;
;;; Managed Bash interface

(defun niwa-execution--spool-path (record)
  "Return RECORD's process-owned raw spool path."
  (when-let* ((child (niwa-execution--record-child record)))
    (niwa-execution-process-spool-path child)))

(defun niwa-execution--create-child (record spool-path)
  "Create RECORD's process child using SPOOL-PATH."
  (niwa-execution-process-create
   :diagnostic-scope
   (niwa-diagnostic-scope
    (niwa-execution--origin-session (niwa-execution--record-origin record)))
   :workdir (niwa-execution--record-workdir record)
   :tty (niwa-execution--record-tty-p record)
   :spool-path spool-path
   :pipeline-marker (niwa-execution--record-pipeline-marker record)
   :filter-function
   (lambda (_child chunk)
     (niwa-execution--managed-filter record chunk))
   :output-function
   (lambda (_child chunk)
     (niwa-execution--managed-append record chunk))
   :stderr-function
   (lambda (_child chunk)
     (niwa-execution--managed-append record chunk t))
   :terminal-function
   (lambda (child result)
     (niwa-execution--managed-process-terminal record child result))))

(defun niwa-execution--record-output-bytes (record)
  "Return the current spool size for RECORD."
  (let ((path (niwa-execution--spool-path record)))
    (or (and path (file-readable-p path)
             (file-attribute-size (file-attributes path)))
        0)))

(defun niwa-execution--managed-count (state)
  "Return the number of live managed records in STATE."
  (let ((count 0))
    (maphash
     (lambda (_key record)
       (when (niwa-execution--managed-live-p record)
         (setq count (1+ count))))
     (niwa-execution--state-records state))
    count))

(defun niwa-execution--telemetry (record event &rest props)
  "Record safe execution EVENT and PROPS for RECORD."
  (niwa-execution-telemetry-record
   (niwa-execution--record-telemetry-context record) event props t))

(defun niwa-execution--next-id (state)
  "Return the next opaque execution id in STATE.
The prefix distinguishes fresh states after a session reload from IDs
retained in the transcript and output artifacts."
  (let ((next (1+ (niwa-execution--state-next-id state))))
    (setf (niwa-execution--state-next-id state) next)
    (format "%s-%06d" (niwa-execution--state-id-prefix state) next)))

(defun niwa-execution--retain-output (record text)
  "Retain bounded whole and unread preview TEXT in RECORD."
  (cl-labels
      ((next (chars head tail)
         (let* ((limit niwa-execution-inline-output-limit)
                (head (concat (or head "") text))
                (tail (concat (or tail "") text)))
           (list (+ (or chars 0) (length text))
                 (substring head 0 (min limit (length head)))
                 (substring tail (max 0 (- (length tail) limit)))))))
    (pcase-let ((`(,chars ,head ,tail)
                 (next (niwa-execution--record-output-chars record)
                       (niwa-execution--record-output-head record)
                       (niwa-execution--record-output-tail record))))
      (setf (niwa-execution--record-output-chars record) chars
            (niwa-execution--record-output-head record) head
            (niwa-execution--record-output-tail record) tail))
    (pcase-let ((`(,chars ,head ,tail)
                 (next (niwa-execution--record-unread-chars record)
                       (niwa-execution--record-unread-head record)
                       (niwa-execution--record-unread-tail record))))
      (setf (niwa-execution--record-unread-chars record) chars
            (niwa-execution--record-unread-head record) head
            (niwa-execution--record-unread-tail record) tail))))

(defun niwa-execution--managed-filter (record chunk)
  "Remove RECORD's private sandbox marker from CHUNK."
  (unless (niwa-execution--record-finished-p record)
    (let ((marker (niwa-execution--record-marker record)))
      (when (and marker
                 (not (eq (niwa-execution--record-marker-buffer record)
                          :done)))
        (setq chunk
              (concat (or (niwa-execution--record-marker-buffer record) "")
                      chunk))
        (if-let* ((newline (string-search "\n" chunk)))
            (let ((first (substring chunk 0 newline)))
              (setf (niwa-execution--record-marker-buffer record) :done)
              (when (equal first marker)
                (setf (niwa-execution--record-marker-seen-p record) t)
                (setq chunk (substring chunk (1+ newline)))))
          (setf (niwa-execution--record-marker-buffer record) chunk)
          (setq chunk "")))
      chunk)))

(defun niwa-execution--managed-append (record chunk &optional stderr)
  "Record bounded CHUNK in RECORD's previews, framing stdout, not STDERR."
  (unless (or (niwa-execution--record-finished-p record)
              (string-empty-p chunk))
    (when (zerop (or (niwa-execution--record-output-chars record) 0))
      (niwa-execution--telemetry
       record 'execution-first-output :chunk-bytes (string-bytes chunk)))
    (if (eq (niwa-execution--record-kind record) 'monitor)
        (unless (or stderr (niwa-execution--record-stop-p record))
          (niwa-execution--monitor-frame record chunk))
      (niwa-execution--hasten-progress record))
    (niwa-execution--retain-text record chunk)))

(defun niwa-execution--retain-text (record chunk)
  "Count spooled CHUNK in RECORD's previews and line facts."
  (niwa-execution--retain-output record chunk)
  (cl-incf (niwa-execution--record-newline-count record)
           (cl-count ?\n chunk))
  (setf (niwa-execution--record-last-byte-newline-p record)
        (eq (aref chunk (1- (length chunk))) ?\n)))

(defun niwa-execution--collect-stderr (record)
  "Move RECORD's target-side stderr file into its spool and previews.
Only a remote launch through TRAMP's shell adapter keeps one.  Delivery
calls this where target I/O may run, before building the final result; a
failure is reported and the result is delivered without that stderr.  Text
cut by the output cap is marked in the result, as native stderr's cap stop
is."
  (when-let* ((child (niwa-execution--record-child record)))
    (condition-case err
        (when-let* ((collected (niwa-execution-process-collect-stderr child)))
          (unless (string-empty-p (car collected))
            (niwa-execution--retain-text record (car collected)))
          (when (cdr collected)
            (setf (niwa-execution--record-output-limit-p record) t)
            ;; Preview only: the spool is full, and the model must not
            ;; read the cut file as complete.
            (niwa-execution--retain-output
             record "\n[stderr truncated at the output limit]\n")))
      (error
       (niwa--warn-once
        (niwa-execution--diagnostic-scope record)
        'execution-remote-stderr
        "Could not collect remote execution stderr: %s"
        (error-message-string err))))))

(defun niwa-execution--monitor-body (record)
  "Return RECORD's bounded pending event batch without consuming it."
  (concat
   (mapconcat #'identity
              (reverse (niwa-execution--record-monitor-lines record)) "\n")
   (when (niwa-execution--record-monitor-lines record) "\n")
   (when (> (niwa-execution--record-monitor-omitted record) 0)
     (format "\n[%d more event lines in the output file]\n"
             (niwa-execution--record-monitor-omitted record)))))

(defun niwa-execution--monitor-batch (record)
  "Return RECORD's pending framed lines and omitted count as event properties."
  (list :monitor-lines
        (reverse (copy-sequence (niwa-execution--record-monitor-lines record)))
        :monitor-omitted (niwa-execution--record-monitor-omitted record)))

(defun niwa-execution--monitor-batch-settled
    (record event lines omitted success)
  "Settle RECORD's in-flight batch of LINES and OMITTED after delivery.
SUCCESS drops exactly the delivered lines, keeping any that arrived while the
batch was pending, and publishes EVENT.  Failure keeps every line."
  (setf (niwa-execution--record-monitor-inflight record) nil)
  (when success
    (let ((kept (butlast (niwa-execution--record-monitor-lines record)
                         lines)))
      (setf (niwa-execution--record-monitor-lines record) kept
            (niwa-execution--record-monitor-bytes record)
            (cl-loop for line in kept sum (1+ (string-bytes line)))
            (niwa-execution--record-monitor-omitted record)
            (max 0 (- (niwa-execution--record-monitor-omitted record)
                      omitted))))
    (niwa-execution--emit-event event)
    (niwa-execution--notify-state-change record)
    (when (and (not (niwa-execution--record-finished-p record))
               (> (niwa-agent-control-monitor-unread-bytes
                   (niwa-execution--origin-session
                    (niwa-execution--record-origin record))
                   (niwa-execution--record-execution-id record))
                  (* 64 1024)))
      (niwa-execution--stop-process record 'flood))
    (when (and (niwa-execution--record-monitor-lines record)
               (not (niwa-execution--record-finished-p record))
               (not (niwa-execution--record-stop-p record))
               (not (niwa-execution--record-monitor-timer record)))
      (setf (niwa-execution--record-monitor-timer record)
            (niwa-transport-run-at-time
             0.2 #'niwa-execution--monitor-flush record))))
  (when-let* ((after (niwa-execution--record-monitor-after record)))
    (setf (niwa-execution--record-monitor-after record) nil)
    (funcall after)))

(defun niwa-execution--monitor-flush (record)
  "Secure RECORD's pending batch in its mailbox, retaining it until acknowledged.
One batch is in flight at a time; its lines stay in RECORD until the sink
acknowledges success, and a failure keeps them for the next flush."
  (setf (niwa-execution--record-monitor-timer record) nil)
  (unless (or (niwa-execution--record-finished-p record)
              (niwa-execution--record-stop-p record)
              (niwa-execution--record-monitor-inflight record)
              (eq (niwa-execution--record-delivery-state record) 'discarded))
    (let ((body (niwa-execution--monitor-body record)))
      (unless (string-empty-p body)
        (let* ((batch (niwa-execution--monitor-batch record))
               (lines (length (plist-get batch :monitor-lines)))
               (omitted (plist-get batch :monitor-omitted))
               (event (apply #'niwa-execution--event
                             record 'monitor-batch :body body batch)))
          (setf (niwa-execution--record-monitor-inflight record) t)
          (niwa-execution--deliver-mailbox-event
           record event
           (lambda (success _error)
             (niwa-execution--monitor-batch-settled
              record event lines omitted success))))))))

(defun niwa-execution--monitor-line (record &optional final)
  "Complete RECORD's bounded partial line and update its event limits.
FINAL retains the last line at settlement without stopping or arming a timer."
  (let* ((length (niwa-execution--record-monitor-partial-length record))
         (length (if (eq (niwa-execution--record-monitor-last-char record) ?\r)
                     (1- length) length))
         (partial (niwa-execution--record-monitor-partial record))
         (retained (min length (length partial)))
         (line (concat (substring partial 0 retained)
                       (when (> length retained)
                         (format " [%d characters truncated]" (- length retained)))))
         (bytes (1+ (string-bytes line)))
         (now (float-time)))
    (cl-incf (niwa-execution--record-monitor-count record))
    (setf (niwa-execution--record-monitor-partial record) ""
          (niwa-execution--record-monitor-partial-length record) 0
          (niwa-execution--record-monitor-last-char record) nil
          (niwa-execution--record-monitor-times record)
          (cons now (cl-delete-if
                     (lambda (time) (<= time (- now 60)))
                     (niwa-execution--record-monitor-times record))))
    ;; Reserve room for the omitted-line summary in the 8 KiB batch.
    (if (and (zerop (niwa-execution--record-monitor-omitted record))
             (<= (+ bytes (niwa-execution--record-monitor-bytes record))
                 (- 8192 128)))
        (progn
          (push line (niwa-execution--record-monitor-lines record))
          (cl-incf (niwa-execution--record-monitor-bytes record) bytes))
      (cl-incf (niwa-execution--record-monitor-omitted record)))
    (unless final
      (if (> (length (niwa-execution--record-monitor-times record)) 100)
          (niwa-execution--stop-process record 'flood)
        (unless (or (niwa-execution--record-monitor-timer record)
                    (niwa-execution--record-stop-p record))
          (setf (niwa-execution--record-monitor-timer record)
                (niwa-transport-run-at-time
                 0.2 #'niwa-execution--monitor-flush record)))))))

(defun niwa-execution--monitor-frame (record chunk)
  "Frame stdout CHUNK into RECORD's bounded partial line and pending batch."
  (let ((start 0) end)
    (while (and (< start (length chunk))
                (not (niwa-execution--record-stop-p record)))
      (setq end (or (string-search "\n" chunk start) (length chunk)))
      (let* ((count (- end start))
             (partial (niwa-execution--record-monitor-partial record))
             (keep (min count (- 2000 (length partial)))))
        (setf (niwa-execution--record-monitor-partial record)
              (concat partial (substring chunk start (+ start keep))))
        (cl-incf (niwa-execution--record-monitor-partial-length record) count)
        (when (> count 0)
          (setf (niwa-execution--record-monitor-last-char record)
                (aref chunk (1- end)))))
      (when (< end (length chunk))
        (niwa-execution--monitor-line record))
      (setq start (1+ end)))))

(defun niwa-execution--managed-lines (record)
  "Return the retained logical line count for RECORD."
  (let ((bytes (niwa-execution--record-output-bytes record))
        (newlines (or (niwa-execution--record-newline-count record) 0)))
    (+ newlines
       (if (and (> bytes 0)
                (not (niwa-execution--record-last-byte-newline-p record)))
           1
         0))))

(defun niwa-execution--termination (record)
  "Return RECORD's canonical termination symbol, or nil while running."
  (when (niwa-execution--record-finished-p record)
    (or (niwa-execution--record-termination record)
        (and (niwa-execution--record-error-data record) 'spawn-failed)
        'exited)))

(defun niwa-execution--resolve-outcome
    (outcome-function exit-code termination &optional scope)
  "Resolve a canonical outcome without risking execution settlement.
SCOPE identifies the originating workspace for resolver failure reports.

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
         (niwa-diagnostic-warning
          (niwa-diagnostic-scope scope)
          'niwa
          (format "Execution outcome resolver failed: %s"
                  (error-message-string err))
          :warning)
         default-outcome)))))

(defun niwa-execution--lifecycle-state (record)
  "Return RECORD's canonical public lifecycle state.
A launched child that already exited is still `running' until its record
settles: it is no longer waiting for admission."
  (cond
   ((niwa-execution--record-finished-p record) 'completed)
   ((niwa-execution--record-stop-p record) 'stopping)
   ((or (niwa-execution--record-service record)
        (when-let* ((child (niwa-execution--record-child record)))
          (niwa-execution-process-launch-attempted-p child)))
    'running)
   (t 'queued)))

(defun niwa-execution--artifact-address (record)
  "Return RECORD's logical model-visible artifact address, or nil.

A remote execution keeps its recoverable output artifact, so its address
does not depend on the local spool the target never wrote to.  The address
is remembered for its path: every progress event carries it, and resolving
it walks the path's components for symlinks, which dominated a live
execution's progress cost."
  (when-let* ((path
               (if (file-remote-p
                    (or (niwa-execution--record-workdir record) ""))
                   (niwa-execution--record-recoverable-output-path record)
                 (and (or (niwa-execution--record-yielded-p record)
                          (niwa-execution--record-retained-p record))
                      (niwa-execution--spool-path record)))))
    (let ((memo (niwa-execution--record-artifact-address record)))
      (if (equal path (car memo))
          (cdr memo)
        (let ((address (niwa-resource-artifact-address
                        path
                        (niwa-execution--origin-session
                         (niwa-execution--record-origin record)))))
          (setf (niwa-execution--record-artifact-address record)
                (cons path address))
          address)))))

(defun niwa-execution--facts (record)
  "Return an immutable public fact snapshot for RECORD."
  (let* ((finished (niwa-execution--record-finished-p record))
         (child-status
          (when-let* ((child (niwa-execution--record-child record)))
            (niwa-execution-process-status child)))
         (origin (niwa-execution--record-origin record))
         (exit-code (and finished
                         (niwa-execution--record-exit-code record)))
         (termination (niwa-execution--termination record))
         (outcome-function
          (niwa-execution--record-outcome-function record))
         (pipeline-codes (plist-get child-status :pipeline-exit-codes))
         (pipeline-facts
          (cond
           ((and finished (null pipeline-codes)
                 (niwa-execution--record-pipeline-marker record))
            '(:pipeline-classification unavailable))
           ((and finished (> (length pipeline-codes) 1))
               (condition-case nil
                   (funcall (niwa-execution--record-pipeline-facts-function record)
                            pipeline-codes)
                 (error '(:pipeline-classification unavailable)))))))
    (list :kind (or (niwa-execution--record-kind record) 'bash)
          :eval-mode (and (niwa-execution--record-service-call-p record) 'sandbox)
          :description (niwa-execution--record-description record)
          :expires-at (niwa-execution--record-expires-at record)
          :event-count (and (eq (niwa-execution--record-kind record) 'monitor)
                            (niwa-execution--record-monitor-count record))
          :execution-id
          (and (niwa-execution--record-yielded-p record)
               (niwa-execution--record-execution-id record))
          :command
          (plist-get (niwa-execution--origin-tool-args origin) :command)
          :workdir (niwa-execution--record-workdir record)
          :state (niwa-execution--lifecycle-state record)
          :termination termination
          :exit-code exit-code
          :pipeline-exit-codes (and (> (length pipeline-codes) 1) pipeline-codes)
          :pipeline-failed-stages (plist-get pipeline-facts :pipeline-failed-stages)
          :pipeline-classification (plist-get pipeline-facts :pipeline-classification)
          :outcome (and finished
                        (or (plist-get (niwa-execution--record-terminal-result record)
                                       :outcome)
                            (and (eq termination 'exited)
                                 (or (plist-get pipeline-facts :pipeline-failed-stages)
                                     (equal exit-code (car (last pipeline-codes))))
                                 (plist-get pipeline-facts :pipeline-outcome))
                            (niwa-execution--resolve-outcome
                             outcome-function exit-code termination
                             (niwa-execution--diagnostic-scope record))))
          :wall-time-seconds
          (- (float-time) (niwa-execution--record-started-at record))
          :output-bytes (niwa-execution--record-output-bytes record)
          :output-lines (niwa-execution--managed-lines record)
          :omitted-output-bytes
          (or (niwa-execution--record-omitted-output-bytes record) 0)
          :tty (and (plist-get child-status :tty-p) t)
          :direct-async
          (and (plist-get child-status :direct-async-p) t)
          :group-id (plist-get child-status :group-id)
          :output-path (niwa-execution--artifact-address record))))

(defun niwa-execution--event (record type &rest properties)
  "Return an immutable TYPE event for RECORD with PROPERTIES."
  (append
   (list :type type
         :emitted-at (float-time)
         :started-at (niwa-execution--record-started-at record)
         :session (niwa-execution--origin-session
                   (niwa-execution--record-origin record))
         :data-buffer (niwa-execution--origin-data-buffer
                       (niwa-execution--record-origin record))
         :owner (niwa-execution--origin-owner
                 (niwa-execution--record-origin record))
         :tool-args
         (copy-tree
          (niwa-execution--origin-tool-args
           (niwa-execution--record-origin record)))
         :tool-use-id (niwa-execution--origin-tool-use-id
                       (niwa-execution--record-origin record))
         :permission-approvals
         (niwa-execution--copy-value
          (niwa-execution--record-permission-approvals record))
         :facts (niwa-execution--facts record))
   properties))

(defun niwa-execution--copy-value (value)
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

(defun niwa-execution--emit-event (event)
  "Publish EVENT to passive consumers, ignoring their return values."
  (dolist (function niwa-execution-event-functions)
    (when (functionp function)
      (condition-case err
          (funcall function (niwa-execution--copy-value event))
        (error
         (niwa--warn-once
          (niwa-diagnostic-scope (plist-get event :session))
          'execution-event-consumer
          "Execution event consumer failed: %s"
          (error-message-string err)))))))

(defun niwa-execution--deliver-mailbox-event (record event acknowledgement)
  "Hand RECORD's EVENT to the mailbox sink; ACKNOWLEDGEMENT runs exactly once.
ACKNOWLEDGEMENT receives SUCCESS and ERROR.  A missing sink, a sink that
signals, and a sink that returns without acknowledging or answering `pending'
all acknowledge failure.  Errors in ACKNOWLEDGEMENT are contained here and
never reach the sink.  Return `pending' while the acknowledgement is still
outstanding, else nil."
  (let* ((done nil)
         (ack (lambda (success &optional error)
                (unless done
                  (setq done t)
                  (condition-case err
                      (funcall acknowledgement (and success t) error)
                    (error
                     (niwa--warn-once
                      (niwa-execution--diagnostic-scope record)
                      'execution-mailbox-acknowledgement
                      "Execution mailbox acknowledgement failed: %s"
                      (error-message-string err))))))))
    (if (not (functionp niwa-execution-mailbox-delivery-function))
        (funcall ack nil "No mailbox consumer")
      (condition-case err
          (let ((result
                 (funcall niwa-execution-mailbox-delivery-function
                          (niwa-execution--copy-value event)
                          (niwa-execution--origin-owner-context
                           (niwa-execution--record-origin record))
                          ack)))
            (unless (or done (eq result 'pending))
              (funcall ack nil "Mailbox sink returned without acknowledging")))
        (error
         (niwa--warn-once
          (niwa-execution--diagnostic-scope record)
          'execution-mailbox-delivery
          "Execution mailbox delivery failed: %s"
          (error-message-string err))
         (funcall ack nil (error-message-string err)))))
    (and (not done) 'pending)))

(defun niwa-execution--whole-preview (record)
  "Return RECORD's bounded whole-artifact head-and-tail preview."
  (plist-get
   (niwa--head-tail-preview-parts
    (or (niwa-execution--record-output-head record) "")
    (or (niwa-execution--record-output-tail record) "")
    (or (niwa-execution--record-output-chars record) 0)
    niwa-execution-inline-output-limit
    (niwa-execution--artifact-address record))
   :text))

(defun niwa-execution--emit-progress (record)
  "Publish one bounded progress event for live RECORD.
Quiet events share integral multiples of
`niwa-execution--quiet-progress-interval', at least 250 ms after this
event, or at least `niwa-execution-progress-interval' after it when
that is longer.  Output arriving meanwhile brings the next event forward,
see `niwa-execution--hasten-progress'."
  (unless (niwa-execution--record-finished-p record)
    (setf (niwa-execution--record-progress-emitted-at record) (float-time))
    (niwa-execution--emit-event
     (niwa-execution--event
      record 'progress
      :output-preview-truncated-p
      (> (or (niwa-execution--record-output-chars record) 0)
         niwa-execution-inline-output-limit)
      :output-tail (or (niwa-execution--record-output-tail record) "")))
    (let ((timer (or (niwa-execution--record-progress-timer record)
                     (timer-create))))
      (niwa--ui-timer-cancel timer)
      (timer-set-time
       timer (timer-next-integral-multiple-of-time
              (time-add nil (if (> niwa-execution-progress-interval
                                   niwa-execution--quiet-progress-interval)
                                niwa-execution-progress-interval
                              0.25))
              niwa-execution--quiet-progress-interval))
      (timer-set-function timer #'niwa-execution--emit-progress (list record))
      (setf (niwa-execution--record-progress-timer record) timer)
      (niwa--ui-timer-activate
       timer t (niwa-execution--diagnostic-scope record)))))

(defun niwa-execution--hasten-progress (record)
  "Bring RECORD's next progress event forward after new output.
Before the first event, the initial `niwa-execution-progress-delay'
stands.  Afterwards the next event follows the previous one by
`niwa-execution-progress-interval' instead of the quiet interval.
A filter can run inside a TRAMP wait or
`niwa-transport-with-exclusive-connection', which binds `timer-list'
away: `cancel-timer' there left the pending event armed beside its
replacement, two self-rearming chains, and `run-at-time' would arm the
replacement on a binding about to be discarded.  Move the same timer with
the UI-host primitives instead."
  (when-let* ((emitted (niwa-execution--record-progress-emitted-at record))
              (timer (niwa-execution--record-progress-timer record))
              ((timerp timer))
              (due (+ emitted (max 0.25 niwa-execution-progress-interval)))
              ;; Rearming lands a hair after DUE; only a quiet wait is late.
              ((> (float-time (timer--time timer)) (+ due 0.01))))
    (niwa--ui-timer-cancel timer)
    (timer-set-time timer (seconds-to-time (max due (float-time))))
    (niwa--ui-timer-activate
     timer nil (niwa-execution--diagnostic-scope record))))

(defun niwa-execution--unread-preview (record unread-bytes)
  "Return RECORD's bounded unread preview for UNREAD-BYTES."
  (let* ((parts
          (niwa--head-tail-preview-parts
           (or (niwa-execution--record-unread-head record) "")
           (or (niwa-execution--record-unread-tail record) "")
           (or (niwa-execution--record-unread-chars record) 0)
           niwa-execution-inline-output-limit
           (niwa-execution--artifact-address record)))
         (visible-bytes
          (+ (string-bytes (plist-get parts :head))
             (string-bytes (plist-get parts :tail)))))
    (list :output (plist-get parts :text)
          :omitted (max 0 (- unread-bytes visible-bytes)))))

(defun niwa-execution--unread-range (record)
  "Return RECORD's next unread range without consuming it."
  ;; A foreground result about to be truncated keeps its spool reachable:
  ;; move it out of the hidden pending directory and retain it.  Small
  ;; outputs never reach this branch, so they cost nothing.
  (when (and (not (niwa-execution--record-yielded-p record))
             (not (file-remote-p
                   (or (niwa-execution--record-workdir record) "")))
             (> (or (niwa-execution--record-unread-chars record) 0)
                niwa-execution-inline-output-limit))
    (niwa-execution--publish-yielded-artifact record)
    (setf (niwa-execution--record-retained-p record) t))
  (let* ((start (or (niwa-execution--record-read-offset record) 0))
         (end (niwa-execution--record-output-bytes record))
         (preview (niwa-execution--unread-preview record (- end start))))
    (list :end end
          :omitted (plist-get preview :omitted)
          :output (plist-get preview :output))))

(defun niwa-execution--publish-remote-output
    (record range &optional terminal-p)
  "Publish RECORD's recoverable remote output for RANGE.

Publish when RANGE omits bytes, after a prior snapshot, or when TERMINAL-P
settles a yielded execution.  The live spool always remains local."
  (let* ((origin (niwa-execution--record-origin record))
         (session (niwa-execution--origin-session origin))
         (target (and session (niwa-session-execution-target session)))
         (save-path (and session (niwa-session-save-path session)))
         (bytes (niwa-execution--record-output-bytes record)))
    (when (and target save-path
               (niwa-execution-target-remote-p target)
               (> bytes 0)
               (or (> (or (plist-get range :omitted) 0) 0)
                   (niwa-execution--record-recoverable-output-path record)
                   (and terminal-p
                        (niwa-execution--record-yielded-p record)))
               (not (equal bytes
                           (niwa-execution--record-recoverable-output-bytes
                            record))))
      (let* ((qualified
              (file-name-concat
               save-path "tool-results" "executions"
               (concat (niwa-execution--record-execution-id record)
                       ".log")))
             (native
              (niwa-execution-target-native-path target qualified)))
        (condition-case err
            (progn
              (when
                  (memq
                   (niwa-session-artifacts-publish-text
                    session qualified
                    (niwa-execution--read-output record)
                    'utf-8-unix)
                   '(published queued))
                (setf
                 (niwa-execution--record-recoverable-output-bytes record)
                 bytes
                 (niwa-execution--record-recoverable-output-path record)
                 native)))
          (error
           (niwa-diagnostic-warning
            (niwa-execution--diagnostic-scope record)
            'niwa
            (format "Could not publish remote execution output: %s"
                    (error-message-string err))
            :warning)))))))

(defun niwa-execution--consume-unread-range (record range)
  "Commit consumption of RECORD's unread RANGE."
  (let ((omitted (plist-get range :omitted)))
    (setf (niwa-execution--record-read-offset record)
          (plist-get range :end)
          (niwa-execution--record-unread-chars record) 0
          (niwa-execution--record-unread-head record) ""
          (niwa-execution--record-unread-tail record) ""
          (niwa-execution--record-omitted-output-bytes record)
          (+ (or (niwa-execution--record-omitted-output-bytes record) 0)
             omitted))
    (when (> omitted 0)
      (setf (niwa-execution--record-retained-p record) t))))

(defun niwa-execution--range-observation
    (record range &optional claimed project-unconsumed)
  "Return an observation of RECORD and unread RANGE.
CLAIMED non-nil marks it as the final model delivery.
PROJECT-UNCONSUMED includes RANGE's omission in facts before commitment."
  (let ((facts (niwa-execution--facts record))
        (result (plist-get (niwa-execution--record-terminal-result record) :output)))
    (when project-unconsumed
      (setq facts
            (plist-put
             facts :omitted-output-bytes
             (+ (or (plist-get facts :omitted-output-bytes) 0)
                (plist-get range :omitted)))))
    (list :result (if (and result (niwa-execution--record-yielded-p record))
                      (plist-get
                       (niwa--head-tail-preview-parts
                        result result (length result)
                        niwa-execution-inline-output-limit
                        (niwa-execution--artifact-address record))
                       :text)
                    result)
          :output (plist-get range :output)
          :facts facts
          :compound-profile-misses
          (niwa-execution--copy-value
           (plist-get (niwa-execution--origin-tool-args
                       (niwa-execution--record-origin record))
                      :compound-profile-misses))
          :sandbox-facts (niwa-execution--record-sandbox-facts record)
          :sandbox-summary
          (niwa-execution-telemetry-context-summary
           (niwa-execution--record-telemetry-context record))
          :permission-approvals
          (niwa-execution--copy-value
           (niwa-execution--record-permission-approvals record))
          :error (niwa-execution--record-error-data record)
          :claimed-final-p (and claimed t))))

(defun niwa-execution--terminal-event (record delivery observation)
  "Return RECORD's terminal event for DELIVERY and OBSERVATION."
  (let ((event
         (niwa-execution--event
          record 'terminal
          :delivery delivery
          :observation observation
          :monitor-lines
          (when (eq (niwa-execution--record-kind record) 'monitor)
            (plist-get (niwa-execution--monitor-batch record)
                       :monitor-lines))
          :monitor-omitted
          (when (eq (niwa-execution--record-kind record) 'monitor)
            (niwa-execution--record-monitor-omitted record))
          :whole-output-truncated-p
          (> (if-let* ((result (plist-get (niwa-execution--record-terminal-result record)
                                         :output)))
                 (length result)
               (or (niwa-execution--record-output-chars record) 0))
             niwa-execution-inline-output-limit)
          :whole-output (or (plist-get observation :result)
                            (niwa-execution--whole-preview record)))))
    (plist-put event :facts (plist-get observation :facts))))

(defun niwa-execution--observation (record &optional claim-final)
  "Return RECORD's next unread observation.

When CLAIM-FINAL is non-nil, publish the first terminal observation and cache
it briefly so repeated owner polls return the same result."
  (if-let* ((terminal
             (and claim-final
                  (niwa-execution--record-terminal-observation record))))
      (niwa-execution--copy-value terminal)
    (when (and claim-final
               (niwa-execution--record-delivery-state record))
      (signal 'niwa-execution-not-found
              (list (if (niwa-execution--mailbox-pending-p record)
                        "Execution terminal result delivery is pending"
                      "Execution terminal result is already claimed"))))
    (if (niwa-execution--mailbox-pending-p record)
        ;; An acknowledgement is outstanding for this range: observe it
        ;; without consuming, so a failed delivery loses nothing.
        (niwa-execution--range-observation
         record (niwa-execution--unread-range record))
      (when claim-final
        (setf (niwa-execution--record-delivery-state record) 'model))
      (let ((range (niwa-execution--unread-range record)))
        (niwa-execution--publish-remote-output record range claim-final)
        (niwa-execution--consume-unread-range record range)
        (let ((observation
               (niwa-execution--range-observation
                record range claim-final)))
          (when claim-final
            (setf (niwa-execution--record-terminal-observation record)
                  (niwa-execution--copy-value observation))
            (niwa-execution--emit-event
             (niwa-execution--terminal-event record 'model observation))
            (niwa-execution--retire-terminal-record record))
          observation)))))

(defun niwa-execution--mailbox-pending-p (record)
  "Return non-nil while RECORD's terminal mailbox delivery awaits acknowledgement."
  (and (eq (niwa-execution--record-delivery-state record) 'mailbox)
       (null (niwa-execution--record-terminal-observation record))))

(defun niwa-execution--deliver-independent (record &optional settled)
  "Secure RECORD's unread terminal result in its owner mailbox.
The record keeps its identity, unread output and observation until the sink
acknowledges success; polls cannot claim the result meanwhile.  A failure
restores the record for retry.  SETTLED, if given, runs once with the outcome
when the delivery is resolved, including when nothing needed delivering.
Return `pending' while acknowledgement is outstanding, else nil."
  (cond
   ((niwa-execution--record-monitor-inflight record)
    ;; A monitor batch holds lines the terminal event would repeat; resume
    ;; once its acknowledgement has settled them.
    (setf (niwa-execution--record-monitor-after record)
          (lambda () (niwa-execution--deliver-independent record settled)))
    'pending)
   ((niwa-execution--record-delivery-state record)
    (when settled (funcall settled nil))
    nil)
   (t
    (setf (niwa-execution--record-delivery-state record) 'mailbox)
    (let* ((range (niwa-execution--unread-range record))
           (_ (niwa-execution--publish-remote-output record range t))
           (observation
            (niwa-execution--range-observation record range t t))
           (event
            (niwa-execution--terminal-event record 'mailbox observation)))
      (niwa-execution--deliver-mailbox-event
       record event
       (lambda (success error)
         (when (eq (niwa-execution--record-delivery-state record) 'mailbox)
           (niwa-execution--emit-event event)
           (if success
               (progn
                 (setf (niwa-execution--record-terminal-observation record)
                       (niwa-execution--copy-value observation))
                 (niwa-execution--consume-unread-range record range)
                 (niwa-execution--retire-terminal-record record))
             (niwa-diagnostic-warning
              (niwa-execution--diagnostic-scope record)
              'niwa
              (format "Execution %s completion was not delivered: %s"
                      (niwa-execution--record-execution-id record)
                      (or error "unknown failure"))
              :warning)
             (setf (niwa-execution--record-delivery-state record) nil
                   (niwa-execution--record-terminal-observation record)
                   nil)))
         (when settled (funcall settled success))))))))

(defun niwa-execution--deliver-observer (record)
  "Deliver RECORD's terminal observation to its waiting observer."
  (when-let* ((callback (niwa-execution--record-observer record)))
    (setf (niwa-execution--record-observer record) nil
          (niwa-execution--record-observer-timer record) nil)
    (funcall callback (niwa-execution--observation record t))))

(defun niwa-execution--cancel-observer (record callback)
  "Detach CALLBACK from RECORD without consuming unread output."
  (when (eq callback (niwa-execution--record-observer record))
    (when-let* ((timer (niwa-execution--record-observer-timer record)))
      (cancel-timer timer))
    (setf (niwa-execution--record-observer record) nil
          (niwa-execution--record-observer-timer record) nil)))

(defun niwa-execution--flush-observer (record)
  "Complete RECORD's pending poll before a control operation takes over."
  (when-let* ((callback (niwa-execution--record-observer record)))
    (when-let* ((timer (niwa-execution--record-observer-timer record)))
      (cancel-timer timer))
    (setf (niwa-execution--record-observer record) nil
          (niwa-execution--record-observer-timer record) nil)
    (condition-case err
        (funcall callback (niwa-execution--observation record))
      (error
       (niwa--warn-once
        (niwa-execution--diagnostic-scope record)
        'execution-poll-callback
        "Execution poll callback failed: %s"
        (error-message-string err))))))

(defun niwa-execution--poll-expired (record)
  "Deliver a running observation when RECORD's poll wait expires."
  (when-let* ((callback (niwa-execution--record-observer record)))
    (setf (niwa-execution--record-observer record) nil
          (niwa-execution--record-observer-timer record) nil)
    (funcall callback (niwa-execution--observation record))))

(defun niwa-execution--sandbox-launch-result (record)
  "Return the terminal child-result subset used for sandbox launch checks."
  (list :exit-code (niwa-execution--record-exit-code record)
        :output (niwa-execution--read-output record)
        :termination (niwa-execution--termination record)
        :timed-out-p (niwa-execution--record-timed-out-p record)
        :error (niwa-execution--record-error-data record)))

(defun niwa-execution--release-scheduler (record)
  "Release RECORD's scheduler lease exactly once."
  (when-let* ((lease (niwa-execution--record-scheduler-lease record)))
    (setf (niwa-execution--record-scheduler-lease record) nil)
    (niwa-execution-scheduler-release lease)))

(defun niwa-execution--finish-managed (record &optional defer)
  "Settle managed RECORD without delivering unsolicited model output.
DEFER delivers in a later event-loop turn, for a process that ended on its
own and settles from a timer.  Stops, aborts and teardown deliver before
returning: their callers expect the outcome settled."
  (unless (niwa-execution--record-finished-p record)
    (let* ((preparation
            (niwa-execution--record-sandbox-preparation record))
           (child-result (and (eq (plist-get preparation :state) 'confined)
                              (not (niwa-execution--record-marker-seen-p
                                    record))
                              (niwa-execution--sandbox-launch-result record)))
           (launch-failed
            (and child-result
                 (niwa-sandbox-launch-failed-p preparation child-result))))
      (when launch-failed
        (setf (niwa-execution--record-sandbox-facts record)
              (niwa-sandbox--record-launch-failure
               child-result (niwa-execution--record-workdir record))))
      (when preparation
        (niwa-sandbox-cleanup preparation))
      ;; A short-lived monitor can exit inside process startup (notably TRAMP).
      ;; It still has an execution identity and owes one terminal mailbox notice.
      (when (and (eq (niwa-execution--record-kind record) 'monitor)
                 (not (niwa-execution--record-error-data record))
                 (not (eq (niwa-execution--record-delivery-state record) 'discarded))
                 (niwa-execution-process-launch-attempted-p
                  (niwa-execution--record-child record)))
        (niwa-execution--yield-managed record))
      (when (and (eq (niwa-execution--record-kind record) 'monitor)
                 (> (niwa-execution--record-monitor-partial-length record) 0))
        (niwa-execution--monitor-line record t))
      (setf (niwa-execution--record-finished-p record) t
            (niwa-execution--record-delivery-pending-p record) t)
      (when-let* ((function (niwa-execution--record-result-function record)))
        (unwind-protect
            (progn
              (niwa-execution--collect-stderr record)
              (setf (niwa-execution--record-terminal-result record)
                    (condition-case err
                        (funcall function (niwa-execution--sandbox-launch-result record))
                      (error (list :output (format "Execution result failed: %s" err)
                                   :outcome 'failure)))))
          (setf (niwa-execution--record-result-function record) nil)
          (niwa-execution--teardown-record record)))
      (niwa-execution--settle-mutation record)
      (let* ((facts (niwa-execution--record-sandbox-facts record))
             (refused-p (plist-get facts :refused))
             (started-p
              (and (not refused-p)
                   (not launch-failed)
                   (when-let* ((child (niwa-execution--record-child
                                      (or (niwa-execution--record-service record)
                                          record))))
                     (niwa-execution-process-launch-attempted-p child)))))
        (niwa-execution-telemetry-record-sandbox-attempt
         (niwa-execution--record-telemetry-context record)
         facts started-p refused-p))
      (apply #'niwa-execution--telemetry
             record 'execution-finished
             :exit-code (niwa-execution--record-exit-code record)
             :termination (niwa-execution--termination record)
             :duration-ms
             (round (* 1000.0
                       (- (float-time)
                          (niwa-execution--record-started-at record))))
             :output-bytes (niwa-execution--record-output-bytes record)
             :output-limit (and (niwa-execution--record-output-limit-p
                                 record)
                                t)
             :timed-out (and (niwa-execution--record-timed-out-p record)
                             t)
             (append
              (niwa-execution-telemetry-context-properties
               (niwa-execution--record-telemetry-context record))
              (niwa-execution-telemetry-safe-facts
               (niwa-execution--record-sandbox-facts record))))
      (niwa-execution--notify-state-change record)
      (niwa-execution--release-runtime record)
      (niwa-execution--release-scheduler record)
      (let ((deliver
             (lambda ()
               (let ((awaiting nil)
                     (clear
                      (lambda (&rest _)
                        (setf (niwa-execution--record-delivery-pending-p
                               record)
                              nil)
                        (when (niwa-execution--record-observer record)
                          (niwa-execution--deliver-observer record)))))
                 ;; An independent delivery keeps the record pending until its
                 ;; sink acknowledges; every other path settles in place.
                 (unwind-protect
                     (progn
                       (niwa-execution--collect-stderr record)
                       (if (niwa-execution--record-yielded-p record)
                           (if (and (niwa-execution--record-observer record)
                                    (not (eq (niwa-execution--record-kind
                                              record)
                                             'monitor)))
                               (niwa-execution--deliver-observer record)
                             (if niwa-execution-mailbox-delivery-function
                                 (setq awaiting
                                       (niwa-execution--deliver-independent
                                        record clear))
                               (when (niwa-execution--record-observer record)
                                 (niwa-execution--deliver-observer record))))
                         (let ((callback
                                (niwa-execution--record-callback record)))
                           (unless (eq (niwa-execution--record-delivery-state
                                        record)
                                       'discarded)
                             (funcall callback
                                      (niwa-execution--observation
                                       record t))))))
                   (unless awaiting
                     (setf (niwa-execution--record-delivery-pending-p
                            record)
                           nil))))))
            (workdir (niwa-execution--record-workdir record)))
        ;; Delivery starts the tool's continuation -- result handling and,
        ;; for gptel, the next request -- and for background commands
        ;; publishes a breadcrumb and durable mail.  Sharing one callback
        ;; with process settlement made each tool completion one long
        ;; pause, so DEFER moves it to a later turn.  Settlement can also be
        ;; reached from a sentinel inside another remote command, where the
        ;; control filesystem refuses to nest; delivery then waits for an
        ;; idle transport.  A disabled transport delivers in place.
        (unless (and (or defer (niwa-transport-busy-p workdir))
                     (niwa-transport-run-when-idle
                      (list 'execution-delivery
                            (niwa-execution--record-token record))
                      workdir deliver
                      (lambda ()
                        (setf (niwa-execution--record-delivery-pending-p record) nil))
                      (and defer 0)))
          (funcall deliver))))))

(defun niwa-execution--publish-yielded-artifact (record)
  "Publish RECORD's spool without moving a file still being appended.

Foreground output stays under a hidden pending directory until the first
yield.  The initial bytes are copied into the retained directory, then future
process-filter appends use the published path."
  (let* ((path (niwa-execution--spool-path record))
         (private-directory (and path (file-name-directory path)))
         (private-name (and private-directory
                            (file-name-nondirectory
                             (directory-file-name private-directory)))))
    (when (and path
               (equal private-name ".niwa-pending-executions"))
      (let* ((public-directory
              (file-name-directory (directory-file-name private-directory)))
             (target (file-name-concat
                      public-directory (file-name-nondirectory path))))
        (condition-case nil
            (progn
              (make-directory public-directory t)
              (niwa-execution-process-relocate-spool
               (niwa-execution--record-child record) target))
          (error nil))))))

(defun niwa-execution--yield-managed (record)
  "Deliver RECORD's initial running observation and detach it from request."
  (unless (or (niwa-execution--record-finished-p record)
              (niwa-execution--record-yielded-p record))
    (when (or (eq (niwa-execution--record-kind record) 'monitor)
              (niwa-execution--record-service record)
              (niwa-execution-process-live-p
               (niwa-execution--record-child record)))
      (niwa-execution--publish-yielded-artifact record)
      (setf (niwa-execution--record-yielded-p record) t
            (niwa-execution--record-retained-p record) t)
      ;; The command now runs in the background, where observers list it.
      (niwa-execution--notify-state-change record)
      (niwa-execution--release-scheduler record)
      (niwa-execution--emit-event
       (niwa-execution--event record 'yield))
      (funcall (niwa-execution--record-callback record)
               (if (eq (niwa-execution--record-kind record) 'monitor)
                   ;; Monitor stdout belongs to its mailbox batches, including
                   ;; output that arrived during a synchronous remote startup.
                   (niwa-execution--range-observation record '(:output ""))
                 (niwa-execution--observation record))))))

(defun niwa-execution--arm-managed-timers (record)
  "Arm RECORD's progress and yield clocks."
  (when (and (not (eq (niwa-execution--record-kind record) 'monitor))
             niwa-execution-event-functions
             (niwa-execution--origin-tool-use-id
              (niwa-execution--record-origin record)))
    (setf (niwa-execution--record-progress-timer record)
          (run-at-time
           (max 0 niwa-execution-progress-delay)
           nil
           #'niwa-execution--emit-progress record)))
  (if (eq (niwa-execution--record-kind record) 'monitor)
      (niwa-execution--yield-managed record)
    (when-let* ((yield-time-ms
                (niwa-execution--record-yield-time-ms record)))
      (setf (niwa-execution--record-yield-timer record)
            (run-at-time
             (/ yield-time-ms 1000.0)
             nil #'niwa-execution--yield-managed record)))))

(defun niwa-execution--managed-process-terminal (record child result)
  "Settle RECORD from the exact current CHILD terminal RESULT."
  (when (and (eq child (niwa-execution--record-child record))
             (not (niwa-execution--record-finished-p record)))
    (let* ((missing-remote-group-p
            (and (niwa-execution--record-mutation-armed-p record)
                 (niwa-execution-process-launch-attempted-p child)
                 (file-remote-p (or (plist-get result :workdir) ""))
                 (not (plist-get result :group-id))))
           (error-data
            (or (plist-get result :error)
                (and missing-remote-group-p
                     '(error "Remote process-group identity was not received"))))
           (child-termination (plist-get result :termination))
           (unknown-p
            (and (niwa-execution--record-mutation-armed-p record)
                 (niwa-execution-process-launch-attempted-p child)
                 (or (eq child-termination 'unknown)
                     missing-remote-group-p
                     (and error-data (null child-termination)))))
           (termination
            (if unknown-p
                'unknown
              (or child-termination
                  (and error-data 'spawn-failed)))))
      (setf (niwa-execution--record-error-data record)
            error-data
            (niwa-execution--record-exit-code record)
            (plist-get result :exit-code)
            (niwa-execution--record-output-limit-p record)
            (plist-get result :output-limit-p)
            (niwa-execution--record-termination record)
            termination
            (niwa-execution--record-timed-out-p record)
            (plist-get result :timed-out-p))
      (when (eq 'unknown termination)
        (niwa-execution--mark-unknown
         record error-data result))
      ;; Only an end nobody requested may deliver later; a caller that
      ;; stopped or aborted the process waits for its settled outcome.
      (niwa-execution--finish-managed
       record (memq termination '(nil exited signaled timed-out))))))

(defun niwa-execution--launch-managed (record command)
  "Launch managed RECORD with raw COMMAND."
  (condition-case err
      (let* ((origin (niwa-execution--record-origin record))
             (session (niwa-execution--origin-session origin))
             (child (niwa-execution--record-child record)))
        (when (eq (niwa-execution--record-kind record) 'monitor)
          (setf (niwa-execution--record-expires-at record)
                (+ (float-time) (/ (niwa-execution--record-timeout-ms record)
                                   1000.0))))
        (niwa-execution-process-start
         child
         :name "niwa-bash" :command command
         :target (niwa-session-execution-target session)
         :coding 'utf-8-unix
         :separate-stderr (eq (niwa-execution--record-kind record) 'monitor)
         :timeout (when (eq (niwa-execution--record-kind record) 'monitor)
                    (/ (niwa-execution--record-timeout-ms record) 1000.0))
         :confined
         (eq 'bubblewrap
             (plist-get (niwa-execution--record-sandbox-facts record)
                        :sandbox)))
        (when (niwa-execution-process-live-p child)
          (apply #'niwa-execution--telemetry
                 record 'execution-started
                 (niwa-execution-telemetry-safe-facts
                  (niwa-execution--record-sandbox-facts record)))))
    (error
     (if (and (niwa-execution--record-mutation-armed-p record)
              (niwa-execution-process-launch-attempted-p
               (niwa-execution--record-child record)))
         (niwa-execution--mark-unknown record err)
       (setf (niwa-execution--record-error-data record) err
             (niwa-execution--record-termination record) 'spawn-failed))
     (setf (niwa-execution--record-exit-code record) -1)
     (niwa-execution--finish-managed record))))

(defun niwa-execution--start-admitted (record preparation)
  "Start scheduler-admitted RECORD from PREPARATION."
  (when (and (eq (plist-get preparation :state) 'unrestricted)
             (eq (plist-get (plist-get preparation :facts) :sandbox)
                 'unavailable))
    (plist-put
     preparation :facts
     (niwa-execution-telemetry-mark-direct-fallback
      (niwa-execution--origin-session
       (niwa-execution--record-origin record))
      (plist-get preparation :facts))))
  (apply #'niwa-execution--telemetry
         record 'execution-admitted
         :queue-duration-ms
         (round (* 1000.0
                   (- (float-time)
                      (niwa-execution--record-started-at record))))
         :preparation-state (plist-get preparation :state)
         (niwa-execution-telemetry-safe-facts
          (plist-get preparation :facts)))
  (when (eq (plist-get preparation :state) 'unrestricted)
    (apply #'niwa-execution--telemetry
           record 'execution-unrestricted
           :reason-class (plist-get (plist-get preparation :facts) :sandbox)
           (niwa-execution-telemetry-safe-facts
            (plist-get preparation :facts))))
  (pcase (plist-get preparation :state)
    ('refused
     (setf (niwa-execution--record-error-data record)
           (list 'error (plist-get preparation :error))
           (niwa-execution--record-exit-code record) -1
           (niwa-execution--record-sandbox-facts record)
           (plist-get preparation :facts)
           (niwa-execution--record-termination record) 'spawn-failed)
     (niwa-execution--finish-managed record))
    ((or 'unrestricted 'confined)
     (setf (niwa-execution--record-marker record)
           (plist-get preparation :marker)
           (niwa-execution--record-marker-buffer record)
           (unless (plist-get preparation :marker) :done)
           (niwa-execution--record-sandbox-facts record)
           (plist-get preparation :facts)
           (niwa-execution--record-sandbox-preparation record)
           preparation)
     (niwa-execution--arm-mutation record)
     (niwa-execution--launch-managed
      record (plist-get preparation :command))
     (unless (niwa-execution--record-finished-p record)
       (niwa-execution--arm-managed-timers record)))
    (_
     (niwa-execution--cleanup-record record)
     (error "Unknown sandbox preparation state: %s"
            (plist-get preparation :state)))))

(defun niwa-execution--owner-admissible-p (owner-context)
  "Return non-nil when OWNER-CONTEXT may still start queued execution work."
  (not (and owner-context
            (fboundp 'niwa-agent-invocation-p)
            (niwa-agent-invocation-p owner-context)
            (niwa-agent-invocation-runtime-settled-p owner-context))))

(defun niwa-execution--reject-owner-record (record lease)
  "Settle RECORD rejected from LEASE because its owner is terminal."
  (setf (niwa-execution--record-scheduler-lease record) lease
        (niwa-execution--record-error-data record)
        '(niwa-execution-error "Execution owner is terminal")
        (niwa-execution--record-exit-code record) -1
        (niwa-execution--record-termination record) 'owner-stopped)
  (niwa-execution--finish-managed record))

(defun niwa-execution--stop-process (record reason)
  "Stop RECORD's child with managed REASON."
  (unless (or (niwa-execution--record-finished-p record)
              (niwa-execution--record-stop-p record))
    (setf (niwa-execution--record-stop-p record) t
          (niwa-execution--record-termination record) reason)
    (cond
     ((niwa-execution--record-service record)
      (niwa-execution-service-stop (niwa-execution--record-service record)))
     ((niwa-execution--record-preparation-cancel record)
      (setf (niwa-execution--record-exit-code record) -1)
      (niwa-execution--finish-managed record))
     (t
      (niwa-execution-process-stop
       (niwa-execution--record-child record) reason)))))

(defun niwa-execution--abort-request-record (record)
  "Abort queued or foreground RECORD for its originating request."
  (unless (or (niwa-execution--record-finished-p record)
              (niwa-execution--record-yielded-p record))
    (if (niwa-execution-scheduler-cancel
         (niwa-execution--record-scheduler-lease record))
        (progn
          (setf (niwa-execution--record-exit-code record) -1
                (niwa-execution--record-termination record) 'aborted)
          (niwa-execution--finish-managed record))
      (niwa-execution--stop-process record 'aborted))))

(cl-defun niwa-execution-start-bash
    (callback &key session data-buffer owner owner-context request
              command workdir
              writable-roots temporary-root
              additional-permissions sandbox-permissions artifact-directory
              outcome-function result-function teardown-function
              read-only-p tool-args tool-use-id tty
              pipeline-marker pipeline-facts-function
              permission-approvals (kind 'bash) description (timeout-ms 300000)
              (yield-time-ms 10000))
  "Start managed COMMAND and call CALLBACK at terminal or yield.

SESSION, canonical OWNER, and OWNER-CONTEXT fix the control boundary.
OWNER-CONTEXT is the durable mailbox object captured at spawn.
REQUEST owns the foreground lifetime only.  Remaining confinement arguments
match the one-shot interface; TEMPORARY-ROOT is the writable temporary
directory that protected-path discovery skips.  ARTIFACT-DIRECTORY owns the
spool after yield.
OUTCOME-FUNCTION derives canonical outcome from exit code and termination.
RESULT-FUNCTION receives terminal child facts and returns :output and :outcome.
Its result is frozen before TEARDOWN-FUNCTION releases caller-owned resources.
PIPELINE-MARKER identifies a private final-pipeline status frame;
PIPELINE-FACTS-FUNCTION derives final pipeline facts from its exit codes.
READ-ONLY-P selects the overlapping reader lane; all other calls are exclusive.
DATA-BUFFER and TOOL-USE-ID identify the authoritative transcript row.
TOOL-ARGS correlates immutable events with the original call.
PERMISSION-APPROVALS is trusted display metadata snapshotted at admission,
not execution authority.
TTY non-nil explicitly allocates a terminal and retains writable stdin.
YIELD-TIME-MS may be nil only for trusted internal callers that must wait for
terminal settlement.  KIND `monitor' yields immediately, frames stdout lines,
and uses DESCRIPTION and TIMEOUT-MS for its root-owned event lifetime.
KIND `eval' uses the exclusive lane, without pipeline framing or input."
  (unless session
    (signal 'niwa-execution-error
            (list "Managed Bash requires an active session")))
  (when (eq kind 'monitor)
    (unless (equal (or owner "/root") "/root")
      (error "Monitor is available only to /root"))
    (when tty (error "Monitor does not support TTY execution"))
    (unless (and (stringp description) (not (string-empty-p (string-trim description))))
      (error "Monitor requires a non-empty description"))
    (unless (and (integerp timeout-ms) (<= 1000 timeout-ms 3600000))
      (error "Monitor timeout_ms must be between 1000 and 3600000")))
  (when (eq kind 'eval)
    (when tty (error "Fresh Eval does not support TTY execution"))
    (setq read-only-p nil pipeline-marker nil pipeline-facts-function nil))
  (when (and tty (eq system-type 'windows-nt))
    (signal 'niwa-execution-input-error
            (list "PTY execution is unavailable on Windows")))
  (let* ((state (niwa-execution--state-for-session session))
         (authority (niwa-execution--mutation-target session)))
    (unless read-only-p
      (niwa-session-artifacts-assert-new-mutation-authority authority))
    (when (and (not read-only-p)
               (niwa-execution-mutation-refused-p session))
      (signal
       'niwa-execution-error
       (list niwa-execution-mutation-blocked-message)))
    (when (>= (niwa-execution--managed-count state)
              niwa-execution-live-limit)
      (signal 'niwa-execution-limit
              (list "A session may have at most 64 live Bash processes")))
    (let* ((invocation (and (buffer-live-p data-buffer)
                            (buffer-local-value 'niwa--agent-invocation data-buffer)))
           (permission-mode (niwa-permission-mode-effective
                             session data-buffer nil request invocation))
           (raw-command (plist-get tool-args :command))
           (command-text (and (stringp raw-command) raw-command))
           (command-properties
            (and command-text
                 (niwa-execution-telemetry-command-properties
                  command-text)))
           (id (niwa-execution--next-id state))
           (telemetry-context
            (niwa-execution-telemetry-context-create
             :session session :execution-id id :tool-use-id tool-use-id
             :owner (or owner "/root") :invocation owner-context
             :pipeline-summary-cell niwa-execution-telemetry-summary-cell))
           (resource-command
            (and command-text
                 (listp command)
                 (niwa-execution-telemetry-prepare-resource-capture
                  telemetry-context command-text command)))
           (command (or resource-command command))
           (artifact-directory
            (or artifact-directory temporary-file-directory))
           (pending-artifact-directory
            (file-name-concat artifact-directory
                              ".niwa-pending-executions"))
           (_ (make-directory pending-artifact-directory t))
           (spool-path
            (make-temp-file
             (file-name-concat pending-artifact-directory "execution-")
             nil ".log"))
           (record
            (niwa-execution--record-create
             :callback callback :execution-id id
             :kind kind :description description :timeout-ms timeout-ms
             :marker-buffer nil :newline-count 0
             :origin
             (niwa-execution--origin-create
              :data-buffer data-buffer
              :owner (or owner "/root")
              :owner-context owner-context
              :session session
              :tool-args tool-args
              :tool-use-id tool-use-id)
             :outcome-function outcome-function
             :result-function result-function
             :teardown-function teardown-function
             :pipeline-marker pipeline-marker
             :pipeline-facts-function pipeline-facts-function
             :permission-approvals
             (niwa-execution--copy-value permission-approvals)
             :mutating-p (not read-only-p)
             :output-chars 0 :output-head "" :output-tail ""
             :read-offset 0
             :started-at (float-time) :token id
             :telemetry-context telemetry-context
             :tty-p (and tty t) :workdir workdir
             :yield-time-ms yield-time-ms)))
      (setf (niwa-execution--record-child record)
            (niwa-execution--create-child record spool-path))
      (puthash id record (niwa-execution--state-records state))
      (apply #'niwa-execution--telemetry
             record 'execution-enqueued
             :lane (if read-only-p 'read 'exclusive)
             :queue-depth (niwa-execution--managed-count state)
             :overlap-count
             (max 0 (1- (niwa-execution--managed-count state)))
             :command-hash (and command-text
                                (secure-hash 'sha256 command-text))
             :tty (and tty t)
             :yield-time-ms yield-time-ms
             (append
              (niwa-execution-telemetry-context-properties
               telemetry-context)
              command-properties))
      (niwa-execution--notify-state-change record)
      (let ((lease
             (niwa-execution-scheduler-submit
              (niwa-execution--state-scheduler state)
              (if read-only-p 'read 'exclusive)
              (lambda (admitted-lease)
                (setf (niwa-execution--record-scheduler-lease record)
                      admitted-lease)
                (condition-case err
                    (with-current-buffer (or data-buffer (current-buffer))
                      (let ((niwa--current-request request)
                            (niwa--agent-invocation invocation))
                        (unless (eq permission-mode
                                    (niwa-permission-mode-effective
                                     session data-buffer nil request invocation))
                          (signal 'niwa-execution-error
                                  '("Permission mode changed while execution was queued; submit the operation again")))
                        (let ((cancel
                               (niwa-execution--prepare
                                (lambda (preparation)
                                  (unless (niwa-execution--record-finished-p record)
                                    (setf (niwa-execution--record-preparation-cancel record) nil)
                                    (condition-case failure
                                        (progn
                                          (unless (niwa-execution--owner-admissible-p owner-context)
                                            (error "Execution owner is terminal"))
                                          (niwa-execution--start-admitted record preparation))
                                      (error
                                       (niwa-sandbox-cleanup preparation)
                                       (setf (niwa-execution--record-error-data record) failure
                                             (niwa-execution--record-exit-code record) -1
                                             (niwa-execution--record-termination record) 'spawn-failed)
                                       (niwa-execution--finish-managed record)))))
                                command workdir writable-roots additional-permissions
                                sandbox-permissions session owner temporary-root)))
                          (unless (or (niwa-execution--record-finished-p record)
                                      (niwa-execution-process-launch-attempted-p
                                       (niwa-execution--record-child record)))
                            (setf (niwa-execution--record-preparation-cancel record) cancel)))))
                  (error
                   (setf (niwa-execution--record-error-data record) err
                         (niwa-execution--record-exit-code record) -1
                         (niwa-execution--record-termination record)
                         'spawn-failed)
                   (niwa-execution--finish-managed record))))
              (lambda ()
                (and
                 (niwa-execution--owner-admissible-p owner-context)
                 (or read-only-p
                     (null
                      (niwa-execution-mutation-blocked-p session)))))
              (lambda (rejected-lease)
                (if (and (not read-only-p)
                         (niwa-execution-mutation-blocked-p session))
                    (progn
                      (setf
                       (niwa-execution--record-scheduler-lease record)
                       rejected-lease
                       (niwa-execution--record-error-data record)
                       (list 'niwa-execution-error
                             niwa-execution-mutation-blocked-message)
                       (niwa-execution--record-exit-code record) -1
                       (niwa-execution--record-termination record)
                       'unknown)
                      (niwa-execution--finish-managed record))
                  (niwa-execution--reject-owner-record
                   record rejected-lease)))
              (niwa-diagnostic-scope session))))
        (unless (or (niwa-execution--record-finished-p record)
                    (niwa-execution--record-scheduler-lease record))
          (setf (niwa-execution--record-scheduler-lease record) lease)))
      (when (and request
                 (not (niwa-execution--record-finished-p record))
                 (not (niwa-execution--record-yielded-p record)))
        (niwa-request-push-canceller
         request
         (lambda () (niwa-execution--abort-request-record record))))
      nil)))

(defun niwa-execution--owned-yielded-record (session owner execution-id)
  "Return yielded EXECUTION-ID owned by OWNER in SESSION, or signal."
  (let* ((state (niwa-execution--state-for-session session))
         (record (gethash execution-id
                          (niwa-execution--state-records state))))
    (unless (and record
                 (niwa-execution--record-execution-id record)
                 (niwa-execution--record-yielded-p record)
                 (equal (niwa-execution--origin-owner
                         (niwa-execution--record-origin record))
                        (or owner "/root")))
      (signal 'niwa-execution-not-found
              (list "No yielded execution with that id")))
    record))

(cl-defun niwa-execution-observe
    (session owner execution-id callback &key chars (wait-ms 0) request)
  "Observe unread output from owner-scoped yielded EXECUTION-ID.

Ordinary non-empty CHARS require a PTY; a single Ctrl-C character interrupts
either process mode.  CALLBACK receives immediately for terminal state or
after WAIT-MS while the process remains live."
  (let* ((record
          (niwa-execution--owned-yielded-record
           session owner execution-id))
         (input-p (and (stringp chars) (not (string-empty-p chars)))))
    (unless (or (null chars) (stringp chars))
      (signal 'niwa-execution-input-error
              (list "Execution input must be a string")))
    (when (and input-p (eq (niwa-execution--record-kind record) 'eval))
      (signal 'niwa-execution-input-error
              (list "Eval accepts only empty polling input; use StopExecution to stop it")))
    (when (and input-p (niwa-execution--record-observer record))
      (niwa-execution--flush-observer record))
    (when (niwa-execution--record-observer record)
      (signal 'niwa-execution-error
              (list "An observation is already waiting")))
    (when input-p
      (cond
       ((equal chars (string 3))
        (when (eq system-type 'windows-nt)
          (signal
           'niwa-execution-input-error
           (list "Interrupting managed execution is unavailable on Windows")))
        (unless (or (niwa-execution--record-finished-p record)
                    (niwa-execution--record-stop-p record)
                    (not (niwa-execution-process-live-p
                          (niwa-execution--record-child record))))
          (niwa-execution-process-interrupt
           (niwa-execution--record-child record))))
       ((not (niwa-execution--record-tty-p record))
        (signal 'niwa-execution-input-error
                (list "Pipe-mode Bash stdin is closed")))
       ((or (niwa-execution--record-finished-p record)
            (niwa-execution--record-stop-p record)
            (not (niwa-execution-process-live-p
                  (niwa-execution--record-child record))))
        (signal 'niwa-execution-input-error
                (list "Execution is no longer running")))
       (t
        (condition-case nil
            (niwa-execution-process-write
             (niwa-execution--record-child record) chars)
          (error
           (signal 'niwa-execution-input-error
                   (list "Execution is no longer running")))))))
    (cond
     ((niwa-execution--record-finished-p record)
      (funcall callback (niwa-execution--observation record t)))
     ((<= wait-ms 0)
      (funcall callback (niwa-execution--observation record)))
     (t
      (setf (niwa-execution--record-observer record) callback
            (niwa-execution--record-observer-timer record)
            (run-at-time (/ wait-ms 1000.0) nil
                         #'niwa-execution--poll-expired record))
      (when request
        (niwa-request-push-canceller
         request
         (lambda ()
           (niwa-execution--cancel-observer record callback))))))
    nil))

(defun niwa-execution-list (session owner)
  "Return immutable fact snapshots for OWNER's yielded SESSION executions."
  (let ((state (niwa-execution--state-for-session session))
        facts)
    (maphash
     (lambda (_key record)
       (when (and (niwa-execution--record-execution-id record)
                  (niwa-execution--record-yielded-p record)
                  (not (niwa-execution--record-finished-p record))
                  (equal (niwa-execution--origin-owner
                          (niwa-execution--record-origin record))
                         (or owner "/root")))
         (push (niwa-execution--facts record) facts)))
     (niwa-execution--state-records state))
    (nreverse facts)))

(defun niwa-execution--user-snapshot (record)
  "Return an immutable user-authority snapshot for live RECORD."
  (let* ((origin (niwa-execution--record-origin record))
         (sandbox-facts (niwa-execution--record-sandbox-facts record))
         (facts
          (plist-put
           (niwa-execution--facts record)
           :execution-id
           (niwa-execution--record-execution-id record))))
    (niwa-execution--copy-value
     (append
      facts
      (list :owner (niwa-execution--origin-owner origin)
            :tool-use-id (niwa-execution--origin-tool-use-id origin)
            :permission-approvals (niwa-execution--record-permission-approvals record)
            :yielded (and (niwa-execution--record-yielded-p record) t)
            :started-at (niwa-execution--record-started-at record)
            :output-tail
            (or (niwa-execution--record-output-tail record) "")
            :output-preview-truncated-p
            (> (or (niwa-execution--record-output-chars record) 0)
               niwa-execution-inline-output-limit)
            :artifact-path (niwa-execution--spool-path record)
            :sandbox-state (or (plist-get sandbox-facts :sandbox) 'pending)
            :sandbox-facts sandbox-facts)))))

(defun niwa-execution-list-user (session)
  "Return immutable snapshots for every live execution in SESSION."
  (when-let* ((state (and session
                          (niwa-session-execution-state session))))
    (let (snapshots)
      (maphash
       (lambda (_key record)
         (when (niwa-execution--managed-live-p record)
           (push (niwa-execution--user-snapshot record) snapshots)))
       (niwa-execution--state-records state))
      (sort snapshots
            (lambda (left right)
              (< (plist-get left :started-at)
                 (plist-get right :started-at)))))))

(defun niwa-execution-count-user (session)
  "Return the number of live executions in SESSION."
  (if-let* ((state (and session
                        (niwa-session-execution-state session))))
      (niwa-execution--managed-count state)
    0))

(defun niwa-execution--user-live-record (session execution-id)
  "Return live EXECUTION-ID in SESSION with user authority, or signal."
  (let* ((state (and session
                     (niwa-session-execution-state session)))
         (record (and state
                      (gethash execution-id
                               (niwa-execution--state-records state)))))
    (unless (and record
                 (not (niwa-execution--record-finished-p record)))
      (signal 'niwa-execution-not-found
              (list "No live execution with that id")))
    record))

(defun niwa-execution-write-user (session execution-id chars)
  "Send CHARS to PTY EXECUTION-ID in SESSION with user authority."
  (unless (and (stringp chars) (not (string-empty-p chars)))
    (signal 'niwa-execution-input-error
            (list "Execution input must be a non-empty string")))
  (let ((record (niwa-execution--user-live-record session execution-id)))
    (unless (niwa-execution--record-tty-p record)
      (signal 'niwa-execution-input-error
              (list "Pipe-mode Bash stdin is closed")))
    (unless (niwa-execution-process-live-p
             (niwa-execution--record-child record))
      (signal 'niwa-execution-input-error
              (list "Execution is no longer running")))
    (niwa-execution-process-write
     (niwa-execution--record-child record) chars)
    t))

(defun niwa-execution-interrupt-user (session execution-id)
  "Interrupt live EXECUTION-ID in SESSION with user authority."
  (when (eq system-type 'windows-nt)
    (signal 'niwa-execution-input-error
            (list "Interrupting managed execution is unavailable on Windows")))
  (let ((record
         (niwa-execution--user-live-record session execution-id)))
    (when (niwa-execution--record-service-call-p record)
      (signal 'niwa-execution-input-error
              '("Sandbox Eval cannot be interrupted in place; stop it instead")))
    (unless (niwa-execution-process-launch-attempted-p
             (niwa-execution--record-child record))
      (signal 'niwa-execution-input-error
              (list "Execution has not started")))
    (unless (niwa-execution-process-live-p
             (niwa-execution--record-child record))
      (signal 'niwa-execution-not-found
              (list "Execution is no longer running")))
    (niwa-execution-process-interrupt
     (niwa-execution--record-child record))
    t))

(defun niwa-execution--state-record-list (state)
  "Return a snapshot list of every private record in STATE."
  (let (records)
    (when state
      (maphash (lambda (_key record) (push record records))
               (niwa-execution--state-records state)))
    records))

(defun niwa-execution--idle-service-p (record)
  "Return non-nil when RECORD is a service with no call in flight."
  (and (eq (niwa-execution--record-kind record) 'service)
       (not (niwa-execution--record-service-active-p record))))

(defun niwa-execution-owner-live-p (session owner)
  "Return non-nil when OWNER has an unsettled execution in SESSION."
  (when-let* ((state (and session
                          (niwa-session-execution-state session))))
    (let (live-p)
      (dolist (record (niwa-execution--state-record-list state))
         (when (and (not (niwa-execution--idle-service-p record))
                    (not (niwa-execution--record-delivery-state record))
                    (equal (niwa-execution--origin-owner
                            (niwa-execution--record-origin record))
                           owner))
           (setq live-p t)))
      live-p)))

(defun niwa-execution-session-live-p (session)
  "Return non-nil while SESSION owns a process or pending result delivery.
An unclaimed result after a failed delivery remains owner-pollable, but is
not active work and cannot keep session transfer or history changes blocked."
  (when-let* ((state (and session
                          (niwa-session-execution-state session))))
    (let (live-p)
      (dolist (record (niwa-execution--state-record-list state))
        (when (and (not (niwa-execution--idle-service-p record))
                   (or (not (niwa-execution--record-finished-p record))
                       (niwa-execution--record-delivery-pending-p record)))
          (setq live-p t)))
      live-p)))

(defun niwa-execution-session-live-spools (session)
  "Return spool paths still written by SESSION's unfinished executions."
  (when-let* ((state (and session (niwa-session-execution-state session))))
    (cl-loop for record in (niwa-execution--state-record-list state)
             unless (niwa-execution--record-finished-p record)
             when (niwa-execution--spool-path record)
             collect (expand-file-name (niwa-execution--spool-path record)))))

(defun niwa-execution-working-p ()
  "Return non-nil while any session's managed execution is working.
A monitor waiting for events is live but not working."
  (catch 'working
    (maphash
     (lambda (session _present)
       (when-let* ((state (niwa-session-execution-state session)))
         (maphash (lambda (_id record)
                    (when (and (not (eq (niwa-execution--record-kind record)
                                        'monitor))
                               (or (niwa-execution--managed-live-p record)
                                   (and (eq (niwa-execution--record-kind record) 'service)
                                        (niwa-execution--record-service-active-p record)
                                        (not (niwa-execution--record-finished-p record)))))
                      (throw 'working t)))
                  (niwa-execution--state-records state))))
     niwa-execution--sessions)
    nil))

(defun niwa-execution--teardown-record (record)
  "Release RECORD's caller-owned resources exactly once."
  (when-let* ((function (niwa-execution--record-teardown-function record)))
    (setf (niwa-execution--record-teardown-function record) nil)
    (ignore-errors (funcall function))))

(defun niwa-execution--finalize-discarded-record
    (record managed-live-p)
  "Finish and remove discarded RECORD.
MANAGED-LIVE-P records whether RECORD was user-visible before teardown."
  (unless (niwa-execution--record-finished-p record)
    (if managed-live-p
        (niwa-execution--finish-managed record)
      (setf (niwa-execution--record-finished-p record) t)
      (when-let* ((preparation
                   (niwa-execution--record-sandbox-preparation record)))
        (niwa-sandbox-cleanup preparation))))
  (niwa-execution--release-scheduler record)
  (niwa-execution--cleanup-record record)
  (niwa-execution--teardown-record record))

(defun niwa-execution--discard-record (record reason)
  "Kill and forget RECORD because of lifecycle REASON.
A finished record whose mailbox delivery still awaits its acknowledgement
survives an owner stop: the acknowledgement retires it on success and a
failure leaves its output unread for a poll or retry.  Ending the session
instead discards it, and the late acknowledgement then finds it gone."
  (unless (and (eq reason 'owner-stopped)
               (niwa-execution--record-finished-p record)
               (niwa-execution--mailbox-pending-p record))
    (when (niwa-execution--mailbox-pending-p record)
      (setf (niwa-execution--record-delivery-state record) 'discarded))
    (let ((managed-live-p (niwa-execution--managed-live-p record)))
      (when (and (niwa-execution--record-execution-id record)
		 (niwa-execution--record-finished-p record)
		 (not (niwa-execution--record-delivery-state record)))
	;; Settlement and delivery are separate event-loop turns.  Teardown
	;; can arrive between them: a foreground owner-stop still owes its
	;; caller the outcome, but session teardown and yielded work discard it.
	(niwa-transport-cancel-pending
	 (list 'execution-delivery (niwa-execution--record-token record)))
	(if (and (eq reason 'owner-stopped)
		 (not (niwa-execution--record-yielded-p record)))
            (funcall (niwa-execution--record-callback record)
                     (niwa-execution--observation record t))
          (setf (niwa-execution--record-delivery-state record) 'discarded)))
      (unless (niwa-execution--record-finished-p record)
	(setf (niwa-execution--record-termination record) reason)
	(when managed-live-p
          (setf (niwa-execution--record-error-data record)
		'(niwa-execution-error "Execution owner was stopped")
		(niwa-execution--record-exit-code record) -1)
          (when (or (niwa-execution--record-yielded-p record)
                    (eq reason 'session-ended))
            (setf (niwa-execution--record-delivery-state record) 'discarded)))
	(when-let* ((lease
                     (niwa-execution--record-scheduler-lease record)))
          (niwa-execution-scheduler-cancel lease))
	(when-let* ((service (niwa-execution--record-service record)))
          (niwa-execution-service-stop service))
	(if (when-let* ((child (niwa-execution--record-child record)))
              (niwa-execution-process-launch-attempted-p child))
            (if managed-live-p
		(progn
                  (niwa-execution--stop-process record reason)
                  (while (and (not (niwa-execution--record-finished-p record))
                              (not (niwa-execution-process-terminal-p
                                    (niwa-execution--record-child record))))
                    (accept-process-output nil 0.01)))
              (setf (niwa-execution--record-finished-p record) t
                    (niwa-execution--record-stop-p record) t)
              (niwa-execution-process-stop
               (niwa-execution--record-child record) reason)
              (while (not (niwa-execution-process-terminal-p
                           (niwa-execution--record-child record)))
		(accept-process-output nil 0.01)))
          (setf (niwa-execution--record-stop-p record) t)))
      (niwa-execution--finalize-discarded-record record managed-live-p))))

(defun niwa-execution-stop-owner (session owner)
  "Discard every execution record belonging to OWNER in SESSION.
Return the number of executions selected."
  (let ((state (and session (niwa-session-execution-state session)))
        records)
    (when state
      (dolist (record (niwa-execution--state-record-list state))
         (when (and (not (niwa-execution--idle-service-p record))
                    (equal owner
                           (niwa-execution--origin-owner
                            (niwa-execution--record-origin record))))
           (push record records)))
      (dolist (record records)
        (niwa-execution--discard-record record 'owner-stopped)))
    (length records)))

(defun niwa-execution-teardown-session (session)
  "Discard all child records owned by SESSION.
Remote armed mutations wait through their bounded KILL proof.
Return the number selected, including queued one-shot helpers."
  (let ((state (and session (niwa-session-execution-state session)))
        records)
    (when state
      (setq records (niwa-execution--state-record-list state))
      (dolist (record records)
        (niwa-execution--discard-record record 'session-ended)))
    (length records)))

(defun niwa-execution-teardown-all ()
  "Synchronously discard all session and orphan child records."
  (let ((count 0)
        sessions)
    (maphash (lambda (session _present) (push session sessions))
             niwa-execution--sessions)
    (dolist (session sessions)
      (cl-incf count (niwa-execution-teardown-session session)))
    (when niwa-execution--orphan-state
      (let ((records
             (niwa-execution--state-record-list
              niwa-execution--orphan-state)))
        (dolist (record records)
          (niwa-execution--discard-record record 'session-ended))
        (cl-incf count (length records))))
    count))

(defun niwa-execution-relocate-artifacts (session old-root new-root)
  "Retarget SESSION execution artifacts moved from OLD-ROOT to NEW-ROOT.
Return the number of live or retained records updated."
  (let ((state (and session (niwa-session-execution-state session)))
        (target (and session (niwa-session-execution-target session)))
        (old-prefix (file-name-as-directory (expand-file-name old-root)))
        (new-prefix (file-name-as-directory (expand-file-name new-root)))
        (count 0))
    (let ((old-native-prefix
           (and target
                (niwa-execution-target-remote-p target)
                (file-name-as-directory
                 (niwa-execution-target-native-path target old-prefix))))
          (new-native-prefix
           (and target
                (niwa-execution-target-remote-p target)
                (file-name-as-directory
                 (niwa-execution-target-native-path target new-prefix)))))
      (when state
        (maphash
         (lambda (_key record)
           (let* ((spool-path
                   (niwa-execution--spool-path record))
                  (expanded-spool (and spool-path
                                       (expand-file-name spool-path)))
                  (new-spool
                   (and expanded-spool
                        (string-prefix-p old-prefix expanded-spool)
                        (concat new-prefix
                                (substring expanded-spool
                                           (length old-prefix)))))
                  (recoverable-path
                   (niwa-execution--record-recoverable-output-path record))
                  (new-recoverable
                   (and recoverable-path old-native-prefix
                        (string-prefix-p old-native-prefix recoverable-path)
                        (concat new-native-prefix
                                (substring recoverable-path
                                           (length old-native-prefix))))))
             (when (or new-spool new-recoverable)
               (when new-spool
                 (niwa-execution-process-rebind-spool
                  (niwa-execution--record-child record) new-spool))
               (when new-recoverable
                 (setf (niwa-execution--record-recoverable-output-path record)
                       new-recoverable))
               (cl-incf count))))
         (niwa-execution--state-records state)))
      count)))

(defun niwa-execution-stop (session owner execution-id callback)
  "Stop owner-scoped yielded EXECUTION-ID and call CALLBACK at settlement."
  (let ((record
         (niwa-execution--owned-yielded-record
          session owner execution-id)))
    (if (niwa-execution--record-finished-p record)
        (funcall callback (niwa-execution--observation record t))
      (niwa-execution--flush-observer record)
      (niwa-execution-observe
       session owner execution-id callback :wait-ms 300000)
      (niwa-execution--stop-process record 'stopped))
    nil))

(defun niwa-execution--stop-user-record (record)
  "Stop live RECORD with user delivery authority."
  (niwa-execution--flush-observer record)
  (if (and (niwa-execution--record-scheduler-lease record)
           (niwa-execution-scheduler-cancel
            (niwa-execution--record-scheduler-lease record)))
      (progn
        (setf (niwa-execution--record-exit-code record) -1
              (niwa-execution--record-stop-p record) t
              (niwa-execution--record-termination record) 'stopped)
        (niwa-execution--finish-managed record))
    (niwa-execution--stop-process record 'stopped))
  t)

(defun niwa-execution-stop-user (session execution-id)
  "Stop live EXECUTION-ID in SESSION with user delivery authority.

Unlike the owner-scoped model tool, user control may target every owner and
foreground state.  Yielded terminal output still goes to its owner mailbox."
  (niwa-execution--stop-user-record
   (niwa-execution--user-live-record session execution-id)))

(defun niwa-execution-stop-all-user (session)
  "Stop every live execution in SESSION and return the number selected."
  (let* ((state (and session (niwa-session-execution-state session)))
         (records
          (cl-delete-if
           #'niwa-execution--record-finished-p
           (niwa-execution--state-record-list state))))
    (dolist (record records)
      (if (eq (niwa-execution--record-kind record) 'service)
          (progn
            (when-let* ((call (niwa-execution--record-active-call record)))
              (niwa-execution--stop-user-record call))
            (niwa-execution-service-stop record))
        (niwa-execution--stop-user-record record)))
    (length records)))


;;
;;; Confined services and one-shot interface

(cl-defun niwa-execution-start-service-call
    (callback start &key session owner owner-context data-buffer request tool-args
              tool-use-id permission-approvals workdir (yield-time-ms 10000))
  "Admit a childless Eval call and invoke START with its opaque handle.
CALLBACK receives ordinary execution observations.  START binds a service
and eventually settles the handle.  SESSION, OWNER and OWNER-CONTEXT fix
authority; DATA-BUFFER, TOOL-ARGS, TOOL-USE-ID and PERMISSION-APPROVALS fix
presentation.  REQUEST owns foreground cancellation.  WORKDIR and
YIELD-TIME-MS have the managed execution meaning."
  (unless session (error "Sandbox Eval requires an active session"))
  (let ((state (niwa-execution--state-for-session session)))
    (dolist (record (niwa-execution--state-record-list state))
      (when (and (niwa-execution--record-service-call-p record)
                 (not (niwa-execution--record-finished-p record))
                 (equal owner (niwa-execution--origin-owner
                               (niwa-execution--record-origin record))))
        (if (niwa-execution--record-yielded-p record)
            (error "Sandbox is busy; running execution_id: %s"
                   (niwa-execution--record-execution-id record))
          (error "Sandbox is busy; its current call is still starting or running in the foreground"))))
    (niwa-session-artifacts-assert-new-mutation-authority
     (niwa-execution--mutation-target session))
    (when (niwa-execution-mutation-refused-p session)
      (error "%s" niwa-execution-mutation-blocked-message))
    (when (>= (niwa-execution--managed-count state) niwa-execution-live-limit)
      (signal 'niwa-execution-limit '("Session execution limit reached")))
    (let* ((id (niwa-execution--next-id state))
           (record (niwa-execution--record-create
                    :kind 'eval :service-call-p t :mutating-p t
                    :execution-id id :token id :callback callback
                    :workdir workdir :started-at (float-time)
                    :telemetry-context
                    (niwa-execution-telemetry-context-create
                     :session session :execution-id id :tool-use-id tool-use-id
                     :owner owner :invocation owner-context)
                    :output-chars 0 :output-head "" :output-tail "" :read-offset 0
                    :yield-time-ms yield-time-ms
                    :permission-approvals (niwa-execution--copy-value permission-approvals)
                    :origin (niwa-execution--origin-create
                             :session session :owner owner :owner-context owner-context
                             :data-buffer data-buffer :tool-args tool-args :tool-use-id tool-use-id)))
           (invocation (and (buffer-live-p data-buffer)
                            (buffer-local-value 'niwa--agent-invocation data-buffer)))
           (mode (niwa-permission-mode-effective session data-buffer nil request invocation)))
      (puthash id record (niwa-execution--state-records state))
      (niwa-execution--notify-state-change record)
      (let ((lease
             (niwa-execution-scheduler-submit
              (niwa-execution--state-scheduler state) 'exclusive
              (lambda (lease)
                (setf (niwa-execution--record-scheduler-lease record) lease)
                (condition-case err
                    (progn
                      (unless (eq mode (niwa-permission-mode-effective
                                        session data-buffer nil request invocation))
                        (error "Permission mode changed while Eval was queued; submit again"))
                      (with-current-buffer (or data-buffer (current-buffer))
                        (let ((niwa--current-request request)
                              (niwa--agent-invocation invocation))
                          (funcall start record))))
                  (error
                   (niwa-execution-service-call-settle
                    record (list :output (error-message-string err) :outcome 'failure)))))
              (lambda () (and (niwa-execution--owner-admissible-p owner-context)
                              (not (niwa-execution-mutation-blocked-p session))))
              (lambda (lease) (niwa-execution--reject-owner-record record lease))
              (niwa-diagnostic-scope session))))
        (unless (or (niwa-execution--record-finished-p record)
                    (niwa-execution--record-scheduler-lease record))
          (setf (niwa-execution--record-scheduler-lease record) lease)))
      (when request
        (niwa-request-push-canceller
         request (lambda () (niwa-execution--abort-request-record record))))
      record)))

(defun niwa-execution-service-bind-call (service call)
  "Bind opaque SERVICE to CALL, refusing any other in-flight call."
  (unless (and (niwa-execution-service-live-p service)
               (not (niwa-execution--record-finished-p call)))
    (error "Sandbox call has ended"))
  (when-let* ((active (niwa-execution--record-active-call service)))
    (unless (eq active call) (error "Sandbox already has a call")))
  (setf (niwa-execution--record-active-call service) call
        (niwa-execution--record-service call) service
        (niwa-execution--record-sandbox-facts call)
        (plist-get (niwa-execution--record-sandbox-preparation service) :facts))
  ;; Preparation still belongs to the foreground request.  Start the yield
  ;; clock only at READY, just as fresh Eval does after launching its child.
  (when (and (niwa-execution-process-launch-attempted-p
              (niwa-execution--record-child service))
             (not (niwa-execution--record-yield-timer call)))
    (niwa-execution--arm-managed-timers call)))

(defun niwa-execution-service-call-settle (call result)
  "Settle opaque CALL once with formatted RESULT (:output and :outcome)."
  (unless (niwa-execution--record-finished-p call)
    (when-let* ((service (niwa-execution--record-service call)))
      (when (eq call (niwa-execution--record-active-call service))
        (setf (niwa-execution--record-active-call service) nil)))
    (setf (niwa-execution--record-terminal-result call) result
          (niwa-execution--record-exit-code call)
          (if (eq (plist-get result :outcome) 'success) 0 -1)
          (niwa-execution--record-termination call)
          (or (niwa-execution--record-termination call) 'exited))
    (niwa-execution--finish-managed call)))

(cl-defun niwa-execution-start-service
    (ready receive closed &key session owner command workdir writable-roots
           temporary-root)
  "Start an always-confined session service and return its opaque handle.
READY receives the handle after launch; RECEIVE receives (HANDLE CHUNK).
CLOSED receives terminal facts exactly once, including preparation failure.
SESSION and OWNER fix its lifetime.  COMMAND, WORKDIR, WRITABLE-ROOTS and
TEMPORARY-ROOT describe its default confinement, without remembered grants.
The caller bounds protocol output; the service has no spool or periodic watch.
Preparation counts as active work until the caller settles its first call."
  (unless session (error "A service requires an active session"))
  (when (or (file-remote-p workdir)
            (niwa-execution-target-remote-p
             (niwa-session-execution-target session)))
    (error "Sandbox services require a local target"))
  (niwa-session-artifacts-assert-new-mutation-authority
   (niwa-execution--mutation-target session))
  (let* ((record (niwa-execution--record-create
                  :kind 'service :service-active-p t :workdir workdir
                  :token (gensym "service-")
                  :origin (niwa-execution--origin-create
                           :session session :owner owner
                           :data-buffer (current-buffer))))
         (state (niwa-execution--state-for-session session)))
    (setf
     (niwa-execution--record-teardown-function record)
     (lambda ()
       (niwa-sandbox-cleanup
        (niwa-execution--record-sandbox-preparation record))
       (niwa-execution--notify-state-change record)
       (when-let* ((call (niwa-execution--record-active-call record)))
         (unless (niwa-execution--record-termination call)
           (setf (niwa-execution--record-termination call)
                 (niwa-execution--record-termination record))))
       (unwind-protect
           (funcall closed
                    (list :error (niwa-execution--record-error-data record)
                          :termination (niwa-execution--record-termination record)))
         (when-let* ((call (niwa-execution--record-active-call record)))
           (niwa-execution-service-call-settle
            call (list :output
                       (concat "Sandbox ended."
                               (when (niwa-execution-process-launch-attempted-p
                                      (niwa-execution--record-child record))
                                 " Earlier definitions are gone."))
                       :outcome 'failure)))))
     (niwa-execution--record-child record)
     (niwa-execution-process-create
      :service t :workdir workdir
      :diagnostic-scope (niwa-diagnostic-scope session)
      :output-function (lambda (_child chunk) (funcall receive record chunk))
      :terminal-function
      (lambda (_child result)
        (unless (niwa-execution--record-finished-p record)
          (setf (niwa-execution--record-finished-p record) t
                (niwa-execution--record-error-data record) (plist-get result :error)
                (niwa-execution--record-termination record) (plist-get result :termination))
          (niwa-execution--cleanup-record record)
          (niwa-execution--teardown-record record)))))
    (puthash (niwa-execution--record-token record) record
             (niwa-execution--state-records state))
    (niwa-execution--notify-state-change record)
    (condition-case err
        (let ((cancel
               (niwa-execution--prepare
                (lambda (preparation)
                  (if (niwa-execution--record-finished-p record)
                      (niwa-sandbox-cleanup preparation)
                    (setf (niwa-execution--record-preparation-cancel record) nil
                          (niwa-execution--record-sandbox-preparation record) preparation)
                    (if (not (eq (plist-get preparation :state) 'confined))
			(progn
                          (setf (niwa-execution--record-error-data record)
				(list 'error (format "Sandbox requires confinement: %s. Use fresh Eval instead."
                                                     (plist-get preparation :error))))
                          (niwa-execution-service-stop record))
                      (when (niwa-execution-process-start
                             (niwa-execution--record-child record)
                             :name "niwa-service" :command (plist-get preparation :command)
                             :coding 'utf-8-unix :confined t)
			(funcall ready record)))))
                command workdir writable-roots nil nil session owner
                temporary-root (lambda () (niwa-execution-service-stop record)) nil t)))
          (cond
           ((niwa-execution--record-finished-p record)
            (when cancel (funcall cancel)))
           ((not (niwa-execution-process-launch-attempted-p
                  (niwa-execution--record-child record)))
            (setf (niwa-execution--record-preparation-cancel record) cancel))))
      (error
       (setf (niwa-execution--record-error-data record) err)
       (niwa-execution-service-stop record)))
    record))

(defun niwa-execution-service-live-p (service)
  "Return whether opaque SERVICE is preparing or owns a live child."
  (and service (not (niwa-execution--record-finished-p service))))

(defun niwa-execution-service-set-active (service active)
  "Mark opaque SERVICE's foreground call ACTIVE, or settled when nil."
  (when active
    (unless (niwa-execution-service-live-p service) (error "Service has ended"))
    (niwa-session-artifacts-assert-new-mutation-authority
     (niwa-execution--mutation-target
      (niwa-execution--origin-session (niwa-execution--record-origin service)))))
  (setf (niwa-execution--record-service-active-p service) active)
  (niwa-execution--notify-state-change service))

(defun niwa-execution-service-write (service text)
  "Send protocol TEXT to opaque SERVICE's writable pipe."
  (unless (and (niwa-execution-service-live-p service)
               (niwa-execution-process-write
                (niwa-execution--record-child service) text))
    (error "Service pipe is closed")))

(defun niwa-execution-service-stop (service)
  "Stop opaque SERVICE, including its process group and pending preparation."
  (when (niwa-execution-service-live-p service)
    (niwa-execution--discard-record service 'service-stopped)))

(defun niwa-execution-stop-services (session)
  "Stop all services in SESSION before changing its history or control."
  (dolist (record (niwa-execution--state-record-list
                   (niwa-session-execution-state session)))
    (when (eq (niwa-execution--record-kind record) 'service)
      (niwa-execution-service-stop record))))

(cl-defun niwa-execution-start-one-shot
    (callback &key name command workdir writable-roots
              temporary-root timeout additional-permissions sandbox-permissions
              session owner teardown-function settlement)
  "Start one confined COMMAND and call CALLBACK with terminal facts.
Return an idempotent cancellation function covering discovery and execution.
NAME, WORKDIR, WRITABLE-ROOTS, TEMPORARY-ROOT, TIMEOUT, ADDITIONAL-PERMISSIONS,
SANDBOX-PERMISSIONS, SESSION and OWNER describe the authorized child.
TEARDOWN-FUNCTION releases caller resources on lifecycle destruction.
SETTLEMENT permits terminal snapshot work after producer cancellation, without
relaxing request identity, owner, target or policy validation."
  (let (preparation-cancel child-cancel settled preparing)
    (setq preparing t)
    (cl-labels ((discard ()
                  (unless settled
                    (setq settled t)
                    (when teardown-function (funcall teardown-function)))))
      (setq preparation-cancel
            (niwa-execution--prepare
             (lambda (preparation)
               (unless settled
                 (setq preparing nil
                       child-cancel
                       (niwa-execution--start-prepared-one-shot
                              (lambda (result)
                                (unless settled
                                  (setq settled t)
                                  (funcall callback result)))
                              preparation :name name :command command
                              :workdir workdir :timeout timeout :session session
                              :owner owner :teardown-function #'discard))))
             command workdir writable-roots additional-permissions
             sandbox-permissions session owner temporary-root #'discard settlement))
      (lambda ()
        (unless settled
          (if preparing
              (progn
                (setq settled t)
                (when preparation-cancel (funcall preparation-cancel))
                (funcall callback
                         '(:exit-code -1 :output "" :output-bytes 0
                           :termination aborted :timed-out-p nil
                           :output-limit-p nil :wall-time-seconds 0.0)))
            (when child-cancel (funcall child-cancel))))))))

(cl-defun niwa-execution--start-prepared-one-shot
    (callback preparation &key name command workdir timeout session owner
              teardown-function)
  "Launch PREPARATION and deliver terminal facts through CALLBACK.
NAME, COMMAND, WORKDIR, TIMEOUT, SESSION and OWNER describe the child and
its telemetry.  TEARDOWN-FUNCTION handles owner destruction without delivery."
  (let* ((invocation
          (and (boundp 'niwa--agent-invocation)
               niwa--agent-invocation))
         (telemetry-context
          (niwa-execution-telemetry-context-create
           :session session :owner owner :invocation invocation
           :pipeline-summary-cell niwa-execution-telemetry-summary-cell
           :span-event 'child-process
           :span-properties
           (list :name name :owner owner
                 :command-hash
                 (secure-hash 'sha256 (format "%S" command)))))
         (attempt-recorded-p nil)
         (started-p nil)
         child
         (current-facts nil)
         (_
          (when (and (eq (plist-get preparation :state) 'unrestricted)
                     (eq (plist-get (plist-get preparation :facts) :sandbox)
                         'unavailable))
            (plist-put
             preparation :facts
             (niwa-execution-telemetry-mark-direct-fallback
              session (plist-get preparation :facts))))))
    (cl-labels
        ((record-attempt ()
           (unless attempt-recorded-p
             (setq attempt-recorded-p t)
             (niwa-execution-telemetry-record-sandbox-attempt
              telemetry-context current-facts started-p
              (plist-get current-facts :refused))))
         (finish (child-result facts)
           (setq current-facts facts)
           (record-attempt)
           (apply #'niwa-execution-telemetry-finish
                  telemetry-context
                  :outcome (if (zerop (or (plist-get child-result :exit-code)
                                          -1))
                               'success
                             'error)
                  :exit-code (plist-get child-result :exit-code)
                  :output-bytes (plist-get child-result :output-bytes)
                  :timed-out (and (plist-get child-result :timed-out-p) t)
                  (niwa-execution-telemetry-safe-facts facts))
           (funcall callback
                    (let ((result (copy-sequence child-result)))
                      (setq result (plist-put result :sandbox-facts facts))
                      (plist-put result :sandbox-summary
                                 (niwa-execution-telemetry-context-summary
                                  telemetry-context)))))
         (teardown ()
           (record-attempt)
           (niwa-sandbox-cleanup preparation)
           (when teardown-function
             (funcall teardown-function))))
      (pcase (plist-get preparation :state)
        ('refused
         (setq current-facts (plist-get preparation :facts))
         (record-attempt)
         (apply #'niwa-execution-telemetry-finish
                telemetry-context :outcome 'refused
                (niwa-execution-telemetry-safe-facts
                 (plist-get preparation :facts)))
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
                :sandbox-summary
                (niwa-execution-telemetry-context-summary
                 telemetry-context)))
         nil)
        ((or 'unrestricted 'confined)
         (setq current-facts (plist-get preparation :facts))
         (when (eq (plist-get preparation :state) 'unrestricted)
           (niwa-execution-telemetry-record
            telemetry-context 'execution-unrestricted
            (append
             (list :name name :owner owner
                   :reason-class (plist-get current-facts :sandbox))
             (niwa-execution-telemetry-safe-facts current-facts))))
         (setq started-p
               (setq child
                     (niwa-execution--start-process
                      (lambda (child-result)
                        (if (eq (plist-get preparation :state) 'unrestricted)
                            (finish child-result current-facts)
                          (let* ((launch-failed
                                  (niwa-sandbox-launch-failed-p
                                   preparation child-result))
                                 (facts
                                  (if launch-failed
                                      (niwa-sandbox--record-launch-failure
                                       child-result workdir)
                                    (plist-get preparation :facts)))
                                 (clean-result
                                  (niwa-sandbox-strip-marker
                                   preparation child-result)))
                            (setq started-p (not launch-failed)
                                  current-facts facts)
                            (niwa-sandbox-cleanup preparation)
                            (finish clean-result facts))))
                      name (plist-get preparation :command) workdir timeout
                      session owner #'teardown))))
        (_ (error "Unknown sandbox preparation state: %s"
                  (plist-get preparation :state))))
      (lambda ()
        (when child
          (niwa-execution-process-stop child 'aborted))))))

(defun niwa-execution--owner-teardown-result ()
  "Return structured settlement for a synchronously discarded child."
  '(:exit-code -1 :output "" :output-bytes 0
    :timed-out-p nil :output-limit-p nil :wall-time-seconds 0.0
    :error (error "Execution owner was torn down")))

(cl-defun niwa-execution-run-one-shot
    (&key name command workdir writable-roots timeout
          additional-permissions sandbox-permissions session owner)
  "Run one confined COMMAND synchronously and return terminal facts.

All keyword arguments follow `niwa-execution-start-one-shot'."
  (let (done result)
    (niwa-execution-start-one-shot
     (lambda (child-result)
       (setq result child-result
             done t))
     :name name :command command :workdir workdir
     :writable-roots writable-roots :timeout timeout
     :additional-permissions additional-permissions
     :sandbox-permissions sandbox-permissions :session session :owner owner
     :teardown-function
     (lambda ()
       (setq result (niwa-execution--owner-teardown-result)
             done t)))
    (while (not done)
      (accept-process-output nil 0.05))
    result))


;;
;;; External helper interface

(cl-defun niwa-execution-start-helper
    (callback name command read-paths writable-roots
              &key timeout session owner teardown-callback settlement)
  "Start external helper COMMAND and call CALLBACK when it settles.
Return the helper's idempotent cancellation function.

READ-PATHS are mounted read-only.  WRITABLE-ROOTS are explicit artifact
directories.  A private writable scratch directory is the helper's working
directory and is removed before CALLBACK runs -- except on a busy remote
transport, where removal waits for the connection rather than nesting
inside the command already running on it.  Nothing reads the scratch after
the helper exits.  SETTLEMENT permits terminal snapshot work after producer
cancellation, retaining all other launch-authority checks."
  (let* ((target (and session
                      (niwa-session-execution-target session)))
         (remote-root
          (and target
               (niwa-execution-target-remote-p target)
               (niwa-execution-target-workspace-root target)))
         (scratch
          (if remote-root
              (make-nearby-temp-file
               (file-name-concat remote-root ".niwa-helper-") t)
            (make-temp-file "niwa-helper-" t)))
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
              ;; The removal is reached from the child's terminal
              ;; settlement, which runs from its sentinel and from the
              ;; settle timer -- inside whatever remote command those
              ;; interrupted.  A remote scratch directory removed there
              ;; issues a command on a connection that already has one in
              ;; flight, and the two cross replies.  Nothing reads the
              ;; scratch after the helper exits, so a busy transport
              ;; removes it once the connection is free.
              (unless (and (niwa-transport-busy-p scratch)
                           (niwa-transport-run-when-idle
                            (list 'helper-scratch scratch) scratch
                            (lambda ()
                              (ignore-errors (delete-directory scratch t)))))
                (ignore-errors (delete-directory scratch t))))))
         (teardown
          (lambda ()
            (funcall cleanup)
            (when teardown-callback
              (funcall teardown-callback)))))
    (condition-case err
        (niwa-execution-start-one-shot
         (lambda (child-result)
           (funcall cleanup)
           (funcall callback child-result))
         :name name :command command :workdir scratch
         :writable-roots roots :timeout timeout
         :additional-permissions permissions :session session :owner owner
         :teardown-function teardown :settlement settlement)
      (error
       (funcall cleanup)
       (signal (car err) (cdr err))))))

(cl-defun niwa-execution-run-helper
    (name command read-paths writable-roots &key timeout session owner)
  "Run external helper COMMAND synchronously and return terminal facts.

NAME, READ-PATHS, WRITABLE-ROOTS, TIMEOUT, and SESSION follow
`niwa-execution-start-helper'."
  (let (done result)
    (niwa-execution-start-helper
     (lambda (child-result)
       (setq result child-result
             done t))
     name command read-paths writable-roots
     :timeout timeout :session session :owner owner
     :teardown-callback
     (lambda ()
       (setq result (niwa-execution--owner-teardown-result)
             done t)))
    (while (not done)
      (accept-process-output nil 0.05))
    result))

(provide 'niwa-execution)
;;; niwa-execution.el ends here
