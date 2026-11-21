;;; niwa-session-publication.el -- Immutable session publication -*- lexical-binding: t -*-

;;; Commentary:

;; Owns immutable publication manifests, critical artifact transactions, retryable recovery, and diagnostic publication.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'niwa-structs))

;; Every control operation in this file runs through the session control
;; filesystem, so its feature is a hard load-time dependency rather than a
;; lazily reachable one.
(require 'niwa-session-control-fs)
(require 'niwa-journal-pins)
;; Publication validates and reads through durability's shared primitives.
;; Durability reaches back into publication only from inside functions, so
;; this direction is the loadable one.
(require 'niwa-session-durability)
(require 'niwa-telemetry)
(require 'niwa-transport)
(require 'niwa-utilities)

;; `files'
(defvar remote-file-name-inhibit-cache)

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-recover-working-directory "niwa-session-artifacts" (session))
(declare-function niwa-session-artifacts--sidecar-artifact "niwa-session-artifacts" (session buffer &optional checkpoint))
(autoload 'niwa-session-artifacts-recover-working-directory "niwa-session-artifacts")

;; `niwa-session-checkpoint'
(declare-function niwa-session-checkpoint-install "niwa-session-checkpoint" (session candidate))
(declare-function niwa-session-checkpoint-project "niwa-session-checkpoint" (session sidecar &optional candidate))
(declare-function niwa-session-checkpoint-release-proofs "niwa-session-checkpoint" (session))
(declare-function niwa-session-checkpoint-seed "niwa-session-checkpoint" (session sidecar))
(declare-function niwa-session-checkpoint-valid-p "niwa-session-checkpoint" (session candidate))
(autoload 'niwa-session-checkpoint-install "niwa-session-checkpoint")
(autoload 'niwa-session-checkpoint-project "niwa-session-checkpoint")
(autoload 'niwa-session-checkpoint-release-proofs "niwa-session-checkpoint")
(autoload 'niwa-session-checkpoint-seed "niwa-session-checkpoint")
(autoload 'niwa-session-checkpoint-valid-p "niwa-session-checkpoint")

;; `niwa-session-collection'
(declare-function niwa-session-collection-schedule "niwa-session-collection" (session))
(autoload 'niwa-session-collection-schedule "niwa-session-collection")

;; `niwa-session-control-fs'
(declare-function niwa-session-control-fs-append-file
                  "niwa-session-control-fs"
                  (path content &optional coding-system))
(declare-function niwa-session-control-fs-make-directory
                  "niwa-session-control-fs" (path &optional parents))
(declare-function niwa-session-control-fs-physical-path
                  "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-program-value
                  "niwa-session-control-fs" (result))
(declare-function niwa-session-control-fs-run-program
                  "niwa-session-control-fs" (operations))
(declare-function niwa-session-control-fs-run-program-async
                  "niwa-session-control-fs" (operations callback &optional scope))

;; `niwa-session-codec'
(declare-function niwa-session-codec-portable-authority-p
                  "niwa-session-codec" (session))
(declare-function niwa-session-codec-read "niwa-session-codec" (path))
(declare-function niwa-session-codec-validate-current-sidecar "niwa-session-codec" (plist))
(declare-function niwa-session-codec-write "niwa-session-codec" (path plist))
(autoload 'niwa-session-codec-portable-authority-p "niwa-session-codec")
(autoload 'niwa-session-codec-read "niwa-session-codec")
(autoload 'niwa-session-codec-validate-current-sidecar "niwa-session-codec")
(autoload 'niwa-session-codec-write "niwa-session-codec")

;; `niwa-session-durability'
(declare-function niwa-session-durability--assert-no-pid-lock
                  "niwa-session-durability" (session-dir))
(declare-function niwa-session-durability--create-plist
                  "niwa-session-durability" (path plist))
(declare-function niwa-session-durability--lease-head
                  "niwa-session-durability" (directory))
(declare-function niwa-session-durability--lease-path
                  "niwa-session-durability" (session-dir))
(declare-function niwa-session-durability--read-plist
                  "niwa-session-durability" (path))
(declare-function niwa-session-durability--renew-publication-lease
                  "niwa-session-durability" (session))
(declare-function niwa-session-durability--valid-relative-path-p
                  "niwa-session-durability" (path))
(declare-function niwa-session-durability-call-with-reserved-lease
                  "niwa-session-durability" (session function &optional token))
(declare-function niwa-session-durability-call-in-context
                  "niwa-session-durability" (context function))
(declare-function niwa-session-durability-call-outside-transaction
                  "niwa-session-durability" (function))
(declare-function niwa-session-durability-commit-publication-head
                  "niwa-session-durability" (session head))
(declare-function niwa-session-durability-fresh-context
                  "niwa-session-durability" (process-function))
(declare-function niwa-session-durability-lease-owned-p
                  "niwa-session-durability" (session))
(declare-function niwa-session-durability-lease-renew
                  "niwa-session-durability" (session &optional token))
(declare-function niwa-session-durability-publication-head
                  "niwa-session-durability" (session-dir &optional names))
(defvar niwa-session-durability--asserted-directories)
(defvar niwa-session-durability--final-commit)
(defvar niwa-session-durability--transaction-clock)

;; `niwa-session-recovery'
(declare-function niwa-session-recovery--abandon
                  "niwa-session-recovery" (session))
(declare-function niwa-session-recovery--delete-local
                  "niwa-session-recovery" (path))
(declare-function niwa-session-recovery-refresh
                  "niwa-session-recovery" (session &optional root-observation))
(declare-function niwa-session-recovery-refresh-session-buffers
                  "niwa-session-recovery" (session))
(autoload 'niwa-session-recovery--abandon "niwa-session-recovery")
(autoload 'niwa-session-recovery--delete-local "niwa-session-recovery")
(autoload 'niwa-session-recovery-refresh "niwa-session-recovery")
(autoload 'niwa-session-recovery-refresh-session-buffers
  "niwa-session-recovery")

;; `niwa-structs'
(declare-function niwa-session-control-transfer "niwa-structs" (cl-x) t)
(declare-function niwa-session-lease "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-publication
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-publication
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-publication-active-p
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-publication-queue
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-publication-uncommitted-batches
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)

;; `niwa-utilities'
(declare-function niwa--warn-once "niwa-utilities" (scope key format &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-warning "niwa-utilities" (scope type message &optional level buffer-name))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")
(autoload 'niwa--warn-once "niwa-utilities")

;; `niwa-workspace'
(declare-function niwa-workspace-state-dir
                  "niwa-workspace" (workspace))
(autoload 'niwa-workspace-state-dir "niwa-workspace")

(cl-defstruct (niwa-session-publication--job
               (:constructor niwa-session-publication--job-create))
  ;; CONTEXT is the job's own durability transaction context, entered by
  ;; every phase.
  session path epoch prepare acknowledge mail-ids batch batches candidate
  expected-lease expected-head context riders phase process error
  retain-failure checkpoint on-superseded
  ;; Raw frozen artifacts awaiting staging, the staged batch as first built,
  ;; and the optional selector run once digests are known.
  artifacts original selector
  (attempts 0) acknowledged-p success-p scheduled-p waiting-p)

(defun niwa-session-publication-busy-p (session)
  "Return non-nil when SESSION owns accepted asynchronous publication work.
Finalization alone is excluded: an acknowledgement may stage a new artifact
after adoption.  Dispatch and notifications separately respect finishing."
  (or (niwa-session-lease-renewal-job session)
      (niwa-session-publication-job session)
      (niwa-session-publication-preparation-token session)
      (niwa-session-publication-preparations session)))

(defvar niwa-session-publication-gate-open-functions nil
  "Abnormal hook run with SESSION when its gate opens between preparations.
It runs before the next queued preparation starts, with no job, finalization,
preparation token or synchronous publication in flight, so deferred work gets
a turn even when producers keep the queue non-empty.  A
function that mutates the session queues behind the waiting preparations.
Errors are reported and do not stop the queue.")

(defun niwa-session-publication--takeover-error (session)
  "Return the refusal error when SESSION has lost its publication authority."
  (when (and (memq (plist-get (niwa-session-lease session) :state) '(lost foreign))
             (memq (plist-get (niwa-session-control-transfer session) :state)
                   '(taken-over following)))
    (list 'user-error "Session lease taken over")))

(defun niwa-session-publication--fail-preparation (session entry err)
  "Decline queued ENTRY for SESSION with ERR, isolating its failure handler."
  (condition-case handler-error
      (if (cdr entry) (funcall (cdr entry) err)
        (niwa-diagnostic-warning
         (niwa-diagnostic-scope session) 'niwa (error-message-string err) :warning))
    ((error quit)
     (niwa-diagnostic-warning
      (niwa-diagnostic-scope session) 'niwa
      (error-message-string handler-error) :warning))))

(defun niwa-session-publication--schedule (session)
  "Schedule SESSION's next preparation outside the current callback."
  (let* ((thunk
          (lambda ()
            (unless (or (niwa-session-lease-renewal-job session)
                        (niwa-session-publication-job session)
                        (niwa-session-publication-finishing-p session)
                        (niwa-session-publication-preparation-token session)
                        (niwa-session-publication-active-p session))
              (condition-case err
                  (run-hook-with-args
                   'niwa-session-publication-gate-open-functions session)
                ((error quit)
                 (niwa-diagnostic-warning (niwa-diagnostic-scope session) 'niwa (error-message-string err) :warning))))
            ;; The hook may have started work; recheck the gate.
            (unless (or (niwa-session-lease-renewal-job session)
                        (niwa-session-publication-job session)
                        (niwa-session-publication-finishing-p session)
                        (niwa-session-publication-preparation-token session))
              (if (niwa-session-publication-active-p session)
                  (niwa-session-publication--schedule session)
                (when-let* ((entry (pop (niwa-session-publication-preparations session))))
                  (let ((token (list session)))
                    (setf (niwa-session-publication-preparation-token session) token)
                    (unwind-protect
                        (condition-case err
                            (if-let* ((refusal (niwa-session-publication--takeover-error session)))
                                (signal (car refusal) (cdr refusal))
                              (funcall (car entry) token))
                          ((error quit)
                           (niwa-session-publication--fail-preparation session entry err)))
                      (setf (niwa-session-publication-preparation-token session) nil)
                      (when (and (not (niwa-session-publication-job session))
                                 (niwa-session-publication-preparations session))
                        (niwa-session-publication--schedule session)))))))))
         (later (lambda () (niwa-transport-run-at-time 0 thunk))))
    (unless (niwa-transport-run-when-idle
             (list 'session-publication-preparation session)
             (niwa-session-save-path session) thunk later 0)
      (funcall later))))

(defun niwa-session-publication-enqueue (session preparation &optional failure)
  "Queue PREPARATION for SESSION's next transport-safe mutation opportunity.
PREPARATION receives the gate token; FAILURE receives any error it signals.
Unlike `niwa-session-publication-defer', always queue, even at an idle gate.
Synchronous saves and teardown drain this accepted work through the barrier."
  (if-let* ((err (niwa-session-publication--takeover-error session)))
      (niwa-session-publication--fail-preparation session (cons preparation failure) err)
    (setf (niwa-session-publication-preparations session)
          (nconc (niwa-session-publication-preparations session)
                 (list (cons preparation failure))))
    (niwa-session-publication--schedule session))
  t)

(defun niwa-session-publication-defer (session preparation &optional failure token)
  "Defer PREPARATION before it mutates or captures SESSION when its gate is held.
Return t when queued, nil when the caller may proceed.  FAILURE receives an
error if the queued preparation fails; no queued entry contains frozen state.
PREPARATION receives an explicit ownership token.  Only a caller carrying that
TOKEN may reenter its own gate; unrelated event callbacks cannot inherit it."
  (when (and (not (and token (eq token (niwa-session-publication-preparation-token session))))
             (niwa-session-publication-busy-p session))
    (niwa-session-publication-enqueue session preparation failure)))

(defvar niwa-session-publication--barrier-seconds 30
  "Drain deadline in seconds, checked between event-pump calls.
Callbacks run by a wait can exceed it; this is not a hard blocking bound.")

(defun niwa-session-publication-barrier (session &optional token)
  "Drain accepted SESSION jobs before an explicitly synchronous operation.
Never report a queued mutation as committed.  An unresolved publication fails
closed and remains available to explicit recovery or abandonment.
TOKEN authorizes only the preparation currently running for this session."
  (unless (and token (eq token (niwa-session-publication-preparation-token session)))
    (let ((deadline (+ (float-time) niwa-session-publication--barrier-seconds)))
     (while (niwa-session-publication-busy-p session)
      ;; A nested transport frame, or a synchronous publication that owns the
      ;; gate on this stack, cannot yield the authority a queued advance needs.
      (when (or (niwa-transport-busy-p (niwa-session-save-path session))
                (niwa-session-publication-finishing-p session)
                (niwa-session-publication-preparation-token session)
                (and (niwa-session-publication-active-p session)
                     (not (niwa-session-publication-job session))))
        (user-error "Session publication cannot drain inside an active operation"))
      (when (>= (float-time) deadline)
        (user-error "Session publication drain timed out; accepted work is still pending"))
      (when (and (niwa-session-publication-job session)
                 (eq 'unresolved
                     (niwa-session-publication--job-phase
                      (niwa-session-publication-job session))))
        (user-error "Session publication outcome is unresolved; retry or abandon it first"))
      (if-let* ((job (niwa-session-publication-job session)))
          (niwa-session-publication--advance-soon job)
        (niwa-session-publication--schedule session))
      (accept-process-output nil 0.01))))
  t)

(defun niwa-session-publication-stopping (session)
  "Stop new asynchronous admissions and drain SESSION before teardown.
Refuse teardown while frozen publication recovery remains unresolved."
  (setf (niwa-session-publication-stopping-p session) t)
  (niwa-session-publication-barrier session)
  (when (niwa-session-pending-publication session)
    (user-error "Session has pending publication; retry or abandon it first"))
  t)

(defun niwa-session-publication-async-eligible-p (session)
  "Return non-nil for a materialized local durable SESSION.
Cold materialization and remote targets stay synchronous."
  (and (not (niwa-session-publication-stopping-p session))
       (niwa-session-save-path session)
       (not (file-remote-p (niwa-session-save-path session)))
       (niwa-session-codec-portable-authority-p session)
       (niwa-session-publication session)
       (assoc "session.meta.el"
              (plist-get (niwa-session-publication session) :artifacts))
       (buffer-live-p (niwa-session-root-buffer session))))

(defun niwa-session-publication-submit
    (session prepare acknowledge &optional mail-ids retain-failure select-staged superseded)
  "Accept an acknowledged local SESSION publication.
PREPARE runs with the mutation gate held, after asynchronous admission, and
returns fresh artifacts as `niwa-session-publication-publish' takes them,
ending in one `session.meta.el' commit marker, which may carry a `:checkpoint'
candidate.  A nil result is a clean no-op success.  A replacing marker is
refused.  ACKNOWLEDGE receives (SUCCESS ERROR) exactly once after head
adoption or definitive rollback.  The checkpoint candidate is installed only
after the head commits.  MAIL-IDS retain producer identity.

A definitive failure before the head commit discards the frozen batch, which a
producer re-derives from its live state.  With RETAIN-FAILURE the batch and
any earlier retained ones become pending recovery instead, as a synchronous
publication failure leaves them: later mutation waits for retry or abandon,
and a successful retry commits without calling ACKNOWLEDGE again.  An
uncertain head outcome retains the job until reconciliation or recovery.

PREPARE only freezes: its artifacts are staged, then hashed by a control
program, in later turns.  SELECT-STAGED, when non-nil, is then called with
the staged batch, every artifact carrying its `:source-sha256', and returns
the batch to publish or nil.  It compares against frozen state it closed
over, never live buffers, and may rewrite a frozen sidecar if it also records
that sidecar's exact digest.  Nil is a clean no-op success: nothing is
reserved or committed.  It is refused while earlier retained batches exist,
which a no-op would otherwise consume.  Every staged batch belongs to the job
until acknowledgement.  A superseded uncertain job is retired without an
acknowledgement and calls SUPERSEDED, if non-nil, without arguments to release
local waits.  Never-launched queued work receives a definitive failure."
  (if-let* ((err (niwa-session-publication--takeover-error session)))
      (funcall acknowledge nil err)
    (when (niwa-session-publication-stopping-p session)
      (user-error "Session publication is stopping"))
    (let ((launch
           (lambda (&optional _token)
             (let ((job (niwa-session-publication--job-create
                         :session session :path (niwa-session-save-path session)
                         :epoch (niwa-session-checkpoint-state session)
                         :prepare prepare :acknowledge acknowledge :mail-ids mail-ids
                         :retain-failure retain-failure :selector select-staged
                         :on-superseded superseded
                         :expected-lease (copy-sequence (niwa-session-lease session))
                         :expected-head (plist-get (niwa-session-publication session) :head)
                         :phase 'admission)))
               (setf (niwa-session-publication--job-context job)
                     (niwa-session-durability-fresh-context
                      (lambda (process)
                        (setf (niwa-session-publication--job-process job) process))))
               (setf (niwa-session-publication-job session) job
                     (niwa-session-publication-active-p session) t)
               (niwa-session-publication--advance-soon job)))))
      (if (or (niwa-session-publication-busy-p session)
              (niwa-session-publication-finishing-p session)
              (niwa-session-publication-active-p session))
          (progn
            (setf (niwa-session-publication-preparations session)
                  (nconc (niwa-session-publication-preparations session)
                         (list (cons launch (lambda (err) (funcall acknowledge nil err))))))
            (niwa-session-publication--schedule session))
        (funcall launch))))
  'pending)

(defun niwa-session-publication--advance-soon (job)
  "Advance JOB outside the process sentinel and through the idle transport."
  (unless (or (niwa-session-publication--job-scheduled-p job)
              (niwa-session-publication--job-waiting-p job))
    (setf (niwa-session-publication--job-scheduled-p job) t)
    (let* (done
           (thunk (lambda ()
                    (unless done
                      (setq done t)
                      (setf (niwa-session-publication--job-scheduled-p job) nil)
                      (niwa-session-publication--advance job))))
           (later (lambda () (niwa-transport-run-at-time 0 thunk))))
      (unless (niwa-transport-run-when-idle
               (list 'session-publication-job job)
               (niwa-session-publication--job-path job) thunk later 0)
        (funcall later)))))

(defun niwa-session-publication--job-complete (job success &optional err abandoned)
  "Finish JOB once with SUCCESS or definitive ERR, releasing its gate.
ABANDONED explicitly discards recovery instead of retaining a failed checkpoint."
  (unless (niwa-session-publication--job-acknowledged-p job)
    (let* ((session (niwa-session-publication--job-session job))
           (retained (and (not success) (not abandoned)
                          (niwa-session-publication--job-retain-failure job)
                          (niwa-session-publication--job-batch job))))
      (setf (niwa-session-publication--job-acknowledged-p job) t
            (niwa-session-publication--job-success-p job) success
            (niwa-session-publication--job-phase job) 'done
            (niwa-session-publication-finishing-p session) t
            (niwa-session-publication-job session) nil
            (niwa-session-publication-active-p session) nil)
      ;; An ACK may stage an artifact after adoption, so ordinary publication
      ;; ownership is released.  Dispatch and arbitrary mailbox callbacks must
      ;; still wait until finalization unwinds, including any yielding cleanup.
      (unwind-protect
          (progn
            ;; The frozen work is recovery state before anyone is told it failed.
            (when retained
              (let ((recovery (delete-dups
                               (append
                                (niwa-session-publication-uncommitted-batches session)
                                (list retained)))))
                (setf (niwa-session-publication-uncommitted-batches session) nil)
                (niwa-session-publication--record-pending
                 session recovery (or err '(error "Session publication failed")))))
            ;; Arbitrary producer callbacks inherit no transaction observations
            ;; or explicit preparation ownership token.
            (unwind-protect
                (niwa-session-durability-call-outside-transaction
                 (lambda ()
                   (condition-case callback-error
                       (funcall (niwa-session-publication--job-acknowledge job) success err)
                     ((error quit)
                      (niwa-diagnostic-warning (niwa-diagnostic-scope session)
                       'niwa (format "Publication acknowledgement failed: %s"
                                        (error-message-string callback-error)) :warning)))))
              (condition-case cleanup
                  (if success
                      (progn
                        (setf (niwa-session-publication-uncommitted-batches session)
                              (cl-set-difference
                               (niwa-session-publication-uncommitted-batches session)
                               (niwa-session-publication--job-batches job) :test #'eq))
                        (niwa-session-publication--delete-batches
                         (niwa-session-publication--job-batches job))
                        ;; A selector may have replaced or dropped the staged batch.
                        (when-let* ((original (niwa-session-publication--job-original job)))
                          (niwa-session-publication--delete-batch original)))
                    (when (and (niwa-session-publication--job-batch job)
                               (not retained))
                      (niwa-session-publication--delete-batch
                       (niwa-session-publication--job-batch job)))
                    ;; A pruned original is no longer part of the transaction,
                    ;; unless the retained batch lives in its directory.
                    (when-let* ((original (niwa-session-publication--job-original job))
                                ((not (equal (plist-get original :directory)
                                             (plist-get retained :directory)))))
                      (niwa-session-publication--delete-batch original)))
                ((error quit)
                 (niwa-diagnostic-warning (niwa-diagnostic-scope session) 'niwa
                                  (format "Session publication cleanup failed: %s"
                                          (error-message-string cleanup)) :warning)))
              (condition-case cleanup
                  (niwa-session-checkpoint-release-proofs session)
                ((error quit)
                 (niwa-diagnostic-warning (niwa-diagnostic-scope session) 'niwa (error-message-string cleanup) :warning)))
              (when (and (not (niwa-session-publication-busy-p session))
                         (plist-get (niwa-session-lease session) :release-pending))
                (condition-case cleanup
                    (niwa-session-durability-lease-release
                     (niwa-session-save-path session) session)
                  ((error quit)
                   (niwa-diagnostic-warning (niwa-diagnostic-scope session) 'niwa (error-message-string cleanup) :warning))))))
        (setf (niwa-session-publication-finishing-p session) nil)
        ;; Offer notifications before admitting the successor, not inside this
        ;; finalizer.  Also wake a last job's offers without waiting for backoff.
        (niwa-session-publication--schedule session)))))

(defun niwa-session-publication--foreign-successor-p (job lease)
  "Return non-nil when activated foreign LEASE irreversibly fences JOB."
  (let ((origin (niwa-session-publication--job-expected-lease job)))
    (and (niwa-session-durability--valid-lease-p lease)
         (memq (plist-get lease :status) '(active publishing released))
         (not (equal niwa-session-durability--client-id
                     (plist-get lease :client-id)))
         (natnump (plist-get origin :generation))
         (> (plist-get lease :generation) (plist-get origin :generation)))))

(defun niwa-session-publication--job-superseded (job observation)
  "Discard irreversibly fenced JOB using its foreign lease OBSERVATION."
  (let* ((session (niwa-session-publication--job-session job))
         (queued (niwa-session-publication-preparations session))
         (superseded (niwa-session-publication--job-on-superseded job)))
    ;; The successor may include this mail: neither roll it back nor
    ;; acknowledge it again from the fenced client.
    (niwa-session-publication-clear-transient session)
    (setf (niwa-session-publication-preparations session) nil
          (niwa-session-publication--job-acknowledge job)
          (if superseded (lambda (&rest _) (funcall superseded)) #'ignore))
    (niwa-session-durability-bind-observed-lease session observation)
    (niwa-session-durability-call-outside-transaction
     (lambda ()
       (dolist (entry queued)
         (niwa-session-publication--fail-preparation
          session entry (list 'user-error "Session lease taken over")))))
    (niwa-session-publication--job-complete job nil nil t)))

(defun niwa-session-publication-settle-taken-over (session)
  "Retire SESSION's unresolved job if its bound lease proves supersession.
An existing poll tick calls this after an observation error.  A validated
newer foreign generation is an irreversible fence, so no target read,
rollback, acknowledgement or retry is needed."
  (when-let* ((job (niwa-session-publication-job session))
              ((eq 'unresolved (niwa-session-publication--job-phase job)))
              (lease (niwa-session-lease session))
              ((eq 'lost (plist-get lease :state)))
              ((niwa-session-publication--foreign-successor-p job lease)))
    (niwa-session-publication--job-superseded job (list :lease lease))
    t))

(defun niwa-session-publication--job-failed (job err)
  "Roll JOB back after a definitive pre-head failure ERR.
Normalize a reserved lease asynchronously before releasing the mutation gate."
  (setf (niwa-session-publication--job-waiting-p job) nil
        (niwa-session-publication--job-error job) err
        (niwa-session-publication--job-phase job) 'rollback)
  (niwa-session-publication--advance-soon job))

(defun niwa-session-publication--job-result (job next success err)
  "Store JOB's asynchronous SUCCESS or ERR and arrange NEXT advancement."
  (when (eq job (niwa-session-publication-job
                 (niwa-session-publication--job-session job)))
    (setf (niwa-session-publication--job-waiting-p job) nil
          (niwa-session-publication--job-process job) nil)
    (if (and success (not err))
        (progn
          (setf (niwa-session-publication--job-phase job) next)
          (niwa-session-publication--advance-soon job))
      (setq err (or err (list 'user-error
                              (format "Portable session publication %s failed (cause unavailable)"
                                      (niwa-session-publication--job-phase job)))))
      (if (memq next '(adopt))
          (progn
            (setf (niwa-session-publication--job-error job)
                  err
                  (niwa-session-publication--job-phase job) 'reconcile)
            (niwa-session-publication--advance-soon job))
        (niwa-session-publication--job-failed
         job err)))))

(defun niwa-session-publication--fixed-write-error (operations results)
  "Return the blocking fixed-artifact error from OPERATIONS and RESULTS.
Optional operations do not stop the program; skipped results follow its cause."
  (cl-loop for operation in operations for result in results
           unless (or (plist-get operation :optional)
                      (memq (plist-get result :status) '(ok skipped)))
           return (list 'file-error
                        (format "Portable session publication reserve failed: operation=%s status=%s code=%s"
                                (plist-get result :op) (plist-get result :status)
                                (plist-get result :code)))))

(defun niwa-session-publication--advance (job)
  "Advance one phase of JOB inside the job's own transaction context."
  (niwa-session-durability-call-in-context
   (niwa-session-publication--job-context job)
   (lambda () (niwa-session-publication--advance-phase job))))

(defun niwa-session-publication--advance-phase (job)
  "Advance one phase of JOB; the caller has entered the job's context."
  (let ((session (niwa-session-publication--job-session job)))
    (when (eq job (niwa-session-publication-job session))
      (condition-case err
          (progn
            (unless (equal (niwa-session-save-path session)
                           (niwa-session-publication--job-path job))
              (error "Session path changed during publication"))
            (unless (or (memq (niwa-session-publication--job-phase job)
                              '(reconcile rollback adopt unresolved))
                        (eq (niwa-session-checkpoint-state session)
                            (niwa-session-publication--job-epoch job)))
              (error "Session owner epoch changed during publication"))
            (pcase (niwa-session-publication--job-phase job)
              ('admission
               (when (niwa-session-pending-publication session)
                 (user-error "Session has pending publication; retry or abandon it first"))
               (setf (niwa-session-publication--job-waiting-p job) t)
               (niwa-session-durability-admit-async
                session (apply-partially #'niwa-session-publication--job-result job 'freeze)))
              ('freeze
               ;; Capture only: staging and hashing follow in their own turns.
               (let ((artifacts
                      (funcall (niwa-session-publication--job-prepare job))))
                 (if (null artifacts)
                     (niwa-session-publication--job-complete job t)
                   (setf (niwa-session-publication--job-artifacts job) artifacts
                         (niwa-session-publication--job-phase job) 'stage)
                   (niwa-session-publication--advance-soon job))))
              ('stage
               (let* ((artifacts (niwa-session-publication--job-artifacts job))
                      (batch (niwa-session-publication--stage-artifacts
                              session artifacts t))
                      (marker (niwa-session-publication--batch-marker batch))
                      (checkpoint (plist-get marker :checkpoint)))
                 ;; The batch is the job's before any check, so a refusal
                 ;; below cleans it up or retains it like any failure.
                 (setf (niwa-session-publication--job-artifacts job) nil
                       (niwa-session-publication--job-batch job) batch
                       (niwa-session-publication--job-original job) batch
                       (niwa-session-publication--job-batches job)
                       (append (niwa-session-publication-uncommitted-batches session)
                               (list batch))
                       (niwa-session-publication--job-checkpoint job) checkpoint)
                 (unless marker
                   (error "Asynchronous publication requires a commit marker"))
                 (when (plist-get marker :replace)
                   (error "Asynchronous publication cannot replace history"))
                 (when (and checkpoint
                            (not (niwa-session-checkpoint-valid-p session checkpoint)))
                   (error "Checkpoint candidate belongs to a different owner epoch"))
                 ;; Projection rewrites the small sidecar, so it precedes the
                 ;; digest taken of its final bytes.
                 (niwa-session-publication--project-staged-sidecar
                  session marker checkpoint)
                 (setf (niwa-session-publication--job-phase job) 'digest)
                 (niwa-session-publication--advance-soon job)))
              ('digest
               (niwa-session-publication--job-digest job))
              ('select
               (niwa-session-publication--job-select job))
              ('reserve
               (let* ((plan (niwa-session-publication--async-fixed-operations
                             session (niwa-session-publication--job-batch job)))
                      (writes (cdr plan))
                      (riders (list :before nil
                                    :after (car plan)
                                    :consumed nil :before-results nil :after-results nil))
                      (niwa-session-durability--commit-riders riders))
                 (setf (niwa-session-publication--job-riders job) riders)
                 (setf (niwa-session-publication--job-waiting-p job) t)
                 (niwa-session-durability-reserve-publication-lease-async
                  session
                  (lambda (success error)
                    (let* ((results (plist-get riders :after-results))
                           (written (and success
                                         (cl-every (lambda (index)
                                                     (eq 'ok (plist-get (nth index results)
                                                                        :status)))
                                                   writes))))
                      (niwa-session-publication--job-result
                       job 'generation written
                       (or error
                           (and success (not written)
                                (niwa-session-publication--fixed-write-error
                                 (car plan) results)))))))))
              ('generation
               (niwa-session-publication--job-generation job))
              ('head
               (let ((niwa-session-durability--final-commit t))
                 (setf (niwa-session-publication--job-waiting-p job) t)
                 (niwa-session-durability-commit-publication-head-async
                  session (plist-get (niwa-session-publication--job-candidate job) :head)
                  (apply-partially #'niwa-session-publication--job-result job 'adopt))))
              ('adopt
               (setf (niwa-session-publication session)
                     (niwa-session-publication--job-candidate job))
               (when-let* ((checkpoint (niwa-session-publication--job-checkpoint job)))
                 (niwa-session-checkpoint-install session checkpoint))
               (condition-case cleanup
                   (niwa-session-collection-schedule session)
                 ((error quit)
                  (niwa-diagnostic-warning (niwa-diagnostic-scope session)
                   'niwa
                   (format "Session publication committed before collection failed: %s"
                           (error-message-string cleanup)) :warning)))
               (niwa-session-publication--job-complete job t))
              ('rollback
               (setf (niwa-session-publication--job-waiting-p job) t)
               (niwa-session-durability-finish-publication-lease-async
                session
                (lambda (_success _error)
                  (setf (niwa-session-publication--job-waiting-p job) nil)
                  (niwa-session-publication--job-complete
                   job nil (niwa-session-publication--job-error job)))))
              ('reconcile
               (setf (niwa-session-publication--job-waiting-p job) t)
               (niwa-session-durability-observe-publication-head-async
                session
                (lambda (observation error)
                  (setf (niwa-session-publication--job-waiting-p job) nil)
                  (cond
                   (error
                    (setf (niwa-session-publication--job-phase job) 'unresolved)
                    (niwa-session-publication--record-pending
                     session (niwa-session-publication--job-batches job) error))
                   ((equal (plist-get observation :publication-head)
                           (plist-get (niwa-session-publication--job-candidate job) :head))
                    (niwa-session-durability-bind-observed-lease session observation)
                    (setf (niwa-session-pending-publication session) nil
                          (niwa-session-publication--job-phase job) 'adopt)
                    (niwa-session-publication--advance-soon job))
                   ((equal (plist-get observation :publication-head)
                           (niwa-session-publication--job-expected-head job))
                    (niwa-session-durability-bind-observed-lease session observation)
                    (setf (niwa-session-pending-publication session) nil)
                    (niwa-session-publication--job-failed
                     job (niwa-session-publication--job-error job)))
                   ((and (not (plist-get observation :owned))
                         (niwa-session-publication--foreign-successor-p
                          job (plist-get observation :lease)))
                    (niwa-session-publication--job-superseded job observation))
                   (t
                    ;; Another head may have inherited the candidate's mail.
                    ;; Without proof, rollback could acknowledge the same
                    ;; execution again after restoring that successor.
                    (setf (niwa-session-publication--job-phase job) 'unresolved)
                    (niwa-session-publication--record-pending
                     session (niwa-session-publication--job-batches job)
                     '(error "Publication head changed during reconciliation")))))))))
        ((error quit)
         (setf (niwa-session-publication--job-waiting-p job) nil)
         (if (memq (niwa-session-publication--job-phase job) '(head reconcile adopt))
             (progn
               (setf (niwa-session-publication--job-error job) err
                     (niwa-session-publication--job-phase job) 'reconcile)
               (niwa-session-publication--advance-soon job))
           (if (eq 'rollback (niwa-session-publication--job-phase job))
               (niwa-session-publication--job-complete job nil err)
             (niwa-session-publication--job-failed job err))))))))

(defun niwa-session-publication--valid-digest-p (digest)
  "Return non-nil when DIGEST is a lowercase hexadecimal SHA-256."
  (and (stringp digest)
       (let ((case-fold-search nil))
         (string-match-p "\\`[0-9a-f]\\{64\\}\\'" digest))))

(defun niwa-session-publication--require-digests (artifacts)
  "Signal unless every written artifact in ARTIFACTS has a valid digest."
  (dolist (artifact artifacts)
    (unless (or (plist-get artifact :delete)
                (niwa-session-publication--valid-digest-p
                 (plist-get artifact :source-sha256)))
      (error "Publication artifact has no valid digest: %s"
             (or (plist-get artifact :logical) (plist-get artifact :path))))))

(defun niwa-session-publication--job-digest (job)
  "Hash every undigested staged source of JOB with one control program.
Retained batches are included.  Digests are installed only from a complete,
well-formed result; any other outcome fails the job."
  (let* ((pending
          (cl-loop for batch in (niwa-session-publication--job-batches job)
                   append (cl-loop for artifact in (plist-get batch :artifacts)
                                   unless (or (plist-get artifact :delete)
                                              (plist-get artifact :source-sha256))
                                   collect artifact)))
         (sources (delete-dups
                   (mapcar (lambda (artifact) (plist-get artifact :source))
                           pending))))
    (if (null sources)
        (progn
          (setf (niwa-session-publication--job-phase job) 'select)
          (niwa-session-publication--advance-soon job))
      (setf (niwa-session-publication--job-waiting-p job) t)
      (let ((process
             (niwa-session-control-fs-run-program-async
              (mapcar (lambda (source)
                        (list :op 'file-hash
                              :path (niwa-session-control-fs-physical-path
                                     (file-truename source))))
                      sources)
              (lambda (results error)
                (condition-case err
                    (progn
                      (when error (signal (car error) (cdr error)))
                      (unless (= (length results) (length sources))
                        (error "Incomplete staged digest result"))
                      (let ((table (make-hash-table :test #'equal)))
                        (cl-mapc
                         (lambda (source result)
                           (let ((digest (plist-get (plist-get result :value)
                                                    :sha256)))
                             (unless (and (eq 'ok (plist-get result :status))
                                          (niwa-session-publication--valid-digest-p
                                           digest))
                               (error "Invalid staged digest for %s" source))
                             (puthash source digest table)))
                         sources results)
                        (dolist (artifact pending)
                          (plist-put artifact :source-sha256
                                     (gethash (plist-get artifact :source) table))))
                      (niwa-session-publication--job-result
                       job 'select t nil))
                  ((error quit)
                   (niwa-session-publication--job-result
                    job 'select nil err))))
              (niwa-diagnostic-scope (niwa-session-publication--job-session job)))))
        (when (niwa-session-publication--job-waiting-p job)
          (setf (niwa-session-publication--job-process job) process))))))

(defun niwa-session-publication--job-select (job)
  "Run JOB's selector over its digested staged batch, then reserve.
Nil from the selector completes the job as a clean no-op, refused while
earlier retained batches exist.  A selected batch replaces the staged one and
all digests are validated before anything is reserved.  The staged original
stays the job's for cleanup either way."
  (let* ((original (niwa-session-publication--job-original job))
         (select (niwa-session-publication--job-selector job))
         (selected (if select (funcall select original) original)))
    (cond
     ((null selected)
      (unless (equal (niwa-session-publication--job-batches job)
                     (list original))
        (error "Publication selector declined with retained recovery pending"))
      (setf (niwa-session-publication--job-batches job) nil
            (niwa-session-publication--job-batch job) nil
            (niwa-session-publication--job-checkpoint job) nil)
      (niwa-session-publication--job-complete job t))
     (t
      (unless (eq selected original)
        (setf (niwa-session-publication--job-batches job)
              (cl-substitute selected original
                             (niwa-session-publication--job-batches job)
                             :test #'eq)
              (niwa-session-publication--job-batch job) selected))
      ;; A selector may also have pruned or replaced the marker of the very
      ;; batch it returned, so the marker is always revalidated.
      (let ((marker (niwa-session-publication--batch-marker selected)))
        (unless marker
          (error "Selected publication lacks a commit marker"))
        (setf (niwa-session-publication--job-checkpoint job)
              (plist-get marker :checkpoint)))
      (niwa-session-publication--require-digests
       (cl-loop for batch in (niwa-session-publication--job-batches job)
                append (plist-get batch :artifacts)))
      (setf (niwa-session-publication--job-phase job) 'reserve)
      (niwa-session-publication--advance-soon job)))))

(defun niwa-session-publication--job-generation (job)
  "Prepare JOB's immutable generation and dispatch its head commit."
  (let* ((session (niwa-session-publication--job-session job))
         (checkpoint (niwa-session-publication--job-checkpoint job))
         (session-dir (niwa-session-publication--job-path job))
         (artifacts (niwa-session-publication--deduplicate-artifacts
                     (apply #'append
                            (mapcar (lambda (batch) (plist-get batch :artifacts))
                                    (niwa-session-publication--job-batches job)))))
         (entries (niwa-session-publication--raw-artifacts
                   (niwa-session-publication session) session-dir)))
    (when (and checkpoint
               (not (niwa-session-checkpoint-valid-p session checkpoint)))
      (error "Checkpoint candidate belongs to a different owner epoch"))
    ;; Never hash on the main thread here: every digest was installed by the
    ;; digest phase, and a missing one is a defect.
    (niwa-session-publication--require-digests artifacts)
    (pcase-let* ((`(,directory ,overlaid ,operations)
                  (niwa-session-publication--prepare-generation
                   (file-name-concat session-dir ".publications") artifacts session-dir entries
                   (make-hash-table :test #'equal)))
                 (head (file-relative-name (file-name-concat directory "manifest.el") session-dir))
                 (niwa-session-durability--commit-riders
                  (list :before operations :after nil :consumed nil
                        :before-results nil :after-results nil))
                 (niwa-session-durability--final-commit t))
      (setf (niwa-session-publication--job-candidate job)
            (niwa-session-publication--capture-publication
             session-dir (list :head head :sidecar "session.meta.el" :artifacts overlaid))
            (niwa-session-publication--job-phase job) 'head)
      (setf (niwa-session-publication--job-waiting-p job) t)
      (niwa-session-durability-commit-publication-head-async
       session head (apply-partially #'niwa-session-publication--job-result job 'adopt)))))

(defun niwa-session-publication--project-staged-sidecar (session marker checkpoint)
  "Refresh the staged sidecar MARKER of SESSION through its checkpoint owner.
A queued sidecar can predate a root commit that landed since it was frozen, so
its checkpoint projection is advanced from the owner, never from live root
fields, exactly as `niwa-session-publication--commit-marker-publication'
does.  CHECKPOINT is the marker's candidate, or nil.  Local files only."
  (when (niwa-session-checkpoint-state session)
    (let* ((source (plist-get marker :source))
           (raw (niwa-session-codec-validate-current-sidecar
                 (niwa-session-publication--read-staged-sidecar source)))
           (projected (niwa-session-checkpoint-project session raw checkpoint)))
      (unless (equal raw projected)
        ;; A local job-owned file: write it directly, never through a control
        ;; program, and let the digest phase hash its final bytes.
        (niwa-session-publication--write-staged-content
         source
         (let ((print-length nil) (print-level nil)
               (print-circle t) (print-quoted t))
           (prin1-to-string projected))
         'utf-8-unix)
        (setf (plist-get marker :source-sha256) nil)))))

(defun niwa-session-publication--async-fixed-operations (session batch)
  "Return (OPERATIONS . WRITE-INDEXES) writing BATCH's fixed artifacts.
OPERATIONS is one program for SESSION's reservation to carry: a single
`verify-latest' proof that no newer lease generation exists, then for each
fixed artifact the optional creation of every missing parent below the session
directory and its write.  WRITE-INDEXES are the positions of the writes, whose
results say whether the artifact landed.  A parent chain needs no retry: each
level is its own optional operation.  Nil when SESSION has no bound lease
generation to compare against."
  (let ((generation (plist-get (niwa-session-lease session) :generation))
        (session-dir (niwa-session-save-path session))
        operations writes)
    (when (and (natnump generation) session-dir)
      (let ((root (file-name-as-directory (expand-file-name session-dir))))
        (setq operations
              (list (list :op 'verify-latest
                          :path (niwa-session-durability--generation-path
                                 (niwa-session-durability--lease-path session-dir)
                                 generation)
                          :content ".el")))
        (dolist (artifact (plist-get batch :artifacts))
          (when (and (niwa-session-publication--fixed-artifact-p artifact)
                     (plist-get artifact :source))
            (let* ((path (expand-file-name (plist-get artifact :path)))
                   (relative (directory-file-name
                              (or (file-name-directory
                                   (file-relative-name path root))
                                  "")))
                   (current (directory-file-name root)))
              (unless (string-prefix-p root path)
                (error "Publication artifact leaves the session: %s" path))
              (dolist (component (split-string relative "/" t))
                (setq current (file-name-concat current component))
                (niwa-session-control-fs-physical-path current)
                (setq operations
                      (append operations
                              (list (list :op 'make-directory
                                          :path current :optional t)))))
              (setq operations
                    (append operations
                            (list (list :op 'write
                                        :path (niwa-session-control-fs-physical-path
                                               path)
                                        :source-file (plist-get artifact :source))))
                    writes (append writes (list (1- (length operations))))))))))
    (cons operations writes)))

(defun niwa-session-publication--control-logical-path-p (path)
  "Return non-nil when logical PATH names durability control storage."
  (or (equal path ".lock")
      (equal path ".lease")
      (string-prefix-p ".lease/" path)
      (equal path ".publications")
      (string-prefix-p ".publications/" path)
      (equal path ".journal-pins")
      (string-prefix-p ".journal-pins/" path)
      (equal path ".recovery")
      (string-prefix-p ".recovery/" path)))

(defun niwa-session-publication-logical-path-p (path)
  "Return non-nil when PATH is a normalized session artifact path."
  (and (niwa-session-durability--valid-relative-path-p path)
       (not (niwa-session-publication--control-logical-path-p path))))

(defun niwa-session-publication-valid-head-p (head)
  "Return non-nil when HEAD names an immutable publication manifest."
  (and (niwa-session-durability--valid-relative-path-p head)
       (string-prefix-p ".publications/" head)
       (equal (file-name-nondirectory head) "manifest.el")))


;;;; Publication recovery state

(defun niwa-session-publication--delete-batch (batch)
  "Delete BATCH's local recovery directory."
  (when-let* ((directory (plist-get batch :directory))
              ((file-directory-p directory))
              ;; Parent metadata can change between the two stat calls in
              ;; `file-in-directory-p'.  Compare resolved names instead.
              ((string-prefix-p
                (file-name-as-directory (file-truename temporary-file-directory))
                (file-name-as-directory (file-truename directory)))))
    (condition-case err
        (delete-directory directory t)
      (file-error
       (niwa-diagnostic-warning (or (plist-get batch :diagnostic-scope) 'unavailable)
        'niwa
        (format "Could not remove publication recovery: %s"
                (error-message-string err))
        :warning)))))

(defun niwa-session-publication--delete-batches (batches)
  "Delete local recovery directories owned by BATCHES."
  (dolist (batch (delete-dups (copy-sequence batches)))
    (niwa-session-publication--delete-batch batch)))

(defun niwa-session-publication-clear-transient (session)
  "Delete and clear SESSION's locally staged publication state."
  (let ((pending (niwa-session-pending-publication session)))
    (niwa-session-publication--delete-batches
     (append
      (plist-get pending :batches)
      (niwa-session-publication-uncommitted-batches session)
      (niwa-session-publication-queue session)))
    (setf (niwa-session-pending-publication session) nil
          (niwa-session-publication-uncommitted-batches session) nil
          (niwa-session-publication-queue session) nil)
    (niwa-session-checkpoint-release-proofs session)))

(defun niwa-session-publication--batch-live-p (batch)
  "Return non-nil when BATCH still owns a staged source."
  (cl-some
   (lambda (artifact)
     (or (plist-get artifact :delete)
         (file-exists-p (plist-get artifact :source))))
   (plist-get batch :artifacts)))

(defun niwa-session-publication--read-staged-sidecar (source)
  "Return the sidecar plist in staged local SOURCE.
Staged sources live in this client's private temporary directory, never on
the target, so they need no pinned control program: reading one through it
cost a process start inside every committing publication."
  (when (file-remote-p source)
    (error "Staged publication source is not local: %s" source))
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents source))
    (goto-char (point-min))
    (read (current-buffer))))

(defun niwa-session-publication--retain-uncommitted-batch (session batch)
  "Retain BATCH's session-local artifacts on SESSION until a marker."
  (let ((local
         (cl-remove-if-not
          (lambda (artifact) (plist-get artifact :logical))
          (plist-get batch :artifacts))))
    (if local
        (let ((retained (copy-sequence batch)))
          (setf (plist-get retained :artifacts) local)
          (setf (niwa-session-publication-uncommitted-batches session)
                (append
                 (niwa-session-publication-uncommitted-batches session)
                 (list retained)))
          (dolist (artifact (plist-get batch :artifacts))
            (unless (plist-get artifact :logical)
              (condition-case nil
                  (delete-file (plist-get artifact :source))
                (file-error nil)))))
      (niwa-session-publication--delete-batch batch))))

(defun niwa-session-publication--record-pending (session batches err)
  "Retain BATCHES as SESSION recovery after publication error ERR."
  (setf (niwa-session-pending-publication session)
        (list :batches batches
              :failed-at (format-time-string "%FT%T%z")
              :reason (error-message-string err)))
  (niwa-diagnostic-warning (niwa-diagnostic-scope session)
   'niwa
   (format
    (concat "Critical session publication failed: %s. "
            "The completed work is retained locally; retry or explicitly "
            "abandon recovery before starting another turn.")
    (error-message-string err))
   :error))


(defun niwa-session-publication--current-session ()
  "Return the session reached from the current data or view buffer."
  (or (and (boundp 'niwa--session) niwa--session)
      (and (boundp 'niwa--data-buffer)
           (buffer-live-p niwa--data-buffer)
           (buffer-local-value 'niwa--session niwa--data-buffer))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No active niwa session")))

(defun niwa-session-publication--valid-published-path-p (path)
  "Return non-nil when PATH names immutable publication storage.

The shape is pinned to one artifact name directly inside one
generation directory: a manifest entry naming any other spelling under
the publication root -- such as a planted directory a symlink routes
elsewhere -- could route a read through storage the generation's
immutability never covered."
  ;; This fixed prefix already excludes absolute, remote, and home-relative
  ;; spellings.  Do not dispatch filename handlers for every immutable entry.
  (and (stringp path)
       (string-match-p
        "\\`\\.publications/generation-[0-9a-f]\\{20\\}/[^/]+\\'"
        path)
       (not (string-suffix-p "/." path))
       (not (string-suffix-p "/.." path))))

(defun niwa-session-publication--within-p (path directory)
  "Return non-nil when canonical PATH lies strictly below canonical DIRECTORY.

Both arguments have already been through the control filesystem's canonical
spelling, which expands them and collapses any `..', so containment is a
pathname question and this answers exactly that.

`file-in-directory-p' answers a different question -- whether the two names
*resolve* into each other -- by calling `file-truename' on both.  On a target
that is several round trips per artifact, measured at roughly a fifth of a
remote turn's processor time, and it reports a resolution the target is free
to change before the operation runs.  The spelling that matters is proved
target-side instead, in the same process that performs the operation."
  (let ((directory (file-name-as-directory directory)))
    (and (string-prefix-p directory path)
         (> (length path) (length directory)))))

(defun niwa-session-publication--publication-path (session-dir relative)
  "Return RELATIVE below SESSION-DIR's physical publication root."
  (niwa-session-control-fs-physical-path session-dir)
  (let* ((session-root
          (file-name-as-directory
           (niwa-session-control-fs-physical-path session-dir)))
         (root
          (file-name-as-directory
           (niwa-session-control-fs-physical-path
            (file-name-concat session-dir ".publications"))))
         ;; The containment proof compares spellings, so the candidate is
         ;; taken in the same physical spelling as the roots: a session
         ;; directory carrying a doubled slash must not defeat the prefix
         ;; test, and a relative carrying dot components must not pass it.
         (path (niwa-session-control-fs-physical-path
                (file-name-concat session-dir relative))))
    (niwa-session-control-fs-physical-path root)
    (unless (and (not (equal (directory-file-name root)
                             (directory-file-name session-root)))
                 (niwa-session-publication--within-p root session-root))
      (error "Session publication root escapes session storage: %s" root))
    (unless (niwa-session-publication--within-p path root)
      (error "Session publication path escapes immutable storage: %s" path))
    path))

(defun niwa-session-publication--artifact-for-session (session artifact)
  "Return normalized publication ARTIFACT for SESSION.

Session-local artifacts receive a target-native `:logical' path.  Artifacts
elsewhere on the same execution target receive nil."
  (let* ((session-dir
          (file-name-as-directory
           (expand-file-name
            (or (niwa-session-save-path session)
                (error "Session has no durability path")))))
         (raw (plist-get artifact :path))
         (path (and (stringp raw) (expand-file-name raw session-dir)))
         (session-remote (file-remote-p session-dir))
         (path-remote (and path (file-remote-p path))))
    (unless path
      (error "Publication artifact requires a string path and content"))
    (unless (equal session-remote path-remote)
      (error "Publication artifact crosses execution targets: %s" raw))
    (let* ((local-p (string-prefix-p session-dir path))
           (logical (and local-p (substring path (length session-dir))))
           (marker (plist-get artifact :commit-marker))
         (replace (plist-get artifact :replace))
         (delete (plist-get artifact :delete)))
      (when (and logical
                 (not (niwa-session-publication-logical-path-p
                       logical)))
        (error "Invalid session publication path: %s" logical))
      (when (and marker (not (eq marker t)))
        (error "Publication commit marker must be t"))
      (when (and replace (not (eq replace t)))
        (error "Publication replacement marker must be t"))
      (when (and replace (not marker))
        (error "Publication replacement requires the commit marker"))
      (when (and delete (not (eq delete t)))
        (error "Publication deletion marker must be t"))
      (when (and delete (or marker replace))
        (error "Publication deletion cannot be a commit marker"))
      (when (and delete (null logical))
        (error "Publication deletion must name session-local storage"))
      (when (and marker (not (equal logical "session.meta.el")))
        (error "Publication commit marker must be session.meta.el"))
      (let ((normalized (copy-sequence artifact)))
        (setq normalized (plist-put normalized :path path)
              normalized (plist-put normalized :logical logical))
        normalized))))

(defun niwa-session-publication--files-sha256 (paths)
  "Return the SHA-256 digests of files PATHS' literal bytes, in order.
Local files are hashed by `sha256sum', a few hundred per process: reading
each file into a buffer and hashing a copy of its text allocated twice its
size, and a large Save As spent a third of its time hashing and another
third collecting.  Remote files, and all files without `sha256sum', are
hashed in Lisp."
  (let ((digests (make-hash-table :test #'equal))
        (local (and (executable-find "sha256sum")
                    (delete-dups
                     (seq-remove #'file-remote-p
                                 (mapcar #'expand-file-name paths))))))
    (while local
      (let ((chunk (seq-take local 200)))
        (setq local (nthcdr 200 local))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          ;; Inputs are absolute local paths; a removed worktree must not
          ;; prevent launching the hasher from an inherited directory.
          (when (eq 0 (let ((coding-system-for-read 'no-conversion)
                           (default-directory "/"))
                        (apply #'call-process "sha256sum" nil t nil
                               "--binary" "--zero" "--" chunk)))
            (goto-char (point-min))
            (dolist (path chunk)
              ;; Each record is DIGEST, space, `*', the name, then NUL.
              (when (looking-at "\\([0-9a-f]\\{64\\}\\) \\*")
                (let ((digest (match-string 1))
                      (name-start (match-end 0)))
                  (goto-char name-start)
                  (when (and (search-forward "\0" nil t)
                             (equal (encode-coding-string
                                     path (or file-name-coding-system
                                              default-file-name-coding-system))
                                    (buffer-substring-no-properties
                                     name-start (1- (point)))))
                    ;; Recorded only when the name echoes exactly.
                    (puthash path digest digests)))))))))
    (mapcar (lambda (path)
              (or (gethash (expand-file-name path) digests)
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally path)
                    (secure-hash 'sha256 (current-buffer)))))
            paths)))

(defun niwa-session-publication--file-sha256 (path)
  "Return the SHA-256 digest of file PATH's literal bytes."
  (car (niwa-session-publication--files-sha256 (list path))))

(defun niwa-session-publication--content-sha256 (artifact)
  "Return the digest ARTIFACT's staged bytes will have once written.

Encoding mirrors `niwa-session-publication--stage-artifacts' exactly, so
this answers what the manifest would record without staging anything."
  (let ((content (plist-get artifact :content)))
    (secure-hash
     'sha256
     (if (multibyte-string-p content)
         (encode-coding-string
          content (or (plist-get artifact :coding) 'utf-8-unix))
       content))))

(defun niwa-session-publication--comparable-p (session)
  "Return non-nil when SESSION's captured manifest can answer for its bytes.

A queued or retained batch, or pending recovery, means the session owes the
target a transaction whatever any comparison says."
  (and (niwa-session-publication session)
       (null (niwa-session-publication-queue session))
       (null (niwa-session-publication-uncommitted-batches session))
       (null (niwa-session-pending-publication session))))

(defun niwa-session-publication--artifact-committed-p
    (session entries artifact)
  "Return non-nil when ARTIFACT matches SESSION's committed manifest ENTRIES.

An artifact outside the session directory is an ordinary fenced write that
never enters the snapshot, so it can never be answered from a manifest."
  (let* ((normalized
          (niwa-session-publication--artifact-for-session session artifact))
         (logical (plist-get normalized :logical))
         (entry (and logical (cdr (assoc logical entries)))))
    (if (plist-get normalized :delete)
        (null entry)
      (and entry
           (equal (plist-get entry :sha256)
                  (niwa-session-publication--content-sha256 normalized))))))

(defun niwa-session-publication-committed-p (session artifacts)
  "Return non-nil when SESSION already committed ARTIFACTS byte for byte.

Only the captured manifest is consulted, so the answer costs no target I/O."
  (and (niwa-session-publication--comparable-p session)
       (let ((entries
              (plist-get (niwa-session-publication session) :artifacts)))
         (cl-every
          (lambda (artifact)
            (niwa-session-publication--artifact-committed-p
             session entries artifact))
          artifacts))))

(defun niwa-session-publication-prune-committed (session artifacts)
  "Return ARTIFACTS without those SESSION already committed byte for byte.

The manifest overlays the previous one, so an omitted artifact keeps its
existing entry and the resulting logical snapshot is unchanged.  Publication
proves ownership once per artifact written, so dropping bytes the target
already holds removes whole ownership round trips rather than only writes.
The commit marker always survives: it is the transaction's commit point, not
a payload."
  (if (not (niwa-session-publication--comparable-p session))
      artifacts
    (let ((entries
           (plist-get (niwa-session-publication session) :artifacts)))
      (seq-filter
       (lambda (artifact)
         (or (plist-get artifact :commit-marker)
             (plist-get artifact :replace)
             (not (niwa-session-publication--artifact-committed-p
                   session entries artifact))))
       artifacts))))

(defun niwa-session-publication--validate-manifest (manifest path)
  "Return MANIFEST when it has the publication schema, else error at PATH."
  (let ((sidecar (plist-get manifest :sidecar))
        (artifacts (plist-get manifest :artifacts))
        (seen (make-hash-table :test #'equal))
        valid)
    (setq valid
          (and (proper-list-p manifest)
               (equal sidecar "session.meta.el")
               (plist-member manifest :artifacts)
               (proper-list-p artifacts)))
    (dolist (entry artifacts)
      (let ((logical (car-safe entry))
            (properties (cdr-safe entry)))
        (unless (and valid
                     (proper-list-p entry)
                     (niwa-session-publication-logical-path-p
                      logical)
                     (proper-list-p properties)
                     (niwa-session-publication--valid-published-path-p
                      (plist-get properties :published))
                     (stringp (plist-get properties :sha256))
                     (string-match-p "\\`[0-9a-f]\\{64\\}\\'"
                                     (plist-get properties :sha256))
                     (not (gethash logical seen)))
          (setq valid nil))
        (when logical
          (puthash logical t seen))))
    (unless (and valid (gethash sidecar seen))
      (error "Invalid session publication manifest: %s" path))
    manifest))

(defun niwa-session-publication--read-publication-raw
    (session-dir &optional head names)
  "Read and validate SESSION-DIR's raw manifest at HEAD or its lease head.

NAMES is an already-observed listing of SESSION-DIR's lease directory,
passed through to `niwa-session-durability-publication-head'."
  (let* ((remote-file-name-inhibit-cache t)
         (head (or head
                   (niwa-session-durability-publication-head
                    session-dir names))))
    (when head
      (unless (niwa-session-publication-valid-head-p head)
        (error "Invalid session publication head: %s" head))
      (let* ((path (niwa-session-publication--publication-path
                    session-dir head))
             (manifest (niwa-session-durability--read-plist path)))
        (unless manifest
          (error "Missing session publication manifest: %s" path))
        (setq manifest
              (niwa-session-publication--validate-manifest manifest path))
        (list :head head
              :sidecar (plist-get manifest :sidecar)
              :artifacts (plist-get manifest :artifacts))))))

(defun niwa-session-publication--capture-publication
    (session-dir publication)
  "Qualify raw PUBLICATION paths beneath SESSION-DIR for a live client.
Canonicalize the shared session root once.  Each relative artifact must
name one file directly inside an immutable generation; actual reads still
prove the physical spelling on the target."
  (when publication
    (let* ((session-root
            (niwa-session-control-fs-physical-path session-dir))
           (artifacts
            (mapcar
             (lambda (entry)
               (let ((relative (plist-get (cdr entry) :published))
                     (copy (copy-tree entry)))
                 (unless (niwa-session-publication--valid-published-path-p
                          relative)
                   (error "Invalid session publication artifact path: %s"
                          relative))
                 (setf (plist-get (cdr copy) :published)
                       (file-name-concat session-root relative))
                 copy))
             (plist-get publication :artifacts)))
           (sidecar (cdr (assoc "session.meta.el" artifacts))))
      (list :head (plist-get publication :head)
            :sidecar (plist-get sidecar :published)
            :artifacts artifacts))))

(defcustom niwa-session-publication-keep-recent-generations 3
  "How many newest published generations a collection always retains.

Collection keeps every settled-turn head, but a reader that resolved a
head moments before it stopped being current still needs its bytes: a
follower re-reads the owner's current head rather than pinning it.  This
grace window is what makes that race harmless in practice; it is not a
lock, and a reader holding an older non-boundary head can still lose it."
  :type 'integer
  :group 'niwa)

(defcustom niwa-session-publication-summary-scan-max 40
  "How many published sidecars a summary scan reads, newest first.

A sidecar carries the turn count and prompt index a turn-shaped picker
needs, and it is the largest artifact in a generation.  Generations past
this bound are summarized from their file facts alone."
  :type 'integer
  :group 'niwa)

(defun niwa-session-publication--generation-names (session-dir)
  "Return SESSION-DIR's published generation directory names, newest first."
  (let* ((root (file-name-concat session-dir ".publications"))
         (paths (niwa-session-control-fs-list-directory
                 root "\\`generation-[0-9a-f]+\\'")))
    (sort
     (delq nil
           (mapcar
            (lambda (path)
              (let* ((name (file-name-nondirectory
                            (directory-file-name path)))
                     (manifest (file-name-concat path "manifest.el"))
                     (attributes (file-attributes manifest)))
                (when attributes
                  (list :name name
                        :head (file-name-concat
                               ".publications" name "manifest.el")
                        :time (file-attribute-modification-time attributes)))))
            paths))
     (lambda (left right)
       ;; Target filesystems report coarse timestamps, so publishes inside
       ;; one second would otherwise order arbitrarily from run to run.
       ;; Any of them is an equally valid representative of the same
       ;; state; the name only makes the choice reproducible.
       (if (time-equal-p (plist-get left :time) (plist-get right :time))
           (string> (plist-get left :name) (plist-get right :name))
         (time-less-p (plist-get right :time) (plist-get left :time)))))))

(defun niwa-session-publication--manifest-references (manifest)
  "Return generation names MANIFEST resolves artifacts through.

A committed manifest carries unchanged entries forward verbatim, so it
points into the generations that first wrote those bytes.  Those
generations are as live as the head itself."
  (let (names)
    (dolist (entry (plist-get manifest :artifacts) (delete-dups names))
      (when-let* ((published (plist-get (cdr entry) :published))
                  ((string-match
                    "\\`\\.publications/\\(generation-[0-9a-f]+\\)/"
                    published)))
        (push (match-string 1 published) names)))))

(defun niwa-session-publication--sidecar-facts
    (session-dir published)
  "Return PUBLISHED sidecar's turn facts below SESSION-DIR, or nil.
An absent or unreadable sidecar has no facts.

The returned plist carries `:turn-count', `:segment', `:turn',
`:preview', and `:fork-point-id' for the head's latest prompt."
  (when-let*
      ((sidecar (and published (ignore-errors
                  (niwa-session-durability--read-plist
                   (niwa-session-publication--publication-path
                    session-dir published))))))
    (let (latest)
      (dolist (segment (plist-get sidecar :prompt-index))
        (dolist (prompt (cdr segment))
          (when (or (null latest)
                    (> (or (plist-get prompt :cum-turn) 0)
                       (or (plist-get latest :cum-turn) 0)))
            (setq latest (append prompt (list :segment (car segment)))))))
      (list :turn-count (or (plist-get sidecar :total-turn-count) 0)
            :segment (or (plist-get latest :segment)
                         (plist-get sidecar :current-segment))
            :turn (plist-get latest :turn)
            ;; The whole prompt travels with the summary so a picker can
            ;; label a published head exactly as it labels a live turn.
            :prompt latest
            :fork-point-id (plist-get latest :fork-point-id)))))

(defun niwa-session-publication-head-facts (session-dir head)
  "Return HEAD's turn facts, or nil when its publication is unreadable."
  (when-let* ((manifest
               (ignore-errors
                 (niwa-session-publication--read-publication-raw
                  session-dir head))))
    (niwa-session-publication--sidecar-facts
     session-dir
     (plist-get (cdr (assoc "session.meta.el" (plist-get manifest :artifacts)))
                :published))))

(defvar niwa-session-publication--facts-cache
  (make-hash-table :test #'equal)
  "Cache of published generation turn facts, keyed by generation path.

A committed generation is immutable, so its sidecar's answers cannot
change: caching them is sound, and it is what makes collection's
every-sidecar scan affordable.  Reading 752 uncached sidecars measured 17
seconds inside one turn settlement.  Entries live for this Emacs session
or until collection removes their generation.")

(defvar niwa-session-publication--generation-cache
  (make-hash-table :test #'equal)
  "Compact immutable generation observations, keyed by manifest path.

Entries hold only :references, :sidecar and :transcript-bytes.  Retaining a
complete manifest for every generation duplicates the artifact index across
history and keeps hundreds of megabytes alive.  This cache is only for
listing and collection; authority reads keep going through
`niwa-session-publication-read', which verifies bytes rather than
trusting a remembered plist.")

(defun niwa-session-publication--cached-generation (session-dir head)
  "Return independent compact observations of HEAD below SESSION-DIR.
Read and validate its manifest once, retaining only generation references,
the sidecar locator and first-transcript size.  Failed reads are not cached."
  (let ((key (niwa-session-publication--publication-path
              session-dir head)))
    ;; Retention joins and deduplicates these lists destructively.  Keep its
    ;; working references independent from the next observation's cache hit.
    (copy-tree
     (or (gethash key niwa-session-publication--generation-cache)
         (when-let* ((manifest
                     (ignore-errors
                       (niwa-session-publication--read-publication-raw
                        session-dir head))))
           (puthash
            key
            (list :references
                  (niwa-session-publication--manifest-references manifest)
                  :sidecar
                  (plist-get (cdr (assoc "session.meta.el"
                                         (plist-get manifest :artifacts)))
                             :published)
                  :transcript-bytes
                  (niwa-session-publication--transcript-bytes
                   session-dir manifest))
            niwa-session-publication--generation-cache))))))

(defun niwa-session-publication--cached-sidecar-facts
    (session-dir head generation)
  "Return HEAD's turn facts from compact GENERATION below SESSION-DIR."
  (let ((key (niwa-session-publication--publication-path
              session-dir head)))
    (or (gethash key niwa-session-publication--facts-cache)
        (when-let* ((facts (niwa-session-publication--sidecar-facts
                            session-dir (plist-get generation :sidecar))))
          (puthash key facts niwa-session-publication--facts-cache)
          facts))))

(defun niwa-session-publication-generation-summaries
    (session-dir &optional limit)
  "Return newest-first summaries of SESSION-DIR's published generations.

Each summary carries `:head', `:time', `:transcript-bytes', the manifest's
generation `:references', and -- for
the newest LIMIT generations, whose sidecars are read -- the turn facts
from `niwa-session-publication--sidecar-facts'.  LIMIT defaults to
`niwa-session-publication-summary-scan-max'; a sidecar is the largest
artifact in a generation, so scanning every one of them is a cost a
listing does not need to pay."
  (let ((limit (or limit niwa-session-publication-summary-scan-max))
        (index 0)
        summaries)
    (dolist (generation (niwa-session-publication--generation-names
                         session-dir)
                        (nreverse summaries))
      (push (niwa-session-publication-generation-summary
             session-dir generation (< index limit))
            summaries)
      (setq index (1+ index)))))

(defun niwa-session-publication-generation-summary
    (session-dir generation &optional with-facts)
  "Read one GENERATION's immutable summary below SESSION-DIR.
WITH-FACTS includes the sidecar's turn boundary; otherwise read only
its manifest."
  (let* ((head (plist-get generation :head))
         (observed (niwa-session-publication--cached-generation session-dir head)))
    (append (list :head head
                  :name (plist-get generation :name)
                  :time (plist-get generation :time)
                  :manifest-readable-p (and observed t)
                  :transcript-bytes
                  (plist-get observed :transcript-bytes)
                  :references
                  (plist-get observed :references))
            (and with-facts observed
                 (niwa-session-publication--cached-sidecar-facts
                  session-dir head observed)))))

(defun niwa-session-publication--transcript-bytes (session-dir manifest)
  "Return the byte size of MANIFEST's segment transcript, or nil."
  (when-let*
      ((entry (cdr (assoc "segment-0001.chat.org"
                          (plist-get manifest :artifacts))))
       (published (plist-get entry :published))
       (attributes
        (file-attributes (niwa-session-publication--publication-path
                          session-dir published))))
    (file-attribute-size attributes)))

(defun niwa-session-publication-settled-summary-p (summary)
  "Return non-nil when SUMMARY names a settled turn state.

A prompt is recorded when its turn is admitted and the turn count when
that turn settles, so a generation published while a response streamed
carries a latest prompt one turn ahead of its count.  Such a head
restores a half-arrived turn: it is the state a crashed owner resumes
from, never a state anyone chooses."
  (let ((turn-count (plist-get summary :turn-count))
        (prompt (plist-get summary :prompt)))
    (and turn-count
         (or (null prompt)
             (= (or (plist-get prompt :cum-turn) 0) turn-count)))))

(defun niwa-session-publication--retained-heads (summaries)
  "Return heads to retain from a complete newest-first SUMMARIES scan.
Keep one head per settled turn/fork state, the recent reader grace window,
and every head whose turn facts could not be read."
  (let ((seen (make-hash-table :test #'equal))
        (index 0)
        heads)
    (dolist (summary summaries)
      (let* ((settled (niwa-session-publication-settled-summary-p summary))
             (turn-count (plist-get summary :turn-count))
             (key (and settled
                       (list turn-count (plist-get summary :fork-point-id)))))
        (when (or (< index niwa-session-publication-keep-recent-generations)
                  (null turn-count)
                  (and settled (not (gethash key seen))))
          (when key (puthash key t seen))
          (push (plist-get summary :head) heads))
        (setq index (1+ index))))
    (nreverse heads)))

(defun niwa-session-publication-collection-plan (session summaries)
  "Plan file-level collection for SESSION from its complete SUMMARIES scan.
Retained heads are marked one at a time by
`niwa-session-publication-collect-step'.
No files are deleted until every retained manifest has been validated."
  (let* ((directory (niwa-session-save-path session))
         (head (plist-get (niwa-session-publication session) :head))
         (pins (niwa-journal-pins-observe directory)))
    (unless head (error "Collection requires a current publication"))
    (let ((heads (delete-dups
                  (append (list head) (car pins)
                          (niwa-session-publication--retained-heads summaries))))
          (candidates (mapcar (lambda (summary) (plist-get summary :head)) summaries))
          (known (make-hash-table :test #'equal)))
      (dolist (known-head (append heads candidates)) (puthash known-head t known))
      (list :head head :pin-proof (cdr pins)
            :root (file-name-as-directory
                   (niwa-session-control-fs-physical-path directory))
            :heads heads :retained-count (length heads)
            :keep (make-hash-table :test #'equal)
            :known known
            :marked nil :directories nil :operations nil
            :candidates candidates
            :deleted-directories 0 :deleted-files 0))))

(defun niwa-session-publication--collection-follow-head (session plan directory)
  "Queue SESSION's heads published since PLAN for marking, from DIRECTORY.
Only this lease owner publishes, and every generation it adds carries its
manifest, so heads unknown to PLAN are exactly the new ones.  They are
marked before any further listing or deletion; nothing new is ever a
deletion candidate."
  (dolist (generation (niwa-session-publication--generation-names directory))
    (let ((head (plist-get generation :head)))
      (unless (gethash head (plist-get plan :known))
        (puthash head t (plist-get plan :known))
        (setf (plist-get plan :heads) (cons head (plist-get plan :heads))))))
  (setf (plist-get plan :head)
        (plist-get (niwa-session-publication session) :head)))

(defun niwa-session-publication-collect-step (session plan &optional callback)
  "Advance SESSION's collection PLAN by one bounded step.
Read one retained manifest, enumerate up to eight generations, or delete
up to eight files/directories.  Return non-nil while work remains.  A moved
head adds its new manifests to the retained set before any further step.
A plan survives a session that keeps publishing; errors propagate to the idle
collection owner, which reports them and discards the plan.

A deleting step is one program that proves this client's lease generation,
its unexpired deadline on the target clock, and the unchanged journal pin set
before it deletes, through
`niwa-session-durability-run-owned-program-async'.  Where that program runs
in the background this returns `pending' and CALLBACK later receives ERROR,
nil once PLAN has advanced; the callback can run inside another remote
command.  A failed proof ends PLAN quietly, since collection is rescheduled
from a fresh plan.  Without CALLBACK the program runs synchronously and its
failure signals here."
  (let ((directory (niwa-session-save-path session))
        (root (plist-get plan :root))
        (keep (plist-get plan :keep)))
    ;; Local state is checked before every step.  The target proofs guard
    ;; deletion, so only the program that deletes carries them; reading a
    ;; manifest or listing a generation changes nothing they would miss.
    (unless (and (niwa-session-codec-portable-authority-p session)
                 (not (plist-get plan :in-flight))
                 (not (niwa-session-pending-publication session))
                 (null (niwa-session-publication-uncommitted-batches session))
                 (null (niwa-session-publication-queue session))
                 (not (niwa-session-publication-active-p session))
                 (plist-get (niwa-session-publication session) :head)
                 (equal root (file-name-as-directory
                              (niwa-session-control-fs-physical-path directory))))
      (error "Publication collection ownership or retained sources changed"))
    (unless (equal (plist-get plan :head)
                   (plist-get (niwa-session-publication session) :head))
      (niwa-session-publication--collection-follow-head session plan directory))
    ;; A deletion batch planned before a newly marked head must not remove
    ;; what that head references.
    (setf (plist-get plan :operations)
          (seq-remove (lambda (operation)
                        (let ((path (plist-get operation :path)))
                          (gethash (if (eq (plist-get operation :op) 'delete-directory)
                                       (file-name-as-directory path)
                                     path)
                                   keep)))
                      (plist-get plan :operations)))
    (cond
     ((plist-get plan :heads)
      (let* ((head (car (plist-get plan :heads)))
             (manifest (niwa-session-publication--read-publication-raw directory head)))
        (unless manifest (error "Retained publication is unreadable: %s" head))
        ;; Validation fixes every path below .publications, so qualification
        ;; needs no repeated file-handler dispatch for thousands of entries.
        (dolist (relative (cons head (mapcar (lambda (entry)
                                              (plist-get (cdr entry) :published))
                                            (plist-get manifest :artifacts))))
          (let ((path (concat root relative)))
            (puthash path t keep)
            (puthash (file-name-directory path) t keep)))
        (setf (plist-get plan :heads) (cdr (plist-get plan :heads))))
      t)
     ((not (plist-get plan :marked))
      ;; Artifact-only directories have no manifest and are absent from the
      ;; history scan.  They still need collection when their last user goes.
      (setf (plist-get plan :directories)
            (niwa-session-control-fs-list-directory
             (concat root ".publications") "\\`generation-[0-9a-f]\\{20\\}\\'")
            (plist-get plan :operations)
            (mapcar (lambda (head) (list :op 'delete-file :path (concat root head)))
                    (seq-remove (lambda (head) (gethash (concat root head) keep))
                                (plist-get plan :candidates)))
            (plist-get plan :marked) t)
      t)
     ((plist-get plan :operations)
      (let ((operations (seq-take (plist-get plan :operations) 8))
            (expires (plist-get (niwa-session-lease session) :expires-at)))
        (unless (numberp expires)
          (error "Publication collection requires an owned lease"))
        ;; Retire ALL obsolete heads before any payloads, including across
        ;; generations.  Evict observations before the program: it can fail
        ;; after completing only some required operations.
        (dolist (operation operations)
          (let ((path (plist-get operation :path)))
            (when (or (eq (plist-get operation :op) 'delete-directory)
                      (equal (file-name-nondirectory path) "manifest.el"))
              (let ((key (if (eq (plist-get operation :op) 'delete-directory)
                             (file-name-concat path "manifest.el") path)))
                (remhash key niwa-session-publication--generation-cache)
                (remhash key niwa-session-publication--facts-cache)))))
        (let ((proofs (cons (list :op 'before-time :path (directory-file-name root)
                                  :content (number-to-string (floor expires)))
                            (plist-get plan :pin-proof)))
              failure)
          (setf (plist-get plan :in-flight) t)
          (let ((niwa-session-control-fs--async-wait
                 (or niwa-session-control-fs--async-wait (null callback))))
            (niwa-session-durability-run-owned-program-async
           session (append proofs operations)
           (lambda (results error)
             (setf (plist-get plan :in-flight) nil)
             (condition-case err
                 (cond
                  (error (signal (car error) (cdr error)))
                  ((not (and results
                             (cl-every (lambda (result) (eq 'ok (plist-get result :status)))
                                       (seq-take results (length proofs)))))
                   ;; Another owner, a lapsing lease, or a new pin: nothing
                   ;; was deleted, and the next schedule plans afresh.
                   (setf (plist-get plan :operations) nil
                         (plist-get plan :directories) nil
                         (plist-get plan :proof-failed) t))
                  (t
                   (mapc #'niwa-session-control-fs-program-value
                         (nthcdr (length proofs) results))
                   (dolist (operation operations)
                     (cl-incf (plist-get plan (if (eq (plist-get operation :op) 'delete-directory)
                                                 :deleted-directories :deleted-files))))
                   (setf (plist-get plan :operations)
                         (nthcdr (length operations) (plist-get plan :operations)))))
               (error (setq failure err)))
             (if callback
                 (funcall callback failure)
               (when failure (signal (car failure) (cdr failure)))))))
          (if (plist-get plan :in-flight) 'pending t))))
     ((plist-get plan :directories)
      (let ((path (car (plist-get plan :directories))))
        (if (not (gethash (file-name-as-directory path) keep))
            ;; Wholly unreferenced generations need no listing, so up to a
            ;; batch of them share one deletion step and its proofs.  Obsolete
            ;; heads are already retired, so payload order does not matter.
            (let ((unreferenced
                   (seq-take (seq-remove (lambda (directory)
                                           (gethash (file-name-as-directory directory)
                                                    keep))
                                         (plist-get plan :directories))
                             8)))
              (setf (plist-get plan :operations)
                    (mapcar (lambda (directory)
                              (list :op 'delete-directory :path directory))
                            unreferenced)
                    (plist-get plan :directories)
                    (seq-difference (plist-get plan :directories) unreferenced)))
          ;; Most retained directories hold only a few immutable artifacts.
          ;; Listing each in its own target process made an otherwise clean
          ;; real session spend half a second rescanning them per job.
          (let* ((directories
                  (seq-take (seq-filter
                             (lambda (directory)
                               (gethash (file-name-as-directory directory) keep))
                             (plist-get plan :directories)) 8))
                 (results (niwa-session-control-fs-run-program
                           (mapcar (lambda (directory)
                                     (list :op 'list-directory :path directory :optional t))
                                   directories)))
                 operations)
            (cl-mapc
             (lambda (directory result)
               (unless (eq (plist-get result :status) 'absent)
                 (dolist (name (niwa-session-control-fs-program-value result))
                   (let ((file (file-name-concat directory name)))
                     (unless (gethash file keep)
                       (push (list :op 'delete-file :path file) operations))))))
             directories results)
            (setf (plist-get plan :operations) (nreverse operations)
                  (plist-get plan :directories)
                  (seq-difference (plist-get plan :directories) directories)))))
      t))))

(defun niwa-session-publication-read-batch (directories listings)
  "Observe current publications for DIRECTORIES using fresh lease LISTINGS.
Return an alist from directory to (:publication VALUE :sidecar-data PLIST),
with :error CONDITION on failure.  A captured manifest is still returned
when its sidecar fails, for callers that need the sidecar path.
Each stage batches pinned reads across directories;
lease, manifest and sidecar validation remain the ordinary native checks.
LISTINGS must be observations from this discovery, never a retained cache."
  (let ((remote-file-name-inhibit-cache t)
        rows paths)
    (dolist (directory directories)
      (let ((row (list :directory directory)))
        (condition-case err
            (progn
              (niwa-session-durability--assert-no-pid-lock directory)
              (plist-put row :paths
                         (niwa-session-durability--generation-paths
                          (niwa-session-durability--lease-path directory)
                          (cdr (assoc directory listings)))))
          (error (plist-put row :error err)))
        (push row rows)))
    (setq rows (nreverse rows)
          paths (mapcan (lambda (row) (copy-sequence (plist-get row :paths))) rows))
    (let ((records (niwa-session-durability--read-records paths)))
      (dolist (row rows)
        (let* ((count (length (plist-get row :paths)))
               (row-records (seq-take records count)))
          (setq records (nthcdr count records))
          (unless (plist-get row :error)
            (condition-case err
                (let ((lease
                       (niwa-session-durability--resolve-lease-head
                        (niwa-session-durability--lease-path
                         (plist-get row :directory))
                        (plist-get row :paths) row-records)))
                  (when lease
                    (unless (niwa-session-durability--valid-lease-p lease)
                      (error "Invalid portable session lease"))
                    (when-let* ((head (plist-get lease :publication-head)))
                      (plist-put row :head head)
                      (plist-put row :manifest
                                 (niwa-session-publication--publication-path
                                  (plist-get row :directory) head)))))
              (error (plist-put row :error err)))))))
    (let* ((selected (seq-filter (lambda (row) (plist-get row :manifest)) rows))
           (manifests (niwa-session-durability--read-records
                       (mapcar (lambda (row) (plist-get row :manifest)) selected))))
      (dolist (row selected)
        (let ((manifest (pop manifests)))
          (condition-case err
              (progn
                (niwa-session-publication--validate-manifest
                 manifest (plist-get row :manifest))
                (plist-put row :publication
                           (niwa-session-publication--capture-publication
                            (plist-get row :directory)
                            (list :head (plist-get row :head)
                                  :sidecar (plist-get manifest :sidecar)
                                  :artifacts (plist-get manifest :artifacts)))))
            (error (plist-put row :error err))))))
    (let* ((selected (seq-filter (lambda (row) (plist-get row :publication)) rows))
           (results
            (when selected
              (niwa-session-control-fs-run-program
               (mapcar (lambda (row)
                         (list :op 'read :coding 'no-conversion :optional t
                               :path (plist-get (plist-get row :publication) :sidecar)))
                       selected)))))
      (dolist (row selected)
        (let ((result (pop results)))
          (condition-case err
              (let* ((bytes (niwa-session-control-fs-program-value result))
                     (artifact (cdr (assoc "session.meta.el"
                                           (plist-get (plist-get row :publication) :artifacts)))))
                (unless (equal (secure-hash 'sha256 bytes) (plist-get artifact :sha256))
                  (error "Invalid session publication sidecar"))
                (plist-put row :sidecar-data
                           (car (read-from-string (decode-coding-string bytes 'utf-8-unix)))))
            (error (plist-put row :error err))))))
    (mapcar (lambda (row)
              (cons (plist-get row :directory)
                    (list :publication (plist-get row :publication)
                          :sidecar-data (plist-get row :sidecar-data)
                          :error (plist-get row :error)))) rows)))

(defun niwa-session-publication-read (session-dir &optional head names)
  "Return SESSION-DIR's captured immutable publication, or nil.

HEAD reads that published generation instead of the session's current
lease head; every published head remains immutable, so an older one reads
exactly as it was committed.

NAMES, when non-nil, is a listing of SESSION-DIR's lease directory the
caller already observed, and spares this read the list round trip.  Only a
caller that took the observation for this read may supply it; enumerating a
workspace takes all of them in one program.

Manifest structure and control paths are validated uncached.  Only the
sidecar bytes are verified eagerly; consumers verify other artifact hashes
when they read those artifacts."
  (let* ((remote-file-name-inhibit-cache t)
         (_ (niwa-session-durability--assert-no-pid-lock session-dir))
         (publication
          (niwa-session-publication--read-publication-raw
           session-dir head names))
         (captured
          (niwa-session-publication--capture-publication
           session-dir publication)))
    (when captured
      (let* ((sidecar (plist-get captured :sidecar))
             (entry (cdr (assoc "session.meta.el"
                                (plist-get captured :artifacts))))
             (expected (plist-get entry :sha256)))
        (unless (and (file-regular-p sidecar)
                     (equal expected
                            (niwa-session-publication--file-sha256 sidecar)))
          (error "Invalid session publication sidecar: %s" sidecar))))
    captured))

(defun niwa-session-publication--write-staged-content (source content coding)
  "Write CONTENT to local staged file SOURCE as exactly its encoded bytes.
A unibyte CONTENT is written unconverted, a multibyte one through CODING.
The string goes straight to `write-region' from a fundamental temporary
buffer, so no live buffer's locals, point or annotation hooks can alter the
captured bytes."
  (with-temp-buffer
    (let ((coding-system-for-write
           (if (multibyte-string-p content) (or coding 'utf-8-unix) 'no-conversion))
          (write-region-annotate-functions nil)
          (write-region-post-annotation-function nil))
      (write-region content nil source nil 'silent))))

(defun niwa-session-publication--stage-artifacts
    (session artifacts &optional defer-digest)
  "Copy SESSION publication ARTIFACTS into one local recovery batch.
With DEFER-DIGEST no digest is computed here, even for raw bytes: the caller
hashes the staged files itself.

Each input artifact is a plist containing `:path', `:content', and optional
`:coding'; `:path' and a `:source-file' holding its raw bytes; or `:path' and
`:delete t'.  Other metadata is preserved.  Return a
batch whose written artifacts name local `:source' files."
  (let ((directory (make-temp-file "niwa-publication-" t))
        staged
        (marker-count 0))
    (condition-case err
        (progn
          (cl-loop
           for artifact in artifacts
           for index from 1
           for normalized =
           (niwa-session-publication--artifact-for-session session artifact)
           for target = (plist-get normalized :path)
           for content = (plist-get normalized :content)
           for source-file = (plist-get normalized :source-file)
           for source = (file-name-concat directory (format "%06d" index))
           do
           (unless (and (stringp target)
                        (or (plist-get normalized :delete)
                            (stringp content)
                            (and (stringp source-file) (file-regular-p source-file))))
             (error "Publication artifact requires string path and content"))
           (when (plist-get normalized :commit-marker)
             (cl-incf marker-count))
           (cond
            ((plist-get normalized :delete))
            ;; A file-backed artifact is copied as bytes, never read into Lisp.
            (source-file (copy-file source-file source t))
            (t
             ;; Raw bytes hash now, so the generation need not reread them.
             (unless (or defer-digest (multibyte-string-p content))
               (setq normalized
                     (plist-put normalized :source-sha256 (secure-hash 'sha256 content))))
             (niwa-session-publication--write-staged-content
              source content (plist-get normalized :coding))))
           (setq normalized
                 (cl-loop for (key value) on normalized by #'cddr
                          unless (memq key '(:content :source-file))
                          append (list key value))
                 normalized (if (plist-get normalized :delete)
                                normalized
                              (plist-put normalized :source source)))
           (push normalized staged))
          (when (> marker-count 1)
            (error "Publication requires exactly one commit marker"))
          (list :directory directory :artifacts (nreverse staged)
                :diagnostic-scope (niwa-diagnostic-scope session)))
      ((error quit)
       (when (file-directory-p directory)
         (delete-directory directory t))
       (signal (car err) (cdr err))))))

(defun niwa-session-publication--batch-marker (batch)
  "Return BATCH's commit-marker artifact, or nil."
  (cl-find-if
   (lambda (artifact) (plist-get artifact :commit-marker))
   (plist-get batch :artifacts)))

(defun niwa-session-publication--deduplicate-artifacts (artifacts)
  "Return session-local ARTIFACTS in last-write-wins order."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (artifact (reverse artifacts))
      (when-let* ((logical (plist-get artifact :logical)))
        (unless (gethash logical seen)
          (puthash logical t seen)
          (push artifact result))))
    result))

(defun niwa-session-publication--raw-artifacts (publication session-dir)
  "Return captured PUBLICATION artifacts relative to SESSION-DIR."
  (mapcar
   (lambda (entry)
     (let ((copy (copy-tree entry)))
       (setf (plist-get (cdr copy) :published)
             (file-relative-name
              (plist-get (cdr entry) :published) session-dir))
       copy))
   (plist-get publication :artifacts)))

(defun niwa-session-publication--generation-name ()
  "Return a fresh unique immutable generation directory name."
  (format "generation-%s"
          (substring
           (secure-hash 'sha256
                        (format "%S" (list (current-time)
                                           (random most-positive-fixnum))))
           0 20)))

(defun niwa-session-publication--immutable-entry
    (directory artifact index session-dir)
  "Return ARTIFACT number INDEX's manifest entry and bytes below DIRECTORY.

The digest is taken from the staged source, which is what the manifest
records, and the source file is returned for the caller to write.  Neither
becomes a Lisp string: a local target copies the file, and a live segment
was otherwise read back and copied whole on every save."
  (let ((source (plist-get artifact :source))
        (target (file-name-concat directory (format "%06d.data" index))))
    (list :path target
          :source-file source
          :entry (list (plist-get artifact :logical)
                       :published (file-relative-name target session-dir)
                       :sha256 (or (plist-get artifact :source-sha256)
                                   (niwa-session-publication--file-sha256 source))))))

(defun niwa-session-publication--generation-digests (artifacts)
  "Return a table from ARTIFACTS' local sources to their SHA-256 digests.
Only sources without a known digest are hashed."
  (let* ((pending (cl-loop for artifact in artifacts
                           unless (or (plist-get artifact :delete)
                                      (plist-get artifact :source-sha256))
                           collect (plist-get artifact :source)))
         (table (make-hash-table :test #'equal)))
    (cl-mapc (lambda (source digest) (puthash source digest table))
             pending
             (niwa-session-publication--files-sha256 pending))
    table))

(defun niwa-session-publication--prepare-generation
    (root artifacts session-dir entries digests)
  "Plan one fresh generation below ROOT holding ARTIFACTS and its manifest.

Return (DIRECTORY OVERLAID OPERATIONS): the generation directory, the
overlaid manifest ENTRIES, and the program that writes it.  DIGESTS maps
local sources to their SHA-256 values.  The directory, every immutable
artifact, and the manifest land inside the single directory the program
creates, so one pinned parent covers the whole generation.  The manifest is
its last operation, which keeps it the last byte written inside the
generation.  A name that already exists ends the program before anything is
written: the second operation reports `conflict', and a caller picks
another name."
  (let* ((directory
          (file-name-as-directory
           (file-name-concat
            root (niwa-session-publication--generation-name))))
         ;; Callers may stage the same transcript repeatedly, including
         ;; across queued batches.  Compare the final bytes here, where
         ;; last-write-wins and replacement semantics are already known.
         (previous (make-hash-table :test #'equal))
         (changed-artifacts
          (progn
            (dolist (entry entries)
              (puthash (car entry) (plist-get (cdr entry) :sha256) previous))
            (seq-filter
             (lambda (artifact)
               (let ((old (gethash (plist-get artifact :logical) previous)))
                 (if (plist-get artifact :delete)
                     old
                   (not (equal old (or (plist-get artifact :source-sha256)
                                       (gethash (plist-get artifact :source)
                                                digests)))))))
             artifacts)))
         (payloads
          (cl-loop for artifact in changed-artifacts
                   for index from 1
                   unless (plist-get artifact :delete)
                   collect (niwa-session-publication--immutable-entry
                            directory
                            (if (plist-get artifact :source-sha256)
                                artifact
                              (append (list :source-sha256
                                            (gethash (plist-get artifact :source)
                                                     digests))
                                      artifact))
                            index session-dir)))
         (changed (make-hash-table :test #'equal))
         overlaid
         manifest manifest-path)
    (dolist (artifact changed-artifacts)
      (puthash (plist-get artifact :logical) t changed))
    (setq overlaid
          (append
           (cl-remove-if (lambda (entry) (gethash (car entry) changed)) entries)
           (mapcar (lambda (payload) (plist-get payload :entry)) payloads))
          manifest (list :sidecar "session.meta.el" :artifacts overlaid)
          manifest-path (file-name-concat directory "manifest.el"))
    (niwa-session-publication--validate-manifest manifest manifest-path)
    (list directory overlaid
          (append
           ;; The generations root may already exist; the generation
           ;; itself must not, which is the collision signal.
           (list (list :op 'make-directory
                       :path (directory-file-name
                              (file-name-as-directory root))
                       :optional t)
                 (list :op 'make-directory
                       :path (directory-file-name directory)))
           (mapcar (lambda (payload)
                     (list :op 'create
                           :path (plist-get payload :path)
                           :source-file (plist-get payload :source-file)))
                   payloads)
           (list (list :op 'create :path manifest-path
                       :content
                       (niwa-session-durability--record-bytes
                        manifest)))))))

(defun niwa-session-publication--commit-marker-publication
    (session batches marker)
  "Commit retained and current BATCHES for SESSION at sidecar MARKER."
  (let* ((session-dir
          (file-name-as-directory (niwa-session-save-path session)))
         (previous
          (or (niwa-session-publication session)
              (niwa-session-publication-read session-dir)))
         (entries
          (unless (plist-get marker :replace)
            (niwa-session-publication--raw-artifacts
             previous session-dir)))
         (artifacts
          (niwa-session-publication--deduplicate-artifacts
           (apply #'append
                  (mapcar
                   (lambda (batch) (plist-get batch :artifacts))
                   batches))))
         (sidecar (cl-find "session.meta.el" artifacts
                           :key (lambda (artifact) (plist-get artifact :logical))
                           :test #'equal))
         (checkpoint (plist-get sidecar :checkpoint))
         ;; A replacement without a transcript candidate adopts another
         ;; history.  Read and validate its frozen bytes before committing,
         ;; never derive its root projection from the current live session.
         (replacement
          (when (and (plist-get marker :replace) (not checkpoint))
            (niwa-session-codec-validate-current-sidecar
             (niwa-session-publication--read-staged-sidecar
              (plist-get sidecar :source)))))
         (root (file-name-concat session-dir ".publications")))
    (when (and checkpoint
               (not (niwa-session-checkpoint-valid-p session checkpoint)))
      (error "Checkpoint candidate belongs to a different owner epoch"))
    ;; A queued metadata or agent sidecar can predate a root commit earlier
    ;; in this drain.  Refresh only its checkpoint projection from the owner,
    ;; never from live root fields.  Explicit replacements adopt their own
    ;; history instead.  Blob-only publication has no checkpoint owner.
    (when (and (not replacement) (not (plist-get marker :replace))
               (niwa-session-checkpoint-state session))
      (let* ((source (plist-get sidecar :source))
             (raw (niwa-session-codec-validate-current-sidecar
                   (niwa-session-publication--read-staged-sidecar source)))
             (projected (niwa-session-checkpoint-project session raw checkpoint)))
        (unless (equal raw projected)
          (niwa-session-codec-write source projected)
          (setf (plist-get sidecar :source-sha256) nil))))
    (niwa-session-control-fs-physical-path root)
    (let ((digests (niwa-session-publication--generation-digests artifacts)))
      (catch 'committed
        (dotimes (_ 8)
          ;; Unchanged entries keep their existing immutable payloads.
          (pcase-let* ((`(,directory ,overlaid ,operations)
                        (niwa-session-publication--prepare-generation
                         root artifacts session-dir entries digests))
                       (manifest-path (file-name-concat directory "manifest.el"))
                       (head (file-relative-name manifest-path session-dir))
                       (captured
                        (niwa-session-publication--capture-publication
                         session-dir (list :head head
                                           :sidecar "session.meta.el"
                                           :artifacts overlaid)))
                       (committed nil))
            (when (and checkpoint
                       (not (niwa-session-checkpoint-valid-p session checkpoint)))
              (error "Checkpoint candidate belongs to a different owner epoch"))
            ;; The generation is the head commit's prelude, in its own program:
            ;; the same operations in the same order, one process fewer.
            (setq committed
                  (condition-case err
                      (let ((niwa-session-durability--commit-riders
                             (list :before operations :after nil :consumed nil
                                   :before-results nil :after-results nil)))
                        (if (niwa-session-durability-commit-publication-head
                             session head)
                            'committed
                          'lost))
                    (niwa-session-durability-prelude-failed
                     (let ((results (cadr err)))
                       (cond
                        ;; A taken generation name simply picks another one.
                        ((eq 'conflict (plist-get (nth 1 results) :status)) 'retry)
                        ;; A lease deadline passed while the generation was
                        ;; written, see `niwa-session-durability--commit-lease'.
                        ;; The generation is complete; commit its head afresh,
                        ;; from a new clock reading, which fails closed only
                        ;; once the held window itself has lapsed.
                        ((eq 'mismatch
                             (plist-get (nth (length operations) results) :status))
                         (if (niwa-session-durability-commit-publication-head
                              session head)
                             'committed
                           'lost))
                        (t
                         ;; The optional root ensure may report a conflict.
                         (dolist (result (cdr results))
                           (unless (eq 'ok (plist-get result :status))
                             (niwa-session-control-fs-program-value result)))
                         (error "Session publication generation was not written")))))))
            (pcase committed
              ('retry nil)
              ('lost
               (user-error "Portable session lease was lost before publication commit"))
              (_
               (setf (niwa-session-publication session) captured)
               (cond
                (checkpoint (niwa-session-checkpoint-install session checkpoint))
                (replacement (niwa-session-checkpoint-seed session replacement)))
               ;; A first root turn can run for hours while agents checkpoint.
               ;; Waiting for root settlement lets those superseded transcripts
               ;; grow unbounded.
               (niwa-session-collection-schedule session)
               (throw 'committed captured)))))
        (error "Could not create a fresh session publication directory: %s"
               root)))))

(defun niwa-session-publication-uncommitted-artifact (session logical)
  "Return SESSION's newest retained or queued local source for LOGICAL.

Return nil when no uncommitted write exists.  This operation never consults
the target's fixed cache or mutates the publication accumulator."
  (unless (niwa-session-publication-logical-path-p logical)
    (error "Invalid session publication path: %s" logical))
  (catch 'source
    (dolist
        (batch
         (reverse
          (append
           (niwa-session-publication-uncommitted-batches session)
           (niwa-session-publication-queue session))))
      (dolist (artifact (reverse (plist-get batch :artifacts)))
        (when (equal logical (plist-get artifact :logical))
          (if (plist-get artifact :delete)
              (throw 'source nil)
            (let ((source (plist-get artifact :source)))
              (when (and source (file-exists-p source))
                (throw 'source source)))))))))

(defvar niwa-session-publication--ensured-directories nil
  "Cons cell collecting directories this publication created, or nil.

Every artifact write ensures its own parent, and a publication writes several
artifacts into the same few directories.  Re-creating a directory that this
transaction already created is one target process for a known answer.  A
caller that spans one transaction binds this to a fresh `(list nil)'; outside
such a binding every call reaches the target.")

(defvar niwa-session-publication--prewritten nil
  "A list (ARTIFACT RIDERS) whose fixed write rode the publication's reservation.
RIDERS are the reservation commit's riders, see
`niwa-session-durability--commit-riders'; their after-results answer
ARTIFACT's write.  The first `niwa-session-publication--publish-artifact'
for ARTIFACT consumes the entry.")

(defun niwa-session-publication--prewrite-operations (session artifact)
  "Return the operations writing fixed ARTIFACT behind SESSION's reservation.

They follow the reservation's lease write and listing in its program, as
the separate write program did, with nothing written in between: the
reservation is still the ownership proof before the first fixed write.  A
leading `verify-latest' stops them when a newer lease generation exists,
which the reservation's listing would otherwise have shown Lisp before the
write; the caller then writes as before, after its own decision.  Return
nil when SESSION has no bound generation to compare against."
  (let ((generation (plist-get (niwa-session-lease session) :generation))
        (session-dir (niwa-session-save-path session))
        (path (plist-get artifact :path)))
    (when (and (natnump generation) session-dir path (plist-get artifact :source))
      (let ((directory (file-name-directory (expand-file-name path))))
        (niwa-session-control-fs-physical-path directory)
        (list (list :op 'verify-latest
                    :path (niwa-session-durability--generation-path
                           (niwa-session-durability--lease-path session-dir)
                           generation)
                    :content ".el")
              (list :op 'make-directory
                    :path (directory-file-name directory)
                    :optional t)
              (list :op 'write
                    :path (niwa-session-control-fs-physical-path path)
                    :source-file (plist-get artifact :source)))))))

(defun niwa-session-publication--publish-artifact (artifact)
  "Publish staged ARTIFACT through a nearby target temporary file.

Ensuring the parent and writing the bytes are one target process.  The ensure
is optional so an existing directory does not end the program, which is what
lets both share one round trip.  That single-level ensure cannot create a
chain of missing parents -- each pinned operation needs its own live parent
directory -- so a write the chain refuses builds the chain component by
component and retries once."
  (let* ((path (plist-get artifact :path))
         (source (plist-get artifact :source))
         (directory (file-name-directory (expand-file-name path)))
         (ensure (not (member directory
                              (car niwa-session-publication--ensured-directories))))
         (prewritten
          (when (eq artifact (car niwa-session-publication--prewritten))
            (prog1 (plist-get (cadr niwa-session-publication--prewritten)
                              :after-results)
              (setcar niwa-session-publication--prewritten nil))))
         (write-operation
          (list :op 'write
                :path (niwa-session-control-fs-physical-path path)
                :source-file source))
         result)
    (niwa-session-control-fs-physical-path directory)
    (setq result
          (if (eq 'ok (plist-get (nth 2 prewritten) :status))
              ;; The reservation's program already ensured and wrote it.
              (nth 2 prewritten)
            (car (last (niwa-session-control-fs-run-program
                        (append
                         (when ensure
                           (list (list :op 'make-directory
                                       :path (directory-file-name directory)
                                       :optional t)))
                         (list write-operation)))))))
    (when (eq 'absent (plist-get result :status))
      (niwa-session-control-fs-make-directory
       (directory-file-name directory) t)
      (setq result
            (car (niwa-session-control-fs-run-program
                  (list write-operation)))))
    (when (and ensure niwa-session-publication--ensured-directories)
      (push directory
            (car niwa-session-publication--ensured-directories)))
    (niwa-session-control-fs-program-value result)))

(defun niwa-session-publication--fixed-artifact-p (artifact)
  "Return non-nil when ARTIFACT is also written to its fixed target file.

Transcripts and file history are read through the
immutable publication (or owned staging before its marker), never fixed
files.  The small discovery sidecar and ordinary artifact files keep their
fixed path."
  (not (or (plist-get artifact :delete)
           (when-let* ((logical (plist-get artifact :logical)))
             (or (string-prefix-p "file-history/" logical)
                 (and (string-match-p
                       "\\`\\(?:segment-[0-9]+\\|agents/.+\\)\\.chat\\.org\\'"
                       logical)
                      ;; Numbered archives are crash recovery files
                      ;; written before the live compaction rewrite.
                      (not (string-match-p
                            "\\.compact-[0-9]+\\.chat\\.org\\'" logical))))))))

(defvar niwa-session-publication--proved nil
  "Cons cell whose car is non-nil while ownership was proved since a write.

A drain binds it.  A lease commit that just proved ownership makes the next
artifact write's own proof redundant: no target write happened in between.
Every artifact write clears it, so the next write, and the end of a batch
that does not commit, prove ownership again.")

(defun niwa-session-publication--prove-ownership (session)
  "Prove SESSION still owns its reserved lease, unless that was just proved."
  (unless (car niwa-session-publication--proved)
    (unless (niwa-session-durability--renew-publication-lease session)
      (user-error "Portable session lease was lost during publication"))
    (when niwa-session-publication--proved
      (setcar niwa-session-publication--proved t))))

(defun niwa-session-publication--publish-batch (session batch)
  "Publish every staged artifact in BATCH while SESSION remains owner."
  (let (wrote)
    (dolist (artifact (plist-get batch :artifacts))
      (when (niwa-session-publication--fixed-artifact-p artifact)
        (niwa-session-publication--prove-ownership session)
        (niwa-session-publication--publish-artifact artifact)
        (setq wrote t)
        (when niwa-session-publication--proved
          (setcar niwa-session-publication--proved nil))))
    ;; Ownership is proved immediately before every write and once after the
    ;; last one.  A batch that commits proves it again through the head
    ;; commit's own exact-generation check, which follows the last write with
    ;; only the immutable generation in between.
    (when (and wrote
               (not (niwa-session-publication--batch-marker batch)))
      (niwa-session-publication--prove-ownership session)))
  batch)

(defun niwa-session-publication--drain (session batches &optional proved)
  "Publish BATCHES and any batches queued reentrantly for SESSION.
PROVED is non-nil when the caller's last lease commit just proved ownership,
as the reservation that opens a publication does."
  (let ((remaining batches)
        (niwa-session-publication--proved (list proved))
        (niwa-session-publication--ensured-directories (list nil))
        ;; One drain is one transaction: the per-artifact lease renewals
        ;; inside it share the clock reading and the pid-lock assertions,
        ;; which is what lets a renewal assume instead of re-observing.
        ;; Read tolerantly: the durability module loads lazily.
        (niwa-session-durability--transaction-clock
         (or (bound-and-true-p niwa-session-durability--transaction-clock)
             (list nil)))
        (niwa-session-durability--asserted-directories
         (or (bound-and-true-p
              niwa-session-durability--asserted-directories)
             (list nil)))
        current
        committed
        result)
    (condition-case err
        (progn
          (while (or remaining
                     (niwa-session-publication-queue session))
            (unless remaining
              (setq remaining
                    (prog1 (niwa-session-publication-queue session)
                      (setf (niwa-session-publication-queue session) nil))))
            (setq current (pop remaining))
            ;; A final commit returned the lease to `active'; work that
            ;; queued afterwards reserves the window again before writing.
            (when (eq 'active (plist-get (niwa-session-lease session) :status))
              (unless (niwa-session-durability--renew-publication-lease session)
                (user-error "Portable session lease could not reserve publication"))
              (setcar niwa-session-publication--proved t))
            (niwa-session-publication--publish-batch session current)
            (if-let* ((marker
                      (niwa-session-publication--batch-marker current)))
                (let ((transaction
                       (append
                        (niwa-session-publication-uncommitted-batches
                         session)
                        (list current)))
                      (publication-before (niwa-session-publication session)))
                  (unwind-protect
                      (let ((niwa-session-durability--final-commit
                             (and (null remaining)
                                  (null (niwa-session-publication-queue session)))))
                        (setq result
                              (niwa-session-publication--commit-marker-publication
                               session transaction marker)
                              committed t))
                    ;; The head commit proved ownership after every write.
                    (setcar niwa-session-publication--proved committed)
                    ;; A changed head commits this transaction even if its
                    ;; callback fails before returning.  Later queued batches
                    ;; still need recovery; these sources must not be replayed.
                    (unless (equal publication-before
                                   (niwa-session-publication session))
                      (setq current nil)
                      (setf (niwa-session-publication-uncommitted-batches session) nil)
                      (niwa-session-publication--delete-batches transaction))))
              (niwa-session-publication--retain-uncommitted-batch
               session current)
              (setq result 'published))
            (setq current nil))
          (setf (niwa-session-pending-publication session) nil)
          (list :committed committed :result result))
      (error
       (let ((recovery
              (delete-dups
               (append
                (niwa-session-publication-uncommitted-batches session)
                (and current (list current))
                remaining
                (niwa-session-publication-queue session)))))
         (setf (niwa-session-publication-uncommitted-batches session) nil
               (niwa-session-publication-queue session) nil)
         (when recovery
           (niwa-session-publication--record-pending session recovery err)))
       (signal (car err) (cdr err))))))

(defun niwa-session-publication--publish-critical-batches (session batches &optional token)
  "Publish BATCHES and discard recovery only after SESSION becomes active."
  (unwind-protect
      (let ((publication-before (niwa-session-publication session))
            outcome)
        (condition-case err
            (setq outcome
                  (let* ((first (and batches
                                     (seq-find
                                      #'niwa-session-publication--fixed-artifact-p
                                      (plist-get (car batches) :artifacts))))
                         (operations
                          (and first
                               (niwa-session-publication--prewrite-operations
                                session first)))
                         ;; The first fixed write rides the reservation's own
                         ;; program: one process start fewer per publication.
                         (riders (and operations
                                      (list :before nil :after operations
                                            :consumed nil :before-results nil
                                            :after-results nil)))
                         (niwa-session-durability--commit-riders riders)
                         (niwa-session-publication--prewritten
                          (and riders (list first riders))))
                    (niwa-session-durability-call-with-reserved-lease
                     session
                     (lambda ()
                       ;; Only the reservation carries them.
                       (when riders (setf (plist-get riders :consumed) t))
                       (setf (niwa-session-pending-publication session) nil)
                       ;; The reservation that just ran proved ownership.
                       (niwa-session-publication--drain session batches t)) token)))
          (error
           (unless (niwa-session-pending-publication session)
             (let* ((committed-p
                     (not (equal publication-before
                                 (niwa-session-publication session))))
                    (recovery
                     (cl-remove-if-not
                      #'niwa-session-publication--batch-live-p
                      (delete-dups
                       (append
                        (niwa-session-publication-uncommitted-batches session)
                        (unless committed-p batches)
                        (niwa-session-publication-queue session))))))
               (setf (niwa-session-publication-uncommitted-batches session)
                     nil
                     (niwa-session-publication-queue session) nil)
               (when committed-p
                 (niwa-session-publication--delete-batches batches))
               (when recovery
                 (niwa-session-publication--record-pending
                  session recovery err))))
           ;; A changed captured head proves the marker CAS committed.  Its local
           ;; sources are terminal even when final lease normalization failed.
           (when (and (not (equal publication-before
                                  (niwa-session-publication session)))
                      (not (niwa-session-pending-publication session)))
             (setf (niwa-session-publication-uncommitted-batches session) nil))
           (signal (car err) (cdr err))))
        (plist-get outcome :result))
    (niwa-session-checkpoint-release-proofs session)))

(defvar niwa-session-artifacts--terminal-sessions)

(defun niwa-session-publication-publish
    (session artifacts &optional require-commit prepared token)
  "Serialize and publish SESSION's critical ARTIFACTS.

Each artifact is `(:path TARGET-PATH :content STRING [:coding CODING])' or a
session-local `(:path TARGET-PATH :delete t)' tombstone.  One in-session
`session.meta.el' may carry `:commit-marker t' and optional `:replace t' to
commit an immutable logical snapshot.  The complete batch is staged locally
before target I/O.  Pre-commit failure retains the staged batch
in SESSION and blocks later mutation.  When REQUIRE-COMMIT is non-nil, reject
reentrant queueing so the caller returns only after its own batch commits, and
treat a post-commit cleanup error as diagnostic.  Ordinary callers receive
that error so their lifecycle owner can classify it.
PREPARED admits the owning terminal checkpoint's final frozen artifacts, never
a nested writer called while its registry and RESULT are still tentative.
TOKEN explicitly authorizes a preparation reentering its own publication gate."
  (niwa-session-publication-barrier session token)
  (when (and (memq session (bound-and-true-p niwa-session-artifacts--terminal-sessions))
             (not prepared))
    (user-error "Session terminal checkpoint is being prepared"))
  (niwa-telemetry-measure
   session 'session-publication 0
   (lambda () (niwa-session-publication--publish session artifacts require-commit token))
   :artifact-count (length artifacts)
   :input-bytes (apply #'+ (mapcar (lambda (artifact)
                                     (let ((content (plist-get artifact :content)))
                                       (if (stringp content) (string-bytes content) 0)))
                                   artifacts))))

(defun niwa-session-publication--publish (session artifacts require-commit &optional token)
  "Publish SESSION's ARTIFACTS as `niwa-session-publication-publish' does.
REQUIRE-COMMIT is its commit requirement."
  ;; Admission, reservation and commit are one publication transaction.
  ;; Reuse its bounded target clock and recovery observations, including for
  ;; agent callers that do not enter through the root save wrapper.
  (niwa-session-durability-with-transaction
    (niwa-session-recovery-refresh session)
    (when (niwa-session-pending-publication session)
      (user-error "Session has pending publication; retry or abandon it first"))
    (when (and require-commit
               (niwa-session-publication-active-p session))
      (user-error "Required session publication cannot be queued"))
    (let ((batch (niwa-session-publication--stage-artifacts
                  session artifacts))
          (publication-before (niwa-session-publication session)))
      (condition-case err
          (progn
            ;; The reservation below renews an owned lease itself; a
            ;; separate renewal only matters for one that needs reclaiming.
            (unless (or (niwa-session-publication-active-p session)
                        (niwa-session-durability-lease-owned-p session)
                        (niwa-session-durability-lease-renew session token))
              (user-error
               "Portable session lease could not be renewed for publication"))
            (unless (niwa-session-durability-lease-owned-p session)
              (user-error "Portable session mutation requires its live lease"))
            (cond
             ((niwa-session-publication-active-p session)
              (setf (niwa-session-publication-queue session)
                    (append (niwa-session-publication-queue session)
                            (list batch)))
              'queued)
             ;; A batch that writes no fixed file and commits nothing only
             ;; stages bytes for the next commit, which proves ownership
             ;; itself; reserving the lease for it wrote nothing at all.
             ((not (or (niwa-session-publication--batch-marker batch)
                       (seq-some #'niwa-session-publication--fixed-artifact-p
                                 (plist-get batch :artifacts))))
              (niwa-session-publication--retain-uncommitted-batch
               session batch)
              'published)
             (t
              (niwa-session-publication--publish-critical-batches
               session (list batch) token))))
        (error
         (let ((committed-p
                (not (equal publication-before
                            (niwa-session-publication session)))))
           (unless (niwa-session-pending-publication session)
             (if (not committed-p)
                 (niwa-session-publication--record-pending
                  session (list batch) err)
               (niwa-session-publication--delete-batch batch)))
           (if (and committed-p require-commit)
               (progn
                 (niwa-diagnostic-warning (niwa-diagnostic-scope session)
                  'niwa
                  (format "Session publication committed before cleanup failed: %s"
                          (error-message-string err))
                  :warning)
                 (niwa-session-publication session))
             (signal (car err) (cdr err)))))))))

(cl-defun niwa-session-publication-retry (&optional session &aux scope)
  "Retry SESSION's pending critical publication.
SCOPE captures diagnostic ownership before publication work starts."
  (interactive)
  (setq session (or session
                    (niwa-session-publication--current-session))
        scope (niwa-diagnostic-scope session))
  (when-let* ((job (niwa-session-publication-job session)))
    (unless (eq 'unresolved (niwa-session-publication--job-phase job))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Session publication is still running"))
    (setf (niwa-session-publication--job-phase job) 'reconcile)
    (niwa-session-publication--advance-soon job)
    (niwa-with-diagnostic-errors scope
      (niwa-session-publication-barrier session))
    ;; The barrier also drains later accepted jobs, whose successful heads can
    ;; supersede this candidate.  Judge this job's acknowledgement, not the
    ;; session's head after all those preparations finish.
    (unless (niwa-session-publication--job-success-p job)
      (let ((err (niwa-session-publication--job-error job)))
        (niwa-with-diagnostic-errors scope
          (signal (car err) (cdr err)))))
    (cl-return-from niwa-session-publication-retry
      (niwa-session-publication session)))
  ;; A failure caused by a vanished working directory must be retryable.
  (niwa-with-diagnostic-errors scope
    (niwa-session-artifacts-recover-working-directory session)
    (niwa-session-recovery-refresh session))
  (let* ((pending (or (niwa-session-pending-publication session)
                      (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Session has no pending publication")))
         (batches (plist-get pending :batches)))
    (when (plist-get pending :manual-recovery-marker)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope session)
       "Pending transaction requires manual recovery at %s"
       (plist-get pending :manual-recovery)))
    (unless batches
      (niwa-diagnostic-user-error (niwa-diagnostic-scope session)
       "Pending transaction requires manual recovery at %s"
       (or (plist-get pending :manual-recovery) "its retained target state")))
    (unless (niwa-session-durability-lease-renew session)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Portable session lease could not be renewed for publication"))
    (unless (niwa-session-durability-lease-owned-p session)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Portable session mutation requires its live lease"))
    (prog1
        (niwa-with-diagnostic-errors scope
          (niwa-session-publication--publish-critical-batches
           session batches))
      (niwa-session-recovery-refresh-session-buffers session))))

(cl-defun niwa-session-publication-abandon (&optional session &aux scope)
  "Explicitly discard SESSION's pending local recovery material.
SCOPE captures diagnostic ownership before recovery work starts."
  (interactive)
  (setq session (or session
                    (niwa-session-publication--current-session))
        scope (niwa-diagnostic-scope session))
  (when-let* ((job (niwa-session-publication-job session)))
    (unless (eq 'unresolved (niwa-session-publication--job-phase job))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Session publication is still running")))
  (niwa-with-diagnostic-errors scope
    (niwa-session-recovery-refresh session))
  (when-let* ((pending (niwa-session-pending-publication session)))
    (let ((specialized-p (plist-get pending :manual-recovery-marker)))
      (unless
          (yes-or-no-p
           (if specialized-p
               (concat
                "Abandon incomplete portable project Rewind recovery? This permanently "
                "deletes its target recovery bytes and marker. Continue? ")
             (concat
              "Abandon pending session publication? The target project effects "
              "remain, but transcript chronology and recovery state may be lost. ")))
        (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Pending publication was not abandoned"))
      (when specialized-p
        (niwa-with-diagnostic-errors scope
          (niwa-session-recovery--abandon session)))
      (when-let* ((local-recovery
                  (plist-get pending :manual-recovery-local)))
        (condition-case err
            (niwa-session-recovery--delete-local local-recovery)
          (error
           (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Could not remove local recovery: %s"
                       (error-message-string err)))))
      (niwa-session-publication-clear-transient session)
      (when-let* ((job (niwa-session-publication-job session)))
        (unless (eq 'unresolved (niwa-session-publication--job-phase job))
          (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Session publication is still running"))
        (niwa-session-publication--job-complete
         job nil '(error "Session publication was explicitly abandoned") t))
      (niwa-session-recovery-refresh-session-buffers session)
      (niwa-diagnostic-warning (niwa-diagnostic-scope session)
       'niwa
       "Pending session publication was explicitly abandoned"
       :warning)
      t)))

(defun niwa-session-publication-discard-rolled-back (session)
  "Discard SESSION recovery after its caller fully rolled back an operation.

The operation must have started without pending publication and restored all
pre-operation session and project state.  This function touches no target
state or publication head."
  (niwa-session-publication-clear-transient session)
  (niwa-session-recovery-refresh-session-buffers session)
  t)

(defvar niwa-session-publication--diagnostic-batch nil
  "The session, state and collected appends of a diagnostic batch.

The value is (SESSION STATE . APPENDS).  `open' means SESSION's lease can
carry diagnostics: each append is collected, newest first, as (PATH .
CONTENT), and the batch writes them together when it ends.  `unavailable'
means the batch established that SESSION's lease cannot carry diagnostics
now, so its appends decline without re-testing.")

(defvar niwa-session-publication--diagnostic-backlog
  (make-hash-table :test #'eq :weakness 'key)
  "Session to (IN-FLIGHT . APPENDS): its diagnostics not yet on the target.
APPENDS, oldest first, are appends a program did not write and appends
collected while an earlier program was still running; the next batch
writes them first.")

(defconst niwa-session-publication--diagnostic-backlog-max-bytes
  (* 8 1024 1024)
  "Largest diagnostic backlog kept for a session whose appends keep failing.")

(defun niwa-session-publication--diagnostic-ready-p (session)
  "Return non-nil when SESSION's lease can carry diagnostics right now.

Its owned lease must also be one whose committed bytes this client knows, so
each append can prove ownership in its own program; a renewal supplies them."
  (niwa-session-recovery-refresh session)
  (not (or (niwa-session-pending-publication session)
           (niwa-session-publication-active-p session)
           (not (or (and (niwa-session-durability-lease-owned-p session)
                         (plist-get (niwa-session-lease session) :bytes))
                    (niwa-session-durability-lease-renew session)))
           (not (niwa-session-durability-lease-owned-p session)))))

(defun niwa-session-publication-call-with-diagnostic-batch (session function)
  "Call FUNCTION with SESSION's diagnostic appends sharing one admission.

The recovery refresh and lease check run once, and each append then proves
ownership inside its own target program.  Diagnostics used to reserve the
lease around the appends -- a renewal, a reservation write, and a release
write around one or a few appends of best-effort data nothing reads live --
which cost several control programs per flush point on a remote target.

Publication is held active while FUNCTION runs, so a critical publisher
reached from a sentinel during an append queues instead of nesting inside
it; its batches are published through the ordinary reserved path once
FUNCTION returns.

The appends are written together once FUNCTION returns, as one program
that `niwa-session-publication--write-diagnostics' runs without the
editor waiting for it where the target allows.  An append it does not
write stays in the session's backlog for the next batch."
  (cond
   ((eq (car-safe niwa-session-publication--diagnostic-batch) session)
    (funcall function))
   ;; A session that does not publish through the lease writes its
   ;; diagnostics directly, so there is no lease to prove.
   ((not (niwa-session-codec-portable-authority-p session))
    (funcall function))
   (t
    (niwa-session-durability-with-transaction
      (if (not (niwa-session-publication--diagnostic-ready-p session))
          (let ((niwa-session-publication--diagnostic-batch
                 (list session 'unavailable)))
            (funcall function))
        (let ((batch (list session 'open)))
          (prog1 (niwa-session-durability-call-with-held-lease
                  session
                  (lambda ()
                    (prog1 (let ((niwa-session-publication--diagnostic-batch batch))
                             (funcall function))
                      ;; Started inside the hold: where the program runs
                      ;; synchronously, a critical publisher reached from a
                      ;; sentinel meanwhile still queues instead of nesting.
                      (niwa-session-publication--write-diagnostics
                       session (reverse (cddr batch))))))
            ;; A critical publisher may have queued meanwhile.  It retains
            ;; precedence.
            (when (niwa-session-publication-queue session)
              (niwa-session-publication--publish-critical-batches
               session nil)))))))))

(defun niwa-session-publication--write-diagnostics (session appends)
  "Write SESSION's diagnostic backlog, then APPENDS, as one program.

Each (PATH . CONTENT) is an optional append behind the lease proof, so one
unwritable log does not stop the others.  On a target that allows a direct
pipe nothing waits for the program; an append it did not write, and every
append collected while it runs, joins the backlog the next batch writes
first, which keeps each log in order.  The backlog is bounded: past
`niwa-session-publication--diagnostic-backlog-max-bytes' the oldest
appends are dropped with a warning.

Where the editor is about to exit (`niwa-session-control-fs--async-wait'),
an append still in flight is waited for first, so what it leaves and APPENDS
are written now instead of queueing for a batch that never comes."
  (let ((state (or (gethash session niwa-session-publication--diagnostic-backlog)
                   (puthash session (cons nil nil)
                            niwa-session-publication--diagnostic-backlog)))
        entries)
    (when (and (car state) niwa-session-control-fs--async-wait)
      (niwa-session-control-fs-wait-async))
    (setq entries (append (cdr state) appends))
    (cond
     ((null entries))
     ((car state) (setcdr state entries))
     (t
      (setcar state t)
      (setcdr state nil)
      (niwa-session-durability-run-owned-program-async
       session
       (mapcar (lambda (entry)
                 (list :op 'append
                       :path (niwa-session-control-fs-physical-path (car entry))
                       :content (cdr entry)
                       :optional t))
               entries)
       (lambda (results error)
         (when error
           (niwa--warn-once
            (niwa-diagnostic-scope session)
            (list 'diagnostic-append (niwa-session-save-path session))
            "Diagnostic append failed, retaining for retry: %s"
            (error-message-string error)))
         (cl-loop for entry in entries
                  for result in results
                  when (eq 'failed (plist-get result :status))
                  do (niwa--warn-once
                      (niwa-diagnostic-scope session)
                      (list 'diagnostic-append (car entry))
                      "Diagnostic append failed for %s, retaining for retry: %s"
                      (car entry)
                      (or (plist-get result :diagnostic) "target refused the append")))
         (let ((unwritten (cl-loop for entry in entries
                                   for index from 0
                                   unless (eq 'ok (plist-get (nth index results) :status))
                                   collect entry)))
           (setcar state nil)
           (setcdr state (niwa-session-publication--bound-backlog
                          session (append unwritten (cdr state)))))))))))

(defun niwa-session-publication-diagnostic-backlog-p (session)
  "Return non-nil when SESSION has diagnostics waiting for the next batch."
  (and (cdr (gethash session niwa-session-publication--diagnostic-backlog)) t))

(defun niwa-session-publication--bound-backlog (session entries)
  "Return SESSION's diagnostic backlog ENTRIES, oldest dropped past its bound."
  (let ((bytes 0) kept)
    (dolist (entry (reverse entries))
      (setq bytes (+ bytes (string-bytes (cdr entry))))
      (if (<= bytes niwa-session-publication--diagnostic-backlog-max-bytes)
          (push entry kept)
        (niwa--warn-once
         (niwa-diagnostic-scope session)
         (list 'diagnostic-backlog (niwa-session-save-path session))
         "Dropping diagnostics that could not be written to %s"
         (niwa-session-save-path session))))
    kept))

(defun niwa-session-publication-append-diagnostic (session path content)
  "Append diagnostic CONTENT to PATH for portable project SESSION.

Return nil when the lease cannot carry a diagnostic right now -- a critical
publication is active or pending, renewal is momentarily unavailable, or the
lease is waiting to be reacquired -- so the caller retains CONTENT for a
later retry.  A diagnostic is best-effort data nothing reads live, so an
unavailable moment is a quiet retry, not an error echoed per attempt.
Diagnostic failure never creates critical pending publication.

Otherwise the append is taken, returning non-nil: it is written with the
rest of its batch as one pinned program carrying only the deltas, behind an
ownership proof in the same program, and an append that program does not
write is retried with the next batch.  Republishing the whole file per
flush was quadratic in stream size and dominated remote-save allocations
once a log grew to megabytes."
  (unless (and (stringp path) (stringp content))
    (error "Diagnostic publication requires string path and content"))
  (let ((batch niwa-session-publication--diagnostic-batch))
    (cond
     ((not (eq (car-safe batch) session))
      (let (appended)
        (niwa-session-publication-call-with-diagnostic-batch
         session
         (lambda ()
           (setq appended
                 (niwa-session-publication-append-diagnostic
                  session path content))))
        appended))
     ((eq (cadr batch) 'unavailable) nil)
     (t
      ;; Validate that the path stays inside the session before any
      ;; target I/O; :content is not staged, only checked for shape.
      (niwa-session-publication--artifact-for-session
       session (list :path path :content content))
      (setcdr (cdr batch) (cons (cons path content) (cddr batch)))
      t))))

(defun niwa-session-publication-status (session)
  "Return SESSION's stable read-only publication status plist."
  (let ((lease (niwa-session-lease session))
        (pending (niwa-session-pending-publication session)))
    (list :lease-state (plist-get lease :state)
          :publication-head (plist-get lease :publication-head)
          :pending-publication (and pending t)
          :pending-reason (and pending (plist-get pending :reason))
          :pending-recovery (and pending
                                 (plist-get pending :manual-recovery))
          :pending-recovery-marker
          (and pending (plist-get pending :manual-recovery-marker))
          :pending-recovery-portable
          (and pending (plist-get pending :recovery-portable))
          :authoritative-state-path
          (niwa-workspace-state-dir
           (niwa-session-workspace session)))))

(defun niwa-session-publication-observe-child (_workspace payload)
  "Observe frozen PAYLOAD generations without editor collection or deletion."
  (let ((directory (plist-get payload :directory)))
    (unless (and (stringp directory) (file-name-absolute-p directory)
                 (not (file-remote-p directory)))
      (error "Publication observations require a local absolute directory"))
    (list :ok t :observations
          (mapcar
           (lambda (generation)
             (let* ((head (plist-get generation :head))
                    (observed (niwa-session-publication--cached-generation directory head)))
               (list head observed
                     (and observed (niwa-session-publication--cached-sidecar-facts directory head observed)))))
           (plist-get payload :generations)))))

(provide 'niwa-session-publication)

;;; niwa-session-publication.el ends here
