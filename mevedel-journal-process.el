;;; mevedel-journal-process.el -- Bounded background digest requests -*- lexical-binding: t -*-

;;; Commentary:

;; Processes one sealed capture per workspace opportunity.  A workspace
;; admission claim limits digest concurrency; a capture claim accepts the
;; immutable result.  Both expire at the same target-clock deadline.  Model
;; requests use frozen policy and evidence without a live conversation.
;; Accepted results are recovered before replacement inference is admitted.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-context-summary)
(require 'mevedel-journal-capture)
(require 'mevedel-journal-claim)
(require 'mevedel-journal-discard)

;; `mevedel-journal-recovery'
(declare-function mevedel-journal-recovery-run "mevedel-journal-recovery" (workspace))
(autoload 'mevedel-journal-recovery-run "mevedel-journal-recovery")

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record-workspace "mevedel-telemetry"
                  (workspace event &rest props))
(autoload 'mevedel-telemetry-record-workspace "mevedel-telemetry")

;; `mevedel-transport'
(declare-function mevedel-transport-cancel-idle "mevedel-transport" (table tag &optional path-of-key))
(declare-function mevedel-transport-schedule-idle "mevedel-transport" (table key tag path thunk))
(autoload 'mevedel-transport-cancel-idle "mevedel-transport")
(autoload 'mevedel-transport-schedule-idle "mevedel-transport")

(defvar mevedel-journal-process--inhibit-scheduling nil
  "Non-nil while exiting or isolating a lifecycle operation from scheduling.")

(defvar mevedel-journal-process--pending (make-hash-table :test #'equal)
  "Coalesced lifecycle opportunities waiting for the caller and transport.")

(defvar mevedel-journal-process--recover-requested (make-hash-table :test #'equal)
  "Journal directories whose next opportunity recovers abandoned captures first.")

(defconst mevedel-journal-process--timeout-seconds 120
  "Fixed request deadline, including policy resolution and input admission.")

(defvar mevedel-journal-process--running (make-hash-table :test #'equal)
  "Live digest request states, keyed by physical workspace journal directory.")

(defun mevedel-journal-process--key (workspace)
  "Return WORKSPACE's physical journal directory and local request key."
  (mevedel-journal-store-directory (mevedel-workspace-root workspace)))

(defun mevedel-journal-process-schedule (workspace &optional recover)
  "Schedule one digest opportunity for WORKSPACE after the caller returns.
Repeated lifecycle events coalesce; busy requests wait for a later opportunity
rather than recursively draining the backlog.  Exit suppresses scheduling.
RECOVER requests abandoned-capture recovery before this processing opportunity."
  (when (and mevedel-journal-enabled (not mevedel-journal-process--inhibit-scheduling))
    (let ((key (mevedel-journal-process--key workspace)))
      (when recover (puthash key t mevedel-journal-process--recover-requested))
      (unless (gethash key mevedel-journal-process--running)
        (mevedel-transport-schedule-idle
         mevedel-journal-process--pending key 'journal-process key
         (lambda ()
           (let ((recover (gethash key mevedel-journal-process--recover-requested)))
             (remhash key mevedel-journal-process--recover-requested)
             (unless mevedel-journal-process--inhibit-scheduling
               (condition-case err
                   (progn
                     (when recover (mevedel-journal-recovery-run workspace))
                     (mevedel-journal-process-next workspace))
                 (error
                  (display-warning 'mevedel
                                   (format "Journal processing failed: %s" (error-message-string err))
                                   :warning)))))))))))

(defun mevedel-journal-process-stop-all ()
  "Cancel this client's queued and active digest work without awaiting a model."
  (mevedel-transport-cancel-idle mevedel-journal-process--pending 'journal-process)
  (clrhash mevedel-journal-process--recover-requested)
  (maphash (lambda (_key state) (mevedel-journal-process-cancel (plist-get state :workspace)))
           mevedel-journal-process--running))

(defun mevedel-journal-process--with-admission (workspace function &optional busy-message)
  "Call FUNCTION with WORKSPACE's digest-run admission claim, then settle it.
Return FUNCTION's value.  When another request holds admission, signal
BUSY-MESSAGE as a user error, or return nil without it."
  (let ((claim (mevedel-journal-claim-acquire
                (mevedel-journal-store-claim-directory (mevedel-journal-process--key workspace) 'digest-run)
                mevedel-journal-process--timeout-seconds)))
    (cond (claim (unwind-protect (funcall function claim)
                   (mevedel-journal-claim-settle claim 'completed "")))
          (busy-message (user-error "%s" busy-message)))))

(defun mevedel-journal-process--attempts (workspace capture)
  "Return the private attempt claim directory for CAPTURE in WORKSPACE."
  (mevedel-journal-capture--file workspace (plist-get capture :id) "attempts"))

(defun mevedel-journal-process--metadata (workspace capture)
  "Return frozen public metadata for CAPTURE with its authoritative seal."
  (let ((metadata (mevedel-journal-capture--metadata capture)))
    (when-let* ((trigger (mevedel-journal-capture-trigger workspace capture)))
      (plist-put metadata :trigger trigger))
    metadata))

(defun mevedel-journal-process--recover (workspace capture entries &optional usage)
  "Complete CAPTURE's accepted result or reconcile its entry from ENTRIES.
Return the completed entry or omission, otherwise nil.  Source pins are released
only after publication or an accepted explicit discard.  Recovery errors never
admit replacement inference.  USAGE carries available provider token counts."
  (let* ((id (plist-get capture :id))
         (entry (mevedel-journal-store-entry-for-capture entries id))
         (claim (mevedel-journal-claim-current (mevedel-journal-process--attempts workspace capture)))
         (outcome (and claim (mevedel-journal-claim-outcome claim)))
         (discard (mevedel-journal-discard-accepted outcome id)))
    (if discard
        (mevedel-journal-discard-complete workspace discard)
      (unless (plist-get capture :unreadable)
        (let ((body (or (plist-get entry :body)
                        (and (eq 'completed (plist-get outcome :status)) (plist-get outcome :payload)))))
          (when body
            (setq entry (mevedel-journal-store-publish-digest
                         (mevedel-workspace-root workspace)
                         (mevedel-journal-process--metadata workspace capture) body))
            (setf (mevedel-workspace-journal-observation workspace) nil)
            (mevedel-telemetry-record-workspace
             workspace 'journal-digest-written
             :capture-id id :trigger (plist-get entry :trigger)
             :output-bytes (string-bytes body)
             :attempt-generation (plist-get claim :generation)
             :input-tokens (plist-get usage :input-tokens)
             :cached-tokens (plist-get usage :cached-tokens)
             :output-tokens (plist-get usage :output-tokens)
             :outcome 'published)
            (mevedel-journal-capture--retire workspace capture (format "published %s\n" (plist-get entry :id)))
            entry))))))

(defun mevedel-journal-process--policy (workspace capture)
  "Resolve CAPTURE's frozen selection without substituting another provider.
WORKSPACE is used to verify that client-owned evidence remains available."
  (mevedel-journal-capture--source-directory workspace capture)
  (let* ((frozen (plist-get capture :policy))
         (provider (plist-get frozen :provider)))
    (unless provider
      (error "Captured summarization policy is unavailable: %s" (plist-get frozen :error)))
    (let* ((policy (mevedel-model-resolve-provider provider))
           (name (plist-get frozen :effort))
           (effort (and name (or (intern-soft name)
                                (error "Captured reasoning effort is unavailable: %s" name)))))
      (mevedel-model-validate-effort (plist-get policy :model) effort)
      (append policy (list :effort effort
                           :max-tokens (plist-get frozen :max-tokens)
                           :request-params nil)))))

(defun mevedel-journal-process--source (workspace capture policy)
  "Return verified CAPTURE evidence bounded to POLICY's usable input budget.
Use the generator's exact prompt accounting, retaining explicit omissions."
  (let* ((source (mevedel-journal-capture-evidence workspace capture))
         (system (mevedel-context-summary--prompt 'digest))
         (usable (mevedel-model-usable-input-tokens policy))
         (marker "\n[omitted completed-work evidence: summarization input limit]\n"))
    (cl-labels ((fits (text)
                  (<= (mevedel-context-summary--estimated-tokens
                       system (mevedel-context-summary--input text 'digest nil nil nil))
                      usable)))
      (cond
       ((fits source) source)
       ((not (fits marker)) (error "Digest prompt cannot fit the captured model budget"))
       (t
        (let ((low 0) (high (length source)))
          (while (< low high)
            (let ((middle (/ (+ low high 1) 2)))
              (if (fits (concat (substring source 0 middle) marker))
                  (setq low middle)
                (setq high (1- middle)))))
          (concat (substring source 0 low) marker)))))))

(defun mevedel-journal-process--finish (state result)
  "Settle STATE once with generator RESULT, then publish only an accepted body."
  (unless (plist-get state :settled)
    (plist-put state :settled t)
    (let* ((workspace (plist-get state :workspace))
           (capture (plist-get state :capture))
           (success (eq 'success (plist-get result :outcome)))
           (status (if success 'completed
                     (if (eq 'aborted (plist-get result :outcome)) 'cancelled 'failed)))
           (payload (if success (plist-get result :summary)
                      (json-serialize
                       (list :error (or (plist-get result :error) "Digest cancelled")
                             :class (symbol-name (or (plist-get result :error-class) 'cancelled)))))))
      (unwind-protect
          (condition-case err
              (progn
                (plist-put state :outcome
                           (mevedel-journal-claim-settle (plist-get state :claim) status payload))
                (when (and success (plist-get state :outcome))
                  (plist-put state :entry (mevedel-journal-process--recover workspace capture nil result))))
            (error (plist-put state :error (error-message-string err))))
        (unless (plist-get state :entry)
          (mevedel-telemetry-record-workspace
           workspace 'journal-digest-failed
           :capture-id (plist-get capture :id)
           :trigger (ignore-errors (mevedel-journal-capture-trigger workspace capture))
           :attempt-generation (plist-get (plist-get state :claim) :generation)
           :input-tokens (plist-get result :input-tokens)
           :cached-tokens (plist-get result :cached-tokens)
           :output-tokens (plist-get result :output-tokens)
           :error-class (or (plist-get result :error-class)
                            (if (plist-get state :error) 'publication 'cancelled))
           :outcome (cond ((plist-get state :error) 'failed)
                          ((plist-get state :outcome) status)
                          (t 'unaccepted))))
        (when (timerp (plist-get state :timer))
          (cancel-timer (plist-get state :timer)))
        (ignore-errors
          (mevedel-journal-claim-settle (plist-get state :admission) status ""))
        (when (eq state (gethash (plist-get state :key) mevedel-journal-process--running))
          (remhash (plist-get state :key) mevedel-journal-process--running))))))

(defun mevedel-journal-process-cancel (workspace)
  "Cancel this client's running digest for WORKSPACE, preserving its evidence."
  (when-let* ((state (gethash (mevedel-journal-process--key workspace)
                              mevedel-journal-process--running)))
    (let ((cancel (plist-get state :cancel)))
      (mevedel-journal-process--finish state '(:outcome aborted))
      (when cancel (funcall cancel)))
    t))

(defun mevedel-journal-process--start (state)
  "Start STATE's bounded request from frozen policy and evidence."
  (let ((workspace (plist-get state :workspace))
        (capture (plist-get state :capture)))
    (condition-case err
        (let* ((policy (mevedel-journal-process--policy workspace capture))
               (source (mevedel-journal-process--source workspace capture policy)))
          (plist-put
           state :timer
           (run-at-time
            (max 0 (- (plist-get (plist-get state :claim) :expires-at)
                      (mevedel-session-control-fs-target-time (plist-get state :key))))
            nil
            (lambda ()
              (let ((cancel (plist-get state :cancel)))
                (mevedel-journal-process--finish
                 state '(:outcome error :error-class timeout :error "Digest request timed out"))
                (when cancel (funcall cancel))))))
          ;; The generator snapshots configuration in its own request buffer.
          ;; This temporary caller supplies streaming policy, not a session.
          (with-temp-buffer
            (setq-local gptel-stream (plist-get (plist-get capture :policy) :stream))
            (let ((cancel (mevedel-context-summary-generate
                           source 'digest
                           (lambda (result) (mevedel-journal-process--finish state result))
                           :policy policy)))
              (unless (plist-get state :settled) (plist-put state :cancel cancel)))))
      (error
       (mevedel-journal-process--finish
        state (list :outcome 'error :error-class 'unavailable :error (error-message-string err))))))
  state)

(defun mevedel-journal-process-discard (workspace id &optional original-source)
  "Explicitly discard WORKSPACE's pending capture ID, retaining an omission.
Cancel this client's matching request first.  A foreign or unrelated live
request remains untouched.  An already accepted digest must recover before
its job can retire.  ORIGINAL-SOURCE identifies an unreadable capture's
original source and must contain its matching valid pin."
  (let* ((key (mevedel-journal-process--key workspace))
         (running (gethash key mevedel-journal-process--running)))
    (when (and running (equal id (plist-get (plist-get running :capture) :id)))
      (mevedel-journal-process-cancel workspace))
    (mevedel-journal-process--with-admission
     workspace
     (lambda (admission)
       (let* ((attempts (mevedel-journal-process--attempts workspace (list :id id)))
              (previous (mevedel-journal-claim-current attempts))
              (outcome (and previous (mevedel-journal-claim-outcome previous)))
              (discard (mevedel-journal-discard-accepted outcome id))
              (capture (ignore-errors (mevedel-journal-capture--read workspace id))))
         (cond
          (discard (mevedel-journal-discard-complete workspace discard))
          ((mevedel-journal-capture--marked-p workspace id "retired")
           (or (mevedel-journal-store-entry-for-capture
                (mevedel-journal-store-entries (mevedel-workspace-root workspace)) id)
               (user-error "Journal capture is no longer pending")))
          ((eq 'completed (plist-get outcome :status))
           (unless capture (user-error "Restore the capture descriptor to recover its accepted result"))
           (mevedel-journal-process--recover workspace capture nil))
          ((not (mevedel-journal-capture--marked-p workspace id "ready"))
           (user-error "Journal capture is no longer pending"))
          (t
           (let* ((payload (mevedel-journal-discard-prepare workspace id original-source))
                  (claim (mevedel-journal-claim-acquire attempts mevedel-journal-process--timeout-seconds
                                                        (plist-get admission :expires-at)))
                  (accepted (and claim (mevedel-journal-claim-settle claim 'cancelled payload))))
             (unless accepted (user-error "Journal capture has another live owner"))
             (mevedel-journal-discard-complete
              workspace (mevedel-journal-discard-accepted accepted id)))))))
     "Journal processing is busy; discard when its current claim settles")))

(defun mevedel-journal-process-retry (workspace id)
  "Explicitly retry WORKSPACE's pending capture ID once, or recover its result.
This never resets the durable automatic-attempt count or changes model policy."
  (unless mevedel-journal-enabled (user-error "Enable journaling before retrying a capture"))
  (let ((capture (cl-find id (mevedel-journal-capture-list workspace)
                          :key (lambda (capture) (plist-get capture :id)) :test #'equal)))
    (unless capture (user-error "Journal capture is no longer pending"))
    (when (plist-get capture :unreadable)
      (user-error "Journal capture is unreadable: %s" (plist-get capture :error)))
    (unless (mevedel-journal-capture-trigger workspace capture)
      (user-error "Journal checkpoint is not sealed yet"))
    (or (mevedel-journal-process-next workspace id)
        (user-error "Journal processing is busy; retry when its current claim settles"))))

(defun mevedel-journal-process-next (workspace &optional retry-id)
  "Recover accepted results and start at most one digest in WORKSPACE.
This is one processing opportunity: failures never trigger a tight retry loop.
Only sealed jobs run; three automatic attempts exhaust a job.  Return the new
request state or nil when disabled, busy, or no eligible work remains.
An unavailable recovery is retained without blocking unrelated captures.
RETRY-ID selects an explicitly requested additional attempt; recovery of its
accepted result returns the public entry without starting another request.
Explicit retry reports recovery errors to the caller."
  (when mevedel-journal-enabled
    (let* ((key (mevedel-journal-process--key workspace))
           (admission (and (not (gethash key mevedel-journal-process--running))
                           (mevedel-journal-claim-acquire
                            (mevedel-journal-store-claim-directory key 'digest-run)
                            mevedel-journal-process--timeout-seconds)))
           state recovered)
      (when admission
        (unwind-protect
            (let ((entries (mevedel-journal-store-entries (mevedel-workspace-root workspace))))
              (dolist (capture (mevedel-journal-capture-list workspace))
                (unless (or state
                            (and retry-id (not (equal retry-id (plist-get capture :id)))))
                  (condition-case err
                      (let ((entry (mevedel-journal-process--recover workspace capture entries)))
                        (if entry
                            (when retry-id (setq recovered entry))
                          (when (and (not (plist-get capture :unreadable))
                                     (mevedel-journal-capture-trigger workspace capture))
                            (let* ((directory (mevedel-journal-process--attempts workspace capture))
                                   (previous (mevedel-journal-claim-current directory))
                                   (claim (and (or retry-id (< (or (plist-get previous :generation) 0) 3))
                                               (mevedel-journal-claim-acquire
                                                directory mevedel-journal-process--timeout-seconds
                                                (plist-get admission :expires-at)))))
                              (when claim
                                (setq state (list :key key :workspace workspace :capture capture
                                                  :claim claim :admission admission :settled nil))
                                (puthash key state mevedel-journal-process--running)
                                (mevedel-journal-process--start state))))))
                    (error
                     (if (or retry-id state)
                         (signal (car err) (cdr err))
                       (mevedel-telemetry-record-workspace
                        workspace 'journal-digest-failed :capture-id (plist-get capture :id)
                        :outcome 'retained :error-class 'recovery)))))))
          (unless state (mevedel-journal-claim-settle admission 'completed ""))))
      (or state recovered))))

(provide 'mevedel-journal-process)
;;; mevedel-journal-process.el ends here
