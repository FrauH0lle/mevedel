;;; niwa-journal-process.el -- Bounded background digest requests -*- lexical-binding: t -*-

;;; Commentary:

;; Processes one sealed capture per workspace opportunity.  A workspace
;; admission claim limits digest concurrency; a capture claim accepts the
;; immutable result.  Both expire at the same target-clock deadline.  Model
;; requests use frozen policy and evidence without a live conversation.
;; Accepted results are recovered before replacement inference is admitted.
;; A capture whose three automatic attempts failed is reported once per Emacs
;; session and waits for an explicit retry or discard.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-context-summary)
(require 'niwa-journal-worker)
(require 'niwa-journal-capture)
(require 'niwa-journal-claim)
(require 'niwa-journal-discard)

;; `niwa-journal-recovery'
(declare-function niwa-journal-recovery-run "niwa-journal-recovery" (workspace))
(autoload 'niwa-journal-recovery-run "niwa-journal-recovery")

;; `niwa-memory-pass'
(declare-function niwa-memory-pass-schedule "niwa-memory-pass" (workspace))
(autoload 'niwa-memory-pass-schedule "niwa-memory-pass")

;; `niwa-telemetry'
(declare-function niwa-telemetry-record-workspace "niwa-telemetry"
                  (workspace event &rest props))
(autoload 'niwa-telemetry-record-workspace "niwa-telemetry")

;; `niwa-transport'
(declare-function niwa-transport-cancel-idle "niwa-transport" (table tag))
(declare-function niwa-transport-schedule-idle "niwa-transport" (table key tag path thunk))
(autoload 'niwa-transport-cancel-idle "niwa-transport")
(autoload 'niwa-transport-schedule-idle "niwa-transport")

;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-warning "niwa-utilities" (scope type message &optional level buffer-name))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")

(defvar niwa-journal-process--inhibit-scheduling nil
  "Non-nil while exiting or isolating a lifecycle operation from scheduling.")

(defvar niwa-journal-process--pending (make-hash-table :test #'equal)
  "Coalesced lifecycle opportunities waiting for the caller and transport.")

(defvar niwa-journal-process--recover-requested (make-hash-table :test #'equal)
  "Journal directories whose next opportunity recovers abandoned captures first.")

(defconst niwa-journal-process--timeout-seconds 120
  "Fixed request deadline, including policy resolution and input admission.")

(defconst niwa-journal-process--automatic-attempts 3
  "Attempts a sealed capture gets before only an explicit retry runs it.")

(defvar niwa-journal-process--exhausted-warned (make-hash-table :test #'equal)
  "Workspace/capture keys whose exhausted attempts this Emacs session reported.")

(defvar niwa-journal-process--running (make-hash-table :test #'equal)
  "Live digest request states, keyed by physical workspace journal directory.")

(defun niwa-journal-process--key (workspace)
  "Return WORKSPACE's physical journal directory and local request key."
  (niwa-journal-store-directory (niwa-workspace-root workspace)))

(defvar niwa-journal-process--workers (make-hash-table :test #'equal)
  "Local recovery children keyed by workspace journal directory.")

(defun niwa-journal-process-schedule (workspace &optional recover)
  "Schedule one digest opportunity for WORKSPACE after the caller returns.
Repeated lifecycle events coalesce; busy requests wait for a later opportunity
rather than recursively draining the backlog.  Exit suppresses scheduling.
RECOVER requests abandoned-capture recovery before this processing opportunity."
  (when (and niwa-journal-enabled (not niwa-journal-process--inhibit-scheduling))
    (let ((key (niwa-journal-process--key workspace))
          (scope (niwa-diagnostic-scope workspace)))
      (when recover (puthash key t niwa-journal-process--recover-requested))
      (unless (or (gethash key niwa-journal-process--running)
                  (gethash key niwa-journal-process--workers))
        (niwa-transport-schedule-idle
         niwa-journal-process--pending key 'journal-process key
         (lambda ()
           (let ((recover (gethash key niwa-journal-process--recover-requested)))
             (remhash key niwa-journal-process--recover-requested)
             (unless niwa-journal-process--inhibit-scheduling
               (condition-case err
                   (if (niwa-journal-worker-local-p workspace)
                       (let (worker)
                         (setq worker
                               (niwa-journal-worker-start
                                workspace 'digest-prepare
                                (list :recover recover)
                                (lambda (result)
                                  (when (eq worker (gethash key niwa-journal-process--workers))
                                    (remhash key niwa-journal-process--workers)
                                    (if (plist-get result :error)
                                        (unless niwa-journal-process--inhibit-scheduling
                                          (niwa-diagnostic-warning scope 'niwa (plist-get result :error) :warning))
                                      (setf (niwa-workspace-journal-observation workspace) nil)
                                      (when recover (niwa-memory-pass-schedule workspace))
                                      (condition-case failure
                                          (niwa-journal-process--prepared
                                           workspace (plist-get result :prepared))
                                        (error
                                         (niwa-diagnostic-warning scope 'niwa (error-message-string failure) :warning))))))))
                         (puthash key worker niwa-journal-process--workers))
                     (when recover (niwa-journal-recovery-run workspace))
                     (niwa-journal-process-next workspace))
                 (error
                  (niwa-diagnostic-warning scope 'niwa
                                   (format "Journal processing failed: %s" (error-message-string err))
                                   :warning)))))))))))

(defun niwa-journal-process-stop-all ()
  "Cancel this client's queued and active digest work without awaiting a model.
Attempt all work before re-signalling the first cancellation error."
  (niwa-transport-cancel-idle niwa-journal-process--pending 'journal-process)
  (clrhash niwa-journal-process--recover-requested)
  (let (workers failure)
    (maphash (lambda (_key process) (push process workers)) niwa-journal-process--workers)
    (clrhash niwa-journal-process--workers)
    (dolist (process workers)
      (condition-case err
          (niwa-journal-worker-cancel process t)
        (error (unless failure (setq failure err)))))
    (maphash (lambda (_key state)
               (condition-case err
                   (niwa-journal-process-cancel (plist-get state :workspace))
                 (error (unless failure (setq failure err)))))
             niwa-journal-process--running)
    (when failure (signal (car failure) (cdr failure)))))

(defun niwa-journal-process--with-admission (workspace function &optional busy-message)
  "Call FUNCTION with WORKSPACE's digest-run admission claim, then settle it.
Return FUNCTION's value.  When another request holds admission, signal
BUSY-MESSAGE as a user error, or return nil without it."
  (let ((claim (niwa-journal-claim-acquire
                (niwa-journal-store-claim-directory (niwa-journal-process--key workspace) 'digest-run)
                niwa-journal-process--timeout-seconds)))
    (cond (claim (unwind-protect (funcall function claim)
                   (niwa-journal-claim-settle claim 'completed "")))
          (busy-message (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "%s" busy-message)))))

(defun niwa-journal-process--attempts (workspace capture)
  "Return the private attempt claim directory for CAPTURE in WORKSPACE."
  (niwa-journal-capture--file workspace (plist-get capture :id) "attempts"))

(defun niwa-journal-process--metadata (workspace capture)
  "Return frozen public metadata for CAPTURE with its authoritative seal."
  (let ((metadata (niwa-journal-capture--metadata capture)))
    (when-let* ((trigger (niwa-journal-capture-trigger workspace capture)))
      (plist-put metadata :trigger trigger))
    metadata))

(defun niwa-journal-process--recover (workspace capture entries &optional usage)
  "Complete CAPTURE's accepted result or reconcile its entry from ENTRIES.
Return the completed entry or omission, otherwise nil.  Source pins are released
only after publication or an accepted omission.  Recovery errors never
admit replacement inference.  USAGE carries available provider token counts."
  (let* ((id (plist-get capture :id))
         (entry (niwa-journal-store-entry-for-capture entries id))
         (settlement (niwa-journal-claim-current-settlement
                      (niwa-journal-process--attempts workspace capture)))
         (claim (car settlement))
         (outcome (cdr settlement))
         (discard (niwa-journal-discard-accepted outcome id)))
    (if discard
        (niwa-journal-discard-complete workspace discard)
      (unless (plist-get capture :unreadable)
        (let ((body (or (plist-get entry :body)
                        (and (eq 'completed (plist-get outcome :status)) (plist-get outcome :payload)))))
          (when body
            (let* ((metadata (niwa-journal-process--metadata workspace capture))
                   (omitted (and (not entry) (niwa-context-summary-digest-empty-p body))))
              (if omitted
                  (progn
                    (niwa-journal-store--record-coverage (niwa-workspace-root workspace) metadata)
                    (setq entry (list :capture-id id :trigger (plist-get metadata :trigger) :outcome 'omitted)))
                (setq entry (niwa-journal-store-publish-digest
                             (niwa-workspace-root workspace) metadata body)))
              (setf (niwa-workspace-journal-observation workspace) nil)
              (niwa-telemetry-record-workspace
               workspace (if omitted 'journal-digest-omitted 'journal-digest-written)
               :capture-id id :trigger (plist-get entry :trigger)
               :output-bytes (string-bytes body)
               :attempt-generation (plist-get claim :generation)
               :input-tokens (plist-get usage :input-tokens)
               :cached-tokens (plist-get usage :cached-tokens)
               :output-tokens (plist-get usage :output-tokens)
               :outcome (if omitted 'omitted 'published))
              (niwa-journal-capture--retire
               workspace capture (if omitted "nothing noteworthy\n" (format "published %s\n" (plist-get entry :id))))
              (unless omitted (niwa-memory-pass-schedule workspace))
              entry)))))))

(defun niwa-journal-process--policy (workspace capture)
  "Resolve CAPTURE's frozen selection without substituting another provider.
WORKSPACE is used to verify that client-owned evidence remains available."
  (niwa-journal-capture--source-directory workspace capture)
  (let* ((frozen (plist-get capture :policy))
         (provider (plist-get frozen :provider)))
    (unless provider
      (error "Captured journal policy is unavailable: %s" (plist-get frozen :error)))
    (let* ((policy (niwa-model-resolve-provider provider))
           (name (plist-get frozen :effort))
           (effort (and name (or (intern-soft name)
                                 (error "Captured reasoning effort is unavailable: %s" name)))))
      (niwa-model-validate-effort (plist-get policy :model) effort)
      (append policy (list :effort effort
                           :max-tokens (plist-get frozen :max-tokens)
                           :request-params nil)))))

(defun niwa-journal-process--source (workspace capture policy)
  "Return verified CAPTURE evidence bounded to POLICY's usable input budget.
Use the generator's exact prompt accounting, retaining explicit omissions."
  (let* ((source (niwa-journal-evidence-digest-source
                  (niwa-journal-capture-evidence workspace capture)))
         (system (niwa-context-summary--prompt 'digest))
         (usable (niwa-model-usable-input-tokens policy))
         (marker "\n[omitted completed-work evidence: summarization input limit]\n"))
    (cl-labels ((fits (text)
                  (<= (niwa-context-summary--estimated-tokens
                       system (niwa-context-summary--input text 'digest nil nil nil))
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

(defun niwa-journal-process--finish (state result)
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
                           (niwa-journal-claim-settle (plist-get state :claim) status payload))
                (when (and success (plist-get state :outcome))
                  (plist-put state :entry (niwa-journal-process--recover workspace capture nil result))))
            (error (plist-put state :error (error-message-string err))))
        (unless (plist-get state :entry)
          (niwa-telemetry-record-workspace
           workspace 'journal-digest-failed
           :capture-id (plist-get capture :id)
           :trigger (ignore-errors (niwa-journal-capture-trigger workspace capture))
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
          (niwa-journal-claim-settle (plist-get state :admission) status ""))
        (when (eq state (gethash (plist-get state :key) niwa-journal-process--running))
          (remhash (plist-get state :key) niwa-journal-process--running))))))

(defun niwa-journal-process-cancel (workspace)
  "Cancel this client's running digest for WORKSPACE, preserving its evidence."
  (when-let* ((state (gethash (niwa-journal-process--key workspace)
                              niwa-journal-process--running)))
    (let ((cancel (plist-get state :cancel)))
      (niwa-journal-process--finish state '(:outcome aborted))
      (when cancel (funcall cancel)))
    t))

(defun niwa-journal-process--start (state)
  "Start STATE's bounded request from frozen policy and evidence."
  (let ((workspace (plist-get state :workspace))
        (capture (plist-get state :capture)))
    (condition-case err
        (let* ((policy (niwa-journal-process--policy workspace capture))
               (source (niwa-journal-process--source workspace capture policy)))
          (plist-put
           state :timer
           (run-at-time
            (max 0 (- (plist-get (plist-get state :claim) :expires-at)
                      (niwa-session-control-fs-target-time
                       (plist-get (plist-get state :claim) :directory))))
            nil
            (lambda ()
              (let ((cancel (plist-get state :cancel)))
                (niwa-journal-process--finish
                 state '(:outcome error :error-class timeout :error "Digest request timed out"))
                (when cancel (funcall cancel))))))
          (unless (niwa-journal-claim-owned-p
                   (plist-get state :admission) (plist-get state :claim))
            (error "Digest preparation expired before inference"))
          ;; The generator snapshots configuration in its own request buffer.
          ;; This temporary caller supplies streaming policy, not a session.
          (with-temp-buffer
            (setq-local gptel-stream (plist-get (plist-get capture :policy) :stream))
            (let ((cancel (niwa-context-summary-generate
                           source 'digest
                           (lambda (result) (niwa-journal-process--finish state result))
                           :policy policy)))
              (unless (plist-get state :settled) (plist-put state :cancel cancel)))))
      (error
       (niwa-journal-process--finish
        state (list :outcome 'error :error-class 'unavailable :error (error-message-string err))))))
  state)

(defun niwa-journal-process-discard (workspace id &optional original-source)
  "Explicitly discard WORKSPACE's pending capture ID, retaining an omission.
Cancel this client's matching request first.  A foreign or unrelated live
request remains untouched.  An already accepted digest must recover before
its job can retire.  ORIGINAL-SOURCE identifies an unreadable capture's
original source and must contain its matching valid pin."
  (let* ((key (niwa-journal-process--key workspace))
         (running (gethash key niwa-journal-process--running)))
    (when (and running (equal id (plist-get (plist-get running :capture) :id)))
      (niwa-journal-process-cancel workspace))
    (niwa-journal-process--with-admission
     workspace
     (lambda (admission)
       (let* ((attempts (niwa-journal-process--attempts workspace (list :id id)))
              (outcome (cdr (niwa-journal-claim-current-settlement attempts)))
              (discard (niwa-journal-discard-accepted outcome id))
              (capture (ignore-errors (niwa-journal-capture--read workspace id))))
         (cond
          (discard (niwa-journal-discard-complete workspace discard))
          ((niwa-journal-capture--marked-p workspace id "retired")
           (or (niwa-journal-store-entry-for-capture
                (niwa-journal-store-entries (niwa-workspace-root workspace)) id)
               (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Journal capture is no longer pending")))
          ((eq 'completed (plist-get outcome :status))
           (unless capture (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Restore the capture descriptor to recover its accepted result"))
           (niwa-journal-process--recover workspace capture nil))
          ((not (niwa-journal-capture--marked-p workspace id "ready"))
           (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Journal capture is no longer pending"))
          (t
           (let* ((payload (niwa-journal-discard-prepare workspace id original-source))
                  (claim (niwa-journal-claim-acquire attempts niwa-journal-process--timeout-seconds
                                                        (plist-get admission :expires-at)))
                  (accepted (and claim (niwa-journal-claim-settle claim 'cancelled payload))))
             (unless accepted (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Journal capture has another live owner"))
             (niwa-journal-discard-complete
              workspace (niwa-journal-discard-accepted accepted id)))))))
     "Journal processing is busy; discard when its current claim settles")))

(defun niwa-journal-process-retry (workspace id)
  "Explicitly retry WORKSPACE's pending capture ID once, or recover its result.
This never resets the durable automatic-attempt count or changes model policy."
  (unless niwa-journal-enabled (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Enable journaling before retrying a capture"))
  (let ((capture (cl-find id (niwa-journal-capture-list workspace)
                          :key (lambda (capture) (plist-get capture :id)) :test #'equal)))
    (unless capture (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Journal capture is no longer pending"))
    (when (plist-get capture :unreadable)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Journal capture is unreadable: %s" (plist-get capture :error)))
    (unless (niwa-journal-capture-trigger workspace capture)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Journal checkpoint is not sealed yet"))
    (or (niwa-journal-process-next workspace id)
        (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Journal processing is busy; retry when its current claim settles"))))

(defun niwa-journal-process--prepare (workspace &optional retry-id)
  "Recover and select one fenced capture in WORKSPACE without model access.
Return compact claims and a capture ID, an :entry for recovered RETRY-ID,
or nil.  The selected capture and admission share their original deadline.
Sealed captures whose automatic attempts have all settled or expired are
listed under :exhausted, at most eight, as (:id ID :session-name NAME)."
  (when niwa-journal-enabled
    (let* ((key (niwa-journal-process--key workspace))
           (admission (and (not (gethash key niwa-journal-process--running))
                           (niwa-journal-claim-acquire
                            (niwa-journal-store-claim-directory key 'digest-run)
                            niwa-journal-process--timeout-seconds)))
           state recovered exhausted)
      (when admission
        (unwind-protect
            (let ((entries (niwa-journal-store-entries (niwa-workspace-root workspace))))
              (dolist (capture (niwa-journal-capture-list workspace))
                (unless (or state
                            (and retry-id (not (equal retry-id (plist-get capture :id)))))
                  (condition-case err
                      (let ((entry (niwa-journal-process--recover workspace capture entries)))
                        (if entry
                            (when retry-id (setq recovered entry))
                          (when (and (not (plist-get capture :unreadable))
                                     (niwa-journal-capture-trigger workspace capture))
                            (let* ((directory (niwa-journal-process--attempts workspace capture))
                                   (settlement (niwa-journal-claim-current-settlement directory))
                                   (previous (car settlement))
                                   (spent (and (not retry-id)
                                               (>= (or (plist-get previous :generation) 0)
                                                   niwa-journal-process--automatic-attempts)))
                                   (claim (and (not spent)
                                               (niwa-journal-claim-acquire
                                                directory niwa-journal-process--timeout-seconds
                                                (plist-get admission :expires-at)))))
                              (when (and spent
                                         (or (cdr settlement)
                                             (>= (niwa-session-control-fs-target-time directory)
                                                 (plist-get previous :expires-at))))
                                (push (list :id (plist-get capture :id)
                                            :session-name
                                            (truncate-string-to-width
                                             (or (plist-get (plist-get capture :metadata) :session-name) "")
                                             60))
                                      exhausted))
                              (when claim
                                (setq state (list :capture-id (plist-get capture :id)
                                                  :claim claim :admission admission)))))))
                    (error
                     (if (or retry-id state)
                         (signal (car err) (cdr err))
                       (niwa-telemetry-record-workspace
                        workspace 'journal-digest-failed :capture-id (plist-get capture :id)
                        :outcome 'retained :error-class 'recovery)))))))
          (unless state (niwa-journal-claim-settle admission 'completed ""))))
      (let ((result (or state (and recovered (list :entry recovered)))))
        ;; The worker reply is bounded; the job browser lists every capture.
        (if exhausted
            (append result (list :exhausted (seq-take (nreverse exhausted) 8)))
          result)))))

(defun niwa-journal-process--warn-exhausted (workspace exhausted)
  "Warn once per Emacs session about newly EXHAUSTED captures in WORKSPACE.
EXHAUSTED holds (:id ID :session-name NAME) descriptions from preparation."
  (when-let* ((fresh (seq-remove (lambda (capture)
                                   (gethash (list (niwa-diagnostic-scope workspace) (plist-get capture :id))
                                            niwa-journal-process--exhausted-warned))
                                 exhausted)))
    (dolist (capture fresh)
      (puthash (list (niwa-diagnostic-scope workspace) (plist-get capture :id)) t niwa-journal-process--exhausted-warned))
    (niwa-diagnostic-warning
     (niwa-diagnostic-scope workspace) 'niwa
     (format "Journal digest generation failed %d times for %s; retry or discard %s from M-x niwa-journal-jobs"
             niwa-journal-process--automatic-attempts
             (mapconcat (lambda (capture)
                          (let ((name (plist-get capture :session-name)))
                            (format "%s%s" (substring (plist-get capture :id) 0 12)
                                    (if (string-empty-p name) "" (format " (%s)" name)))))
                        fresh ", ")
             (if (cdr fresh) "them" "it"))
     :warning)))

(defun niwa-journal-process--prepared (workspace prepared)
  "Start WORKSPACE's request from compact PREPARED claims in the editor.
Freshly read the selected capture and recheck both claims before inference.
A failed handoff releases admission while retaining the capture for retry.
Newly exhausted captures listed in PREPARED are reported first."
  (niwa-journal-process--warn-exhausted workspace (plist-get prepared :exhausted))
  (if (plist-get prepared :entry)
      (plist-get prepared :entry)
    (when (plist-get prepared :claim)
      (let* ((key (niwa-journal-process--key workspace))
             (claim (plist-get prepared :claim))
             (admission (plist-get prepared :admission))
             state)
        (unwind-protect
            (when niwa-journal-enabled
              (unless (and (not (gethash key niwa-journal-process--running))
                           (niwa-journal-claim-owned-p admission claim))
                (error "Digest preparation no longer owns its claims"))
              (let ((capture (or (niwa-journal-capture--read
                                  workspace (plist-get prepared :capture-id))
                                 (error "Prepared journal capture is missing"))))
                (setq state (list :key key :workspace workspace :capture capture
                                  :claim claim :admission admission :settled nil))
                (puthash key state niwa-journal-process--running)
                (niwa-journal-process--start state)))
          (unless state
            (ignore-errors (niwa-journal-claim-settle claim 'cancelled ""))
            (ignore-errors (niwa-journal-claim-settle admission 'cancelled ""))))))))

(defun niwa-journal-process-next (workspace &optional retry-id)
  "Recover accepted results and start at most one digest in WORKSPACE.
Only sealed jobs run; three automatic attempts exhaust a job, which is
reported once as a warning.  Return the new
request state or nil when disabled, busy, or no eligible work remains.
RETRY-ID selects an explicit additional attempt; a recovered accepted result
returns its public entry.  Explicit retry reports recovery errors."
  (niwa-journal-process--prepared
   workspace (niwa-journal-process--prepare workspace retry-id)))

(defun niwa-journal-process-prepare-child (workspace payload)
  "Recover when requested by PAYLOAD and prepare WORKSPACE digest admission."
  (when (plist-get payload :recover) (niwa-journal-recovery-run workspace))
  (list :ok t :prepared (niwa-journal-process--prepare workspace)))

(provide 'niwa-journal-process)
;;; niwa-journal-process.el ends here
