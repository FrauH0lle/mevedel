;;; mevedel-memory-pass.el -- Workspace consolidation coordination -*- lexical-binding: t -*-

;;; Commentary:

;; Run one workspace-owned consolidation independently of a live conversation.
;; Target-native claims fence inference, accepted storage precedes publication,
;; and a successor recovers older results before selecting its own evidence.

;;; Code:

(require 'cl-lib)
(require 'mevedel-journal-cleanup)
(require 'mevedel-journal-index)
(require 'mevedel-journal-worker)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-review)
(require 'mevedel-memory-store)

;; `mevedel-memory-list'
(declare-function mevedel-memory-list--finished "mevedel-memory-list" (workspace result))
(autoload 'mevedel-memory-list--finished "mevedel-memory-list")

;; `mevedel-telemetry'
(declare-function mevedel-telemetry--monotonic-now "mevedel-telemetry" ())
(declare-function mevedel-telemetry-memory-pass-settle "mevedel-telemetry" (handle))
(declare-function mevedel-telemetry-memory-pass-start "mevedel-telemetry" (workspace pass-id))
(declare-function mevedel-telemetry-record-workspace "mevedel-telemetry" (workspace event &rest props))
(autoload 'mevedel-telemetry--monotonic-now "mevedel-telemetry")
(autoload 'mevedel-telemetry-memory-pass-settle "mevedel-telemetry")
(autoload 'mevedel-telemetry-memory-pass-start "mevedel-telemetry")
(autoload 'mevedel-telemetry-record-workspace "mevedel-telemetry")

(defcustom mevedel-memory-consolidation-mode 'propose
  "How memory consolidation is scheduled and proposals are handled.
Manual runs only on request.  Propose also runs automatically, at digest
publication, completed root turns, session opening and idle maintenance, and
leaves its proposals for approval.  Auto catches ready backlogs up at later idle
opportunities while shared-note decisions make progress, under a daily ceiling,
and applies fresh
memory and instruction proposals, including `AGENTS.md' changes, through the
checked decision path without prior review."
  :type '(choice (const manual) (const propose) (const auto))
  :group 'mevedel)

(defcustom mevedel-memory-consolidation-min-hours 24
  "Minimum hours between successful general memory reviews.
Focused reviews do not move this clock.  Explicit commands bypass the gate."
  :type '(number :tag "Hours") :group 'mevedel)

(defcustom mevedel-memory-consolidation-min-digests 5
  "Minimum eligible unreviewed digests needed for automatic memory review.
Completed digests from running sessions are eligible. One day before ordinary
journal expiry, a smaller backlog may be reviewed. Explicit commands bypass
this threshold."
  :type '(integer :tag "Digests") :group 'mevedel)

(defcustom mevedel-memory-consolidation-max-batches-per-day 48
  "Maximum automatic general reviews in auto mode per rolling 24 hours.
Successful general reviews, including explicit reviews, consume this allowance.
Auto catches up a ready backlog at later idle opportunities only while applied
shared-note decisions make progress, instead of waiting for
`mevedel-memory-consolidation-min-hours'.  This is a hard spend ceiling, not the
main catch-up throttle.  Each batch retains the ordinary request limits; the
default permits up to 48 successful batches, not one larger request.  Zero
disables automatic review in auto mode.  Explicit review and drain bypass it."
  :type 'natnum :group 'mevedel)

(defvar mevedel-memory-pass--inhibit-scheduling nil
  "Non-nil suppresses automatic opportunities during exit or isolated tests.")

(defvar mevedel-memory-pass--pending (make-hash-table :test #'eq)
  "Coalesced turn-completion timers, keyed by their workspace objects.")

(defvar mevedel-memory-pass--running (make-hash-table :test #'equal)
  "This client's active requests, keyed by physical pass ownership directory.")

(defvar mevedel-memory-pass--deferred (make-hash-table :test #'eq)
  "Original application contexts and timers for held automatic note decisions.")

(defvar mevedel-memory-pass--drains (make-hash-table :test #'equal)
  "Explicit drains keyed by physical pass ownership directory.")

(defun mevedel-memory-pass--timing-add (state key duration)
  "Accumulate known millisecond DURATION under KEY in STATE's timings."
  (when (numberp duration)
    (let ((timings (plist-get state :phase-durations)))
      (plist-put state :phase-durations
                 (plist-put timings key (+ (or (plist-get timings key) 0) duration))))))

(defun mevedel-memory-pass--timing-start (state key)
  "Start parent-local interval KEY in STATE, without resetting a live interval."
  (unless (assq key (plist-get state :phase-starts))
    (plist-put state :phase-starts
               (cons (cons key (mevedel-telemetry--monotonic-now)) (plist-get state :phase-starts)))))

(defun mevedel-memory-pass--timing-end (state key)
  "Accumulate and close parent-local interval KEY in STATE, if it started."
  (when-let* ((start (assq key (plist-get state :phase-starts))))
    (plist-put state :phase-starts (delq start (plist-get state :phase-starts)))
    (mevedel-memory-pass--timing-add
     state key (round (* 1000 (max 0 (- (mevedel-telemetry--monotonic-now) (cdr start))))))))

(defun mevedel-memory-pass--producer (result)
  "Return the producer label of review RESULT; housekeeping has no model."
  (if (plist-get result :policy) (mevedel-model--provider-label (plist-get result :policy))
    "housekeeping"))

(defun mevedel-memory-pass--general-times (workspace entries)
  "Return successful general-review times from WORKSPACE ENTRIES.
Include retired completion metadata so expiry cannot erase the clock.  A
malformed retirement marker stops automatic admission for inspection."
  (let (times)
    (dolist (entry entries)
      (when (and (eq (plist-get entry :kind) 'consolidation)
                 (string-empty-p (plist-get entry :focus)))
        (push (float-time (date-to-time (plist-get entry :created))) times)))
    (dolist (path (mevedel-session-control-fs-list-directory
                   (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))) "retired-passes")
                   mevedel-journal-store-id-regexp))
      (let* ((text (mevedel-session-control-fs-read-file path 'utf-8-unix 513))
             (record (json-parse-string text))
             (scope (and (hash-table-p record) (gethash "scope" record)))
             (date (and (hash-table-p record) (gethash "created" record)))
             (hash (and (hash-table-p record) (gethash "sha256" record))))
        (unless (and (<= (string-bytes text) 512) (hash-table-p record)
                     (= 3 (hash-table-count record)) (member scope '("general" "focused"))
                     (stringp hash) (mevedel-journal-store-id-p hash)
                     (stringp date) (string-match-p (concat "\\`" mevedel-journal-store--time-regexp "\\'") date))
          (error "Invalid retired memory review clock"))
        (when (equal scope "general")
          (push (float-time (date-to-time date)) times))))
    times))

(defun mevedel-memory-pass--automatic-selection (workspace entries &optional notes-ready catch-up)
  "Return an eligible automatic batch from fresh WORKSPACE ENTRIES, or nil.
The caller owns consolidation admission and has recovered prior publications.
Check elapsed target time before selecting completed published evidence.
NOTES-READY, when non-nil, is called only after the time gate passes and the
digest thresholds fail; non-nil makes ready shared-folder work sufficient by
itself, irrespective of the digest count or whether journaling is enabled.
With CATCH-UP in auto mode, bypass time/count gates, but not the daily cap.
Cache only the next client-side opportunity, never coverage or write authority."
  (unless (and (numberp mevedel-memory-consolidation-min-hours)
               (>= mevedel-memory-consolidation-min-hours 0)
               (integerp mevedel-memory-consolidation-min-digests)
               (> mevedel-memory-consolidation-min-digests 0)
               (natnump mevedel-memory-consolidation-max-batches-per-day))
    (error "Invalid memory consolidation thresholds"))
  (let* ((times (mevedel-memory-pass--general-times workspace entries))
         (last (and times (apply #'max times)))
         (now (mevedel-session-control-fs-target-time (mevedel-workspace-root workspace)))
         (recent (seq-filter (lambda (time) (> time (- now 86400))) times))
         (auto (eq mevedel-memory-consolidation-mode 'auto))
         (catch-up (and auto catch-up))
         (capped (and auto (>= (length recent) mevedel-memory-consolidation-max-batches-per-day)))
         (remaining (cond (capped (if recent (- (+ (apply #'min recent) 86400) now) 86400))
                          (catch-up 0)
                          (last (- (+ last (* 3600 mevedel-memory-consolidation-min-hours)) now))
                          (t 0))))
    (setf (mevedel-workspace-memory-schedule workspace)
          (list :after (+ (float-time) (if (> remaining 0) remaining 600))
                :hours mevedel-memory-consolidation-min-hours
                :digests mevedel-memory-consolidation-min-digests
                :age mevedel-journal-max-age-days
                :mode mevedel-memory-consolidation-mode
                :batches mevedel-memory-consolidation-max-batches-per-day
                :catch-up catch-up))
    (when (and (not capped) (<= remaining 0))
      (let ((selection (mevedel-memory-pass-select entries)))
        (when (or (and catch-up (or (plist-get selection :entries)
                                   (and notes-ready (funcall notes-ready))))
                  (>= (plist-get selection :eligible) mevedel-memory-consolidation-min-digests)
                  (and (plist-get selection :entries)
                       (integerp mevedel-journal-max-age-days)
                       (>= mevedel-journal-max-age-days 0)
                       (>= (- now (float-time (date-to-time
                                               (plist-get (car (plist-get selection :entries)) :created))))
                           (* (max 0 (1- mevedel-journal-max-age-days)) 86400)))
                  (and notes-ready (funcall notes-ready)))
          selection)))))

;;;###autoload
(defun mevedel-memory-pass-schedule (workspace)
  "Offer automatic review at an activity boundary in WORKSPACE.
Manual mode, cached thresholds and pending opportunities need no target I/O.
Defer the cold observation until transport is idle; a pass rechecks admission
under target ownership. No completion recursively schedules another pass.
Either published journal state or a shared working folder makes a workspace
a candidate, so shared notes are cleaned up even with journaling disabled."
  (unless (or mevedel-memory-pass--inhibit-scheduling
              (eq mevedel-memory-consolidation-mode 'manual)
              (gethash (mevedel-memory-store--claim-directory workspace) mevedel-memory-pass--drains))
    (let ((cached (mevedel-workspace-memory-schedule workspace)) opportunity queued-at)
      (unless (or (gethash workspace mevedel-memory-pass--pending)
                  (and (equal (plist-get cached :hours) mevedel-memory-consolidation-min-hours)
                       (equal (plist-get cached :digests) mevedel-memory-consolidation-min-digests)
                       (equal (plist-get cached :age) mevedel-journal-max-age-days)
                       (equal (plist-get cached :mode) mevedel-memory-consolidation-mode)
                       (equal (plist-get cached :batches) mevedel-memory-consolidation-max-batches-per-day)
                       (< (float-time) (or (plist-get cached :after) 0))))
        (setf (mevedel-workspace-memory-schedule workspace)
              (setq opportunity
                    (list :after (+ (float-time) 600)
                          :hours mevedel-memory-consolidation-min-hours
                          :digests mevedel-memory-consolidation-min-digests
                          :age mevedel-journal-max-age-days
                          :mode mevedel-memory-consolidation-mode
                          :batches mevedel-memory-consolidation-max-batches-per-day
                          :catch-up (plist-get cached :catch-up))))
        (setq queued-at (mevedel-telemetry--monotonic-now))
        (mevedel-transport-schedule-idle
         mevedel-memory-pass--pending workspace 'memory-pass (mevedel-workspace-root workspace)
         (lambda ()
           (unless (or mevedel-memory-pass--inhibit-scheduling
                       (eq mevedel-memory-consolidation-mode 'manual))
             (condition-case err
                 (when (or (mevedel-session-control-fs-path-exists-p
                            (mevedel-journal-store-directory (mevedel-workspace-root workspace)))
                           (mevedel-session-control-fs-path-exists-p
                            (mevedel-memory-notes-directory workspace)))
                   (unless (mevedel-memory-pass-start
                            workspace (lambda (result) (mevedel-memory-list--finished workspace result))
                            :automatic t :queued-at queued-at)
                     ;; A busy owner is not an admission observation. Preserve
                     ;; any fresh gate result, but allow the next activity offer.
                     (when (eq opportunity (mevedel-workspace-memory-schedule workspace))
                       (setf (mevedel-workspace-memory-schedule workspace) nil))))
               (error (display-warning 'mevedel
                                       (format "Automatic memory review failed: %s" (error-message-string err))
                                       :warning))))))))))

(defun mevedel-memory-pass-stop-all ()
  "Cancel this client's queued and running memory reviews on exit."
  (mevedel-transport-cancel-idle mevedel-memory-pass--pending 'memory-pass)
  (maphash (lambda (_workspace retry) (cancel-timer (plist-get retry :timer))) mevedel-memory-pass--deferred)
  (clrhash mevedel-memory-pass--deferred)
  (maphash (lambda (_key state) (mevedel-memory-pass-cancel (plist-get state :workspace)))
           mevedel-memory-pass--drains)
  (maphash (lambda (_key state) (mevedel-memory-pass-cancel (plist-get state :workspace)))
           mevedel-memory-pass--running))

(defun mevedel-memory-pass-select (entries &optional focused)
  "Select at most twenty completed digests from validated public ENTRIES.
FOCUSED also admits previously reviewed evidence. Published digests are
immutable completed work, including from sessions that are still running.

Return :entries oldest first, :eligible count and :remaining eligible backlog.
Break equal timestamps by digest ID. This observation acquires no ownership,
pins no evidence and consumes no coverage; the caller coordinates publication."
  (let ((eligible (if focused
                      (seq-filter (lambda (entry) (eq (plist-get entry :kind) 'digest)) entries)
                    (mevedel-journal-index-unreviewed entries))))
    (setq eligible
          (sort eligible (lambda (left right)
                           (let ((first (plist-get left :created)) (second (plist-get right :created)))
                             (if (equal first second)
                                 (string-lessp (plist-get left :id) (plist-get right :id))
                               (string-lessp first second))))))
    (list :entries (seq-take eligible 20) :eligible (length eligible)
          :remaining (max 0 (- (length eligible) 20)))))

(defun mevedel-memory-pass-running (workspace)
  "Return this client's running consolidation state for WORKSPACE, or nil.
Its :request contains an inspectable :buffer. Other clients' ownership is
represented by the target claim and does not grant local cancellation rights."
  (gethash (mevedel-memory-store--claim-directory workspace) mevedel-memory-pass--running))

(defun mevedel-memory-pass-drain-status (workspace)
  "Return this client's explicit backlog drain state for WORKSPACE, or nil."
  (gethash (mevedel-memory-store--claim-directory workspace) mevedel-memory-pass--drains))

(defun mevedel-memory-pass--drain-step (state)
  "Offer the next bounded review for explicit drain STATE.
Local applications, cleanup and busy transports wait sixty seconds, without
capturing notes or starting an empty model request.  Other sessions' turns
do not; capture leaves the notes they use for later.  Every continuation
yields to input."
  (let* ((workspace (plist-get state :workspace))
         (key (plist-get state :key)))
    (when (eq state (gethash key mevedel-memory-pass--drains))
      (plist-put state :timer nil)
      (condition-case err
          (let* ((busy (or (mevedel-journal-cleanup-active-p workspace)
                           (mevedel-transport-busy-p (mevedel-workspace-root workspace))
                           (input-pending-p)))
                 (fresh-key (mevedel-memory-store--claim-directory workspace)))
            ;; Busy/path IO can dispatch cancellation, recovery or another pass.
            ;; Only exact, non-IO observations follow this admission boundary.
            (when (eq state (gethash key mevedel-memory-pass--drains))
              (unless (and (equal fresh-key key)
                           (equal (plist-get state :root) (mevedel-workspace-root workspace)))
                (error "Memory drain workspace changed"))
              (cond
               ((not (buffer-live-p (plist-get state :origin-buffer)))
		(error "Memory drain caller buffer was closed"))
               ((or busy (gethash key mevedel-memory-pass--running)
                    (mevedel-memory-decision-admitted-p key))
		(plist-put state :status 'waiting)
		(plist-put state :timer (run-at-time 60 nil #'mevedel-memory-pass--drain-step state)))
               (t
		(plist-put state :status 'running)
		(with-current-buffer (plist-get state :origin-buffer)
		  (mevedel-memory-pass-start
		   workspace
		   (lambda (result)
                     (when (eq state (gethash key mevedel-memory-pass--drains))
                       (when (eq (plist-get result :outcome) 'success)
                         (plist-put state :queued-at (mevedel-telemetry--monotonic-now))
			 (plist-put state :batch (1+ (plist-get state :batch))))
                       (if (and (memq (plist-get result :outcome) '(success restart-required))
				(not (plist-get result :stopped))
				(not (plist-get result :error))
				(not (plist-get result :recovery-required)))
			   (progn
                             (plist-put state :notes-remaining (plist-get result :notes-remaining))
                             (plist-put state :remaining (plist-get result :remaining))
                             (plist-put state :timer (run-at-time .05 nil #'mevedel-memory-pass--drain-step state)))
			 (remhash key mevedel-memory-pass--drains)
			 (plist-put state :status
                                    (cond ((plist-get result :stopped) 'aborted)
					  ((or (plist-get result :error) (plist-get result :recovery-required)) 'error)
					  (t (plist-get result :outcome)))))
                       (funcall (plist-get state :callback)
				(append (list :batch (plist-get state :batch)) result))))
			   :drain state :queued-at (plist-get state :queued-at)))))))
        (error
         (when (eq state (gethash key mevedel-memory-pass--drains))
           (remhash key mevedel-memory-pass--drains)
           (plist-put state :status 'error)
           (funcall (plist-get state :callback)
                    (list :outcome 'error :error (error-message-string err) :batch (plist-get state :batch)))))))))

(defun mevedel-memory-pass-drain (workspace callback)
  "Drain ready WORKSPACE notes and digests through successive bounded reviews.
CALLBACK receives each result with a :batch count of completed batches.
The fresh final empty selection starts no model request.  Stop on failure,
application errors, recovery requirements, caller closure or
`mevedel-memory-pass-cancel'.  Explicit drain bypasses
automatic timing/count gates and daily cap, not ownership or request limits."
  (when (or (mevedel-memory-pass-drain-status workspace)
            (mevedel-memory-pass-running workspace))
    (user-error "Memory consolidation is already running"))
  (let* ((root (copy-sequence (mevedel-workspace-root workspace)))
         (key (mevedel-memory-store--claim-directory workspace))
         (state (list :workspace workspace :root root :key key :origin-buffer (current-buffer)
                      :queued-at (mevedel-telemetry--monotonic-now)
                      :callback callback :batch 0 :status 'queued :timer nil)))
    (unless (equal root (mevedel-workspace-root workspace)) (error "Memory drain workspace changed"))
    (when (or (gethash key mevedel-memory-pass--drains) (gethash key mevedel-memory-pass--running))
      (user-error "Memory consolidation is already running"))
    (puthash key state mevedel-memory-pass--drains)
    (plist-put state :timer (run-at-time 0 nil #'mevedel-memory-pass--drain-step state))
    state))

(defun mevedel-memory-pass--apply (state accepted)
  "Apply ACCEPTED memory proposals sequentially for completed pass STATE.
Each application reacquires workspace and original-target ownership, for
instruction proposals as for memory ones.  Stop before further proposals if
cancellation was requested, holding the rest.
A shared-note decision waits while an open session uses its file; a retry
applies it once that use ends.  Other notes and earlier held ones proceed.
Each note rechecks use before and at its write, since IO yields.
Count distinct files only through this call's confirmed-write notification."
  (mevedel-memory-pass--timing-start state :application-duration-ms)
  (let ((workspace (plist-get state :workspace))
        (pass (plist-get (plist-get accepted :prepared) :id))
        (applied 0) (held 0) (unapplied 0) deferred files)
    (plist-put state :application-context
               (mevedel-memory-decision--operation-context workspace (plist-get state :origin-buffer)))
    (dolist (proposal (plist-get accepted :proposals))
      (if (plist-get state :stop-apply)
          (cl-incf held)
        (pcase (mevedel-memory-pass--apply-automatic
                workspace pass proposal
                (lambda (paths) (cl-incf applied) (setq files (append paths files))))
          ('held (push (plist-get proposal :file) deferred) (cl-incf held))
          ('quit (plist-put state :stop-apply t) (cl-incf unapplied))
          ('applied nil)
          (_ (cl-incf unapplied)))))
    (unless (plist-get state :stop-apply)
      (let ((earlier (mevedel-memory-pass--apply-pending
                      workspace pass (lambda (file) (push file deferred)))))
        (cl-incf applied (car earlier))
        (cl-incf unapplied (cdr earlier))))
    (when deferred
      (mevedel-memory-pass--defer-notes state deferred))
    (list :updated-files (length (delete-dups files)) :applied-count applied
          :held-count held :unapplied-count unapplied)))

(defun mevedel-memory-pass--supersede (workspace accepted)
  "Supersede older undecided note dispositions replaced by ACCEPTED.
Report dispositions that could not be superseded in WORKSPACE; they stay
in the cockpit for an explicit decision."
  (condition-case err
      (let ((outcome (mevedel-memory-notes-supersede workspace accepted)))
        (when (> (cdr outcome) 0)
          (display-warning 'mevedel (format "%d older shared-note decisions could not be superseded"
                                            (cdr outcome))
                           :warning)))
    (error (display-warning 'mevedel (format "Superseding older shared-note decisions failed: %s"
                                             (error-message-string err))
                            :warning))))

(defun mevedel-memory-pass--async-application-p (workspace accepted)
  "Return non-nil when WORKSPACE and ACCEPTED's targets support local workers."
  (and (mevedel-journal-worker-supported-p workspace)
       (if accepted
           (not (cl-some (lambda (root) (file-remote-p (plist-get (cdr root) :dir)))
                         (plist-get (plist-get (plist-get accepted :prepared) :scope) :roots)))
         (not (cl-some #'file-remote-p
                       (append (mapcar (lambda (root) (plist-get root :dir))
                                       (mevedel-system--memory-roots workspace))
                               (mevedel-system-workspace-config-files workspace)
                               (list (or (mevedel-memory-notes-directory workspace) ""))))))))

(defun mevedel-memory-pass--pending-items (workspace skip)
  "Return pending automatic proposal identities in WORKSPACE, excluding SKIP.
Authenticate status within this read-only selection to omit every decided
proposal, as synchronous pending selection does.  Retain identities only;
each later attempt still freshly reads accepted proof and status."
  (let ((entries (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
        (passes (make-hash-table :test #'equal)) items)
    (dolist (accepted (mevedel-memory-notes-passes workspace entries))
      (clrhash passes)
      (let ((pass (plist-get (plist-get accepted :prepared) :id)))
        (when (and (not (equal pass skip))
                   (eq 'auto (plist-get (cdr (mevedel-memory-notes-root
                                              (plist-get (plist-get accepted :prepared) :scope))) :mode)))
          (dolist (proposal (plist-get accepted :proposals))
            (unless (mevedel-memory-decision-status workspace (plist-get proposal :id) entries passes)
              (push (list :pass pass :proposal (plist-get proposal :id) :pending t
                          :note (mevedel-memory-proposal-note-action-p (plist-get proposal :action))
                          :file (plist-get proposal :file))
                    items))))))
    (nreverse items)))

(defun mevedel-memory-pass--application-context-current-p (context)
  "Return non-nil when CONTEXT's original actor still admits automatic writes.
Stored identity is an expectation, not authority; read it afresh on each check."
  (and (not mevedel-journal-worker--stopping)
       (mevedel-memory-decision--operation-origin-current-p context)
       (eq (buffer-local-value 'mevedel-memory-consolidation-mode (plist-get context :origin)) 'auto)
       (let ((mevedel-workspace-identity--cache (make-hash-table :test #'equal)))
         (equal (plist-get context :workspace-id)
                (mevedel-workspace-identity-read (plist-get context :root))))
       ;; Identity IO may dispatch events; never reuse the actor check from
       ;; before that yield to admit a dead, reassigned or newly held origin.
       (not mevedel-journal-worker--stopping)
       (mevedel-memory-decision--operation-origin-current-p context)
       (eq (buffer-local-value 'mevedel-memory-consolidation-mode (plist-get context :origin)) 'auto)))

(defun mevedel-memory-pass--application-live-p (state job)
  "Return non-nil while STATE's current JOB can admit a forward automatic write."
  (and (eq job (plist-get state :application)) (not (plist-get job :done))
       (not (plist-get state :stop-apply))
       (mevedel-memory-pass--application-context-current-p job)))

(defun mevedel-memory-pass--application-next (state job)
  "Yield before continuing STATE's current application JOB."
  (when (and (eq job (plist-get state :application)) (not (plist-get job :done)))
    (plist-put job :timer (run-at-time .01 nil #'mevedel-memory-pass--application-step state job))))

(defun mevedel-memory-pass--hold-file (job file)
  "Record held note FILE in JOB, mutating the shared plist in place.
The `setf' expansion of `plist-get' would rebind a local copy instead."
  (plist-put job :deferred (cons file (plist-get job :deferred))))

(defun mevedel-memory-pass--application-pending (state job)
  "Select pending identities for STATE's JOB in its owned recovery child."
  (with-current-buffer (plist-get state :origin-buffer)
    (plist-put job :operation
               (mevedel-memory-decision--start-operation
                (plist-get state :workspace)
                (lambda (_claim _currentp reply)
                  (if (plist-get reply :synchronous)
                      (mevedel-memory-pass--pending-items (plist-get state :workspace) (plist-get job :pass))
                    (plist-get reply :items)))
                (lambda (reply)
                  (plist-put job :operation nil)
                  (when (and (eq job (plist-get state :application)) (not (plist-get job :done)))
                    (plist-put job :recovery (plist-get reply :recovery))
                    (mevedel-memory-pass--timing-add state :application-recovery-round-trip-ms
                                                   (plist-get reply :recovery-round-trip-ms))
                    (when (plist-get reply :cancelled)
                      (plist-put job :cancelled t)
                      (plist-put state :stop-apply t))
                    (cond
                     ((plist-get reply :error)
                      (plist-put job :error (or (plist-get job :error) (plist-get reply :error)))
                      (cl-incf (plist-get job :unapplied)))
                     ((or (plist-get reply :held) (plist-get reply :cancelled))
                      (plist-put state :stop-apply t))
                     (t (plist-put job :items (plist-get reply :value))))
                    (mevedel-memory-pass--application-next state job)))
                (lambda () (mevedel-memory-pass--application-live-p state job))
                nil (list :pending-selection t :skip (plist-get job :pass))
                (plist-get job :recovery)))))

(cl-defun mevedel-memory-pass--application-step (state job)
  "Run one owned recovery/commit operation for STATE's sequential JOB."
  (when (and (eq job (plist-get state :application)) (not (plist-get job :done)))
    (plist-put job :timer nil)
    (let ((workspace (plist-get state :workspace)) item)
      (condition-case err
	  (progn
	    (unless (mevedel-memory-pass--application-live-p state job)
	      (plist-put state :stop-apply t)
	      (cl-incf (plist-get job :held) (length (plist-get job :items)))
	      (plist-put job :items nil)
	      (plist-put job :pending-loaded t))
	    (when (and (null (plist-get job :items)) (not (plist-get job :pending-loaded)))
	      (plist-put job :pending-loaded t)
	      (mevedel-memory-pass--application-pending state job)
	      (cl-return-from mevedel-memory-pass--application-step nil))
	    (if (setq item (pop (plist-get job :items)))
		(if (and (plist-get item :note)
			 (mevedel-memory-notes-in-use-p workspace (plist-get item :file)))
		    (progn
                              (plist-put job :recovery nil)
		      (mevedel-memory-pass--hold-file job (plist-get item :file))
		      (cl-incf (plist-get job :held))
		      (mevedel-memory-pass--application-next state job))
		  (with-current-buffer (plist-get state :origin-buffer)
		    (plist-put job :operation
			       (mevedel-memory-decision--start-operation
				workspace
				(lambda (claim currentp _reply)
				  (mevedel-memory-decision--apply-owned
				   workspace claim (plist-get item :pass) (plist-get item :proposal)
				   (lambda (paths)
				     (cl-incf (plist-get job :applied))
				     (plist-put job :files (append paths (plist-get job :files))))
				   currentp (plist-get item :pending)))
				(lambda (reply)
				  (plist-put job :operation nil)
				  (when (and (eq job (plist-get state :application)) (not (plist-get job :done)))
					    (plist-put job :recovery (plist-get reply :recovery))
                                    (mevedel-memory-pass--timing-add state :application-recovery-round-trip-ms
                                                                   (plist-get reply :recovery-round-trip-ms))
				    (when (plist-get reply :error)
				      (plist-put job :error (or (plist-get job :error) (plist-get reply :error))))
				    (when (plist-get reply :cancelled)
                                      (plist-put job :cancelled t)
                                      (plist-put state :stop-apply t))
				    (let ((status (plist-get (plist-get reply :value) :status)))
				      (cond
                                               ((or (eq status 'skipped) (mevedel-memory-decision-terminal-status-p status)))
                                       ((plist-get reply :error) (cl-incf (plist-get job :unapplied)))
                                       ((and (not status) (or (plist-get reply :held) (plist-get reply :cancelled)))
                                        (if (and (plist-get reply :held) (plist-get item :note)
                                                 (mevedel-memory-pass--application-live-p state job))
                                            (mevedel-memory-pass--hold-file job (plist-get item :file))
                                          (plist-put state :stop-apply t))
                                        (cl-incf (plist-get job :held)))
				       ((eq status 'held)
					(cl-incf (plist-get job :held))
					(when (plist-get item :note) (mevedel-memory-pass--hold-file job (plist-get item :file))))
					       (t (cl-incf (plist-get job :unapplied)))))
					    ;; Only an exact recovery predecessor may survive;
					    ;; accepted/status/target proofs are always read afresh.
				    (mevedel-memory-pass--application-next state job)))
					(lambda () (mevedel-memory-pass--application-live-p state job))
                                        nil nil (plist-get job :recovery)))))
	      (plist-put job :done t)
	      (when (and (plist-get job :deferred) (not (plist-get state :stop-apply)))
		(mevedel-memory-pass--defer-notes state (plist-get job :deferred)))
	      (funcall (plist-get job :callback)
		       (list :updated-files (length (delete-dups (plist-get job :files)))
			     :cancelled (plist-get job :cancelled)
			     :applied-count (plist-get job :applied) :held-count (plist-get job :held)
			     :unapplied-count (plist-get job :unapplied) :error (plist-get job :error)))))
	((error quit)
	         ;; A completed job cannot retry its callback.  Preserve failures
	         ;; from that callback instead of silently dropping its condition.
	         (when (plist-get job :done) (signal (car err) (cdr err)))
		 (plist-put job :recovery nil)
	 (when (eq (car err) 'quit)
           (plist-put job :cancelled t)
           (plist-put state :stop-apply t)
           (when item (cl-incf (plist-get job :held))))
	 (unless (eq (car err) 'quit)
           (cl-incf (plist-get job :unapplied))
	   (plist-put job :error (or (plist-get job :error) (error-message-string err))))
	 (mevedel-memory-pass--application-next state job))))))

(defun mevedel-memory-pass--apply-async (state accepted callback)
  "Apply ACCEPTED for STATE through yielding owned operations, then CALLBACK.
Storage recovery runs in a real child; original target ownership and all live
buffer checks and mutations remain in the editor.  Nil ACCEPTED retries pending
automatic proposals without starting inference.  Return the application job."
  (mevedel-memory-pass--timing-start state :application-duration-ms)
  (let* ((pass (plist-get (plist-get accepted :prepared) :id))
         (context (or (plist-get state :application-context)
                      (mevedel-memory-decision--operation-context
                       (plist-get state :workspace) (plist-get state :origin-buffer))))
         (job (append (list :pass pass :callback callback :applied 0 :held 0 :unapplied 0 :deferred nil
                            :items (mapcar (lambda (proposal)
                                             (list :pass pass :proposal (plist-get proposal :id)
                                                   :note (mevedel-memory-proposal-note-action-p (plist-get proposal :action))
                                                   :file (plist-get proposal :file)))
                                           (plist-get accepted :proposals)))
                      (copy-sequence context))))
    (plist-put state :application-context context)
    (plist-put state :application job)
    (mevedel-memory-pass--application-next state job)
    job))

(defun mevedel-memory-pass--apply-automatic (workspace pass proposal &optional on-applied)
  "Synchronously apply PROPOSAL of PASS in WORKSPACE as automatic application.
ON-APPLIED receives confirmed paths.  Return `applied' for a terminal
decision, `held' while an open session uses a note's file (checked before and
at its write), `quit' after interruption, and `unapplied' otherwise."
  (let ((note (mevedel-memory-proposal-note-action-p (plist-get proposal :action))))
    (if (and note (mevedel-memory-notes-in-use-p workspace (plist-get proposal :file)))
        'held
      (condition-case nil
          (let ((status (plist-get (mevedel-memory-decision-apply
                                    workspace pass (plist-get proposal :id) on-applied
                                    (and note #'always))
                                   :status)))
            (cond ((mevedel-memory-decision-terminal-status-p status) 'applied)
                  ((and note (eq status 'held)) 'held)
                  (t 'unapplied)))
        (mevedel-memory-decision-held 'held)
        (quit 'quit)
        (error 'unapplied)))))

(defun mevedel-memory-pass--apply-pending (workspace &optional skip on-held)
  "Apply undecided proposals of earlier automatic note reviews in WORKSPACE.
Only reviews whose frozen mode was `auto' qualify, and only while the
current mode is still `auto'.  SKIP names a pass already handled.  Return
\(APPLIED . UNAPPLIED): proposals reaching a terminal decision, and those
that did not, including ones still waiting for their extraction.  A note
decision whose file an open session uses stays undecided; ON-HELD, when
non-nil, receives that file."
  (let ((count 0) (failed 0)
        entries
        (passes (make-hash-table :test #'equal)))
    (when (eq mevedel-memory-consolidation-mode 'auto)
      (setq entries (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
      (dolist (accepted (mevedel-memory-notes-passes workspace entries))
        (let ((pass (plist-get (plist-get accepted :prepared) :id)))
          (clrhash passes)
          (when (and (not (equal pass skip))
                     (eq 'auto (plist-get (cdr (mevedel-memory-notes-root
                                                (plist-get (plist-get accepted :prepared) :scope)))
                                          :mode)))
            (dolist (proposal (plist-get accepted :proposals))
              (cond
               ((mevedel-memory-decision-status workspace (plist-get proposal :id) entries passes))
               (t
                (unwind-protect
                    (pcase (mevedel-memory-pass--apply-automatic workspace pass proposal)
                      ('applied (cl-incf count))
                      ('held (when on-held (funcall on-held (plist-get proposal :file))))
                      ('quit (signal 'quit nil))
                      (_ (cl-incf failed)))
                  ;; Application recovers and mutates under fresh ownership;
                  ;; no earlier proof may authorize the next selection.
                  (setq entries (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
                  (clrhash passes)))))))))
    (cons count failed)))

(defun mevedel-memory-pass--defer-notes (state &optional files)
  "Retry STATE's held note decisions using its original application context.
FILES are the held notes' relative paths; the retry waits until one is free.
Without FILES, the next tick retries."
  (let ((workspace (plist-get state :workspace)))
    (unless (plist-get state :stop-apply)
      (if-let* ((retry (gethash workspace mevedel-memory-pass--deferred)))
          (plist-put retry :files (seq-union (plist-get retry :files) files))
        (let ((retry (list :timer nil :context (plist-get state :application-context)
                           :files (delete-dups (copy-sequence files)))))
          (puthash workspace retry mevedel-memory-pass--deferred)
          (plist-put retry :timer (run-with-timer 60 60 #'mevedel-memory-pass--retry-notes workspace retry)))))))

(defun mevedel-memory-pass--retry-notes (workspace &optional expected)
  "Apply WORKSPACE's held automatic note decisions once one of them is free.
A held note is free when no open session uses its file and no local
consolidation, drain or busy transport runs.
EXPECTED, when non-nil, is this timer's exact context, not a newer retry.
The retry ends after an idle attempt or invalidation of its original actor."
  (when-let* ((retry (gethash workspace mevedel-memory-pass--deferred)))
    (when (or (null expected) (eq retry expected))
      (condition-case err
          (let* ((context (plist-get retry :context))
                 (busy (or (let ((in-use (mevedel-memory-notes-in-use workspace)))
                             (and (plist-get retry :files)
                                  (cl-every (lambda (file) (member file in-use))
                                            (plist-get retry :files))))
                           (mevedel-transport-busy-p (mevedel-workspace-root workspace))))
                 (current (mevedel-memory-pass--application-context-current-p context))
                 (key (mevedel-memory-store--claim-directory workspace)))
            ;; Busy and freshness IO can dispatch cancellation or another local
            ;; admission.  Only non-IO observations follow this final boundary.
            (when (and (eq retry (gethash workspace mevedel-memory-pass--deferred))
                       (or (not current)
                           (and (not busy)
                                (not (gethash key mevedel-memory-pass--running))
                                (not (gethash key mevedel-memory-pass--drains)))))
              (cancel-timer (plist-get retry :timer))
              (remhash workspace mevedel-memory-pass--deferred)
              (when current
                (let* ((state (list :workspace workspace :origin-buffer (plist-get context :origin)
                                    :application-context context :settled t :claim (list :directory key))))
                  (puthash key state mevedel-memory-pass--running)
                  (with-current-buffer (plist-get context :origin)
                    (mevedel-memory-pass--apply-async
                     state nil
                     (lambda (outcome)
                       (when (eq state (gethash key mevedel-memory-pass--running))
                         (remhash key mevedel-memory-pass--running))
                       (when (or (plist-get outcome :error) (> (plist-get outcome :unapplied-count) 0))
                         (display-warning 'mevedel
                                          (format "Deferred memory application needs attention; see the memory cockpit%s"
                                                  (if-let* ((failure (plist-get outcome :error))) (concat ": " failure) ""))
                                          :warning)))))))))
        (error
         (cancel-timer (plist-get retry :timer))
         (when (eq retry (gethash workspace mevedel-memory-pass--deferred))
           (remhash workspace mevedel-memory-pass--deferred))
         (display-warning 'mevedel (format "Held shared-note decisions failed: %s"
                                           (error-message-string err))
                          :warning))))))

(defconst mevedel-memory-pass--rejected-replies 8
  "Number of rejected review replies retained for diagnosis.")

(defun mevedel-memory-pass--retain-rejected-reply (workspace id result)
  "Retain RESULT's rejected reply for pass ID in local WORKSPACE diagnostics.
Only the newest `mevedel-memory-pass--rejected-replies' are kept.  The text
is evidence for diagnosing the output contract, never proposals.  Failures
here are diagnostic only."
  (when-let* ((reply (plist-get result :rejected-reply))
              (root (mevedel-workspace-root workspace))
              ((not (file-remote-p root))))
    (ignore-errors
      (let ((directory (file-name-concat root ".mevedel" "state" "diagnostics" "rejected-replies")))
        (make-directory directory t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region (concat (format "<!-- pass %s; error: %s -->\n" id (plist-get result :error)) reply)
                        nil (file-name-concat directory (concat id ".md")) nil 'silent))
        (let ((files (sort (directory-files directory t "\\.md\\'")
                           (lambda (a b) (time-less-p (file-attribute-modification-time (file-attributes b))
                                                      (file-attribute-modification-time (file-attributes a)))))))
          (mapc #'delete-file (nthcdr mevedel-memory-pass--rejected-replies files)))))))

(defun mevedel-memory-pass--progress-p (workspace accepted)
  "Return non-nil if ACCEPTED's batch made shared-note progress in WORKSPACE.
Authenticate applied decisions, not proposal or aggregate application counts.
Only this batch's delete, condense or newly suppressing keep qualifies;
older pending applications and digest coverage alone do not."
  (let ((entries (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
        (passes (make-hash-table :test #'equal)))
    (seq-some
     (lambda (proposal)
       (let ((action (plist-get proposal :action)))
         (when (memq action '(note-delete note-condense note-keep))
           (let ((decision (mevedel-memory-decision-status workspace (plist-get proposal :id) entries passes)))
             (and (eq (plist-get decision :status) 'applied)
                  (or (not (eq action 'note-keep))
                      (let ((before (cdr (assoc (plist-get proposal :file) (plist-get proposal :before)))))
                        (and (mevedel-memory-notes-current-p before)
                             (mevedel-memory-notes--suppressed
                              (list (list :action action :status 'applied :hash (plist-get before :hash)
                                          :at (float-time (date-to-time (plist-get decision :created)))))
                              (plist-get before :hash)
                              (mevedel-session-control-fs-target-time (mevedel-workspace-root workspace))
                              mevedel-memory-notes-review-days)))))))))
     (plist-get accepted :proposals))))

(cl-defun mevedel-memory-pass--finish (state result)
  "Settle STATE once from RESULT and publish only its accepted immutable reply."
  (unless (plist-get state :settled)
    (dolist (key '(:preparation-duration-ms :review-request-duration-ms :publication-duration-ms))
      (mevedel-memory-pass--timing-end state key))
    (when (and (eq (plist-get result :outcome) 'success)
               (not (plist-get state :publication-finished))
               (mevedel-journal-worker-supported-p (plist-get state :workspace))
               (not (cl-some (lambda (root) (file-remote-p (plist-get (cdr root) :dir)))
                             (plist-get (plist-get (plist-get state :prepared) :scope) :roots))))
      (unless (plist-get state :publishing)
        (plist-put state :publishing t)
        (mevedel-memory-pass--timing-start state :publication-duration-ms)
        (condition-case err
            (plist-put state :worker
                       (mevedel-journal-worker-start
                        (plist-get state :workspace) 'memory-publish
                        (lambda (reply)
                          (unless (plist-get state :settled)
                            (plist-put state :worker nil)
                            (plist-put state :publication-finished t)
                            (plist-put state :accepted (plist-get reply :accepted))
                            (plist-put state :entry (plist-get reply :entry))
                            (mevedel-memory-pass--finish
                             state (if (plist-get reply :entry) result
                                     (append (list :outcome 'error :error
                                                   (or (plist-get reply :error) "Memory publication returned no entry")) result)))))
                        nil (list :prepared (plist-get state :prepared)
                                  :reply (plist-get result :reply) :entries (plist-get result :entries)
                                  :notes (plist-get result :notes)
                                  :inspected (plist-get result :inspected)
                                  :model (mevedel-memory-pass--producer result)
                                  :references (plist-get (plist-get result :references) :references))))
          (error
           (plist-put state :publication-finished t)
           (mevedel-memory-pass--finish
            state (append (list :outcome 'error :error (error-message-string err)) result)))))
      (cl-return-from mevedel-memory-pass--finish nil))
    (plist-put state :settled t)
    (condition-case err
      (let* ((workspace (plist-get state :workspace))
           (claim (plist-get state :claim))
           (id (plist-get claim :owner))
           (status (if (memq (plist-get result :outcome) '(aborted restart-required)) 'cancelled 'failed))
           (reported (if-let* ((read-usage (plist-get (plist-get state :request) :usage)))
                         (funcall read-usage) result))
           (skipped-note-count (length (plist-get result :skipped-notes)))
           (note-format-failures (or (plist-get result :note-format-failure-count) 0))
           ;; Freeze diagnostics before storage can replace RESULT with an error.
           ;; Cancellation has no review callback result, so use its live snapshot.
           (usage (append
                   (cl-loop for key in '(:provider-duration-ms :provider-call-count)
                            when (or (plist-member result key) (plist-member reported key))
                            append (list key (if (plist-member result key)
                                                (plist-get result key) (plist-get reported key))))
                   (cl-loop for key in '(:input-tokens :cached-tokens :output-tokens
                                                      :output-bytes :output-estimated-tokens :result-bytes
                                                      :reasoning-bytes :reply-bytes :tool-call-bytes
                                                      :tool-call-count :rounds
                                                      :budget-kind :output-limit)
                           append (list key (if (plist-member result key)
                                                (plist-get result key)
                                              (or (plist-get reported key)
                                                  (unless (memq key '(:budget-kind :output-limit)) 0)))))))
           (model (when (plist-get result :policy) (mevedel-model--provider-label (plist-get result :policy))))
           (failure-class (cond ((plist-get usage :budget-kind) 'output-limit)
                                ((plist-get state :publishing) 'publication)
                                (t 'review)))
           (accepted (plist-get state :accepted))
           (entry (plist-get state :entry))
           recovery-error progress-error)
      (mevedel-memory-pass--retain-rejected-reply workspace id result)
      (when (timerp (plist-get state :timer)) (cancel-timer (plist-get state :timer)))
      (plist-put state :timer nil)
      (when-let* ((worker (plist-get state :worker)))
        (plist-put state :worker nil)
        (when (process-live-p worker) (delete-process worker)))
      (condition-case err
          (when (eq (plist-get result :outcome) 'success)
            (setq failure-class 'publication)
            (unless (plist-get state :publication-finished)
              (mevedel-memory-pass--timing-start state :publication-duration-ms)
              (setq accepted
                    (mevedel-memory-store-accept
                     workspace (plist-get state :prepared) (plist-get result :reply)
                     (plist-get result :entries) (mevedel-memory-pass--producer result)
                     (plist-get (plist-get result :references) :references)
                     (plist-get result :notes) (plist-get result :inspected)))
              (unless accepted (error "Memory pass lost ownership before accepting its result"))
              (setq entry (mevedel-memory-store-publish workspace id)))
            (when (string-empty-p (plist-get entry :focus))
              (setf (mevedel-workspace-memory-schedule workspace) nil)))
        (error (setq result (append (list :outcome 'error :error (error-message-string err)) result))))
      (mevedel-memory-pass--timing-end state :publication-duration-ms)
      (unless entry
        (mevedel-memory-pass--timing-start state :retirement-duration-ms)
        (condition-case err
            (progn
              (mevedel-journal-claim-settle claim status "")
              (when (eq 'completed (plist-get (mevedel-journal-claim-outcome claim) :status))
                (setq recovery-error "Accepted memory result requires publication verification"))
              ;; A timeout cannot settle as the now-expired owner. Compete for
              ;; the same immutable expiry outcome used by target-side takeover.
              (when (and (not (mevedel-journal-claim-outcome claim))
                         (>= (mevedel-session-control-fs-target-time (plist-get claim :directory))
                             (plist-get claim :expires-at)))
                (mevedel-journal-claim--finish claim 'expired ""))
              (unless (mevedel-journal-claim-outcome claim)
                (error "Memory pass has no durable settlement"))
              (when (and (plist-get state :prepared)
                         (memq (plist-get (mevedel-journal-claim-outcome claim) :status)
                               '(failed cancelled expired)))
                (mevedel-memory-store-release workspace id)))
          (error (setq recovery-error (error-message-string err)))))
      (mevedel-memory-pass--timing-end state :retirement-duration-ms)
      (unless entry
        ;; Preserve fence-before-abort ordering, but close the provider call
        ;; before publishing its terminal timing totals.  Transport callbacks
        ;; cannot reenter this already-settled pass.
        (when-let* ((cancel (plist-get (plist-get state :request) :cancel)))
          (funcall cancel))
        (when-let* ((snapshot (plist-get (plist-get state :request) :usage)))
          (let ((final-usage (funcall snapshot)))
            (dolist (key '(:provider-duration-ms :provider-call-count))
              (when (plist-member final-usage key)
                (setq usage (plist-put usage key (plist-get final-usage key))))))))
      (when (plist-get state :publishing)
        (setf (mevedel-workspace-journal-observation workspace) nil))
      (cl-labels ((complete (applied)
			    (mevedel-memory-pass--timing-end state :application-duration-ms)
			    (let* ((keeps (seq-filter (lambda (proposal) (eq 'note-keep (plist-get proposal :action)))
                                                     (plist-get accepted :proposals)))
	                                   (converted (cl-count-if (lambda (proposal) (plist-get proposal :guard-converted)) keeps))
                                           (preserved (cl-count-if (lambda (proposal) (plist-get proposal :guard-preserved))
                                                                   (plist-get accepted :proposals))))
		    (plist-put state :result
			       (append (list :outcome (if entry 'success (plist-get result :outcome))
					     :id id :entry entry :error (or (plist-get result :error) (plist-get applied :error))
					     :mode (plist-get (plist-get state :telemetry) :mode)
                                             :queue-duration-ms (plist-get (plist-get state :telemetry) :queue-duration-ms)
					     :stopped (plist-get state :stop-apply)
					     :recovery-required (or recovery-error (and accepted (not entry)))
					     :remaining (if entry
							    (+ (or (plist-get (plist-get state :selection) :remaining) 0)
							       (or (plist-get result :omitted-digests) 0))
							  (or (plist-get (plist-get state :selection) :eligible) 0))
					     :notes-reviewed (- (length (plist-get result :notes)) skipped-note-count)
					     :note-format-failure-count note-format-failures
					     :guard-converted-keep-count converted
                                             :guard-preserved-condense-count preserved
					     :model-keep-count (- (length keeps) converted)
					     :notes-remaining (+ (or (plist-get (plist-get state :notes) :remaining) 0)
									 (or (plist-get result :omitted-notes) 0)
									 skipped-note-count)
					     :notes-blocked (or (plist-get (plist-get state :notes) :blocked-count) 0))
				       applied usage)))
		    (when (and entry
				       (not (plist-get state :drain))
			       (not (plist-get state :stop-apply))
				       (not (plist-get (plist-get state :result) :error))
				       (not (plist-get (plist-get state :result) :recovery-required))
			       (string-empty-p (plist-get entry :focus))
			       (eq mevedel-memory-consolidation-mode 'auto)
			       (or (> (plist-get (plist-get state :result) :remaining) 0)
					   (> (plist-get (plist-get state :result) :notes-remaining) 0))
                                       (condition-case err
                                           (mevedel-memory-pass--progress-p workspace accepted)
                                         (error
                                          (setq progress-error (car err))
                                          nil)
                                         (quit
                                          (setq progress-error 'quit)
                                          (plist-put state :stop-apply t)
                                          nil))
                                       ;; Progress authentication can dispatch cancellation.
                                       (not (plist-get state :stop-apply)))
		      ;; Offer, do not recurse: the next maintenance/activity boundary
			      ;; rechecks the rolling ceiling and readiness under ownership.
			      ;; Publication cleared the old chain; zero progress leaves it ended.
		      (setf (mevedel-workspace-memory-schedule workspace)
			    (list :after 0 :catch-up t)))
                            (when (plist-get state :stop-apply)
                              (plist-put (plist-get state :result) :stopped t))
			    (when (and (not entry) (plist-get state :automatic)
                                       (not (plist-get state :admission-aborted))
			       (not (eq (plist-get result :outcome) 'restart-required)))
		      ;; A failed catch-up attempt ends the chain.  A later ordinary
		      ;; opportunity must pass the normal successful-review clock.
		      (setf (mevedel-workspace-memory-schedule workspace)
			    (plist-put (mevedel-workspace-memory-schedule workspace) :catch-up nil)))
                            (when (eq state (gethash (plist-get claim :directory) mevedel-memory-pass--running))
		      (remhash (plist-get claim :directory) mevedel-memory-pass--running))
	                    (unwind-protect
                            (unless (plist-get state :admission-aborted)
			    (apply #'mevedel-telemetry-record-workspace workspace
			   (cond (entry 'memory-consolidation-completed)
				 ((eq status 'cancelled) 'memory-consolidation-killed)
				 (t 'memory-consolidation-failed))
			   (append (plist-get state :telemetry) usage (plist-get state :phase-durations)
				   (list :model model :outcome (plist-get (plist-get state :result) :outcome)
                                                 :reason-class (and progress-error 'catch-up-progress)
                                                 :error-class progress-error
					 :duration-ms (when-let* ((start (plist-get state :started-at)))
                                                        (round (* 1000 (max 0 (- (mevedel-telemetry--monotonic-now) start)))))
					 :failure-class (and (not entry) (not (eq status 'cancelled)) failure-class)
					 :reviewed-count (length (plist-get entry :digests))
					 :covered-count (if (eq 'general (plist-get (plist-get state :telemetry) :scope))
							    (length (plist-get entry :digests)) 0)
					 :proposed-count (length (plist-get entry :proposals))
					 :note-format-failure-count note-format-failures
					 :guard-converted-keep-count (plist-get (plist-get state :result) :guard-converted-keep-count)
                                         :guard-preserved-condense-count (plist-get (plist-get state :result) :guard-preserved-condense-count)
					 :model-keep-count (plist-get (plist-get state :result) :model-keep-count)
					 :updated-file-count (plist-get applied :updated-files)
						 :remaining-count (plist-get (plist-get state :result) :remaining)))))
	                      ;; Async completion runs after this function's outer
	                      ;; handler has returned.  Terminal telemetry must not
	                      ;; leave its watch open when an append quits.
	                      (let ((inhibit-quit t))
	                        (mevedel-telemetry-memory-pass-settle (plist-get state :lag-watch))))
				    (when-let* ((callback (plist-get state :callback)))
		      (funcall callback (plist-get state :result)))))
	(when entry
	  (when (plist-get state :publication-finished)
	    (mevedel-journal-cleanup-schedule workspace t))
	  (when accepted (mevedel-memory-pass--supersede workspace accepted))
	  (setf (mevedel-workspace-memory-observation workspace) nil))
		(if (and entry (eq 'auto (plist-get (plist-get state :telemetry) :mode))
			 (mevedel-memory-pass--async-application-p workspace accepted))
		    (mevedel-memory-pass--apply-async state accepted #'complete)
		  (complete (when (and entry (eq 'auto (plist-get (plist-get state :telemetry) :mode)))
			      (mevedel-memory-pass--apply state accepted))))))
      ((error quit)
       (mevedel-telemetry-memory-pass-settle (plist-get state :lag-watch))
       (signal (car err) (cdr err))))))

(defun mevedel-memory-pass-cancel (workspace)
  "Cancel only this client's running consolidation for WORKSPACE.
Fence its outcome before aborting transport, making late callbacks harmless.
Also stop deferred application and explicit drains.
Return non-nil if any existed."
  (let* ((key (mevedel-memory-store--claim-directory workspace))
         (deferred (gethash workspace mevedel-memory-pass--deferred))
         (drain (gethash key mevedel-memory-pass--drains))
         (state (mevedel-memory-pass-running workspace)))
    (when deferred
      (cancel-timer (plist-get deferred :timer))
      (remhash workspace mevedel-memory-pass--deferred))
    (when drain
      (remhash key mevedel-memory-pass--drains)
      (when (timerp (plist-get drain :timer)) (cancel-timer (plist-get drain :timer)))
      (plist-put drain :status 'cancelled))
    (when state
      (plist-put state :stop-apply t)
      (when-let* ((job (plist-get state :application)))
        (plist-put job :cancelled t)
        (when-let* ((operation (plist-get job :operation)))
          (mevedel-memory-decision--operation-stop operation))
        (unless (plist-get (plist-get job :operation) :executing)
          (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer)))
          (mevedel-memory-pass--application-next state job)))
      (unwind-protect
          (mevedel-memory-pass--finish state '(:outcome aborted))
        (when-let* ((cancel (plist-get (plist-get state :request) :cancel))) (funcall cancel))))
    (when drain
      (funcall (plist-get drain :callback) (list :outcome 'aborted :batch (plist-get drain :batch))))
    (and (or state drain deferred) t)))

(defun mevedel-memory-pass--publish (workspace payload)
  "Accept and publish a validated review in WORKSPACE from frozen PAYLOAD.
The accepted bundle survives a publication failure for checked recovery."
  (let (accepted)
    (condition-case err
        (progn
          (setq accepted
                (mevedel-memory-store-accept
                 workspace (plist-get payload :prepared) (plist-get payload :reply)
                 (plist-get payload :entries) (plist-get payload :model)
                 (plist-get payload :references) (plist-get payload :notes)
                 (plist-get payload :inspected)))
          (unless accepted (error "Memory pass lost ownership before accepting its result"))
          (list :ok t :accepted accepted
                :entry (mevedel-memory-store-publish
                        workspace (plist-get (plist-get payload :prepared) :id))))
      (error (list :error (error-message-string err) :accepted accepted)))))

(cl-defun mevedel-memory-pass--prepare (workspace payload)
  "Recover, select and prepare consolidation inputs from frozen PAYLOAD.
This may run in a storage child; no provider configuration is needed.
General reviews also capture ready shared working files using the frozen
note settings in PAYLOAD.  Ready shared-folder work admits an automatic
review by itself; a review with neither digests nor text notes needs no
inference and is marked :housekeeping-only."
  (unless (equal (plist-get payload :client) (mevedel-workspace-identity-client))
    (error "Memory preparation belongs to another client"))
  (let* ((claim (plist-get payload :claim))
         (automatic (plist-get payload :automatic))
         (focus (plist-get payload :focus))
         (general (string-empty-p focus))
         (mevedel-memory-consolidation-min-hours (plist-get payload :min-hours))
         (mevedel-memory-consolidation-min-digests (plist-get payload :min-digests))
         (mevedel-memory-consolidation-max-batches-per-day (plist-get payload :max-batches))
         (mevedel-memory-consolidation-mode (plist-get payload :mode))
         (mevedel-memory-notes-quiet-hours (plist-get payload :notes-quiet-hours))
         (mevedel-memory-notes-review-days (plist-get payload :notes-review-days))
         (mevedel-memory-notes-undo-quota (plist-get payload :notes-quota))
         (mevedel-memory-notes-max-review-bytes (plist-get payload :notes-max-review-bytes))
         recovery-duration notes scope)
    (mevedel-memory-store--assert-owned claim)
    (let ((start (mevedel-telemetry--monotonic-now)))
      (mevedel-memory-decision-recover workspace)
      (mevedel-memory-store-recover workspace)
      (setq recovery-duration (round (* 1000 (max 0 (- (mevedel-telemetry--monotonic-now) start))))))
    (let* ((observed
            (mevedel-memory-store--mutation
             workspace
             (lambda (_mutation)
               (mevedel-journal-cleanup-recover
                (mevedel-journal-store-directory (mevedel-workspace-root workspace)))
               (mevedel-journal-store-entries (mevedel-workspace-root workspace)))))
           (capture
            (lambda ()
              (unless (or notes (not general))
                (mevedel-memory-store--assert-owned claim)
                ;; Memory and instructions are captured first: files they
                ;; cite are protected from cleanup.
                (setq scope (mevedel-memory-scope-capture workspace nil (plist-get payload :configuration))
                      notes (or (mevedel-memory-pass--capture-notes
                                 workspace observed (plist-get payload :mode)
                                 (mevedel-memory-pass--cited-texts scope)
                                 (plist-get payload :in-use-notes))
                                (list :root nil))))
              (plist-get notes :root)))
           (selection (if automatic
                          (mevedel-memory-pass--automatic-selection workspace observed capture (plist-get payload :catch-up))
                        (mevedel-memory-pass-select observed (not general))))
           (entries (plist-get selection :entries)))
      (when selection (funcall capture))
      (when (cl-some (lambda (note) (plist-get (cdr note) :chunked))
                     (plist-get (cdr (plist-get notes :root)) :notes))
        ;; An oversized note owns this review.  Leave digests unpinned for
        ;; the next batch rather than carrying unused evidence through chunks.
        (setq selection (plist-put selection :remaining
                                   (+ (or (plist-get selection :remaining) 0) (length entries)))
              selection (plist-put selection :entries nil)
              entries nil))
      (if (and automatic (not selection))
          (list :skipped t :schedule (mevedel-workspace-memory-schedule workspace))
        (when (and (plist-get payload :drain) (not entries) (not (plist-get notes :root)))
          (cl-return-from mevedel-memory-pass--prepare
            (list :empty t)))
        (unless (or entries (plist-get notes :root) (plist-get payload :memory-only))
          (error "No eligible digests or shared notes for consolidation"))
        (mevedel-memory-store--assert-owned claim)
        (let* ((root (plist-get notes :root))
               (scope (or scope (mevedel-memory-scope-capture
                                 workspace nil (plist-get payload :configuration))))
               (scope (if root (plist-put scope :roots (append (plist-get scope :roots) (list root))) scope))
               (prepared (mevedel-memory-store-prepare workspace claim scope entries focus)))
          (list :selection selection :schedule (mevedel-workspace-memory-schedule workspace)
                :preparation-recovery-duration-ms recovery-duration
                :record prepared :rejections (mevedel-memory-decision-rejections workspace)
                :notes (cl-loop for key in '(:notes :attachments :directories :remaining :blocked :blocked-count)
                                append (list key (plist-get notes key)))
                :housekeeping-only (and root (not entries) (not (plist-get payload :memory-only))
                                        (null (plist-get (cdr root) :files)))))))))

(defun mevedel-memory-pass--cited-texts (scope)
  "Return the captured memory and instruction texts in SCOPE."
  (cl-loop for row in (plist-get scope :roots)
           append (cl-loop for file in (plist-get (cdr row) :before)
                           when (plist-get (cdr file) :bytes)
                           collect (decode-coding-string (plist-get (cdr file) :bytes) 'utf-8-unix))))

(defun mevedel-memory-pass--capture-notes (workspace entries mode cited &optional in-use)
  "Capture WORKSPACE's ready shared files from public ENTRIES under MODE.
CITED are memory and instruction texts whose citations protect files.
IN-USE lists relative paths open sessions are using; they wait.
First collect undo blobs no retained write needs.  The caller holds the
consolidation claim, which also excludes concurrent application."
  (when-let* ((configured (mevedel-memory-notes-directory workspace))
              ((mevedel-session-control-fs-directory-p configured)))
    ;; An unreadable retained write keeps every blob; collection is retried
    ;; at the next review and before quota refusal.
    (ignore-errors
      (mevedel-memory-notes-collect-blobs
       workspace (mevedel-memory-notes-blob-directory (file-truename configured))))
    (mevedel-memory-notes-capture workspace entries :mode mode :external cited :in-use in-use)))

(defun mevedel-memory-pass--prepared (state result focus memory-only)
  "Start STATE's configured review from prepared RESULT, FOCUS and MEMORY-ONLY.
Cancelled or expired storage replies never start a model request."
  (unless (plist-get state :settled)
    (mevedel-memory-pass--timing-end state :preparation-duration-ms)
    (mevedel-memory-pass--timing-add state :preparation-recovery-duration-ms
                                   (plist-get (plist-get result :prepared) :preparation-recovery-duration-ms))
    (plist-put state :worker nil)
    (condition-case err
        (let* ((workspace (plist-get state :workspace))
               (claim (plist-get state :claim))
               (bundle (plist-get result :prepared))
               (prepared (plist-get bundle :record)))
          (when (plist-get result :error) (error "%s" (plist-get result :error)))
          (mevedel-memory-store--assert-owned claim)
          (setf (mevedel-workspace-memory-schedule workspace) (plist-get bundle :schedule)
                (mevedel-workspace-memory-observation workspace) nil
                (mevedel-workspace-journal-observation workspace) nil)
          (if (or (plist-get bundle :empty)
                  (and (plist-get state :automatic) (plist-get bundle :skipped)))
              (progn
                (mevedel-journal-claim-settle claim 'cancelled "")
                (when (timerp (plist-get state :timer)) (cancel-timer (plist-get state :timer)))
                (plist-put state :timer nil)
                (plist-put state :settled t)
                (plist-put state :skipped t)
                (mevedel-telemetry-memory-pass-settle (plist-get state :lag-watch))
                (remhash (plist-get claim :directory) mevedel-memory-pass--running)
                (when (plist-get bundle :empty)
                  (plist-put state :result
                             (list :outcome 'empty :queue-duration-ms
                                   (plist-get (plist-get state :telemetry) :queue-duration-ms)))
                  (funcall (plist-get state :callback) (plist-get state :result))))
            (unless (and prepared (equal claim (plist-get prepared :claim)))
              (error "Memory preparation returned another claim"))
            (plist-put state :selection (plist-get bundle :selection))
            (plist-put state :prepared prepared)
            (plist-put state :notes (plist-get bundle :notes))
            (when (or (plist-get state :automatic) (plist-get state :drain))
              (apply #'mevedel-telemetry-record-workspace workspace 'memory-consolidation-fired
                     (plist-get state :telemetry)))
            (unless (buffer-live-p (plist-get state :origin-buffer))
              (error "Memory review caller buffer was closed"))
            (if (plist-get bundle :housekeeping-only)
                ;; Proved quiet, unreferenced attachments and empty folders
                ;; need no inference; acceptance derives their decisions.
                (mevedel-memory-pass--finish state (list :outcome 'success :reply nil :entries nil :notes nil))
            (mevedel-memory-pass--timing-start state :review-request-duration-ms)
            (plist-put state :request
                       (with-current-buffer (plist-get state :origin-buffer)
                         (mevedel-memory-review-request
                          (plist-get prepared :scope) (plist-get prepared :entries)
                          (lambda (reply) (mevedel-memory-pass--finish state reply))
                          :workspace workspace :pass-id (plist-get (plist-get state :claim) :owner)
                          :activep (lambda () (not (plist-get state :settled)))
                          :focus focus :memory-only memory-only
                          :rejections (plist-get bundle :rejections)
                          :currentp (lambda ()
                                      (and (not (plist-get state :settled))
                                           (condition-case nil
                                               (progn (mevedel-memory-store--assert-owned claim) t)
                                              (error nil))))))))
            ;; Request construction can yield before publishing its handle.
            (when (plist-get state :settled)
              (when-let* ((cancel (plist-get (plist-get state :request) :cancel)))
                (funcall cancel)))))
      (error (mevedel-memory-pass--finish
              state (list :outcome 'error :error (error-message-string err)))))))

(cl-defun mevedel-memory-pass-start (workspace callback &key (focus "") memory-only automatic drain queued-at)
  "Start one sessionless consolidation in WORKSPACE and return its state.
CALLBACK receives its settled outcome and diagnostics. Automatic admission can
settle the returned state as :skipped without a callback or model request.
Active local cleanup postpones automatic admission and refuses explicit starts
before acquiring a claim; the explicit drain waits and offers again later.
Invalid focus signals before starting. Later errors settle through CALLBACK.
FOCUS selects a bounded focused review; MEMORY-ONLY allows an empty batch.
AUTOMATIC uses the general-review time/count gates after fenced recovery.
DRAIN is the exact explicit drain owner, when non-nil; an empty general
selection settles with outcome `empty' without starting inference or
publishing a review.  QUEUED-AT is an optional parent-local monotonic timestamp
from the automatic or drain queue owner; direct starts omit admission dwell."
  (let ((root (copy-sequence (mevedel-workspace-root workspace))))
    (unless (and (stringp focus) (<= (string-bytes focus) 4096))
      (error "Memory focus exceeds its byte limit"))
    (when (and drain (not (eq drain (gethash (plist-get drain :key) mevedel-memory-pass--drains))))
      (cl-return-from mevedel-memory-pass-start nil))
    (when (and automatic (or mevedel-memory-pass--inhibit-scheduling
			     (eq mevedel-memory-consolidation-mode 'manual)))
      (cl-return-from mevedel-memory-pass-start nil))
    (when (and automatic (or memory-only (not (string-empty-p focus))))
      (error "Automatic review must use general digest admission"))
    (when (and (not drain)
	       (gethash (mevedel-memory-store--claim-directory workspace) mevedel-memory-pass--drains))
      (if automatic (cl-return-from mevedel-memory-pass-start nil)
	(error "Memory backlog drain is already running")))
    (when (mevedel-memory-pass-running workspace)
      (if automatic (cl-return-from mevedel-memory-pass-start nil)
	(error "Memory consolidation is already running")))
    (when (mevedel-journal-cleanup-active-p workspace)
      (if automatic (cl-return-from mevedel-memory-pass-start nil)
	(user-error "Journal cleanup is running; retry memory consolidation when it finishes")))
    (mevedel-workspace-identity-ensure root)
    (unless (equal root (mevedel-workspace-root workspace))
      (error "Memory consolidation workspace changed"))
    (let* ((key (mevedel-memory-store--claim-directory workspace))
	   (currentp (lambda ()
		       (and (equal key (mevedel-memory-store--claim-directory workspace))
                            (equal root (mevedel-workspace-root workspace))
                            (or (null drain)
				(and (eq drain (gethash key mevedel-memory-pass--drains))
				     (equal key (plist-get drain :key))
				     (equal (plist-get drain :root) root))))))
	   (_ (unless (funcall currentp)
		(when (or (null drain) (eq drain (gethash (plist-get drain :key) mevedel-memory-pass--drains)))
		  (error "%s" (if drain "Memory drain workspace changed" "Memory consolidation workspace changed")))
		(cl-return-from mevedel-memory-pass-start nil)))
	   (claim (or (mevedel-journal-claim-acquire key 180)
                      (progn
                        (unless (funcall currentp)
                          (when (or (null drain) (eq drain (gethash (plist-get drain :key) mevedel-memory-pass--drains)))
                            (error "%s" (if drain "Memory drain workspace changed" "Memory consolidation workspace changed")))
                          (cl-return-from mevedel-memory-pass-start nil))
			(if automatic (cl-return-from mevedel-memory-pass-start nil)
			  (error "Memory consolidation is busy")))))
	   (admitted-at (mevedel-telemetry--monotonic-now))
	   (state (list :workspace workspace :claim claim :callback callback :settled nil
			:automatic automatic :drain drain :origin-buffer (current-buffer)
			:started-at admitted-at
			:telemetry (list :pass-id (plist-get claim :owner)
					 :queue-duration-ms (when queued-at (round (* 1000 (max 0 (- admitted-at queued-at)))))
					 :attempt-generation (plist-get claim :generation)
					 :mode mevedel-memory-consolidation-mode :workload 'memory
					 :scope (if (string-empty-p focus) 'general 'focused))))
	   (finish (lambda (result)
		     (plist-put state :admission-aborted t)
		     (mevedel-memory-pass--finish state result)
		     (when-let* ((failure (plist-get (plist-get state :result) :recovery-required)))
		       ;; The cancelled drain's callback may no longer be current.
		       ;; Settlement failure must still be visible, not quiet loss.
		       (display-warning 'mevedel
					(format "Memory admission %s needs recovery: %s" (plist-get claim :owner) failure)
					:warning)))))
      (when (gethash key mevedel-memory-pass--running)
	(funcall finish '(:outcome error :error "Memory consolidation is already running"))
	(cl-return-from mevedel-memory-pass-start nil))
      (puthash key state mevedel-memory-pass--running)
      (condition-case err
          (plist-put state :lag-watch
                     (mevedel-telemetry-memory-pass-start workspace (plist-get claim :owner)))
        (quit
         ;; The watcher has not returned its handle and the deadline is not
         ;; armed yet.  Fence this admitted claim before preserving the quit.
         (let ((inhibit-quit t)) (funcall finish '(:outcome aborted)))
         (signal (car err) (cdr err))))
      ;; Starting the shared native watcher may yield to cancellation.
      (when (plist-get state :settled)
        (mevedel-telemetry-memory-pass-settle (plist-get state :lag-watch)))
      (unless (funcall currentp)
	;; Claim creation may have yielded to cancellation and a new drain.
	;; Keep this exact state observed until its own new claim is fenced.
	(funcall finish (if (or (null drain) (eq drain (gethash (plist-get drain :key) mevedel-memory-pass--drains)))
			    (list :outcome 'error :error (if drain "Memory drain workspace changed" "Memory consolidation workspace changed"))
			  '(:outcome aborted)))
	(cl-return-from mevedel-memory-pass-start nil))
      (unless (or automatic drain)
	(apply #'mevedel-telemetry-record-workspace workspace 'memory-consolidation-fired
	       (plist-get state :telemetry)))
      (when (plist-get state :settled) (cl-return-from mevedel-memory-pass-start nil))
      (condition-case err
	  (progn
	    (plist-put state :timer
		       (run-at-time
			(max 0 (- (plist-get claim :expires-at)
				  (mevedel-session-control-fs-target-time key))) nil
			(lambda ()
			  (unwind-protect
			      (mevedel-memory-pass--finish state '(:outcome error :error "Memory pass timed out"))
			    (when-let* ((cancel (plist-get (plist-get state :request) :cancel)))
			      (funcall cancel))))))
	    (when (plist-get state :settled)
	      (cancel-timer (plist-get state :timer))
	      (plist-put state :timer nil)
	      (cl-return-from mevedel-memory-pass-start nil))
	    (let* ((roots (mevedel-system--memory-roots workspace))
		   (payload (list :claim claim :focus focus :automatic automatic :memory-only memory-only
				  :drain (and drain t)
				  :catch-up (plist-get (mevedel-workspace-memory-schedule workspace) :catch-up)
				  :max-batches mevedel-memory-consolidation-max-batches-per-day
				  :min-hours mevedel-memory-consolidation-min-hours
				  :min-digests mevedel-memory-consolidation-min-digests
				  :mode mevedel-memory-consolidation-mode
				  :notes-quiet-hours mevedel-memory-notes-quiet-hours
				  :notes-review-days mevedel-memory-notes-review-days
				  :notes-quota mevedel-memory-notes-undo-quota
				  :notes-max-review-bytes mevedel-memory-notes-max-review-bytes
				  ;; Notes that open sessions are using wait; others proceed.
				  :in-use-notes (mevedel-memory-notes-in-use workspace)
				  :client (mevedel-workspace-identity-client)
				  :configuration (list :roots roots :instructions
						       (mevedel-system-workspace-config-files workspace))))
		   (ready (lambda (result) (mevedel-memory-pass--prepared state result focus memory-only))))
	      (unless (and (not (plist-get state :settled)) (funcall currentp))
		(mevedel-memory-pass--finish state '(:outcome aborted))
		(cl-return-from mevedel-memory-pass-start nil))
		      (mevedel-memory-pass--timing-start state :preparation-duration-ms)
		      (if (and (mevedel-journal-worker-supported-p workspace)
		       (not (cl-some (lambda (root) (file-remote-p (plist-get root :dir))) roots)))
		  (plist-put state :worker
			     (mevedel-journal-worker-start workspace 'memory-prepare ready nil payload))
		(funcall ready (list :ok t :prepared (mevedel-memory-pass--prepare workspace payload))))))
	(error (mevedel-memory-pass--finish state (list :outcome 'error :error (error-message-string err)))))
      (unless (plist-get state :skipped) state))))

(provide 'mevedel-memory-pass)
;;; mevedel-memory-pass.el ends here
