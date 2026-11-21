;;; niwa-memory-pass.el -- Workspace consolidation coordination -*- lexical-binding: t -*-

;;; Commentary:

;; Run one workspace-owned consolidation independently of a live conversation.
;; Target-native claims fence inference, accepted storage precedes publication,
;; and a successor recovers older results before selecting its own evidence.

;;; Code:

(require 'cl-lib)
(require 'niwa-journal-cleanup)
(require 'niwa-journal-index)
(require 'niwa-journal-worker)
(require 'niwa-memory-cycle)
(require 'niwa-memory-decision)
(require 'niwa-memory-notes)
(require 'niwa-memory-review)
(require 'niwa-memory-store)

;; `niwa-memory-list'
(declare-function niwa-memory-list--finished "niwa-memory-list" (workspace result))
(autoload 'niwa-memory-list--finished "niwa-memory-list")

;; `niwa-telemetry'
(declare-function niwa-telemetry--monotonic-now "niwa-telemetry" ())
(declare-function niwa-telemetry-memory-pass-settle "niwa-telemetry" (handle))
(declare-function niwa-telemetry-memory-pass-start "niwa-telemetry" (workspace pass-id))
(declare-function niwa-telemetry-record-workspace "niwa-telemetry" (workspace event &rest props))
(autoload 'niwa-telemetry--monotonic-now "niwa-telemetry")
(autoload 'niwa-telemetry-memory-pass-settle "niwa-telemetry")
(autoload 'niwa-telemetry-memory-pass-start "niwa-telemetry")
(autoload 'niwa-telemetry-record-workspace "niwa-telemetry")

;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-warning "niwa-utilities" (scope type message &optional level buffer-name))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")

(defcustom niwa-memory-consolidation-min-hours 24
  "Minimum hours between successful journal-bearing general memory reviews.
Focused and note-only reviews do not move this clock.  Explicit commands
bypass the journal gate; shared notes have an independent daily cohort."
  :type '(number :tag "Hours") :group 'niwa)

(defcustom niwa-memory-consolidation-min-digests 5
  "Minimum eligible unreviewed digests needed for automatic memory review.
Completed digests from running sessions are eligible. One day before ordinary
journal expiry, a smaller backlog may be reviewed. Explicit commands bypass
this threshold."
  :type '(integer :tag "Digests") :group 'niwa)

(defvar niwa-memory-pass--inhibit-scheduling nil
  "Non-nil suppresses automatic opportunities during exit or isolated tests.")

(defvar niwa-memory-pass--pending (make-hash-table :test #'eq)
  "Coalesced turn-completion timers, keyed by their workspace objects.")

(defvar niwa-memory-pass--running (make-hash-table :test #'equal)
  "This client's active requests, keyed by physical pass ownership directory.")

(defvar niwa-memory-pass--deferred (make-hash-table :test #'eq)
  "Original application contexts and timers for held automatic note decisions.")

(defvar niwa-memory-pass--drains (make-hash-table :test #'equal)
  "Explicit drains keyed by physical pass ownership directory.")

(defun niwa-memory-pass--timing-add (state key duration)
  "Accumulate known millisecond DURATION under KEY in STATE's timings."
  (when (numberp duration)
    (let ((timings (plist-get state :phase-durations)))
      (plist-put state :phase-durations
                 (plist-put timings key (+ (or (plist-get timings key) 0) duration))))))

(defun niwa-memory-pass--timing-start (state key)
  "Start parent-local interval KEY in STATE, without resetting a live interval."
  (unless (assq key (plist-get state :phase-starts))
    (plist-put state :phase-starts
               (cons (cons key (niwa-telemetry--monotonic-now)) (plist-get state :phase-starts)))))

(defun niwa-memory-pass--timing-end (state key)
  "Accumulate and close parent-local interval KEY in STATE, if it started."
  (when-let* ((start (assq key (plist-get state :phase-starts))))
    (plist-put state :phase-starts (delq start (plist-get state :phase-starts)))
    (niwa-memory-pass--timing-add
     state key (round (* 1000 (max 0 (- (niwa-telemetry--monotonic-now) (cdr start))))))))

(defun niwa-memory-pass--producer (result)
  "Return the producer label of review RESULT; housekeeping has no model."
  (if (plist-get result :policy) (niwa-model--provider-label (plist-get result :policy))
    "housekeeping"))

(defun niwa-memory-pass--general-times (workspace entries)
  "Return journal-bearing general-review times from WORKSPACE ENTRIES.
Note-only reviews do not advance journal admission.  Include retired completion
metadata so expiry cannot erase the clock.  Malformed retirement metadata stops
automatic admission for inspection."
  (let (times)
    (dolist (entry entries)
      (when (and (eq (plist-get entry :kind) 'consolidation)
                 (string-empty-p (plist-get entry :focus))
                 (plist-get entry :digests))
        (push (float-time (date-to-time (plist-get entry :created))) times)))
    (dolist (path (niwa-session-control-fs-list-directory
                   (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory (niwa-workspace-root workspace))) "retired-passes")
                   niwa-journal-store-id-regexp))
      (let* ((text (niwa-session-control-fs-read-file path 'utf-8-unix 513))
             (record (json-parse-string text))
             (scope (and (hash-table-p record) (gethash "scope" record)))
             (journal (and (hash-table-p record) (gethash "journal-bearing" record)))
             (date (and (hash-table-p record) (gethash "created" record)))
             (hash (and (hash-table-p record) (gethash "sha256" record))))
        (unless (and (<= (string-bytes text) 512) (hash-table-p record)
                     (= 4 (hash-table-count record)) (member scope '("general" "focused"))
                     (memq journal '(t :false))
                     (stringp hash) (niwa-journal-store-id-p hash)
                     (stringp date) (string-match-p (concat "\\`" niwa-journal-store--time-regexp "\\'") date))
          (error "Invalid retired memory review clock"))
        (when (and (equal scope "general") (eq journal t))
          (push (float-time (date-to-time date)) times))))
    times))

(defun niwa-memory-pass--automatic-selection (workspace entries)
  "Return an eligible journal-only batch from fresh WORKSPACE ENTRIES.
Shared-note cycle admission is independent of these journal policy gates."
  (unless (and (numberp niwa-memory-consolidation-min-hours)
               (>= niwa-memory-consolidation-min-hours 0)
               (integerp niwa-memory-consolidation-min-digests)
               (> niwa-memory-consolidation-min-digests 0))
    (error "Invalid memory consolidation thresholds"))
  (let* ((times (niwa-memory-pass--general-times workspace entries))
         (last (and times (apply #'max times)))
         (now (niwa-session-control-fs-target-time (niwa-workspace-root workspace)))
         (remaining (cond (last (- (+ last (* 3600 niwa-memory-consolidation-min-hours)) now))
                          (t 0))))
    (setf (niwa-workspace-memory-schedule workspace)
          (list :after (+ (float-time) (if (> remaining 0) (min 600 remaining) 600))
                :hours niwa-memory-consolidation-min-hours
                :digests niwa-memory-consolidation-min-digests
                :age niwa-journal-max-age-days
                :mode niwa-memory-consolidation-mode))
    (when (<= remaining 0)
      (let ((selection (niwa-memory-pass-select entries)))
        (when (or (>= (plist-get selection :eligible) niwa-memory-consolidation-min-digests)
                  (and (plist-get selection :entries)
                       (integerp niwa-journal-max-age-days)
                       (>= niwa-journal-max-age-days 0)
                       (>= (- now (float-time (date-to-time
                                               (plist-get (car (plist-get selection :entries)) :created))))
                           (* (max 0 (1- niwa-journal-max-age-days)) 86400))))
          selection)))))

;;;###autoload
(defun niwa-memory-pass-schedule (workspace)
  "Offer automatic review at an activity boundary in WORKSPACE.
Manual mode, cached thresholds and pending opportunities need no target I/O.
Defer the cold observation until transport is idle; a pass rechecks admission
under target ownership. Finite note cohorts continue through separately
owned idle admissions; journal-only completion does not chain new requests.
Either published journal state or a shared working folder makes a workspace
a candidate, so shared notes are cleaned up even with journaling disabled."
  (unless (or niwa-memory-pass--inhibit-scheduling
              (eq niwa-memory-consolidation-mode 'manual)
              (gethash (niwa-memory-store--claim-directory workspace) niwa-memory-pass--drains))
    (let ((cached (niwa-workspace-memory-schedule workspace))
          (scope (niwa-diagnostic-scope workspace)) opportunity queued-at)
      (unless (or (gethash workspace niwa-memory-pass--pending)
                  (and (equal (plist-get cached :hours) niwa-memory-consolidation-min-hours)
                       (equal (plist-get cached :digests) niwa-memory-consolidation-min-digests)
                       (equal (plist-get cached :age) niwa-journal-max-age-days)
                       (equal (plist-get cached :mode) niwa-memory-consolidation-mode)
                       (< (float-time) (or (plist-get cached :after) 0))))
        (setf (niwa-workspace-memory-schedule workspace)
              (setq opportunity
                    (list :after (+ (float-time) 600)
                          :hours niwa-memory-consolidation-min-hours
                          :digests niwa-memory-consolidation-min-digests
                          :age niwa-journal-max-age-days
                          :mode niwa-memory-consolidation-mode)))
        (setq queued-at (niwa-telemetry--monotonic-now))
        (niwa-transport-schedule-idle
         niwa-memory-pass--pending workspace 'memory-pass (niwa-workspace-root workspace)
         (lambda ()
           (unless (or niwa-memory-pass--inhibit-scheduling
                       (eq niwa-memory-consolidation-mode 'manual))
             (condition-case err
                 (when (or (niwa-session-control-fs-path-exists-p
                            (niwa-journal-store-directory (niwa-workspace-root workspace)))
                           (niwa-session-control-fs-path-exists-p
                            (niwa-memory-notes-directory workspace)))
                   (unless (niwa-memory-pass-start
                            workspace (lambda (result) (niwa-memory-list--finished workspace result))
                            :automatic t :queued-at queued-at)
                     ;; A busy owner is not an admission observation. Preserve
                     ;; any fresh gate result,
                     ;; but allow the next activity offer.
                     (when (eq opportunity (niwa-workspace-memory-schedule workspace))
                       (plist-put opportunity :after 0))))
               (error (niwa-diagnostic-warning scope 'niwa
                                       (format "Automatic memory review failed: %s" (error-message-string err))
                                       :warning))))))))))

(defun niwa-memory-pass-stop-all ()
  "Cancel this client's queued and running memory reviews on exit."
  (niwa-transport-cancel-idle niwa-memory-pass--pending 'memory-pass)
  (maphash (lambda (_workspace retry) (cancel-timer (plist-get retry :timer))) niwa-memory-pass--deferred)
  (clrhash niwa-memory-pass--deferred)
  (maphash (lambda (_key state) (niwa-memory-pass-cancel (plist-get state :workspace)))
           niwa-memory-pass--drains)
  (maphash (lambda (_key state) (niwa-memory-pass-cancel (plist-get state :workspace)))
           niwa-memory-pass--running))

(defun niwa-memory-pass-select (entries &optional focused)
  "Select at most twenty completed digests from validated public ENTRIES.
FOCUSED also admits previously reviewed evidence. Published digests are
immutable completed work, including from sessions that are still running.

Return :entries oldest first, :eligible count and :remaining eligible backlog.
Break equal timestamps by digest ID. This observation acquires no ownership,
pins no evidence and consumes no coverage; the caller coordinates publication."
  (let ((eligible (if focused
                      (seq-filter (lambda (entry) (eq (plist-get entry :kind) 'digest)) entries)
                    (niwa-journal-index-unreviewed entries))))
    (setq eligible
          (sort eligible (lambda (left right)
                           (let ((first (plist-get left :created)) (second (plist-get right :created)))
                             (if (equal first second)
                                 (string-lessp (plist-get left :id) (plist-get right :id))
                               (string-lessp first second))))))
    (list :entries (seq-take eligible 20) :eligible (length eligible)
          :remaining (max 0 (- (length eligible) 20)))))

(defun niwa-memory-pass-running (workspace)
  "Return this client's running consolidation state for WORKSPACE, or nil.
Its :request contains an inspectable :buffer. Other clients' ownership is
represented by the target claim and does not grant local cancellation rights."
  (gethash (niwa-memory-store--claim-directory workspace) niwa-memory-pass--running))

(defun niwa-memory-pass-drain-status (workspace)
  "Return this client's explicit backlog drain state for WORKSPACE, or nil."
  (gethash (niwa-memory-store--claim-directory workspace) niwa-memory-pass--drains))

(defun niwa-memory-pass--drain-step (state)
  "Offer the next bounded review for explicit drain STATE.
Local applications, cleanup and busy transports wait sixty seconds, without
capturing notes or starting an empty model request.  Other sessions' turns
do not; capture leaves the notes they use for later.  Every continuation
yields to input."
  (let* ((workspace (plist-get state :workspace))
         (key (plist-get state :key)))
    (when (eq state (gethash key niwa-memory-pass--drains))
      (plist-put state :timer nil)
      (condition-case err
          (let* ((busy (or (niwa-journal-cleanup-active-p workspace)
                           (niwa-transport-busy-p (niwa-workspace-root workspace))
                           (input-pending-p)))
                 (fresh-key (niwa-memory-store--claim-directory workspace)))
            ;; Busy/path IO can dispatch cancellation, recovery or another pass.
            ;; Only exact, non-IO observations follow this admission boundary.
            (when (eq state (gethash key niwa-memory-pass--drains))
              (unless (and (equal fresh-key key)
                           (equal (plist-get state :root) (niwa-workspace-root workspace)))
                (error "Memory drain workspace changed"))
              (cond
               ((not (buffer-live-p (plist-get state :origin-buffer)))
		(error "Memory drain caller buffer was closed"))
               ((or busy (gethash key niwa-memory-pass--running)
                    (niwa-memory-decision-admitted-p key))
		(plist-put state :status 'waiting)
		(plist-put state :timer (run-at-time 60 nil #'niwa-memory-pass--drain-step state)))
               (t
		(plist-put state :status 'running)
		(with-current-buffer (plist-get state :origin-buffer)
		  (niwa-memory-pass-start
		   workspace
		   (lambda (result)
                     (when (eq state (gethash key niwa-memory-pass--drains))
                       (when (eq (plist-get result :outcome) 'success)
                         (plist-put state :queued-at (niwa-telemetry--monotonic-now))
			 (plist-put state :batch (1+ (plist-get state :batch))))
                       (if (and (memq (plist-get result :outcome) '(success restart-required))
				(not (plist-get result :stopped))
				(not (plist-get result :error))
				(not (plist-get result :pass-recovery-required)))
			   (progn
                             (plist-put state :notes-remaining (plist-get result :notes-remaining))
                             (plist-put state :remaining (plist-get result :remaining))
                             (plist-put state :timer (run-at-time .05 nil #'niwa-memory-pass--drain-step state)))
			 (remhash key niwa-memory-pass--drains)
			 (plist-put state :status
                                    (cond ((plist-get result :stopped) 'aborted)
					  ((or (plist-get result :error) (plist-get result :pass-recovery-required)) 'error)
					  (t (plist-get result :outcome)))))
                       (funcall (plist-get state :callback)
				(append (list :batch (plist-get state :batch)) result))))
			   :drain state :queued-at (plist-get state :queued-at)))))))
        (error
         (when (eq state (gethash key niwa-memory-pass--drains))
           (remhash key niwa-memory-pass--drains)
           (plist-put state :status 'error)
           (funcall (plist-get state :callback)
                    (list :outcome 'error :error (error-message-string err) :batch (plist-get state :batch)))))))))

(defun niwa-memory-pass-drain (workspace callback)
  "Drain ready WORKSPACE notes and digests through successive bounded reviews.
CALLBACK receives each result with a :batch count of completed batches.
The fresh final empty selection starts no model request.  Stop on failure,
application errors, recovery requirements, caller closure or
`niwa-memory-pass-cancel'.  Explicit drain bypasses
	automatic timing gates, not ownership or request limits."
  (when (or (niwa-memory-pass-drain-status workspace)
            (niwa-memory-pass-running workspace))
    (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Memory consolidation is already running"))
  (let* ((root (copy-sequence (niwa-workspace-root workspace)))
         (key (niwa-memory-store--claim-directory workspace))
         (state (list :workspace workspace :root root :key key :origin-buffer (current-buffer)
                      :queued-at (niwa-telemetry--monotonic-now)
                      :callback callback :batch 0 :status 'queued :timer nil)))
    (unless (equal root (niwa-workspace-root workspace)) (error "Memory drain workspace changed"))
    (when (or (gethash key niwa-memory-pass--drains) (gethash key niwa-memory-pass--running))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Memory consolidation is already running"))
    (puthash key state niwa-memory-pass--drains)
    (plist-put state :timer (run-at-time 0 nil #'niwa-memory-pass--drain-step state))
    state))

(defun niwa-memory-pass--apply (state accepted)
  "Apply ACCEPTED memory proposals sequentially for completed pass STATE.
Each decision owns its workspace and original-target ownership, for
instruction proposals as for memory ones; the loop carries only the opaque
continuation of its explicit serial request.  Stop before further proposals if
cancellation was requested, holding the rest.
A shared-note decision waits while an open session uses its file; a retry
applies it once that use ends.  Other notes and earlier held ones proceed.
Count distinct files only on confirmed writes.
An interrupted pending sweep returns its confirmed writes and one unapplied
sweep, without reconstructing its other terminal or failed outcome totals."
  (niwa-memory-pass--timing-start state :application-duration-ms)
  (let* ((workspace (plist-get state :workspace))
         (pass (plist-get (plist-get accepted :prepared) :id))
         (context (niwa-memory-decision-automatic-context workspace (plist-get state :origin-buffer)))
         (applied 0) (held 0) (unapplied 0) deferred files continuation)
    (plist-put state :application-context context)
    (dolist (proposal (plist-get accepted :proposals))
      (if (plist-get state :stop-apply)
          (cl-incf held)
        (let ((outcome (niwa-memory-pass--apply-automatic
                        workspace context pass (plist-get proposal :id)
                        (niwa-memory-proposal-note-action-p (plist-get proposal :action))
                        (plist-get proposal :file)
                        (lambda (paths)
                          (cl-incf applied)
                          (setq files (append paths files)))
                        continuation)))
          (setq continuation (cdr outcome))
          (pcase (car outcome)
            ('held (push (plist-get proposal :file) deferred) (cl-incf held))
            ('stop (plist-put state :stop-apply t) (cl-incf held))
            ('quit (plist-put state :stop-apply t) (cl-incf unapplied))
            ((or 'applied 'skipped) nil)
            (_ (cl-incf unapplied))))))
    (unless (plist-get state :stop-apply)
      (let* ((confirmed 0)
             (earlier (condition-case nil
                          (niwa-memory-pass--apply-pending
                           workspace pass (lambda (file) (push file deferred))
                           (lambda (paths)
                             (cl-incf confirmed)
                             (setq files (append paths files)))
                           context continuation)
                        (quit
                         ;; Preserve reached writes even when this or a later
                         ;; pending operation quits before returning its totals.
                         (plist-put state :stop-apply t)
                         (cons confirmed 1)))))
        (cl-incf applied (car earlier))
        (cl-incf unapplied (cdr earlier))))
    (when deferred
      (niwa-memory-pass--defer-notes state deferred))
    (list :updated-files (length (delete-dups files)) :applied-count applied
          :held-count held :unapplied-count unapplied)))

(defun niwa-memory-pass--supersede (workspace accepted)
  "Supersede older undecided note dispositions replaced by ACCEPTED.
Report dispositions that could not be superseded in WORKSPACE; they stay
in the cockpit for an explicit decision."
  (condition-case err
      (let ((outcome (niwa-memory-notes-supersede workspace accepted)))
        (when (> (cdr outcome) 0)
          (niwa-diagnostic-warning (niwa-diagnostic-scope workspace) 'niwa (format "%d older shared-note decisions could not be superseded"
                                            (cdr outcome))
                           :warning)))
    (error (niwa-diagnostic-warning (niwa-diagnostic-scope workspace) 'niwa (format "Superseding older shared-note decisions failed: %s"
                                             (error-message-string err))
                            :warning))))

(defun niwa-memory-pass--async-application-p (workspace accepted)
  "Return non-nil when WORKSPACE and ACCEPTED's targets support local workers."
  (niwa-journal-worker-local-p
   workspace
   (if accepted
       (mapcar (lambda (root) (plist-get (cdr root) :dir))
               (plist-get (plist-get (plist-get accepted :prepared) :scope) :roots))
     (append (mapcar (lambda (root) (plist-get root :dir)) (niwa-system--memory-roots workspace))
             (niwa-system-workspace-config-files workspace)
             (list (or (niwa-memory-notes-directory workspace) ""))))))

(defun niwa-memory-pass--application-live-p (state job)
  "Return non-nil while STATE's current JOB can admit a forward automatic write."
  (and (eq job (plist-get state :application)) (not (plist-get job :done))
       (not (plist-get state :stop-apply))
       (niwa-memory-decision-automatic-current-p (plist-get job :context))))

(defun niwa-memory-pass--application-idle-p (job)
  "Return non-nil when JOB may proceed without interrupting activity.
Unsupported jobs retain their synchronous scheduling behavior."
  (or (not (plist-get job :idle))
      (and (not (input-pending-p))
           (when-let* ((idle (current-idle-time)))
             (>= (float-time idle) niwa-memory-decision-automatic-idle-seconds)))))

(defun niwa-memory-pass--application-next (state job)
  "Continue STATE's current application JOB after editor idleness.
Cancellation completes promptly, without waiting for another idle period."
  (when (and (eq job (plist-get state :application)) (not (plist-get job :done)))
    (plist-put job :timer
               (if (or (plist-get state :stop-apply) (not (plist-get job :idle)))
                   (run-at-time .01 nil #'niwa-memory-pass--application-step state job)
                 (run-with-idle-timer niwa-memory-decision-automatic-idle-seconds nil
                                      #'niwa-memory-pass--application-step state job)))))

(defun niwa-memory-pass--hold-file (job file)
  "Record held note FILE in JOB, mutating the shared plist in place.
The `setf' expansion of `plist-get' would rebind a local copy instead."
  (plist-put job :deferred (cons file (plist-get job :deferred))))

(defun niwa-memory-pass--application-pending (state job)
  "Select pending identities for STATE's JOB through an owned decision request."
  (with-current-buffer (plist-get state :origin-buffer)
    (niwa-memory-decision-pending
     (plist-get state :workspace) :automatic (plist-get job :context) :skip (plist-get job :pass)
     :continuation (plist-get job :continuation)
     :callback
     (lambda (reply)
       (when (and (eq job (plist-get state :application)) (not (plist-get job :done)))
         (plist-put job :continuation (plist-get reply :continuation))
         (niwa-memory-pass--timing-add state :application-recovery-round-trip-ms
                                        (plist-get reply :recovery-round-trip-ms))
         (when (plist-get reply :cancelled)
           (plist-put job :cancelled t)
           (plist-put state :stop-apply t))
         (cond
          ((plist-get reply :error)
           (plist-put job :error (or (plist-get job :error) (plist-get reply :error)))
           (cl-incf (plist-get job :unapplied)))
          ((or (eq (plist-get reply :status) 'held) (plist-get reply :cancelled))
           (plist-put state :stop-apply t))
          (t (plist-put job :items (plist-get reply :items))))
         (niwa-memory-pass--application-next state job))))))

(defun niwa-memory-pass--application-applied (state job item reply)
  "Account STATE's JOB decision REPLY for ITEM, then continue."
  (when (and (eq job (plist-get state :application)) (not (plist-get job :done)))
    ;; Only the opaque continuation survives; the decision module proves it.
    (plist-put job :continuation (plist-get reply :continuation))
    (niwa-memory-pass--timing-add state :application-recovery-round-trip-ms
                                   (plist-get reply :recovery-round-trip-ms))
    (when (plist-get reply :error)
      (plist-put job :error (or (plist-get job :error) (plist-get reply :error))))
    (when (plist-get reply :cancelled)
      (plist-put job :cancelled t)
      (plist-put state :stop-apply t))
    (let ((status (plist-get reply :status)))
      (cond
       ((and (eq status 'idle-wait) (not (plist-get reply :error)) (not (plist-get reply :cancelled)))
        (push item (plist-get job :items)))
       ((or (eq status 'skipped) (niwa-memory-decision-terminal-status-p status)))
       ((plist-get reply :error) (cl-incf (plist-get job :unapplied)))
       ((or (memq status '(held waiting)) (and (not status) (plist-get reply :cancelled)))
        (if (and (memq status '(held waiting)) (plist-get item :note)
                 (niwa-memory-pass--application-live-p state job))
            (niwa-memory-pass--hold-file job (plist-get item :file))
          (plist-put state :stop-apply t))
        (cl-incf (plist-get job :held)))
       (t (cl-incf (plist-get job :unapplied)))))
    (niwa-memory-pass--application-next state job)))

(cl-defun niwa-memory-pass--application-step (state job)
  "Run one owned automatic decision for STATE's sequential JOB."
  (when (and (eq job (plist-get state :application)) (not (plist-get job :done)))
    (plist-put job :timer nil)
    (when (and (not (plist-get state :stop-apply))
               (not (niwa-memory-pass--application-idle-p job)))
      (niwa-memory-pass--application-next state job)
      (cl-return-from niwa-memory-pass--application-step nil))
    (let ((workspace (plist-get state :workspace)) item)
      (condition-case err
	  (progn
	    (unless (niwa-memory-pass--application-live-p state job)
	      (plist-put state :stop-apply t)
	      (cl-incf (plist-get job :held) (length (plist-get job :items)))
	      (plist-put job :items nil)
	      (plist-put job :pending-loaded t))
	    (when (and (null (plist-get job :items)) (not (plist-get job :pending-loaded)))
	      (plist-put job :pending-loaded t)
	      (niwa-memory-pass--application-pending state job)
	      (cl-return-from niwa-memory-pass--application-step nil))
	    (if (setq item (pop (plist-get job :items)))
		(if (and (plist-get item :note)
			 (niwa-memory-notes-in-use-p workspace (plist-get item :file)))
		    (progn
                      (plist-put job :continuation nil)
		      (niwa-memory-pass--hold-file job (plist-get item :file))
		      (cl-incf (plist-get job :held))
		      (niwa-memory-pass--application-next state job))
		  (with-current-buffer (plist-get state :origin-buffer)
		    (niwa-memory-decision-apply
		     workspace (plist-get item :pass) (plist-get item :proposal)
                     :automatic (plist-get job :context) :continuation (plist-get job :continuation)
		     :on-applied (lambda (paths)
				   (cl-incf (plist-get job :applied))
				   (plist-put job :files (append paths (plist-get job :files))))
                     :callback (lambda (reply) (niwa-memory-pass--application-applied state job item reply)))))
	      (plist-put job :done t)
	      (when (and (plist-get job :deferred) (not (plist-get state :stop-apply)))
		(niwa-memory-pass--defer-notes state (plist-get job :deferred)))
	      (funcall (plist-get job :callback)
		       (list :updated-files (length (delete-dups (plist-get job :files)))
			     :cancelled (plist-get job :cancelled)
			     :applied-count (plist-get job :applied) :held-count (plist-get job :held)
			     :unapplied-count (plist-get job :unapplied) :error (plist-get job :error)))))
	((error quit)
	 ;; A completed job cannot retry its callback.  Preserve failures
	 ;; from that callback instead of silently dropping its condition.
	 (when (plist-get job :done) (signal (car err) (cdr err)))
	 (plist-put job :continuation nil)
	 (when (eq (car err) 'quit)
           (plist-put job :cancelled t)
           (plist-put state :stop-apply t)
           (when item (cl-incf (plist-get job :held))))
	 (unless (eq (car err) 'quit)
           (cl-incf (plist-get job :unapplied))
	   (plist-put job :error (or (plist-get job :error) (error-message-string err))))
	 (niwa-memory-pass--application-next state job))))))

(defun niwa-memory-pass--apply-async (state accepted callback)
  "Apply ACCEPTED for STATE through yielding owned operations, then CALLBACK.
Storage recovery runs in a real child; original target ownership and all live
buffer checks and mutations remain in the editor.  Nil ACCEPTED retries pending
automatic proposals without starting inference.  Return the application job."
  (niwa-memory-pass--timing-start state :application-duration-ms)
  (let* ((pass (plist-get (plist-get accepted :prepared) :id))
         (context (or (plist-get state :application-context)
                      (niwa-memory-decision-automatic-context
                       (plist-get state :workspace) (plist-get state :origin-buffer))))
         (job (append (list :pass pass :callback callback :applied 0 :held 0 :unapplied 0 :deferred nil
                            :context context :continuation nil
                            :idle (niwa-memory-pass--async-application-p (plist-get state :workspace) accepted)
                            :items (mapcar (lambda (proposal)
                                             (list :pass pass :proposal (plist-get proposal :id)
                                                   :note (niwa-memory-proposal-note-action-p (plist-get proposal :action))
                                                   :file (plist-get proposal :file)))
                                           (plist-get accepted :proposals))))))
    (plist-put state :application-context context)
    (plist-put state :application job)
    (niwa-memory-pass--application-next state job)
    job))

(defun niwa-memory-pass--apply-automatic (workspace context pass proposal note file
                                                      &optional on-applied continuation)
  "Synchronously apply PROPOSAL of PASS in WORKSPACE for automatic CONTEXT.
NOTE says it disposes of shared FILE.  ON-APPLIED receives confirmed paths;
CONTINUATION is this serial request's previous result.  Return
\(OUTCOME . CONTINUATION): OUTCOME is `applied' for a terminal decision,
`skipped' for one decided elsewhere, `held' while a note's file is in use or
awaits its extraction, `stop' when CONTEXT no longer admits writes, `quit'
after interruption and `unapplied' otherwise."
  (if (and note (niwa-memory-notes-in-use-p workspace file))
      (cons 'held nil)
    (condition-case nil
        (let* ((result (niwa-memory-decision-apply
                        workspace pass proposal
                        :automatic context :on-applied on-applied :continuation continuation))
               (status (plist-get result :status)))
          (cons (cond ((niwa-memory-decision-terminal-status-p status) 'applied)
                      ((eq status 'skipped) 'skipped)
                      ((and note (memq status '(held waiting))) 'held)
                      ((eq status 'held) 'stop)
                      (t 'unapplied))
                (plist-get result :continuation)))
      (quit (cons 'quit nil))
      (error (cons 'unapplied nil)))))

(defun niwa-memory-pass--apply-pending (workspace &optional skip on-held on-applied context continuation)
  "Apply undecided proposals of earlier automatic note reviews in WORKSPACE.
Only reviews whose frozen mode was `auto' qualify, and only while the
current mode is still `auto'.  SKIP names a pass already handled.  Return
\(APPLIED . UNAPPLIED): proposals reaching a terminal decision, and those
that did not, including ones still waiting for their extraction.  A note
decision whose file an open session uses stays undecided; ON-HELD, when
non-nil, receives that file.  ON-APPLIED, when non-nil, receives confirmed
changed paths; terminal decisions alone do not notify it.  CONTEXT is the
automatic actor (the current buffer by default); CONTINUATION continues
its serial request."
  (let ((count 0) (failed 0)
        (context (or context (niwa-memory-decision-automatic-context workspace (current-buffer)))))
    (when (eq niwa-memory-consolidation-mode 'auto)
      (let ((selection (niwa-memory-decision-pending
                        workspace :automatic context :skip skip :continuation continuation)))
        (setq continuation (plist-get selection :continuation))
        (dolist (item (plist-get selection :items))
          (let ((outcome (niwa-memory-pass--apply-automatic
                          workspace context (plist-get item :pass) (plist-get item :proposal)
                          (plist-get item :note) (plist-get item :file) on-applied continuation)))
            (setq continuation (cdr outcome))
            (pcase (car outcome)
              ('applied (cl-incf count))
              ('skipped nil)
              ('held (when on-held (funcall on-held (plist-get item :file))))
              ('quit (signal 'quit nil))
              (_ (cl-incf failed)))))))
    (cons count failed)))

(defun niwa-memory-pass--defer-notes (state &optional files)
  "Retry STATE's held note decisions using its original application context.
FILES are the held notes' relative paths; the retry waits until one is free.
Without FILES, the next tick retries."
  (let ((workspace (plist-get state :workspace)))
    (unless (plist-get state :stop-apply)
      (if-let* ((retry (gethash workspace niwa-memory-pass--deferred)))
          (plist-put retry :files (seq-union (plist-get retry :files) files))
        (let ((retry (list :timer nil :context (plist-get state :application-context)
                           :files (delete-dups (copy-sequence files)))))
          (puthash workspace retry niwa-memory-pass--deferred)
          (plist-put retry :timer (run-with-timer 60 60 #'niwa-memory-pass--retry-notes workspace retry)))))))

(defun niwa-memory-pass--retry-notes (workspace &optional expected)
  "Apply WORKSPACE's held automatic note decisions once one of them is free.
A held note is free when no open session uses its file and no local
consolidation, drain or busy transport runs.
EXPECTED, when non-nil, is this timer's exact context, not a newer retry.
The retry ends after an idle attempt or invalidation of its original actor."
  (when-let* ((retry (gethash workspace niwa-memory-pass--deferred)))
    (when (or (null expected) (eq retry expected))
      (condition-case err
          (let* ((context (plist-get retry :context))
                 (busy (or (let ((in-use (niwa-memory-notes-in-use workspace)))
                             (and (plist-get retry :files)
                                  (cl-every (lambda (file) (member file in-use))
                                            (plist-get retry :files))))
                           (niwa-transport-busy-p (niwa-workspace-root workspace))))
                 (current (niwa-memory-decision-automatic-current-p context))
                 (key (niwa-memory-store--claim-directory workspace)))
            ;; Busy and freshness IO can dispatch cancellation or another local
            ;; admission.  Only non-IO observations follow this final boundary.
            (when (and (eq retry (gethash workspace niwa-memory-pass--deferred))
                       (or (not current)
                           (and (not busy)
                                (not (gethash key niwa-memory-pass--running))
                                (not (gethash key niwa-memory-pass--drains)))))
              (cancel-timer (plist-get retry :timer))
              (remhash workspace niwa-memory-pass--deferred)
              (when current
                (let* ((state (list :workspace workspace :origin-buffer (plist-get context :origin)
                                    :application-context context :settled t :claim (list :directory key))))
                  (puthash key state niwa-memory-pass--running)
                  (with-current-buffer (plist-get context :origin)
                    (niwa-memory-pass--apply-async
                     state nil
                     (lambda (outcome)
                       (when (eq state (gethash key niwa-memory-pass--running))
                         (remhash key niwa-memory-pass--running))
                       (when (or (plist-get outcome :error) (> (plist-get outcome :unapplied-count) 0))
                         (niwa-diagnostic-warning (niwa-diagnostic-scope workspace) 'niwa
                                          (format "Deferred memory application needs attention; see the memory cockpit%s"
                                                  (if-let* ((failure (plist-get outcome :error))) (concat ": " failure) ""))
                                          :warning)))))))))
        (error
         (cancel-timer (plist-get retry :timer))
         (when (eq retry (gethash workspace niwa-memory-pass--deferred))
           (remhash workspace niwa-memory-pass--deferred))
         (niwa-diagnostic-warning (niwa-diagnostic-scope workspace) 'niwa (format "Held shared-note decisions failed: %s"
                                           (error-message-string err))
                          :warning))))))

(defconst niwa-memory-pass--rejected-replies 8
  "Number of rejected review replies retained for diagnosis.")

(defun niwa-memory-pass--retain-rejected-reply (workspace id result)
  "Retain RESULT's rejected reply for pass ID in local WORKSPACE diagnostics.
Only the newest `niwa-memory-pass--rejected-replies' are kept.  The text
is evidence for diagnosing the output contract, never proposals.  Failures
here are diagnostic only."
  (when-let* ((reply (plist-get result :rejected-reply))
              (root (niwa-workspace-root workspace))
              ((not (file-remote-p root))))
    (ignore-errors
      (let ((directory (file-name-concat root ".niwa" "state" "diagnostics" "rejected-replies")))
        (make-directory directory t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region (concat (format "<!-- pass %s; error: %s -->\n" id (plist-get result :error)) reply)
                        nil (file-name-concat directory (concat id ".md")) nil 'silent))
        (let ((files (sort (directory-files directory t "\\.md\\'")
                           (lambda (a b) (time-less-p (file-attribute-modification-time (file-attributes b))
                                                      (file-attribute-modification-time (file-attributes a)))))))
          (mapc #'delete-file (nthcdr niwa-memory-pass--rejected-replies files)))))))

(cl-defun niwa-memory-pass--finish (state result)
  "Settle STATE once from RESULT and publish only its accepted immutable reply."
  (unless (plist-get state :settled)
    (dolist (key '(:preparation-duration-ms :review-request-duration-ms :publication-duration-ms))
      (niwa-memory-pass--timing-end state key))
    (when (and (eq (plist-get result :outcome) 'success)
               (not (plist-get state :publication-finished))
               (niwa-journal-worker-local-p
                (plist-get state :workspace)
                (mapcar (lambda (root) (plist-get (cdr root) :dir))
                        (plist-get (plist-get (plist-get state :prepared) :scope) :roots))))
      (unless (plist-get state :publishing)
        (plist-put state :publishing t)
        (niwa-memory-pass--timing-start state :publication-duration-ms)
        (condition-case err
            (plist-put state :worker
                       (niwa-journal-worker-start
                        (plist-get state :workspace) 'memory-publish
                        (list :prepared (plist-get state :prepared)
                                  :reply (plist-get result :reply) :entries (plist-get result :entries)
                                  :notes (plist-get result :notes)
                                  :model (niwa-memory-pass--producer result)
                                  :references (plist-get (plist-get result :references) :references))
                        (lambda (reply)
                          (unless (plist-get state :settled)
                            (plist-put state :worker nil)
                            (plist-put state :publication-finished t)
                            (plist-put state :accepted (plist-get reply :accepted))
                            (plist-put state :entry (plist-get reply :entry))
                            (niwa-memory-pass--finish
                             state (if (plist-get reply :entry) result
                                     (append (list :outcome 'error :error
                                                   (or (plist-get reply :error) "Memory publication returned no entry")) result)))))))
          (error
           (plist-put state :publication-finished t)
           (niwa-memory-pass--finish
            state (append (list :outcome 'error :error (error-message-string err)) result)))))
      (cl-return-from niwa-memory-pass--finish nil))
    (plist-put state :settled t)
    (condition-case err
      (let* ((workspace (plist-get state :workspace))
           (claim (plist-get state :claim))
           (id (plist-get claim :owner))
           (status (if (memq (plist-get result :outcome) '(aborted restart-required)) 'cancelled 'failed))
           (reported (if-let* ((read-usage (plist-get (plist-get state :request) :usage)))
                         (funcall read-usage) result))
           ;; Freeze diagnostics before storage can replace RESULT with an error.
           ;; Cancellation has no review callback result, so use its live snapshot.
           (usage (append
                   (cl-loop for key in '(:investigation :investigation-notes :investigation-notes-omitted)
                            append (list key (if (plist-member result key)
                                                (plist-get result key) (plist-get reported key))))
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
           (model (when (plist-get result :policy) (niwa-model--provider-label (plist-get result :policy))))
           (failure-class (cond ((plist-get usage :budget-kind) 'output-limit)
                                ((plist-get state :publishing) 'publication)
                                (t 'review)))
           (accepted (plist-get state :accepted))
           (entry (plist-get state :entry))
           recovery-error)
      (niwa-memory-pass--retain-rejected-reply workspace id result)
      (when (timerp (plist-get state :timer)) (cancel-timer (plist-get state :timer)))
      (plist-put state :timer nil)
      (when-let* ((worker (plist-get state :worker)))
        (plist-put state :worker nil)
        (niwa-journal-worker-cancel worker))
      (condition-case err
          (when (eq (plist-get result :outcome) 'success)
            (setq failure-class 'publication)
            (unless (plist-get state :publication-finished)
              (niwa-memory-pass--timing-start state :publication-duration-ms)
              (setq accepted
                    (niwa-memory-store-accept
                     workspace (plist-get state :prepared) (plist-get result :reply)
                     (plist-get result :entries) (niwa-memory-pass--producer result)
                     (plist-get (plist-get result :references) :references)
                     (plist-get result :notes)))
              (unless accepted (error "Memory pass lost ownership before accepting its result"))
              (setq entry (niwa-memory-store-publish workspace id)))
            (when (string-empty-p (plist-get entry :focus))
              (setf (niwa-workspace-memory-schedule workspace) nil)))
        (error (setq result (append (list :outcome 'error :error (error-message-string err)) result))))
      (niwa-memory-pass--timing-end state :publication-duration-ms)
      (unless entry
        ;; A preparation child can fail (or reenter cancellation) after cycle
        ;; admission but before returning its bundle.  Recover only this exact
        ;; original admission's reserved slot, never a foreign/newer cohort.
        (condition-case err
            (when-let* ((cycle (car (niwa-memory-cycle--read workspace)))
                        ((equal claim (plist-get (plist-get cycle :inflight) :claim))))
              (plist-put state :cycle cycle))
          (error (setq recovery-error (error-message-string err))))
        (niwa-memory-pass--timing-start state :retirement-duration-ms)
        (condition-case err
            (progn
              (niwa-journal-claim-settle claim status "")
              (when (eq 'completed (plist-get (niwa-journal-claim-outcome claim) :status))
                (setq recovery-error "Accepted memory result requires publication verification"))
              ;; A timeout cannot settle as the now-expired owner. Compete for
              ;; the same immutable expiry outcome used by target-side takeover.
              (when (and (not (niwa-journal-claim-outcome claim))
                         (>= (niwa-session-control-fs-target-time (plist-get claim :directory))
                             (plist-get claim :expires-at)))
                (niwa-journal-claim--finish claim 'expired ""))
              (unless (niwa-journal-claim-outcome claim)
                (error "Memory pass has no durable settlement"))
              (when (and (plist-get state :prepared)
                         (memq (plist-get (niwa-journal-claim-outcome claim) :status)
                               '(failed cancelled expired)))
                (niwa-memory-store-release workspace id)))
          (error (setq recovery-error (error-message-string err)))))
      (niwa-memory-pass--timing-end state :retirement-duration-ms)
      (unless entry
        ;; Preserve fence-before-abort ordering, but close the provider call
        ;; before publishing its terminal timing totals.  Transport callbacks
        ;; cannot reenter this already-settled pass.
        (when-let* ((cancel (plist-get (plist-get state :request) :cancel)))
          (funcall cancel))
        (when-let* ((snapshot (plist-get (plist-get state :request) :usage)))
          (let ((final-usage (funcall snapshot)))
            (dolist (key '(:provider-duration-ms :provider-call-count :investigation))
              (when (plist-member final-usage key)
                (setq usage (plist-put usage key (plist-get final-usage key))))))))
      (when (plist-get state :publishing)
        (setf (niwa-workspace-journal-observation workspace) nil))
      (cl-labels ((complete (applied)
			    (niwa-memory-pass--timing-end state :application-duration-ms)
		    (plist-put state :result
			       (append (list :outcome (if entry 'success (plist-get result :outcome))
					     :id id :entry entry :error (or (plist-get result :error) (plist-get applied :error))
					     :mode (plist-get (plist-get state :telemetry) :mode)
                                             :queue-duration-ms (plist-get (plist-get state :telemetry) :queue-duration-ms)
					     :stopped (plist-get state :stop-apply)
					     :pass-recovery-required (or recovery-error (and accepted (not entry)))
					     :remaining (if entry
							    (+ (or (plist-get (plist-get state :selection) :remaining) 0)
							       (or (plist-get result :omitted-digests) 0))
							  (or (plist-get (plist-get state :selection) :eligible) 0))
					     :notes-reviewed (length (plist-get result :notes))
					     :notes-remaining (or (plist-get (plist-get state :notes) :remaining) 0)
					     :notes-blocked (or (plist-get (plist-get state :notes) :blocked-count) 0))
				       applied usage))
                            (when (plist-get state :cycle)
                              (condition-case err
                                  (let* ((failed (or (not entry) (plist-get state :stop-apply)
                                                     (plist-get (plist-get state :result) :error)
                                                     (> (or (plist-get applied :unapplied-count) 0) 0)))
                                         (cycle (if failed
                                                    (niwa-memory-cycle-pause
                                                     workspace (cond ((plist-get state :stop-apply) 'cancelled)
                                                                     ((not entry) status)
                                                                     (t 'application))
                                                     (plist-get (plist-get state :cycle) :id))
                                                  nil)))
                                    (when cycle (plist-put state :cycle cycle))
                                    ;; Reconciliation is performed by the next fresh
                                    ;; admission.  A successful offer continues only
                                    ;; that finite cohort, never newly arrived notes.
                                    (setf (niwa-workspace-memory-schedule workspace)
                                          (list :after (if failed (+ (float-time) 600) 0))))
                                (error
                                 (plist-put (plist-get state :result) :pass-recovery-required (error-message-string err)))))
                            (when (plist-get state :stop-apply)
                              (plist-put (plist-get state :result) :stopped t))
                            (when-let* ((cycle (plist-get state :cycle)))
                              (let ((fields (niwa-memory-cycle-summary cycle)))
                                (plist-put state :telemetry (append (plist-get state :telemetry) fields))
                                (plist-put state :result (append (plist-get state :result) fields))))
                            (when (eq state (gethash (plist-get claim :directory) niwa-memory-pass--running))
		      (remhash (plist-get claim :directory) niwa-memory-pass--running))
			    (when (and entry (plist-get state :automatic)
                                       (plist-get (plist-get state :cycle) :inflight)
                                       (not (plist-get state :stop-apply))
                                       (not (plist-get (plist-get state :result) :error))
                                       (not (plist-get (plist-get state :result) :pass-recovery-required)))
                              ;; Continue by a new idle admission, never recursive
                              ;; provider dispatch or an expanding arrival chain.
                              (niwa-memory-pass-schedule workspace))
	                    (unwind-protect
                            (unless (plist-get state :admission-aborted)
			    (apply #'niwa-telemetry-record-workspace workspace
			   (cond (entry 'memory-consolidation-completed)
				 ((eq status 'cancelled) 'memory-consolidation-killed)
				 (t 'memory-consolidation-failed))
			   (append (plist-get state :telemetry) usage (plist-get state :phase-durations)
				   (list :model model :outcome (plist-get (plist-get state :result) :outcome)
					 :duration-ms (when-let* ((start (plist-get state :started-at)))
                                                        (round (* 1000 (max 0 (- (niwa-telemetry--monotonic-now) start)))))
					 :failure-class (and (not entry) (not (eq status 'cancelled)) failure-class)
					 :reviewed-count (length (plist-get entry :digests))
					 :covered-count (if (eq 'general (plist-get (plist-get state :telemetry) :scope))
							    (length (plist-get entry :digests)) 0)
					 :proposed-count (length (plist-get entry :proposals))
					 :updated-file-count (plist-get applied :updated-files)
						 :remaining-count (plist-get (plist-get state :result) :remaining)))))
	                      ;; Async completion runs after this function's outer
	                      ;; handler has returned.  Terminal telemetry must not
	                      ;; leave its watch open when an append quits.
	                      (let ((inhibit-quit t))
	                        (niwa-telemetry-memory-pass-settle (plist-get state :lag-watch))))
				    (when-let* ((callback (plist-get state :callback)))
		      (funcall callback (plist-get state :result)))))
	(when entry
	  (when (plist-get state :publication-finished)
	    (niwa-journal-cleanup-schedule workspace t))
	  (when accepted (niwa-memory-pass--supersede workspace accepted))
	  (setf (niwa-workspace-memory-observation workspace) nil))
		(if (and entry (eq 'auto (plist-get (plist-get state :telemetry) :mode))
			 (niwa-memory-pass--async-application-p workspace accepted))
		    (niwa-memory-pass--apply-async state accepted #'complete)
		  (complete (when (and entry (eq 'auto (plist-get (plist-get state :telemetry) :mode)))
			      (niwa-memory-pass--apply state accepted))))))
      ((error quit)
       (niwa-telemetry-memory-pass-settle (plist-get state :lag-watch))
       (signal (car err) (cdr err))))))

(defun niwa-memory-pass-cancel (workspace)
  "Cancel only this client's running consolidation for WORKSPACE.
Fence its outcome before aborting transport, making late callbacks harmless.
Also stop deferred application and explicit drains.
Return non-nil if any existed."
  (let* ((key (niwa-memory-store--claim-directory workspace))
         (deferred (gethash workspace niwa-memory-pass--deferred))
         (drain (gethash key niwa-memory-pass--drains))
         (state (niwa-memory-pass-running workspace)))
    (when deferred
      (cancel-timer (plist-get deferred :timer))
      (remhash workspace niwa-memory-pass--deferred))
    (when drain
      (remhash key niwa-memory-pass--drains)
      (when (timerp (plist-get drain :timer)) (cancel-timer (plist-get drain :timer)))
      (plist-put drain :status 'cancelled)
      (when (and (not state) (plist-get drain :cycle-id))
        (condition-case err
            (niwa-memory-cycle-pause workspace 'cancelled (plist-get drain :cycle-id))
          (error (plist-put drain :error (error-message-string err))))))
    (when state
      (plist-put state :stop-apply t)
      (when-let* ((job (plist-get state :application)))
        (plist-put job :cancelled t)
        ;; An executing decision unwinds first and delivers its own callback.
        (unless (niwa-memory-decision-cancel (plist-get job :context))
          (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer)))
          (niwa-memory-pass--application-next state job)))
      (unwind-protect
          (niwa-memory-pass--finish state '(:outcome aborted))
        (when-let* ((cancel (plist-get (plist-get state :request) :cancel))) (funcall cancel))))
    (when drain
      (funcall (plist-get drain :callback) (list :outcome 'aborted :batch (plist-get drain :batch)
                                               :pass-recovery-required (plist-get drain :error))))
    (and (or state drain deferred) t)))

(defun niwa-memory-pass--publish (workspace payload)
  "Accept and publish a validated review in WORKSPACE from frozen PAYLOAD.
The accepted bundle survives a publication failure for checked recovery."
  (let (accepted)
    (condition-case err
        (progn
          (setq accepted
                (niwa-memory-store-accept
                 workspace (plist-get payload :prepared) (plist-get payload :reply)
                 (plist-get payload :entries) (plist-get payload :model)
                 (plist-get payload :references) (plist-get payload :notes)))
          (unless accepted (error "Memory pass lost ownership before accepting its result"))
          (list :ok t :accepted accepted
                :entry (niwa-memory-store-publish
                        workspace (plist-get (plist-get payload :prepared) :id))))
      (error (list :error (error-message-string err) :accepted accepted)))))

(cl-defun niwa-memory-pass--prepare (workspace payload)
  "Recover, select and prepare consolidation inputs from frozen PAYLOAD.
This may run in a storage child; no provider configuration is needed.
General reviews also capture ready shared working files using the frozen
note settings in PAYLOAD.  Ready shared-folder work admits an automatic
review by itself; a review with neither digests nor text notes needs no
inference and is marked :housekeeping-only."
  (unless (equal (plist-get payload :client) (niwa-workspace-identity-client))
    (error "Memory preparation belongs to another client"))
  (let* ((claim (plist-get payload :claim))
         (automatic (plist-get payload :automatic))
         (focus (plist-get payload :focus))
         (general (string-empty-p focus))
         (niwa-memory-consolidation-min-hours (plist-get payload :min-hours))
         (niwa-memory-consolidation-min-digests (plist-get payload :min-digests))
         (niwa-memory-consolidation-mode (plist-get payload :mode))
         (niwa-working-note-max-age-days (plist-get payload :notes-max-age-days))
         (niwa-memory-notes-max-review-bytes (plist-get payload :notes-max-review-bytes))
         recovery-duration notes scope cycle)
    (niwa-memory-store--assert-owned claim)
    (let ((start (niwa-telemetry--monotonic-now)))
      ;; The sole inflight receipt must be reconciled before any recovery that
      ;; can expire its accepted/prepared proof, including zero-proposal passes.
      (niwa-memory-cycle-recover workspace claim)
      (niwa-memory-decision-recover workspace)
      (niwa-memory-store-recover workspace)
      (setq recovery-duration (round (* 1000 (max 0 (- (niwa-telemetry--monotonic-now) start))))))
    (let* ((observed
            (niwa-memory-store--mutation
             workspace
             (lambda (_mutation)
               (niwa-journal-cleanup-recover
                (niwa-journal-store-directory (niwa-workspace-root workspace)))
               (niwa-journal-store-entries (niwa-workspace-root workspace)))))
           (selection (if automatic
                          (niwa-memory-pass--automatic-selection workspace observed)
                        (niwa-memory-pass-select observed (not general))))
           (entries (plist-get selection :entries)))
      (when general
        ;; Capture citations before cohort freezing.  Admission freezes every
        ;; eligible metadata version before ordinary per-request bounds apply.
        (setq scope (niwa-memory-scope-capture workspace nil (plist-get payload :configuration)))
        (setq cycle
              (if (plist-get payload :cycle-id)
                  (let ((existing (car (niwa-memory-cycle--read workspace))))
                    (unless (equal (plist-get payload :cycle-id) (plist-get existing :id))
                      (error "Memory drain note cycle changed"))
                    (and (eq (plist-get existing :status) 'active)
                         (niwa-memory-cycle-admit
                          workspace claim (lambda () (error "Cannot replace a drain's frozen cohort")))))
                (niwa-memory-cycle-admit
                 workspace claim
                 (lambda ()
                   (when-let* ((configured (niwa-memory-notes-directory workspace))
                               ((niwa-session-control-fs-directory-p configured)))
                     (niwa-memory-notes-cohort
                      workspace observed :mode (plist-get payload :mode)
                      :citation-scope scope
                      :in-use (plist-get payload :in-use-notes))))
                 (not automatic))))
        (when cycle
          (setq notes (or (niwa-memory-pass--capture-notes
                           workspace observed (plist-get payload :mode)
                           scope
                           (plist-get payload :in-use-notes)
                           (niwa-memory-cycle-cohort cycle)) (list :root nil)))
          (setq cycle (niwa-memory-cycle-stage workspace claim cycle :capture notes))
          ;; Note readiness never bypasses journal-only evidence policy.  A
          ;; simultaneous journal selection may share the same bounded request.
          (when (and (not selection) (plist-get notes :root))
            (setq selection (list :entries nil :eligible 0 :remaining 0)))))
      (when (cl-some (lambda (note) (plist-get (cdr note) :chunked))
                     (plist-get (cdr (plist-get notes :root)) :notes))
        ;; An oversized note owns this review.  Leave digests unpinned for
        ;; the next batch rather than carrying unused evidence through chunks.
        (setq selection (plist-put selection :remaining
                                   (+ (or (plist-get selection :remaining) 0) (length entries)))
              selection (plist-put selection :entries nil)
              entries nil))
      (if (and automatic (not selection))
          (list :skipped t :cycle (or cycle (car (niwa-memory-cycle--read workspace)))
                :schedule (niwa-workspace-memory-schedule workspace))
        (when (and (plist-get payload :drain) (not entries) (not (plist-get notes :root)))
          (cl-return-from niwa-memory-pass--prepare
            (list :empty t :cycle (or cycle (car (niwa-memory-cycle--read workspace))))))
        (unless (or entries (plist-get notes :root) (plist-get payload :memory-only))
          (error "No eligible digests or shared notes for consolidation"))
        (niwa-memory-store--assert-owned claim)
        (let* ((root (plist-get notes :root))
               (scope (or scope (niwa-memory-scope-capture
                                 workspace nil (plist-get payload :configuration))))
               (scope (if root (plist-put scope :roots (append (plist-get scope :roots) (list root))) scope))
               (scope (if (and root cycle)
                          (plist-put scope :note-cycle (list :id (plist-get cycle :id)
                                                             :cohort-hash (plist-get cycle :cohort-hash))) scope))
               (prepared (niwa-memory-store-prepare workspace claim scope entries focus)))
          (list :selection selection :schedule (niwa-workspace-memory-schedule workspace)
                :cycle cycle
                :preparation-recovery-duration-ms recovery-duration
                :record prepared :rejections (niwa-memory-decision-rejections workspace)
                :notes (cl-loop for key in '(:notes :attachments :directories :remaining :blocked :blocked-count)
                                append (list key (plist-get notes key)))
                :housekeeping-only (and root (not entries) (not (plist-get payload :memory-only))
                                        (null (plist-get (cdr root) :files)))))))))

(defun niwa-memory-pass--capture-notes (workspace entries mode scope in-use cohort)
  "Capture WORKSPACE's ready shared files from public ENTRIES under MODE.
SCOPE retains complete memory and instruction citation evidence and omissions.
IN-USE lists relative paths open sessions are using; they wait.
COHORT is a closed ordered allowlist of frozen full file versions.
The caller holds the consolidation claim, excluding concurrent application."
  (when-let* ((configured (niwa-memory-notes-directory workspace))
              ((niwa-session-control-fs-directory-p configured)))
    (niwa-memory-notes-capture workspace entries :mode mode :citation-scope scope :in-use in-use :cohort cohort)))

(defun niwa-memory-pass--prepared (state result focus memory-only)
  "Start STATE's configured review from prepared RESULT, FOCUS and MEMORY-ONLY.
Cancelled or expired storage replies never start a model request."
  (unless (plist-get state :settled)
    (niwa-memory-pass--timing-end state :preparation-duration-ms)
    (niwa-memory-pass--timing-add state :preparation-recovery-duration-ms
                                   (plist-get (plist-get result :prepared) :preparation-recovery-duration-ms))
    (plist-put state :worker nil)
    (condition-case err
        (let* ((workspace (plist-get state :workspace))
               (claim (plist-get state :claim))
               (bundle (plist-get result :prepared))
               (prepared (plist-get bundle :record)))
          (when (plist-get result :error) (error "%s" (plist-get result :error)))
          (niwa-memory-store--assert-owned claim)
          (plist-put state :cycle (plist-get bundle :cycle))
          (when (plist-get bundle :cycle)
            (apply #'niwa-telemetry-record-workspace workspace 'memory-note-cycle-observed
                   (niwa-memory-cycle-summary (plist-get bundle :cycle))))
          (when-let* ((drain (plist-get state :drain))
                      (id (plist-get (plist-get bundle :cycle) :id)))
            (plist-put drain :cycle-id id))
          (setf (niwa-workspace-memory-schedule workspace) (plist-get bundle :schedule)
                (niwa-workspace-memory-observation workspace) nil
                (niwa-workspace-journal-observation workspace) nil)
          (if (or (plist-get bundle :empty)
                  (and (plist-get state :automatic) (plist-get bundle :skipped)))
              (progn
                (niwa-journal-claim-settle claim 'cancelled "")
                (when (timerp (plist-get state :timer)) (cancel-timer (plist-get state :timer)))
                (plist-put state :timer nil)
                (plist-put state :settled t)
                (plist-put state :skipped t)
                (niwa-telemetry-memory-pass-settle (plist-get state :lag-watch))
                (remhash (plist-get claim :directory) niwa-memory-pass--running)
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
              (apply #'niwa-telemetry-record-workspace workspace 'memory-consolidation-fired
                     (plist-get state :telemetry)))
            (unless (buffer-live-p (plist-get state :origin-buffer))
              (error "Memory review caller buffer was closed"))
            (if (plist-get bundle :housekeeping-only)
                ;; Proved quiet, unreferenced attachments and empty folders
                ;; need no inference; acceptance derives their decisions.
                (niwa-memory-pass--finish state (list :outcome 'success :reply nil :entries nil :notes nil))
            (niwa-memory-pass--timing-start state :review-request-duration-ms)
            (plist-put state :request
                       (with-current-buffer (plist-get state :origin-buffer)
                         (niwa-memory-review-request
                          (plist-get prepared :scope) (plist-get prepared :entries)
                          (lambda (reply) (niwa-memory-pass--finish state reply))
                          :workspace workspace :pass-id (plist-get (plist-get state :claim) :owner)
                          :activep (lambda () (not (plist-get state :settled)))
                          :focus focus :memory-only memory-only
                          :rejections (plist-get bundle :rejections)
                          :currentp (lambda ()
                                      (and (not (plist-get state :settled))
                                           (condition-case nil
                                               (progn (niwa-memory-store--assert-owned claim) t)
                                              (error nil))))))))
            ;; Request construction can yield before publishing its handle.
            (when (plist-get state :settled)
              (when-let* ((cancel (plist-get (plist-get state :request) :cancel)))
                (funcall cancel)))))
      (error (niwa-memory-pass--finish
              state (list :outcome 'error :error (error-message-string err)))))))

(cl-defun niwa-memory-pass-start (workspace callback &key (focus "") memory-only automatic drain queued-at)
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
  (let ((root (copy-sequence (niwa-workspace-root workspace))))
    (unless (and (stringp focus) (<= (string-bytes focus) 4096))
      (error "Memory focus exceeds its byte limit"))
    (when (and drain (not (eq drain (gethash (plist-get drain :key) niwa-memory-pass--drains))))
      (cl-return-from niwa-memory-pass-start nil))
    (when (and automatic (or niwa-memory-pass--inhibit-scheduling
			     (eq niwa-memory-consolidation-mode 'manual)))
      (cl-return-from niwa-memory-pass-start nil))
    (when (and automatic (or memory-only (not (string-empty-p focus))))
      (error "Automatic review must use general digest admission"))
    (when (and (not drain)
	       (gethash (niwa-memory-store--claim-directory workspace) niwa-memory-pass--drains))
      (if automatic (cl-return-from niwa-memory-pass-start nil)
	(error "Memory backlog drain is already running")))
    (when (niwa-memory-pass-running workspace)
      (if automatic (cl-return-from niwa-memory-pass-start nil)
	(error "Memory consolidation is already running")))
    (when (niwa-journal-cleanup-active-p workspace)
      (if automatic (cl-return-from niwa-memory-pass-start nil)
	(niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Journal cleanup is running; retry memory consolidation when it finishes")))
    (niwa-workspace-identity-ensure root)
    (unless (equal root (niwa-workspace-root workspace))
      (error "Memory consolidation workspace changed"))
    (let* ((key (niwa-memory-store--claim-directory workspace))
	   (currentp (lambda ()
		       (and (equal key (niwa-memory-store--claim-directory workspace))
                            (equal root (niwa-workspace-root workspace))
                            (or (null drain)
				(and (eq drain (gethash key niwa-memory-pass--drains))
				     (equal key (plist-get drain :key))
				     (equal (plist-get drain :root) root))))))
	   (_ (unless (funcall currentp)
		(when (or (null drain) (eq drain (gethash (plist-get drain :key) niwa-memory-pass--drains)))
		  (error "%s" (if drain "Memory drain workspace changed" "Memory consolidation workspace changed")))
		(cl-return-from niwa-memory-pass-start nil)))
	   (claim (or (niwa-journal-claim-acquire key 180)
                      (progn
                        (unless (funcall currentp)
                          (when (or (null drain) (eq drain (gethash (plist-get drain :key) niwa-memory-pass--drains)))
                            (error "%s" (if drain "Memory drain workspace changed" "Memory consolidation workspace changed")))
                          (cl-return-from niwa-memory-pass-start nil))
			(if automatic (cl-return-from niwa-memory-pass-start nil)
			  (error "Memory consolidation is busy")))))
	   (admitted-at (niwa-telemetry--monotonic-now))
	   (state (list :workspace workspace :claim claim :callback callback :settled nil
			:automatic automatic :drain drain :origin-buffer (current-buffer)
			:started-at admitted-at
			:telemetry (list :pass-id (plist-get claim :owner)
					 :queue-duration-ms (when queued-at (round (* 1000 (max 0 (- admitted-at queued-at)))))
					 :attempt-generation (plist-get claim :generation)
					 :mode niwa-memory-consolidation-mode :workload 'memory
					 :scope (if (string-empty-p focus) 'general 'focused))))
	   (finish (lambda (result)
		     (plist-put state :admission-aborted t)
		     (niwa-memory-pass--finish state result)
		     (when-let* ((failure (plist-get (plist-get state :result) :pass-recovery-required)))
		       ;; The cancelled drain's callback may no longer be current.
		       ;; Settlement failure must still be visible, not quiet loss.
		       (niwa-diagnostic-warning (niwa-diagnostic-scope workspace) 'niwa
					(format "Memory admission %s needs recovery: %s" (plist-get claim :owner) failure)
					:warning)))))
      (when (gethash key niwa-memory-pass--running)
	(funcall finish '(:outcome error :error "Memory consolidation is already running"))
	(cl-return-from niwa-memory-pass-start nil))
      (puthash key state niwa-memory-pass--running)
      (condition-case err
          (plist-put state :lag-watch
                     (niwa-telemetry-memory-pass-start workspace (plist-get claim :owner)))
        (quit
         ;; The watcher has not returned its handle and the deadline is not
         ;; armed yet.  Fence this admitted claim before preserving the quit.
         (let ((inhibit-quit t)) (funcall finish '(:outcome aborted)))
         (signal (car err) (cdr err))))
      ;; Starting the shared native watcher may yield to cancellation.
      (when (plist-get state :settled)
        (niwa-telemetry-memory-pass-settle (plist-get state :lag-watch)))
      (unless (funcall currentp)
	;; Claim creation may have yielded to cancellation and a new drain.
	;; Keep this exact state observed until its own new claim is fenced.
	(funcall finish (if (or (null drain) (eq drain (gethash (plist-get drain :key) niwa-memory-pass--drains)))
			    (list :outcome 'error :error (if drain "Memory drain workspace changed" "Memory consolidation workspace changed"))
			  '(:outcome aborted)))
	(cl-return-from niwa-memory-pass-start nil))
      (unless (or automatic drain)
	(apply #'niwa-telemetry-record-workspace workspace 'memory-consolidation-fired
	       (plist-get state :telemetry)))
      (when (plist-get state :settled) (cl-return-from niwa-memory-pass-start nil))
      (condition-case err
	  (progn
	    (plist-put state :timer
		       (run-at-time
			(max 0 (- (plist-get claim :expires-at)
				  (niwa-session-control-fs-target-time key))) nil
			(lambda ()
			  (unwind-protect
			      (niwa-memory-pass--finish state '(:outcome error :error "Memory pass timed out"))
			    (when-let* ((cancel (plist-get (plist-get state :request) :cancel)))
			      (funcall cancel))))))
	    (when (plist-get state :settled)
	      (cancel-timer (plist-get state :timer))
	      (plist-put state :timer nil)
	      (cl-return-from niwa-memory-pass-start nil))
	    (let* ((roots (niwa-system--memory-roots workspace))
		   (payload (list :claim claim :focus focus :automatic automatic :memory-only memory-only
				  :drain (and drain t)
				  :cycle-id (plist-get drain :cycle-id)
				  :min-hours niwa-memory-consolidation-min-hours
				  :min-digests niwa-memory-consolidation-min-digests
				  :mode niwa-memory-consolidation-mode
				  :notes-max-age-days niwa-working-note-max-age-days
				  :notes-max-review-bytes niwa-memory-notes-max-review-bytes
				  ;; Notes that open sessions are using wait; others proceed.
				  :in-use-notes (niwa-memory-notes-in-use workspace)
				  :client (niwa-workspace-identity-client)
				  :configuration (list :roots roots :instructions
						       (niwa-system-workspace-config-files workspace))))
		   (ready (lambda (result) (niwa-memory-pass--prepared state result focus memory-only))))
	      (unless (and (not (plist-get state :settled)) (funcall currentp))
		(niwa-memory-pass--finish state '(:outcome aborted))
		(cl-return-from niwa-memory-pass-start nil))
		      (niwa-memory-pass--timing-start state :preparation-duration-ms)
			      (if (niwa-journal-worker-local-p workspace (mapcar (lambda (root) (plist-get root :dir)) roots))
		  (plist-put state :worker
			     (niwa-journal-worker-start workspace 'memory-prepare payload ready))
		(funcall ready (list :ok t :prepared (niwa-memory-pass--prepare workspace payload))))))
	(error (niwa-memory-pass--finish state (list :outcome 'error :error (error-message-string err)))))
      (unless (plist-get state :skipped) state))))

(defun niwa-memory-pass-prepare-child (workspace payload)
  "Prepare WORKSPACE consolidation from frozen PAYLOAD for the editor."
  (list :ok t :prepared (niwa-memory-pass--prepare workspace payload)))

(defun niwa-memory-pass-publish-child (workspace payload)
  "Publish WORKSPACE's accepted consolidation described by PAYLOAD."
  (niwa-memory-pass--publish workspace payload))

(provide 'niwa-memory-pass)
;;; niwa-memory-pass.el ends here
