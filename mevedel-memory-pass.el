;;; mevedel-memory-pass.el -- Workspace consolidation coordination -*- lexical-binding: t -*-

;;; Commentary:

;; Run one workspace-owned consolidation independently of a live conversation.
;; Target-native claims fence inference, accepted storage precedes publication,
;; and a successor recovers older results before selecting its own evidence.

;;; Code:

(eval-when-compile (require 'cl-lib))
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
(declare-function mevedel-telemetry-record-workspace "mevedel-telemetry" (workspace event &rest props))
(autoload 'mevedel-telemetry-record-workspace "mevedel-telemetry")

(defcustom mevedel-memory-consolidation-mode 'propose
  "How memory consolidation is scheduled and proposals are handled.
Manual runs only on request.  Propose also runs automatically, at digest
publication, completed root turns, session opening and idle maintenance, and
leaves its proposals for approval.  Auto catches ready backlogs up at later idle
opportunities under a daily batch cap, and applies fresh
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

(defcustom mevedel-memory-consolidation-max-batches-per-day 8
  "Maximum automatic general reviews in auto mode per rolling 24 hours.
Successful general reviews, including explicit reviews, consume this allowance.
Auto catches up a ready backlog at later idle opportunities instead of waiting
for `mevedel-memory-consolidation-min-hours'.  Each batch retains the ordinary
request limits, so the default permits up to eight normal-sized requests per
day, not one larger request.  Explicit drain bypasses this automatic cap."
  :type 'natnum :group 'mevedel)

(defvar mevedel-memory-pass--inhibit-scheduling nil
  "Non-nil suppresses automatic opportunities during exit or isolated tests.")

(defvar mevedel-memory-pass--pending (make-hash-table :test #'eq)
  "Coalesced turn-completion timers, keyed by their workspace objects.")

(defvar mevedel-memory-pass--running (make-hash-table :test #'equal)
  "This client's active requests, keyed by physical pass ownership directory.")

(defvar mevedel-memory-pass--deferred (make-hash-table :test #'eq)
  "Retry timers for automatic shared-note decisions held during active turns.")

(defvar mevedel-memory-pass--drains (make-hash-table :test #'equal)
  "Explicit drains keyed by physical pass ownership directory.")

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
    (let ((cached (mevedel-workspace-memory-schedule workspace)) opportunity)
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
                            :automatic t)
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
  (maphash (lambda (_workspace timer) (cancel-timer timer)) mevedel-memory-pass--deferred)
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
Active turns and busy transports wait sixty seconds, without capturing notes
or starting an empty model request.  Every continuation yields to user input."
  (let* ((workspace (plist-get state :workspace))
         (key (mevedel-memory-store--claim-directory workspace)))
    (when (eq state (gethash key mevedel-memory-pass--drains))
      (plist-put state :timer nil)
      (condition-case err
          (cond
           ((not (buffer-live-p (plist-get state :origin-buffer)))
            (error "Memory drain caller buffer was closed"))
           ((or (mevedel-memory-notes-workspace-busy-p workspace)
                (mevedel-transport-busy-p (mevedel-workspace-root workspace))
                (input-pending-p))
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
                     (plist-put state :batch (1+ (plist-get state :batch))))
                   (if (and (memq (plist-get result :outcome) '(success waiting restart-required))
                            (not (plist-get result :stopped)))
                       (progn
                         (plist-put state :notes-remaining (plist-get result :notes-remaining))
                         (plist-put state :remaining (plist-get result :remaining))
                         (plist-put state :timer (run-at-time (if (eq (plist-get result :outcome) 'waiting) 60 .05)
                                                                 nil #'mevedel-memory-pass--drain-step state)))
                     (remhash key mevedel-memory-pass--drains)
                     (plist-put state :status (if (plist-get result :stopped) 'aborted
                                                (plist-get result :outcome))))
                   (funcall (plist-get state :callback)
                            (append (list :batch (plist-get state :batch)) result))))
               :drain t))))
        (error
         (remhash key mevedel-memory-pass--drains)
         (plist-put state :status 'error)
         (funcall (plist-get state :callback)
                  (list :outcome 'error :error (error-message-string err) :batch (plist-get state :batch))))))))

(defun mevedel-memory-pass-drain (workspace callback)
  "Drain ready WORKSPACE notes and digests through successive bounded reviews.
CALLBACK receives each result with a :batch count of completed batches.
The fresh final empty selection starts no model request.  Stop on failure,
caller closure or `mevedel-memory-pass-cancel'.  Explicit drain bypasses
automatic timing/count gates and daily cap, not ownership or request limits."
  (when (or (mevedel-memory-pass-drain-status workspace)
            (mevedel-memory-pass-running workspace))
    (user-error "Memory consolidation is already running"))
  (let ((state (list :workspace workspace :origin-buffer (current-buffer)
                     :callback callback :batch 0 :status 'queued :timer nil)))
    (puthash (mevedel-memory-store--claim-directory workspace) state mevedel-memory-pass--drains)
    (plist-put state :timer (run-at-time 0 nil #'mevedel-memory-pass--drain-step state))
    state))

(defun mevedel-memory-pass--apply (state accepted)
  "Apply ACCEPTED memory proposals sequentially for completed pass STATE.
Each application reacquires workspace and original-target ownership, for
instruction proposals as for memory ones.  Stop before further proposals if
cancellation was requested, holding the rest.
Shared-note decisions wait while a conversation in the workspace runs a
turn; a retry applies them, and earlier held ones, once it is idle.
Count distinct files only through this call's confirmed-write notification."
  (let ((workspace (plist-get state :workspace))
        (pass (plist-get (plist-get accepted :prepared) :id))
        (applied 0) (held 0) (unapplied 0) deferred files)
    (dolist (proposal (plist-get accepted :proposals))
      (cond
       ((plist-get state :stop-apply) (cl-incf held))
       ((and (mevedel-memory-proposal-note-action-p (plist-get proposal :action))
             (or deferred (mevedel-memory-notes-workspace-busy-p workspace)))
        (setq deferred t)
        (cl-incf held))
       (t
        (condition-case nil
            (let ((decision
                   (mevedel-memory-decision-apply
                    workspace pass (plist-get proposal :id)
                    (lambda (paths) (cl-incf applied) (setq files (append paths files))))))
              (unless (mevedel-memory-decision-terminal-status-p (plist-get decision :status))
                (cl-incf unapplied)))
          (quit (plist-put state :stop-apply t) (cl-incf unapplied))
          (error (cl-incf unapplied))))))
    (if deferred
        (mevedel-memory-pass--defer-notes workspace)
      (unless (plist-get state :stop-apply)
        (let ((earlier (mevedel-memory-pass--apply-pending workspace pass)))
          (cl-incf applied (car earlier))
          (cl-incf unapplied (cdr earlier)))))
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

(defun mevedel-memory-pass--apply-pending (workspace &optional skip)
  "Apply undecided proposals of earlier automatic note reviews in WORKSPACE.
Only reviews whose frozen mode was `auto' qualify, and only while the
current mode is still `auto'.  SKIP names a pass already handled.  Return
\(APPLIED . UNAPPLIED): proposals reaching a terminal decision, and those
that did not, including ones still waiting for their extraction."
  (let ((count 0) (failed 0))
    (when (eq mevedel-memory-consolidation-mode 'auto)
      (dolist (accepted (mevedel-memory-notes-passes
                         workspace (mevedel-journal-store-entries (mevedel-workspace-root workspace))))
        (let ((pass (plist-get (plist-get accepted :prepared) :id)))
          (when (and (not (equal pass skip))
                     (eq 'auto (plist-get (cdr (mevedel-memory-notes-root
                                                (plist-get (plist-get accepted :prepared) :scope)))
                                          :mode)))
            (dolist (proposal (plist-get accepted :proposals))
              (unless (mevedel-memory-decision-status workspace (plist-get proposal :id))
                (condition-case nil
                    (if (mevedel-memory-decision-terminal-status-p
                         (plist-get (mevedel-memory-decision-apply workspace pass (plist-get proposal :id))
                                    :status))
                        (cl-incf count)
                      (cl-incf failed))
                  (error (cl-incf failed)))))))))
    (cons count failed)))

(defun mevedel-memory-pass--defer-notes (workspace)
  "Retry WORKSPACE's held automatic note decisions once its turns are idle."
  (unless (gethash workspace mevedel-memory-pass--deferred)
    (puthash workspace
             (run-with-timer 60 60 #'mevedel-memory-pass--retry-notes workspace)
             mevedel-memory-pass--deferred)))

(defun mevedel-memory-pass--retry-notes (workspace)
  "Apply WORKSPACE's held automatic note decisions when nothing is running.
The retry ends after one attempt that finds the workspace idle."
  (unless (or (mevedel-memory-notes-workspace-busy-p workspace)
              (mevedel-memory-pass-running workspace)
              (mevedel-transport-busy-p (mevedel-workspace-root workspace)))
    (when-let* ((timer (gethash workspace mevedel-memory-pass--deferred)))
      (cancel-timer timer))
    (remhash workspace mevedel-memory-pass--deferred)
    (condition-case err
        (let ((outcome (mevedel-memory-pass--apply-pending workspace)))
          (when (> (cdr outcome) 0)
            (display-warning 'mevedel (format "%d held memory decisions were not applied; see the memory cockpit"
                                              (cdr outcome))
                             :warning)))
      (error (display-warning 'mevedel (format "Held shared-note decisions failed: %s"
                                               (error-message-string err))
                              :warning)))))

(cl-defun mevedel-memory-pass--finish (state result)
          "Settle STATE once from RESULT and publish only its accepted immutable reply."
          (unless (plist-get state :settled)
            (when (and (eq (plist-get result :outcome) 'success)
                       (not (plist-get state :publication-finished))
                       (mevedel-journal-worker-supported-p (plist-get state :workspace))
                       (not (cl-some (lambda (root) (file-remote-p (plist-get (cdr root) :dir)))
                                     (plist-get (plist-get (plist-get state :prepared) :scope) :roots))))
              (unless (plist-get state :publishing)
                (plist-put state :publishing t)
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
                                          :model (mevedel-memory-pass--producer result)
                                          :references (plist-get (plist-get result :references) :references))))
                  (error
                   (plist-put state :publication-finished t)
                   (mevedel-memory-pass--finish
                    state (append (list :outcome 'error :error (error-message-string err)) result)))))
              (cl-return-from mevedel-memory-pass--finish nil))
            (plist-put state :settled t)
            (let* ((workspace (plist-get state :workspace))
                   (claim (plist-get state :claim))
                   (id (plist-get claim :owner))
                   (status (if (eq (plist-get result :outcome) 'aborted) 'cancelled 'failed))
                   (reported (if-let* ((read-usage (plist-get (plist-get state :request) :usage)))
                                 (funcall read-usage) result))
                   ;; Freeze diagnostics before storage can replace RESULT with an error.
                   ;; Cancellation has no review callback result, so use its live snapshot.
                   (usage (cl-loop for key in '(:input-tokens :cached-tokens :output-tokens
                                                              :output-bytes :output-estimated-tokens :result-bytes
                                                              :reasoning-bytes :reply-bytes :tool-call-bytes
                                                              :tool-call-count :rounds
                                                              :budget-kind :output-limit)
                                   append (list key (if (plist-member result key)
                                                        (plist-get result key)
                                                      (or (plist-get reported key)
                                                          (unless (memq key '(:budget-kind :output-limit)) 0))))))
                   (model (when (plist-get result :policy) (mevedel-model--provider-label (plist-get result :policy))))
                   (failure-class (cond ((plist-get usage :budget-kind) 'output-limit)
                                        ((plist-get state :publishing) 'publication)
                                        (t 'review)))
                   (accepted (plist-get state :accepted))
                   (entry (plist-get state :entry))
                   recovery-error applied)
              (when (timerp (plist-get state :timer)) (cancel-timer (plist-get state :timer)))
              (plist-put state :timer nil)
              (when-let* ((worker (plist-get state :worker)))
                (plist-put state :worker nil)
                (when (process-live-p worker) (delete-process worker)))
              (condition-case err
                  (when (eq (plist-get result :outcome) 'success)
                    (setq failure-class 'publication)
                    (unless (plist-get state :publication-finished)
                      (setq accepted
                            (mevedel-memory-store-accept
                             workspace (plist-get state :prepared) (plist-get result :reply)
                             (plist-get result :entries) (mevedel-memory-pass--producer result)
                             (plist-get (plist-get result :references) :references)
                             (plist-get result :notes)))
                      (unless accepted (error "Memory pass lost ownership before accepting its result"))
                      (setq entry (mevedel-memory-store-publish workspace id)))
                    (when (string-empty-p (plist-get entry :focus))
                      (setf (mevedel-workspace-memory-schedule workspace) nil)))
                (error (setq result (list :outcome 'error :error (error-message-string err)))))
              (unless entry
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
                      (when (and (plist-get state :prepared)
                                 (memq (plist-get (mevedel-journal-claim-outcome claim) :status)
                                       '(failed cancelled expired)))
                        (mevedel-memory-store-release workspace id)))
                  (error (setq recovery-error (error-message-string err)))))
              (when (plist-get state :publishing)
                (setf (mevedel-workspace-journal-observation workspace) nil))
              (when entry
                (when (plist-get state :publication-finished)
                  (mevedel-journal-cleanup-schedule workspace t))
                ;; A later review of a shared file replaces its older undecided
                ;; dispositions, so they neither wait nor retain history.
                (when accepted
                  (mevedel-memory-pass--supersede workspace accepted))
                (when (eq 'auto (plist-get (plist-get state :telemetry) :mode))
                  (setq applied (mevedel-memory-pass--apply state accepted)))
                (setf (mevedel-workspace-memory-observation workspace) nil))
              (plist-put state :result
                         (append (list :outcome (if entry 'success (plist-get result :outcome))
                                       :id id :entry entry :error (plist-get result :error)
                                       :mode (plist-get (plist-get state :telemetry) :mode)
                                       :stopped (plist-get state :stop-apply)
                                       :recovery-required (or recovery-error (and accepted (not entry)))
                                       :remaining (if entry
                                                      (+ (or (plist-get (plist-get state :selection) :remaining) 0)
                                                         (or (plist-get result :omitted-digests) 0))
                                                    (or (plist-get (plist-get state :selection) :eligible) 0))
                                       :notes-reviewed (length (plist-get result :notes))
                                       :notes-remaining (+ (or (plist-get (plist-get state :notes) :remaining) 0)
                                                           (or (plist-get result :omitted-notes) 0))
                                       :notes-blocked (or (plist-get (plist-get state :notes) :blocked-count) 0))
                                 applied usage))
              (when (and entry
                         (not (plist-get state :stop-apply))
                         (string-empty-p (plist-get entry :focus))
                         (eq mevedel-memory-consolidation-mode 'auto)
                         (or (> (plist-get (plist-get state :result) :remaining) 0)
                             (> (plist-get (plist-get state :result) :notes-remaining) 0)))
                ;; Offer, do not recurse: the next maintenance/activity boundary
                ;; rechecks the rolling cap and readiness under target ownership.
                (setf (mevedel-workspace-memory-schedule workspace)
                      (list :after 0 :catch-up t)))
              (when (and (not entry) (plist-get state :automatic))
                ;; A failed catch-up attempt ends the chain.  A later ordinary
                ;; opportunity must pass the normal successful-review clock.
                (setf (mevedel-workspace-memory-schedule workspace)
                      (plist-put (mevedel-workspace-memory-schedule workspace) :catch-up nil)))
              (when (eq state (mevedel-memory-pass-running workspace))
                (remhash (plist-get claim :directory) mevedel-memory-pass--running))
              (apply #'mevedel-telemetry-record-workspace workspace
                     (cond (entry 'memory-consolidation-completed)
                           ((eq status 'cancelled) 'memory-consolidation-killed)
                           (t 'memory-consolidation-failed))
                     (append (plist-get state :telemetry) usage
                             (list :model model :outcome (plist-get (plist-get state :result) :outcome)
                                   :duration-ms (round (* 1000 (max 0 (- (float-time) (or (plist-get state :started-at) (float-time))))))
                                   :failure-class (and (not entry) (not (eq status 'cancelled)) failure-class)
                                   :reviewed-count (length (plist-get entry :digests))
                                   :covered-count (if (eq 'general (plist-get (plist-get state :telemetry) :scope))
                                                      (length (plist-get entry :digests)) 0)
                                   :proposed-count (length (plist-get entry :proposals))
                                   :updated-file-count (plist-get applied :updated-files)
                                   :remaining-count (plist-get (plist-get state :result) :remaining))))
              (when-let* ((callback (plist-get state :callback)))
                (funcall callback (plist-get state :result))))))

(defun mevedel-memory-pass-cancel (workspace)
  "Cancel only this client's running consolidation for WORKSPACE.
Fence its outcome before aborting transport, making late callbacks harmless.
Also stop an explicit drain between batches.  Return non-nil if either existed."
  (let* ((key (mevedel-memory-store--claim-directory workspace))
         (drain (gethash key mevedel-memory-pass--drains))
         (state (mevedel-memory-pass-running workspace)))
    (when drain
      (remhash key mevedel-memory-pass--drains)
      (when (timerp (plist-get drain :timer)) (cancel-timer (plist-get drain :timer)))
      (plist-put drain :status 'cancelled))
    (when state
      (plist-put state :stop-apply t)
      (unwind-protect
          (mevedel-memory-pass--finish state '(:outcome aborted))
        (when-let* ((cancel (plist-get (plist-get state :request) :cancel))) (funcall cancel))))
    (when drain
      (funcall (plist-get drain :callback) (list :outcome 'aborted :batch (plist-get drain :batch))))
    (and (or state drain) t)))

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
                 (plist-get payload :references) (plist-get payload :notes)))
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
         notes scope)
    (mevedel-memory-store--assert-owned claim)
    (mevedel-memory-decision-recover workspace)
    (mevedel-memory-store-recover workspace)
    (let* ((observed
            (mevedel-memory-store--mutation
             workspace
             (lambda (_mutation)
               (mevedel-journal-cleanup-recover
                (mevedel-journal-store-directory (mevedel-workspace-root workspace)))
               (mevedel-journal-store-entries (mevedel-workspace-root workspace)))))
           (capture
            (lambda ()
              (unless (or notes (not general) (plist-get payload :defer-notes))
                (mevedel-memory-store--assert-owned claim)
                ;; Memory and instructions are captured first: files they
                ;; cite are protected from cleanup.
                (setq scope (mevedel-memory-scope-capture workspace nil (plist-get payload :configuration))
                      notes (or (mevedel-memory-pass--capture-notes
                                 workspace observed (plist-get payload :mode)
                                 (mevedel-memory-pass--cited-texts scope))
                                (list :root nil))))
              (plist-get notes :root)))
           (selection (if automatic
                          (mevedel-memory-pass--automatic-selection workspace observed capture (plist-get payload :catch-up))
                        (mevedel-memory-pass-select observed (not general))))
           (entries (plist-get selection :entries)))
      (when selection (funcall capture))
      (if (and automatic (not selection))
          (list :skipped t :schedule (mevedel-workspace-memory-schedule workspace))
        (when (and (plist-get payload :drain) (not entries) (not (plist-get notes :root)))
          (cl-return-from mevedel-memory-pass--prepare
            (if (plist-get payload :defer-notes) (list :waiting t) (list :empty t))))
        (unless (or entries (plist-get notes :root) (plist-get payload :memory-only))
          (error "No eligible digests or shared notes for consolidation"))
        (mevedel-memory-store--assert-owned claim)
        (let* ((root (plist-get notes :root))
               (scope (or scope (mevedel-memory-scope-capture
                                 workspace nil (plist-get payload :configuration))))
               (scope (if root (plist-put scope :roots (append (plist-get scope :roots) (list root))) scope))
               (prepared (mevedel-memory-store-prepare workspace claim scope entries focus)))
          (list :selection selection :schedule (mevedel-workspace-memory-schedule workspace)
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

(defun mevedel-memory-pass--capture-notes (workspace entries mode cited)
  "Capture WORKSPACE's ready shared files from public ENTRIES under MODE.
CITED are memory and instruction texts whose citations protect files.
First collect undo blobs no retained write needs.  The caller holds the
consolidation claim, which also excludes concurrent application."
  (when-let* ((configured (mevedel-memory-notes-directory workspace))
              ((mevedel-session-control-fs-directory-p configured)))
    ;; An unreadable retained write keeps every blob; collection is retried
    ;; at the next review and before quota refusal.
    (ignore-errors
      (mevedel-memory-notes-collect-blobs
       workspace (mevedel-memory-notes-blob-directory (file-truename configured))))
    (mevedel-memory-notes-capture workspace entries :mode mode :external cited)))

(defun mevedel-memory-pass--prepared (state result focus memory-only)
  "Start STATE's configured review from prepared RESULT, FOCUS and MEMORY-ONLY.
Cancelled or expired storage replies never start a model request."
  (unless (plist-get state :settled)
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
          (if (or (plist-get bundle :empty) (plist-get bundle :waiting)
                  (and (plist-get state :automatic) (plist-get bundle :skipped)))
              (progn
                (mevedel-journal-claim-settle claim 'cancelled "")
                (when (timerp (plist-get state :timer)) (cancel-timer (plist-get state :timer)))
                (plist-put state :timer nil)
                (plist-put state :settled t)
                (plist-put state :skipped t)
                (remhash (plist-get claim :directory) mevedel-memory-pass--running)
                (when (or (plist-get bundle :empty) (plist-get bundle :waiting))
                  (let ((outcome (if (plist-get bundle :empty) 'empty 'waiting)))
                    (plist-put state :result (list :outcome outcome))
                    (funcall (plist-get state :callback) (plist-get state :result)))))
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
            (plist-put state :request
                       (with-current-buffer (plist-get state :origin-buffer)
                         (mevedel-memory-review-request
                          (plist-get prepared :scope) (plist-get prepared :entries)
                          (lambda (reply) (mevedel-memory-pass--finish state reply))
                          :focus focus :memory-only memory-only
                          :rejections (plist-get bundle :rejections)
                          :currentp (lambda ()
                                      (and (not (plist-get state :settled))
                                           (condition-case nil
                                               (progn (mevedel-memory-store--assert-owned claim) t)
                                              (error nil))))))))))
      (error (mevedel-memory-pass--finish
              state (list :outcome 'error :error (error-message-string err)))))))

(cl-defun mevedel-memory-pass-start (workspace callback &key (focus "") memory-only automatic drain)
  "Start one sessionless consolidation in WORKSPACE and return its state.
CALLBACK receives its settled outcome and diagnostics. Automatic admission can
settle the returned state as :skipped without a callback or model request.
Invalid focus signals before starting. Later errors settle through CALLBACK.
FOCUS selects a bounded focused review; MEMORY-ONLY allows an empty batch.
AUTOMATIC uses the general-review time/count gates after fenced recovery.
DRAIN is reserved for the explicit drain owner; an empty general selection
settles with outcome `empty' without starting inference or publishing a review."
  (unless (and (stringp focus) (<= (string-bytes focus) 4096))
    (error "Memory focus exceeds its byte limit"))
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
  (mevedel-workspace-identity-ensure (mevedel-workspace-root workspace))
  (let* ((key (mevedel-memory-store--claim-directory workspace))
         (claim (or (mevedel-journal-claim-acquire key 180)
                    (if automatic (cl-return-from mevedel-memory-pass-start nil)
                      (error "Memory consolidation is busy"))))
         (state (list :workspace workspace :claim claim :callback callback :settled nil
                      :automatic automatic :drain drain :origin-buffer (current-buffer)
                      :started-at (float-time)
                      :telemetry (list :pass-id (plist-get claim :owner)
                                       :attempt-generation (plist-get claim :generation)
                                       :mode mevedel-memory-consolidation-mode :workload 'memory
                                       :scope (if (string-empty-p focus) 'general 'focused)))))
    (puthash key state mevedel-memory-pass--running)
    (unless (or automatic drain)
      (apply #'mevedel-telemetry-record-workspace workspace 'memory-consolidation-fired
             (plist-get state :telemetry)))
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
          (let* ((roots (mevedel-system--memory-roots workspace))
                 (payload (list :claim claim :focus focus :automatic automatic :memory-only memory-only
                                :drain drain
                                :catch-up (plist-get (mevedel-workspace-memory-schedule workspace) :catch-up)
                                :max-batches mevedel-memory-consolidation-max-batches-per-day
                                :min-hours mevedel-memory-consolidation-min-hours
                                :min-digests mevedel-memory-consolidation-min-digests
                                :mode mevedel-memory-consolidation-mode
                                :notes-quiet-hours mevedel-memory-notes-quiet-hours
                                :notes-review-days mevedel-memory-notes-review-days
                                :notes-quota mevedel-memory-notes-undo-quota
                                ;; Active conversations may still be using notes.
                                :defer-notes (mevedel-memory-notes-workspace-busy-p workspace)
                                :client (mevedel-workspace-identity-client)
                                :configuration (list :roots roots :instructions
                                                     (mevedel-system-workspace-config-files workspace))))
                 (ready (lambda (result) (mevedel-memory-pass--prepared state result focus memory-only))))
            (if (and (mevedel-journal-worker-supported-p workspace)
                     (not (cl-some (lambda (root) (file-remote-p (plist-get root :dir))) roots)))
                (plist-put state :worker
                           (mevedel-journal-worker-start workspace 'memory-prepare ready nil payload))
              (funcall ready (list :ok t :prepared (mevedel-memory-pass--prepare workspace payload))))))
      (error (mevedel-memory-pass--finish state (list :outcome 'error :error (error-message-string err)))))
    (unless (plist-get state :skipped) state)))

(provide 'mevedel-memory-pass)
;;; mevedel-memory-pass.el ends here
