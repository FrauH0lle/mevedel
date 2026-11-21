;;; niwa-session-collection.el --- Incremental publication cleanup -*- lexical-binding: t -*-

;;; Commentary:

;; Session startup and publication only schedule reclamation.  Immutable history
;; is scanned in idle slices; new heads join the scan and retained-file marking.
;; Publication owns retention and the final mutation authority.

;;; Code:

(require 'cl-lib)
(require 'niwa-session-publication)
(require 'niwa-journal-worker)
(require 'niwa-structs)
(require 'niwa-telemetry)
(require 'niwa-transport)

;; `niwa-telemetry'
;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-warning "niwa-utilities" (scope type message &optional level buffer-name))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")

;; `niwa-structs'
(defvar niwa--session)

(defvar niwa-session-collection--jobs (make-hash-table :test #'eq)
  "Session to its coalesced collection job and idle timer.")

(defvar niwa-session-collection--slice-seconds 0.05
  "Maximum time before yielding between generation reads.")

(defun niwa-session-collection-cancel (session)
  "Cancel SESSION's pending collection and release its scan state."
  (when-let* ((job (gethash session niwa-session-collection--jobs)))
    (when-let* ((timer (plist-get job :timer))) (cancel-timer timer))
    (remhash session niwa-session-collection--jobs)
    (when-let* ((worker (plist-get job :worker)))
      (niwa-journal-worker-cancel worker))
    (when (buffer-live-p (plist-get job :buffer))
      (with-current-buffer (plist-get job :buffer)
        (remove-hook 'kill-buffer-hook #'niwa-session-collection--on-kill t)))))

(defun niwa-session-collection--finish (session job outcome)
  "End SESSION's collection JOB, recording OUTCOME with what it reclaimed.
OUTCOME is `completed', `proof-failed' or `failed'.  Collection used to end
silently, so a session that never reclaimed anything looked no different
from one that did."
  (let ((plan (plist-get job :plan)))
    (niwa-telemetry-record
     session 'publication-collection
     :outcome (if (and (eq outcome 'completed) (plist-get plan :proof-failed))
                  'proof-failed
                outcome)
     :candidate-count (length (plist-get plan :candidates))
     :retained-count (plist-get plan :retained-count)
     :deleted-file-count (or (plist-get plan :deleted-files) 0)
     :deleted-directory-count (or (plist-get plan :deleted-directories) 0)
     :duration-ms (round (* 1000 (- (float-time)
                                    (or (plist-get job :started-at) (float-time)))))))
  (niwa-session-collection-cancel session))

(defun niwa-session-collection--on-kill ()
  "Cancel the current root buffer's publication collection."
  (when niwa--session (niwa-session-collection-cancel niwa--session)))

(defconst niwa-session-collection--slice-interval 1.0
  "Idle seconds between collection slices.
Collection coalesces published heads.  At 0.2 s, a job deleting a few
old files took about fifty slices over fifteen seconds, three or four
wakeups a second; every wakeup repaints a pgtk frame, and old generations
are not urgent.")

(defconst niwa-session-collection--max-wait 8.0
  "Most seconds a collection blocked by other work waits before retrying.")

(defun niwa-session-collection--arm (session job &optional wait)
  "Schedule the next idle slice for SESSION and JOB after WAIT idle seconds.
WAIT defaults to `niwa-session-collection--slice-interval'.  An idle
timer armed while the editor is already idle fires at once unless its
time includes the idleness so far, and after input that sum must elapse
again: one armed during a long absence then waited as long again once
the user returned.  Already idle, WAIT therefore runs on an ordinary
timer, and the slice falls back to an idle timer if input interrupted it."
  (let ((wait (or wait niwa-session-collection--slice-interval)))
    (when-let* ((timer (plist-get job :timer))) (cancel-timer timer))
    (setf (plist-get job :timer)
          (if (current-idle-time)
              (run-with-timer wait nil #'niwa-session-collection--waited
                              session job wait)
            (run-with-idle-timer wait nil #'niwa-session-collection--step
                                 session job)))))

(defun niwa-session-collection--waited (session job wait)
  "Step SESSION's JOB if the editor stayed idle for WAIT seconds.
Otherwise wait for WAIT seconds of the current or next idle period."
  ;; A transport section can restore a timer cancelled while suspended.
  (when (eq job (gethash session niwa-session-collection--jobs))
    (if (>= (if-let* ((idle (current-idle-time))) (float-time idle) 0) wait)
        (niwa-session-collection--step session job)
      (setf (plist-get job :timer)
            (run-with-idle-timer wait nil #'niwa-session-collection--step
                                 session job)))))

(defun niwa-session-collection--retry (session job)
  "Retry JOB for SESSION later, waiting twice as long as last time.
Publication can stay busy for a whole request, or indefinitely while it
awaits recovery; retrying every 0.2 s woke the editor five times a second
throughout, and every wakeup repaints a pgtk frame."
  (let ((wait (min niwa-session-collection--max-wait
                   (if-let* ((last (plist-get job :wait)))
                       (* 2 last)
                     niwa-session-collection--slice-interval))))
    (setf (plist-get job :wait) wait)
    (niwa-session-collection--arm session job wait)))

(defun niwa-session-collection-schedule (session)
  "Schedule bounded publication cleanup for SESSION without doing target I/O."
  (when (and (niwa-session-codec-portable-authority-p session)
             (niwa-session-save-path session)
             (buffer-live-p (niwa-session-root-buffer session))
             (not (gethash session niwa-session-collection--jobs)))
    (let ((job (list :buffer (niwa-session-root-buffer session)
                     :head nil :names nil :remaining nil
                     :summaries (make-hash-table :test #'equal)
                     :plan nil :timer nil :wait nil :started-at (float-time))))
      (puthash session job niwa-session-collection--jobs)
      (with-current-buffer (plist-get job :buffer)
        (add-hook 'kill-buffer-hook #'niwa-session-collection--on-kill nil t))
      (niwa-session-collection--arm session job))))

(defun niwa-session-collection--observed (session job directory head generations result)
  "Accept JOB's immutable observations for HEAD and GENERATIONS from RESULT.
Cache observations only; the editor still checks authority and pins at deletion."
  (when niwa-journal-worker--stopping
    (niwa-session-collection-cancel session))
  (when (eq job (gethash session niwa-session-collection--jobs))
    (plist-put job :worker nil)
    (condition-case err
        (let ((observations (plist-get result :observations))
              (current-directory (niwa-session-save-path session)))
          (unless (equal directory current-directory)
            (error "Collection source directory changed"))
          (when (plist-get result :error) (error "%s" (plist-get result :error)))
          (unless (equal (mapcar #'car observations)
                         (mapcar (lambda (generation) (plist-get generation :head)) generations))
            (error "Collection observations name another generation set"))
          (dolist (observation observations)
            (let ((key (niwa-session-publication--publication-path directory (car observation))))
              (when (nth 1 observation)
                (puthash key (nth 1 observation) niwa-session-publication--generation-cache))
              (when (nth 2 observation)
                (puthash key (nth 2 observation) niwa-session-publication--facts-cache))))
          (plist-put job :worker-scanned head)
          (niwa-session-collection--arm session job))
      (error
       (niwa-session-collection-cancel session)
       (niwa-diagnostic-warning (niwa-diagnostic-scope session) 'niwa
                        (format "Could not prepare publication collection: %s" (error-message-string err))
                        :warning)))))

(defun niwa-session-collection--resumed (session job error)
  "Continue JOB for SESSION after a deleting program, reporting ERROR."
  (when (eq job (gethash session niwa-session-collection--jobs))
    (plist-put job :pending nil)
    (if (not error)
        (niwa-session-collection--arm session job)
      (niwa-session-collection--finish session job 'failed)
      (niwa-diagnostic-warning (niwa-diagnostic-scope session) 'niwa
                       (format "Could not collect published generations: %s"
                               (error-message-string error)) :warning))))

(defun niwa-session-collection--step (session job)
  "Advance JOB by a bounded slice, preserving SESSION's live ownership."
  (when (eq job (gethash session niwa-session-collection--jobs))
    (when-let* ((timer (plist-get job :timer))) (cancel-timer timer))
    (setf (plist-get job :timer) nil)
    (condition-case err
        (let ((buffer (plist-get job :buffer))
              (directory (niwa-session-save-path session)))
          (cond
           ((or (not (buffer-live-p buffer))
                (not (eq buffer (niwa-session-root-buffer session)))
                (not (plist-get (niwa-session-publication session) :head))
                (not (eq 'owned (plist-get (niwa-session-lease session) :state))))
            (niwa-session-collection-cancel session))
           ((or (input-pending-p)
                (active-minibuffer-window)
                (niwa-transport-busy-p directory)
                (niwa-session-pending-publication session)
                (niwa-session-publication-uncommitted-batches session)
                (niwa-session-publication-queue session)
                (niwa-session-publication-active-p session))
            (niwa-session-collection--retry session job))
           ((or (plist-get job :worker) (plist-get job :pending)) nil)
           (t
            (setf (plist-get job :wait) nil)
            (niwa-transport-with-exclusive-connection
             (niwa-session-durability-with-transaction
              ;; Ownership is proved on the target where collection deletes;
              ;; reading immutable generations needs only the local lease.
              (let ((head (plist-get (niwa-session-publication session) :head))
                    (deadline (+ (float-time) niwa-session-collection--slice-seconds))
                    (count 0))
                ;; Generations are immutable, so a moved head only adds
                ;; generations to summarize.  A plan follows the head itself.
                (unless (or (equal head (plist-get job :head)) (plist-get job :plan))
                  (let ((names (niwa-session-publication--generation-names directory))
                        (summaries (plist-get job :summaries)))
                    (setf (plist-get job :names) names
                          (plist-get job :remaining)
                          (seq-remove (lambda (generation)
                                        (gethash (plist-get generation :head) summaries))
                                      names))))
                (setf (plist-get job :head) head)
                (when (and (not (equal head (plist-get job :worker-scanned)))
                           (niwa-session-workspace session)
                           (niwa-journal-worker-local-p (niwa-session-workspace session) (list directory)))
                  (let ((cold
                         (seq-filter
                          (lambda (generation)
                            (let ((key (niwa-session-publication--publication-path directory (plist-get generation :head))))
                              (not (and (gethash key niwa-session-publication--generation-cache)
                                        (gethash key niwa-session-publication--facts-cache)))))
                          (plist-get job :remaining))))
                    ;; Check eligibility once per head, even when too few cold
                    ;; generations need a worker.  Do not rescan the whole
                    ;; remaining history on every bounded slice.
                    (plist-put job :worker-scanned head)
                    (when (> (length cold) 16)
                      (plist-put job :worker
                                 (niwa-journal-worker-start
                                  (niwa-session-workspace session) 'generation-observations
                                  (list :directory directory :generations cold)
                                  (lambda (result) (niwa-session-collection--observed session job directory head cold result)))))))
                (while (and (not (plist-get job :worker))
                            (plist-get job :remaining)
                            (< count 8)
                            (or (zerop count) (< (float-time) deadline))
                            (not (input-pending-p)))
                  (let ((summary (niwa-session-publication-generation-summary
                                  directory (car (plist-get job :remaining)) t)))
                    (puthash (plist-get summary :head) summary (plist-get job :summaries)))
                  (setf (plist-get job :remaining) (cdr (plist-get job :remaining)))
                  (setq count (1+ count)))
                (if (or (plist-get job :remaining) (> count 0) (input-pending-p))
                    (unless (plist-get job :worker)
                      (niwa-session-collection--arm session job))
                  (unless (plist-get job :plan)
                    (setf (plist-get job :plan)
                          (niwa-session-publication-collection-plan
                           session
                           (let ((summaries (plist-get job :summaries)))
                             (delq nil (mapcar (lambda (generation)
                                                 (gethash (plist-get generation :head)
                                                          summaries))
                                               (plist-get job :names)))))))
                  (let ((count 0) failure)
                    ;; Marking and directory enumeration are often cheaper
                    ;; than reading sidecars.  Give them the same bounded
                    ;; slice, rather than waiting a second per tiny step.
                    (while (and (eq job (gethash session niwa-session-collection--jobs))
                                (not (plist-get job :pending))
                                (< count 8)
                                (or (zerop count) (< (float-time) deadline))
                                (not (input-pending-p)))
                      (cl-incf count)
                      (pcase (niwa-session-publication-collect-step
                            session (plist-get job :plan)
                            (lambda (error)
                              ;; A background program reports from a sentinel,
                              ;; perhaps inside another remote command.
                              (if (plist-get job :pending)
                                  (niwa-transport-run-at-time
                                   0 #'niwa-session-collection--resumed session job error)
                                (setq failure error))))
                      ('pending (plist-put job :pending t))
                      ('nil (niwa-session-collection--finish session job 'completed))
                        (_ (when failure (signal (car failure) (cdr failure))))))
                    (when (and (eq job (gethash session niwa-session-collection--jobs))
                               (not (plist-get job :pending)))
                      (niwa-session-collection--arm session job))))))))))

      (error
       (niwa-session-collection--finish session job 'failed)
       (niwa-diagnostic-warning (niwa-diagnostic-scope session) 'niwa
                        (format "Could not collect published generations: %s"
                                (error-message-string err)) :warning)))))

(provide 'niwa-session-collection)
;;; niwa-session-collection.el ends here
