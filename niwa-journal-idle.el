;;; niwa-journal-idle.el -- Idle journal sealing and maintenance -*- lexical-binding: t -*-

;;; Commentary:

;; Keeps journal and memory maintenance automatic while sessions stay open.
;; A completed, saved root turn arms a one-shot timer for its session.  When
;; no newer root turn completes and no request runs before it fires, the
;; session's ready checkpoints are sealed with the `idle' trigger and a digest
;; opportunity is queued, so long-running sessions produce digests without
;; waiting for compaction, clear or close.  Opening a session queues
;; recovery, digest processing and a consolidation offer, so work sealed at
;; exit does not wait for the next turn.  While a root session is live, one
;; maintenance timer gives each local workspace a processing opportunity, a
;; consolidation offer and the hourly-throttled journal and artifact cleanup
;; whenever Emacs has been quiet briefly.  Remote workspaces rely on turn and
;; session-open opportunities.  Everything here only queues: the existing
;; schedulers own transport idleness, worker children, claims and throttles.

;;; Code:

(require 'niwa-journal-worker)
(require 'niwa-structs)
(require 'niwa-transport)

;; `niwa-agents'
(defvar niwa--agent-invocation)

;; `niwa-hooks'
(defvar niwa-session-start-hook)

;; `niwa-journal-capture'
(declare-function niwa-journal-capture-seal-and-schedule "niwa-journal-capture"
                  (session buffer trigger &optional captures))
(defvar niwa-journal-enabled)
(autoload 'niwa-journal-capture-seal-and-schedule "niwa-journal-capture")

;; `niwa-journal-cleanup'
(declare-function niwa-journal-cleanup-schedule "niwa-journal-cleanup" (workspace &optional force))
(autoload 'niwa-journal-cleanup-schedule "niwa-journal-cleanup")

;; `niwa-journal-process'
(declare-function niwa-journal-process-schedule "niwa-journal-process" (workspace &optional recover))
(autoload 'niwa-journal-process-schedule "niwa-journal-process")

;; `niwa-memory-pass'
(declare-function niwa-memory-pass-schedule "niwa-memory-pass" (workspace))
(autoload 'niwa-memory-pass-schedule "niwa-memory-pass")

;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-warning "niwa-utilities" (scope type message &optional level buffer-name))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")

(defcustom niwa-journal-seal-idle-minutes 20
  "Minutes a session stays quiet after a completed root turn before sealing.
When no newer root turn completes in that session and no request is running
when this period ends, its completed checkpoints are sealed with the `idle'
trigger and queued for digest generation.  Later turns form a new checkpoint.
Nil seals only at compaction, clear, and session close or exit."
  :type '(choice (number :tag "Minutes") (const :tag "Disabled" nil))
  :group 'niwa)

(defcustom niwa-journal-idle-maintenance-minutes 10
  "Minutes between background journal maintenance opportunities.
While a root session is open, each local Linux workspace with a live root
session receives one digest-processing opportunity, a memory consolidation
offer and the hourly-throttled journal and artifact cleanup once Emacs has
been without input for a short while.  Each opportunity generates at most one
digest.  Remote workspaces rely on turn and session-open opportunities.  Nil
disables the timer."
  :type '(choice (number :tag "Minutes") (const :tag "Disabled" nil))
  :group 'niwa)

(defconst niwa-journal-idle--quiet-seconds 30
  "Seconds without input before a due maintenance opportunity runs.")

(defvar niwa-journal-idle--inhibit-scheduling nil
  "Non-nil suppresses idle sealing and maintenance during exit or isolated tests.")

(defvar niwa-journal-idle--seal-timers (make-hash-table :test #'eq)
  "Armed idle-seal timers keyed by session.")

(defvar niwa-journal-idle--seal-pending (make-hash-table :test #'eq)
  "Due idle seals waiting for their transport, keyed by session.")

(defvar niwa-journal-idle--maintenance-timer nil
  "Timer for the next maintenance opportunity, or nil when none is armed.")

(defvar niwa-journal-idle--quiet-timer nil
  "Idle timer running a due maintenance opportunity, or nil.")


;;
;;; Idle sealing

(defun niwa-journal-idle--quiet-p (session buffer turn)
  "Return non-nil when BUFFER still roots SESSION, idle since TURN completed."
  (and (buffer-live-p buffer)
       (eq buffer (niwa-session-root-buffer session))
       (eq session (buffer-local-value 'niwa--session buffer))
       (null (buffer-local-value 'niwa--current-request buffer))
       (eql turn (niwa-session-turn-count session))))

(defun niwa-journal-idle--cancel-seal (session)
  "Cancel SESSION's armed or due idle seal."
  (when-let* ((timer (gethash session niwa-journal-idle--seal-timers)))
    (cancel-timer timer)
    (remhash session niwa-journal-idle--seal-timers))
  (when-let* ((timer (gethash session niwa-journal-idle--seal-pending)))
    (cancel-timer timer)
    (remhash session niwa-journal-idle--seal-pending)
    (niwa-transport-cancel-pending (list 'journal-idle-seal session))))

(defun niwa-journal-idle--cancel-seal-on-kill ()
  "Buffer-local `kill-buffer-hook' cancelling the current session's idle seal."
  (when-let* ((session (bound-and-true-p niwa--session)))
    (niwa-journal-idle--cancel-seal session)))

(defun niwa-journal-idle--seal-due (session buffer turn)
  "Seal SESSION's checkpoints from BUFFER if it stayed quiet since TURN.
Sealing waits for the workspace transport, like other journal opportunities.
A request still running without a newer completed turn restarts the period,
so a request that later fails does not leave the checkpoint unsealed."
  (when-let* ((timer (gethash session niwa-journal-idle--seal-timers)))
    (cancel-timer timer)
    (remhash session niwa-journal-idle--seal-timers))
  (unless niwa-journal-idle--inhibit-scheduling
    (cond
     ((niwa-journal-idle--quiet-p session buffer turn)
      (niwa-transport-schedule-idle
       niwa-journal-idle--seal-pending session 'journal-idle-seal
       (niwa-workspace-root (niwa-session-workspace session))
       (lambda ()
         (when (and (not niwa-journal-idle--inhibit-scheduling)
                    (niwa-journal-idle--quiet-p session buffer turn))
           (niwa-journal-capture-seal-and-schedule session buffer 'idle)))))
     ((and (buffer-live-p buffer)
           (eq buffer (niwa-session-root-buffer session))
           (eql turn (niwa-session-turn-count session)))
      (niwa-journal-idle--start-seal-period session buffer turn)))))

(defun niwa-journal-idle--start-seal-period (session buffer turn)
  "Arm SESSION's idle-seal timer for BUFFER, quiet since completed TURN."
  (when (and niwa-journal-seal-idle-minutes
             (bound-and-true-p niwa-journal-enabled))
    (add-hook 'kill-emacs-hook #'niwa-journal-idle-stop-all)
    (puthash session
             (run-at-time (* 60 niwa-journal-seal-idle-minutes) nil
                          #'niwa-journal-idle--seal-due session buffer turn)
             niwa-journal-idle--seal-timers)))

;;;###autoload
(defun niwa-journal-idle-arm-seal (session buffer)
  "Restart SESSION's idle-seal period after BUFFER saved a completed root turn.
The previous period is cancelled, so only the latest completed turn counts.
Also ensure the maintenance timer runs while this session is open."
  (niwa-journal-idle--cancel-seal session)
  (when (and (not niwa-journal-idle--inhibit-scheduling)
             (buffer-live-p buffer))
    (niwa-journal-idle-ensure-maintenance)
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook #'niwa-journal-idle--cancel-seal-on-kill nil t))
    (niwa-journal-idle--start-seal-period
     session buffer (niwa-session-turn-count session))))


;;
;;; Workspace opportunities

(defun niwa-journal-idle--live-workspaces ()
  "Return the distinct workspaces of live root session buffers."
  (let (workspaces roots)
    (dolist (buffer (buffer-list))
      (let ((session (buffer-local-value 'niwa--session buffer)))
        (when (and session
                   (eq buffer (niwa-session-root-buffer session))
                   (not (with-current-buffer buffer
                          niwa--agent-invocation)))
          (let* ((workspace (niwa-session-workspace session))
                 (root (and (niwa-workspace-p workspace)
                            (niwa-workspace-root workspace))))
            (when (and root (not (member root roots)))
              (push root roots)
              (push workspace workspaces))))))
    (nreverse workspaces)))

(defun niwa-journal-idle--maintain ()
  "Queue maintenance for every local workspace with a live root session."
  (setq niwa-journal-idle--quiet-timer nil)
  (unless niwa-journal-idle--inhibit-scheduling
    (dolist (workspace (niwa-journal-idle--live-workspaces))
      (when (niwa-journal-worker-local-p workspace)
        (condition-case err
            (progn
              (niwa-journal-process-schedule workspace t)
              (niwa-memory-pass-schedule workspace)
              (niwa-journal-cleanup-schedule workspace))
          (error
           (niwa-diagnostic-warning (niwa-diagnostic-scope workspace) 'niwa
                            (format "Journal maintenance failed: %s" (error-message-string err))
                            :warning)))))))

(defun niwa-journal-idle--maintenance-due ()
  "Run maintenance once Emacs is quiet, then arm the next opportunity.
Stop when no root session remains."
  (when niwa-journal-idle--maintenance-timer
    (cancel-timer niwa-journal-idle--maintenance-timer)
    (setq niwa-journal-idle--maintenance-timer nil))
  (when (and (not niwa-journal-idle--inhibit-scheduling)
             niwa-journal-idle-maintenance-minutes
             (niwa-journal-idle--live-workspaces))
    ;; An idle timer whose threshold Emacs already passed runs at once.
    (unless niwa-journal-idle--quiet-timer
      (setq niwa-journal-idle--quiet-timer
            (run-with-idle-timer niwa-journal-idle--quiet-seconds nil
                                 #'niwa-journal-idle--maintain)))
    (niwa-journal-idle-ensure-maintenance)))

(defun niwa-journal-idle-ensure-maintenance ()
  "Arm the maintenance timer unless it is armed, disabled or inhibited."
  (unless (or niwa-journal-idle--inhibit-scheduling
              (null niwa-journal-idle-maintenance-minutes)
              niwa-journal-idle--maintenance-timer)
    (add-hook 'kill-emacs-hook #'niwa-journal-idle-stop-all)
    (setq niwa-journal-idle--maintenance-timer
          (run-at-time (* 60 niwa-journal-idle-maintenance-minutes) nil
                       #'niwa-journal-idle--maintenance-due))))

(defun niwa-journal-idle-session-opened ()
  "Queue journal and memory work for the session opened in the current buffer.
Work sealed while no session was open, such as at exit, is recovered and
processed without waiting for a turn.  Only queues; nothing runs here."
  (when-let* ((session (bound-and-true-p niwa--session))
              ((not niwa--agent-invocation))
              ((not niwa-journal-idle--inhibit-scheduling))
              (workspace (niwa-session-workspace session))
              ((niwa-workspace-p workspace)))
    (condition-case err
        (progn
          (niwa-journal-idle-ensure-maintenance)
          (niwa-journal-process-schedule workspace t)
          (niwa-memory-pass-schedule workspace))
      (error
       (niwa-diagnostic-warning (niwa-diagnostic-scope workspace) 'niwa
                        (format "Journal maintenance failed: %s" (error-message-string err))
                        :warning)))))

(defun niwa-journal-idle-stop-all ()
  "Cancel every idle seal and the maintenance timers."
  (maphash (lambda (_session timer) (cancel-timer timer)) niwa-journal-idle--seal-timers)
  (clrhash niwa-journal-idle--seal-timers)
  (niwa-transport-cancel-idle niwa-journal-idle--seal-pending 'journal-idle-seal)
  (when niwa-journal-idle--maintenance-timer
    (cancel-timer niwa-journal-idle--maintenance-timer)
    (setq niwa-journal-idle--maintenance-timer nil))
  (when niwa-journal-idle--quiet-timer
    (cancel-timer niwa-journal-idle--quiet-timer)
    (setq niwa-journal-idle--quiet-timer nil)))

(add-hook 'niwa-session-start-hook #'niwa-journal-idle-session-opened)

(provide 'niwa-journal-idle)
;;; niwa-journal-idle.el ends here
