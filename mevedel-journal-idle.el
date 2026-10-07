;;; mevedel-journal-idle.el -- Idle journal sealing and maintenance -*- lexical-binding: t -*-

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

(require 'mevedel-journal-worker)
(require 'mevedel-structs)
(require 'mevedel-transport)

;; `mevedel-agents'
(defvar mevedel--agent-invocation)

;; `mevedel-hooks'
(defvar mevedel-session-start-hook)

;; `mevedel-journal-capture'
(declare-function mevedel-journal-capture-seal-and-schedule "mevedel-journal-capture"
                  (session buffer trigger &optional captures))
(defvar mevedel-journal-enabled)
(autoload 'mevedel-journal-capture-seal-and-schedule "mevedel-journal-capture")

;; `mevedel-journal-cleanup'
(declare-function mevedel-journal-cleanup-schedule "mevedel-journal-cleanup" (workspace &optional force))
(autoload 'mevedel-journal-cleanup-schedule "mevedel-journal-cleanup")

;; `mevedel-journal-process'
(declare-function mevedel-journal-process-schedule "mevedel-journal-process" (workspace &optional recover))
(autoload 'mevedel-journal-process-schedule "mevedel-journal-process")

;; `mevedel-memory-pass'
(declare-function mevedel-memory-pass-schedule "mevedel-memory-pass" (workspace))
(autoload 'mevedel-memory-pass-schedule "mevedel-memory-pass")

(defcustom mevedel-journal-seal-idle-minutes 20
  "Minutes a session stays quiet after a completed root turn before sealing.
When no newer root turn completes in that session and no request is running
when this period ends, its completed checkpoints are sealed with the `idle'
trigger and queued for digest generation.  Later turns form a new checkpoint.
Nil seals only at compaction, clear, and session close or exit."
  :type '(choice (number :tag "Minutes") (const :tag "Disabled" nil))
  :group 'mevedel)

(defcustom mevedel-journal-idle-maintenance-minutes 10
  "Minutes between background journal maintenance opportunities.
While a root session is open, each local Linux workspace with a live root
session receives one digest-processing opportunity, a memory consolidation
offer and the hourly-throttled journal and artifact cleanup once Emacs has
been without input for a short while.  Each opportunity generates at most one
digest.  Remote workspaces rely on turn and session-open opportunities.  Nil
disables the timer."
  :type '(choice (number :tag "Minutes") (const :tag "Disabled" nil))
  :group 'mevedel)

(defconst mevedel-journal-idle--quiet-seconds 30
  "Seconds without input before a due maintenance opportunity runs.")

(defvar mevedel-journal-idle--inhibit-scheduling nil
  "Non-nil suppresses idle sealing and maintenance during exit or isolated tests.")

(defvar mevedel-journal-idle--seal-timers (make-hash-table :test #'eq)
  "Armed idle-seal timers keyed by session.")

(defvar mevedel-journal-idle--seal-pending (make-hash-table :test #'eq)
  "Due idle seals waiting for their transport, keyed by session.")

(defvar mevedel-journal-idle--maintenance-timer nil
  "Timer for the next maintenance opportunity, or nil when none is armed.")

(defvar mevedel-journal-idle--quiet-timer nil
  "Idle timer running a due maintenance opportunity, or nil.")


;;
;;; Idle sealing

(defun mevedel-journal-idle--quiet-p (session buffer turn)
  "Return non-nil when BUFFER still roots SESSION, idle since TURN completed."
  (and (buffer-live-p buffer)
       (eq buffer (mevedel-session-root-buffer session))
       (eq session (buffer-local-value 'mevedel--session buffer))
       (null (buffer-local-value 'mevedel--current-request buffer))
       (eql turn (mevedel-session-turn-count session))))

(defun mevedel-journal-idle--cancel-seal (session)
  "Cancel SESSION's armed or due idle seal."
  (when-let* ((timer (gethash session mevedel-journal-idle--seal-timers)))
    (cancel-timer timer)
    (remhash session mevedel-journal-idle--seal-timers))
  (when-let* ((timer (gethash session mevedel-journal-idle--seal-pending)))
    (cancel-timer timer)
    (remhash session mevedel-journal-idle--seal-pending)
    (mevedel-transport-cancel-pending (list 'journal-idle-seal session))))

(defun mevedel-journal-idle--cancel-seal-on-kill ()
  "Buffer-local `kill-buffer-hook' cancelling the current session's idle seal."
  (when-let* ((session (bound-and-true-p mevedel--session)))
    (mevedel-journal-idle--cancel-seal session)))

(defun mevedel-journal-idle--seal-due (session buffer turn)
  "Seal SESSION's checkpoints from BUFFER if it stayed quiet since TURN.
Sealing waits for the workspace transport, like other journal opportunities.
A request still running without a newer completed turn restarts the period,
so a request that later fails does not leave the checkpoint unsealed."
  (when-let* ((timer (gethash session mevedel-journal-idle--seal-timers)))
    (cancel-timer timer)
    (remhash session mevedel-journal-idle--seal-timers))
  (unless mevedel-journal-idle--inhibit-scheduling
    (cond
     ((mevedel-journal-idle--quiet-p session buffer turn)
      (mevedel-transport-schedule-idle
       mevedel-journal-idle--seal-pending session 'journal-idle-seal
       (mevedel-workspace-root (mevedel-session-workspace session))
       (lambda ()
         (when (and (not mevedel-journal-idle--inhibit-scheduling)
                    (mevedel-journal-idle--quiet-p session buffer turn))
           (mevedel-journal-capture-seal-and-schedule session buffer 'idle)))))
     ((and (buffer-live-p buffer)
           (eq buffer (mevedel-session-root-buffer session))
           (eql turn (mevedel-session-turn-count session)))
      (mevedel-journal-idle--start-seal-period session buffer turn)))))

(defun mevedel-journal-idle--start-seal-period (session buffer turn)
  "Arm SESSION's idle-seal timer for BUFFER, quiet since completed TURN."
  (when (and mevedel-journal-seal-idle-minutes
             (bound-and-true-p mevedel-journal-enabled))
    (add-hook 'kill-emacs-hook #'mevedel-journal-idle-stop-all)
    (puthash session
             (run-at-time (* 60 mevedel-journal-seal-idle-minutes) nil
                          #'mevedel-journal-idle--seal-due session buffer turn)
             mevedel-journal-idle--seal-timers)))

;;;###autoload
(defun mevedel-journal-idle-arm-seal (session buffer)
  "Restart SESSION's idle-seal period after BUFFER saved a completed root turn.
The previous period is cancelled, so only the latest completed turn counts.
Also ensure the maintenance timer runs while this session is open."
  (mevedel-journal-idle--cancel-seal session)
  (when (and (not mevedel-journal-idle--inhibit-scheduling)
             (buffer-live-p buffer))
    (mevedel-journal-idle-ensure-maintenance)
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook #'mevedel-journal-idle--cancel-seal-on-kill nil t))
    (mevedel-journal-idle--start-seal-period
     session buffer (mevedel-session-turn-count session))))


;;
;;; Workspace opportunities

(defun mevedel-journal-idle--live-workspaces ()
  "Return the distinct workspaces of live root session buffers."
  (let (workspaces roots)
    (dolist (buffer (buffer-list))
      (let ((session (buffer-local-value 'mevedel--session buffer)))
        (when (and session
                   (eq buffer (mevedel-session-root-buffer session))
                   (not (with-current-buffer buffer
                          mevedel--agent-invocation)))
          (let* ((workspace (mevedel-session-workspace session))
                 (root (and (mevedel-workspace-p workspace)
                            (mevedel-workspace-root workspace))))
            (when (and root (not (member root roots)))
              (push root roots)
              (push workspace workspaces))))))
    (nreverse workspaces)))

(defun mevedel-journal-idle--maintain ()
  "Queue maintenance for every local workspace with a live root session."
  (setq mevedel-journal-idle--quiet-timer nil)
  (unless mevedel-journal-idle--inhibit-scheduling
    (dolist (workspace (mevedel-journal-idle--live-workspaces))
      (when (mevedel-journal-worker-supported-p workspace)
        (condition-case err
            (progn
              (mevedel-journal-process-schedule workspace t)
              (mevedel-memory-pass-schedule workspace)
              (mevedel-journal-cleanup-schedule workspace))
          (error
           (display-warning 'mevedel
                            (format "Journal maintenance failed: %s" (error-message-string err))
                            :warning)))))))

(defun mevedel-journal-idle--maintenance-due ()
  "Run maintenance once Emacs is quiet, then arm the next opportunity.
Stop when no root session remains."
  (when mevedel-journal-idle--maintenance-timer
    (cancel-timer mevedel-journal-idle--maintenance-timer)
    (setq mevedel-journal-idle--maintenance-timer nil))
  (when (and (not mevedel-journal-idle--inhibit-scheduling)
             mevedel-journal-idle-maintenance-minutes
             (mevedel-journal-idle--live-workspaces))
    ;; An idle timer whose threshold Emacs already passed runs at once.
    (unless mevedel-journal-idle--quiet-timer
      (setq mevedel-journal-idle--quiet-timer
            (run-with-idle-timer mevedel-journal-idle--quiet-seconds nil
                                 #'mevedel-journal-idle--maintain)))
    (mevedel-journal-idle-ensure-maintenance)))

(defun mevedel-journal-idle-ensure-maintenance ()
  "Arm the maintenance timer unless it is armed, disabled or inhibited."
  (unless (or mevedel-journal-idle--inhibit-scheduling
              (null mevedel-journal-idle-maintenance-minutes)
              mevedel-journal-idle--maintenance-timer)
    (add-hook 'kill-emacs-hook #'mevedel-journal-idle-stop-all)
    (setq mevedel-journal-idle--maintenance-timer
          (run-at-time (* 60 mevedel-journal-idle-maintenance-minutes) nil
                       #'mevedel-journal-idle--maintenance-due))))

(defun mevedel-journal-idle-session-opened ()
  "Queue journal and memory work for the session opened in the current buffer.
Work sealed while no session was open, such as at exit, is recovered and
processed without waiting for a turn.  Only queues; nothing runs here."
  (when-let* ((session (bound-and-true-p mevedel--session))
              ((not mevedel--agent-invocation))
              ((not mevedel-journal-idle--inhibit-scheduling))
              (workspace (mevedel-session-workspace session))
              ((mevedel-workspace-p workspace)))
    (condition-case err
        (progn
          (mevedel-journal-idle-ensure-maintenance)
          (mevedel-journal-process-schedule workspace t)
          (mevedel-memory-pass-schedule workspace))
      (error
       (display-warning 'mevedel
                        (format "Journal maintenance failed: %s" (error-message-string err))
                        :warning)))))

(defun mevedel-journal-idle-stop-all ()
  "Cancel every idle seal and the maintenance timers."
  (maphash (lambda (_session timer) (cancel-timer timer)) mevedel-journal-idle--seal-timers)
  (clrhash mevedel-journal-idle--seal-timers)
  (mevedel-transport-cancel-idle mevedel-journal-idle--seal-pending 'journal-idle-seal)
  (when mevedel-journal-idle--maintenance-timer
    (cancel-timer mevedel-journal-idle--maintenance-timer)
    (setq mevedel-journal-idle--maintenance-timer nil))
  (when mevedel-journal-idle--quiet-timer
    (cancel-timer mevedel-journal-idle--quiet-timer)
    (setq mevedel-journal-idle--quiet-timer nil)))

(add-hook 'mevedel-session-start-hook #'mevedel-journal-idle-session-opened)

(provide 'mevedel-journal-idle)
;;; mevedel-journal-idle.el ends here
