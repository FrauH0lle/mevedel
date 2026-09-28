;;; mevedel-view-power.el -- Shared view power policy -*- lexical-binding: t -*-

;;; Commentary:

;; A shared, conservative battery cache for animated views.  Watching views
;; receive a callback when their effective power state changes; the caller
;; owns visibility and calls `mevedel-view-power-unwatch' when no longer
;; eligible.  Notifications from an existing display-battery-mode are consumed
;; immediately; without them transitions are normally detected within 60
;; seconds.  This module never enables display-battery-mode.

;;; Code:

(require 'battery)
(require 'mevedel-utilities)
(require 'subr-x)

(defconst mevedel-view-power--interval 60
  "Minimum seconds between fallback queries and maximum cache lifetime.")

(defvar mevedel-view-power--watchers (make-hash-table :test #'eq)
  "Live view buffers mapped to zero-argument refresh callbacks.")
(defvar mevedel-view-power--state 'unknown)
(defvar mevedel-view-power--sample-time nil)
(defvar mevedel-view-power--last-query nil)
(defvar mevedel-view-power--timer nil)

(defun mevedel-view-power--normalize (data)
  "Classify battery.el alist DATA as external, battery, or unknown.
Only a recognized line status with a recognized battery status is
evidence.  In particular, a missing battery and a charging flag by
itself do not establish external power."
  (condition-case nil
      (let* ((line (cdr (assq ?L data)))
             (status (cdr (assq ?B data))))
        (if (not (and (stringp line) (stringp status)
                      (not (member (downcase (string-trim status))
                                   '("" "n/a" "unknown" "absent"
                                     "no battery")))))
            'unknown
          (let ((line (downcase (string-trim line))))
            (cond ((member line '("ac" "ac power" "mains" "on-line" "online"
                                  "connected"
                                  "on-line (usb)" "on-line (dock)"
                                  "on-line (wireless)"))
                   'external)
                  ((member line '("bat" "battery" "off-line" "offline"
                                  "disconnected" "on backup" "backup"
                                  "backup power"))
                   'battery)
                  (t 'unknown)))))
    (error 'unknown)))

(defun mevedel-view-power--current-state (&optional now)
  "Return fresh cached power state at NOW, otherwise unknown."
  (if (and mevedel-view-power--sample-time
           (let ((age (- (or now (float-time))
                         mevedel-view-power--sample-time)))
             (and (>= age 0) (< age mevedel-view-power--interval))))
      mevedel-view-power--state
    'unknown))

(defun mevedel-view-power-framerate (normal battery policy animate)
  "Return effective integer FPS given NORMAL, BATTERY, POLICY and ANIMATE.
NORMAL is an integer in 1..60, BATTERY in 0..60; POLICY is one of
`auto', `full' or `save'.  This function only reads the shared cache;
it never calls a battery backend or starts a timer.  Unknown or stale
automatic power state applies the conservative battery ceiling."
  (if (not animate)
      0
    (if (or (eq policy 'save)
            (and (eq policy 'auto)
                 (not (eq (mevedel-view-power--current-state) 'external))))
        (min normal battery)
      normal)))

(defun mevedel-view-power--notify ()
  "Refresh live watchers after a change to the effective power state."
  (let (callbacks)
    (maphash (lambda (buffer callback)
               (if (buffer-live-p buffer)
                   (push (cons buffer callback) callbacks)
                 (remhash buffer mevedel-view-power--watchers)))
             mevedel-view-power--watchers)
    (dolist (entry callbacks)
      (when (and (buffer-live-p (car entry))
                 (eq (gethash (car entry) mevedel-view-power--watchers)
                     (cdr entry)))
        (condition-case nil
            (with-current-buffer (car entry) (funcall (cdr entry)))
          (error nil))))))

(defun mevedel-view-power--on-window-state-change (_frame)
  "Recheck watchers when window deletion can hide their last indicator.
Buffer-local window-change hooks do not run for a deleted window.  Reuse
the views' own scheduling callbacks so visibility policy stays in the view."
  (mevedel-view-power--notify))

(defun mevedel-view-power--sample (data)
  "Record battery.el DATA and notify watchers on effective state changes."
  (let ((old (mevedel-view-power--current-state)))
    (setq mevedel-view-power--state (mevedel-view-power--normalize data)
          mevedel-view-power--sample-time (float-time))
    (unless (eq old mevedel-view-power--state)
      (mevedel-view-power--notify)))
  (mevedel-view-power--schedule))

(defun mevedel-view-power--pending-timer-p (timer)
  "Return whether TIMER is queued, including on TRAMP's hidden outer list."
  (and (timerp timer)
       (or (mevedel--timer-pending-p timer)
           (memq timer (default-toplevel-value 'timer-list)))))

(defun mevedel-view-power--cancel-timer (timer)
  "Cancel TIMER on the current and top-level lists, even inside TRAMP.
`cancel-timer' only edits the current dynamic binding of `timer-list'.
The shared UI-host poll is always installed on the top-level list."
  (when (timerp timer)
    (cancel-timer timer)
    (let ((outer (default-toplevel-value 'timer-list)))
      (when (memq timer outer)
        (set-default-toplevel-value 'timer-list (delq timer outer))))))

(defun mevedel-view-power--schedule ()
  "Keep one deferred fallback check only while watchers remain."
  (when mevedel-view-power--timer
    (mevedel-view-power--cancel-timer mevedel-view-power--timer)
    (setq mevedel-view-power--timer nil))
  (when (> (hash-table-count mevedel-view-power--watchers) 0)
    (let* ((now (float-time))
           (fresh-until (and mevedel-view-power--sample-time
                             (<= mevedel-view-power--sample-time now)
                             (+ mevedel-view-power--sample-time
                                mevedel-view-power--interval)))
           (query-after (and mevedel-view-power--last-query
                             (<= mevedel-view-power--last-query now)
                             (+ mevedel-view-power--last-query
                                mevedel-view-power--interval)))
           (due (max (or fresh-until now) (or query-after now)))
           (timer (timer-create))
           (timer-list (default-toplevel-value 'timer-list)))
      ;; `run-at-time' on TRAMP's temporary timer list would be discarded on
      ;; return.  Activate on a copy of the outer list, then install its new
      ;; sorted head in that binding even when the current list is suspended.
      (timer-set-time timer (time-add nil (max 0 (- due now))))
      (timer-set-function timer #'mevedel-view-power--poll (list timer))
      (timer-activate timer)
      (set-default-toplevel-value 'timer-list timer-list)
      (setq mevedel-view-power--timer timer))))

(defun mevedel-view-power--poll (timer)
  "Check the local battery backend only if TIMER still owns the poll."
  (when (eq timer mevedel-view-power--timer)
    (setq mevedel-view-power--timer nil)
    (when (> (hash-table-count mevedel-view-power--watchers) 0)
      (let* ((now (float-time))
             (old (mevedel-view-power--current-state now))
             (previous mevedel-view-power--state))
        (when (or (null mevedel-view-power--last-query)
                  (< now mevedel-view-power--last-query)
                  (>= (- now mevedel-view-power--last-query)
                      mevedel-view-power--interval))
          (setq mevedel-view-power--last-query now)
          (let ((data (condition-case nil
                          (let ((default-directory temporary-file-directory))
                            (and battery-status-function
                                 (funcall battery-status-function)))
                        (error nil))))
            (setq mevedel-view-power--state (mevedel-view-power--normalize data)
                  mevedel-view-power--sample-time (float-time))))
        ;; A previously confirmed external sample has just expired.  The
        ;; effective policy is already conservative, but its view timer may
        ;; still be running at the old 60 Hz until notified.  Failed polls must
        ;; rearm that timer even though unknown == unknown at this instant.
        (unless (and (eq old (mevedel-view-power--current-state))
                     (not (and (eq previous 'external)
                               (eq (mevedel-view-power--current-state)
                                   'unknown))))
          (mevedel-view-power--notify)))
      (mevedel-view-power--schedule))))

(defun mevedel-view-power--on-kill ()
  "Remove the current buffer's power subscription before it dies."
  (mevedel-view-power-unwatch (current-buffer)))

(defun mevedel-view-power-watch (view callback)
  "Watch live buffer VIEW for power changes, invoking zero-argument CALLBACK.
Call only while VIEW is visibly animating under automatic policy.  The
callback runs in VIEW's buffer and should reschedule its animation timer
using `mevedel-view-power-framerate'.  This function never queries the
battery backend synchronously; stale cache stays conservative until the
deferred, shared poll.  Repeated calls replace the callback, not the timer."
  (when (buffer-live-p view)
    (puthash view callback mevedel-view-power--watchers)
    (with-current-buffer view
      (add-hook 'kill-buffer-hook #'mevedel-view-power--on-kill nil t))
    (add-hook 'battery-update-functions #'mevedel-view-power--sample)
    (add-hook 'window-state-change-functions
              #'mevedel-view-power--on-window-state-change)
    ;; The top-level poll remains pending even if TRAMP hides the current list.
    (unless (mevedel-view-power--pending-timer-p mevedel-view-power--timer)
      (mevedel-view-power--schedule))))

(defun mevedel-view-power-unwatch (view)
  "Stop watching VIEW; release the shared hook and timer after the last view."
  (remhash view mevedel-view-power--watchers)
  (when (buffer-live-p view)
    (with-current-buffer view
      (remove-hook 'kill-buffer-hook #'mevedel-view-power--on-kill t)))
  (when (zerop (hash-table-count mevedel-view-power--watchers))
    (remove-hook 'battery-update-functions #'mevedel-view-power--sample)
    (remove-hook 'window-state-change-functions
                 #'mevedel-view-power--on-window-state-change)
    (when mevedel-view-power--timer
      (mevedel-view-power--cancel-timer mevedel-view-power--timer)
      (setq mevedel-view-power--timer nil))))

(provide 'mevedel-view-power)
;;; mevedel-view-power.el ends here
