;;; niwa-view-power.el -- Shared view power policy -*- lexical-binding: t -*-

;;; Commentary:

;; A shared, conservative battery cache for animated views.  Watching views
;; receive a callback when their effective power state changes; the caller
;; owns visibility and calls `niwa-view-power-unwatch' when no longer
;; eligible.  Notifications from an existing display-battery-mode are consumed
;; immediately; without them transitions are normally detected within 60
;; seconds.  This module never enables display-battery-mode.

;;; Code:

(require 'niwa-utilities)
(require 'subr-x)

;; `battery'
(defvar battery-status-function)

(defconst niwa-view-power--interval 60
  "Minimum seconds between fallback queries and maximum cache lifetime.")

(defvar niwa-view-power--watchers (make-hash-table :test #'eq)
  "Live view buffers mapped to zero-argument refresh callbacks.")
(defvar niwa-view-power--state 'unknown)
(defvar niwa-view-power--sample-time nil)
(defvar niwa-view-power--last-query nil)
(defvar niwa-view-power--timer nil)

(defun niwa-view-power--normalize (data)
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

(defun niwa-view-power--current-state (&optional now)
  "Return fresh cached power state at NOW, otherwise unknown."
  (if (and niwa-view-power--sample-time
           (let ((age (- (or now (float-time))
                         niwa-view-power--sample-time)))
             (and (>= age 0) (< age niwa-view-power--interval))))
      niwa-view-power--state
    'unknown))

(defun niwa-view-power-framerate (normal battery policy animate)
  "Return effective integer FPS given NORMAL, BATTERY, POLICY and ANIMATE.
NORMAL is an integer in 1..60, BATTERY in 0..60; POLICY is one of
`auto', `full' or `save'.  This function only reads the shared cache;
it never calls a battery backend or starts a timer.  Unknown or stale
automatic power state applies the conservative battery ceiling."
  (if (not animate)
      0
    (if (or (eq policy 'save)
            (and (eq policy 'auto)
                 (not (eq (niwa-view-power--current-state) 'external))))
        (min normal battery)
      normal)))

(defun niwa-view-power--notify ()
  "Refresh live watchers after a change to the effective power state."
  (let (callbacks)
    (maphash (lambda (buffer callback)
               (if (buffer-live-p buffer)
                   (push (cons buffer callback) callbacks)
                 (remhash buffer niwa-view-power--watchers)))
             niwa-view-power--watchers)
    (dolist (entry callbacks)
      (when (and (buffer-live-p (car entry))
                 (eq (gethash (car entry) niwa-view-power--watchers)
                     (cdr entry)))
        (condition-case nil
            (with-current-buffer (car entry) (funcall (cdr entry)))
          (error nil))))))

(defun niwa-view-power--on-window-state-change (_frame)
  "Recheck watchers when window deletion can hide their last indicator.
Buffer-local window-change hooks do not run for a deleted window.  Reuse
the views' own scheduling callbacks so visibility policy stays in the view."
  (niwa-view-power--notify))

(defun niwa-view-power--sample (data)
  "Record battery.el DATA and notify watchers on effective state changes."
  (let ((old (niwa-view-power--current-state))
        (previous niwa-view-power--state))
    (setq niwa-view-power--state (niwa-view-power--normalize data)
          niwa-view-power--sample-time (float-time))
    ;; The old sample may have expired before this notification.  Its
    ;; effective state is already unknown, but views can still own timers
    ;; scheduled at the previously confirmed external-power cadence.
    (when (or (not (eq old niwa-view-power--state))
              (and (eq previous 'external)
                   (eq niwa-view-power--state 'unknown)))
      (niwa-view-power--notify)))
  (niwa-view-power--schedule))

(defun niwa-view-power--schedule ()
  "Keep one deferred fallback check only while watchers remain."
  (when niwa-view-power--timer
    (niwa--ui-timer-cancel niwa-view-power--timer)
    (setq niwa-view-power--timer nil))
  (when (> (hash-table-count niwa-view-power--watchers) 0)
    (let* ((now (float-time))
           (fresh-until (and niwa-view-power--sample-time
                             (<= niwa-view-power--sample-time now)
                             (+ niwa-view-power--sample-time
                                niwa-view-power--interval)))
           (query-after (and niwa-view-power--last-query
                             (<= niwa-view-power--last-query now)
                             (+ niwa-view-power--last-query
                                niwa-view-power--interval)))
           (due (max (or fresh-until now) (or query-after now)))
           (timer (timer-create)))
      (timer-set-time timer (time-add nil (max 0 (- due now))))
      (timer-set-function timer #'niwa-view-power--poll (list timer))
      (niwa--ui-timer-activate timer nil 'global)
      (setq niwa-view-power--timer timer))))

(defun niwa-view-power--poll (timer)
  "Check the local battery backend only if TIMER still owns the poll."
  (when (eq timer niwa-view-power--timer)
    (setq niwa-view-power--timer nil)
    (when (> (hash-table-count niwa-view-power--watchers) 0)
      (let* ((now (float-time))
             (old (niwa-view-power--current-state now))
             (previous niwa-view-power--state))
        (when (or (null niwa-view-power--last-query)
                  (< now niwa-view-power--last-query)
                  (>= (- now niwa-view-power--last-query)
                      niwa-view-power--interval))
          (setq niwa-view-power--last-query now)
          (let ((data (condition-case nil
                          (let ((default-directory temporary-file-directory))
                            ;; Loading battery.el can initialize D-Bus.  Keep
                            ;; that optional work inside the deferred poll,
                            ;; never in view loading or browser restoration.
                            (require 'battery)
                            (and battery-status-function
                                 (funcall battery-status-function)))
                        (error nil))))
            (setq niwa-view-power--state (niwa-view-power--normalize data)
                  niwa-view-power--sample-time (float-time))))
        ;; A previously confirmed external sample has just expired.  The
        ;; effective policy is already conservative, but its view timer may
        ;; still be running at the old 60 Hz until notified.  Failed polls must
        ;; rearm that timer even though unknown == unknown at this instant.
        (unless (and (eq old (niwa-view-power--current-state))
                     (not (and (eq previous 'external)
                               (eq (niwa-view-power--current-state)
                                   'unknown))))
          (niwa-view-power--notify)))
      (niwa-view-power--schedule))))

(defun niwa-view-power--on-kill ()
  "Remove the current buffer's power subscription before it dies."
  (niwa-view-power-unwatch (current-buffer)))

(defun niwa-view-power-watch (view callback)
  "Watch live buffer VIEW for power changes, invoking zero-argument CALLBACK.
Call only while VIEW is visibly animating under automatic policy.  The
callback runs in VIEW's buffer and should reschedule its animation timer
using `niwa-view-power-framerate'.  This function never queries the
battery backend synchronously; stale cache stays conservative until the
deferred, shared poll.  Repeated calls replace the callback, not the timer."
  (when (buffer-live-p view)
    (puthash view callback niwa-view-power--watchers)
    (with-current-buffer view
      (add-hook 'kill-buffer-hook #'niwa-view-power--on-kill nil t))
    (add-hook 'battery-update-functions #'niwa-view-power--sample)
    (add-hook 'window-state-change-functions
              #'niwa-view-power--on-window-state-change)
    ;; The top-level poll remains pending even if TRAMP hides the current list.
    (unless (niwa--ui-timer-pending-p niwa-view-power--timer)
      (niwa-view-power--schedule))))

(defun niwa-view-power-unwatch (view)
  "Stop watching VIEW; release the shared hook and timer after the last view."
  (remhash view niwa-view-power--watchers)
  (when (buffer-live-p view)
    (with-current-buffer view
      (remove-hook 'kill-buffer-hook #'niwa-view-power--on-kill t)))
  (when (zerop (hash-table-count niwa-view-power--watchers))
    (remove-hook 'battery-update-functions #'niwa-view-power--sample)
    (remove-hook 'window-state-change-functions
                 #'niwa-view-power--on-window-state-change)
    (when niwa-view-power--timer
      (niwa--ui-timer-cancel niwa-view-power--timer)
      (setq niwa-view-power--timer nil))))

(provide 'niwa-view-power)
;;; niwa-view-power.el ends here
