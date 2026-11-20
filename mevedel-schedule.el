;;; mevedel-schedule.el --- Session wake-ups and their scheduler -*- lexical-binding: t -*-

;;; Commentary:

;; A wake-up is a schedule whose target is its own session: a due time and a
;; note the root conversation wrote for its future self.  The session's
;; persisted state is the source of truth for its wake-ups.
;;
;; One Emacs-wide timer is armed for the earliest pending wake-up across the
;; open root buffers that hold one, and re-armed whenever that set changes;
;; nothing polls while idle.  Every decision reads `mevedel-schedule-clock'.
;; A due wake-up becomes one WAKE_UP record in the root mailbox, committed
;; together with its removal from the store, and wakes an idle root through
;; the ordinary background-event path and its holds.  A wake-up past its
;; optional expiry is kept as `expired' instead, without a model turn.

;;; Code:

(require 'cl-lib)
(require 'iso8601)
(require 'mevedel-structs)
(require 'subr-x)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-commit-session
                  "mevedel-agent-control" (session &optional token))
(declare-function mevedel-agent-control-enqueue-wake-up
                  "mevedel-agent-control" (session note meta acknowledge))
(declare-function mevedel-agent-control-notify
                  "mevedel-agent-control" (session key function))
(autoload 'mevedel-agent-control-commit-session "mevedel-agent-control")
(autoload 'mevedel-agent-control-enqueue-wake-up "mevedel-agent-control")
(autoload 'mevedel-agent-control-notify "mevedel-agent-control")

;; `mevedel-background-list'
(declare-function mevedel-background-list-refresh-session
                  "mevedel-background-list" (session &optional data-buffer))

;; `mevedel-pending-inputs'
(declare-function mevedel-pending-inputs-offer-root-event
                  "mevedel-pending-inputs" (session &optional data-buffer))
(autoload 'mevedel-pending-inputs-offer-root-event "mevedel-pending-inputs")

;; `mevedel-session-persistence'
(defvar mevedel-session--read-only-mode)

;; `mevedel-utilities'
(declare-function mevedel--duration-label "mevedel-utilities" (seconds))
(declare-function mevedel-diagnostic-scope "mevedel-utilities" (context))
(declare-function mevedel-diagnostic-warning "mevedel-utilities"
                  (scope type message &optional level buffer-name))
(autoload 'mevedel--duration-label "mevedel-utilities")
(autoload 'mevedel-diagnostic-scope "mevedel-utilities")
(autoload 'mevedel-diagnostic-warning "mevedel-utilities")

(defconst mevedel-schedule-max-pending 50
  "Most pending wake-ups one session may hold.")

(defconst mevedel-schedule-max-note-length 4000
  "Most characters a wake-up note may contain.")

(defconst mevedel-schedule-overdue-threshold 60
  "Seconds a wake-up may fire late before it is annotated as overdue.")

(defconst mevedel-schedule-retained-expired 20
  "Most expired wake-ups a session keeps on record.")

(defconst mevedel-schedule-retry-delay 60
  "Seconds before retrying a session whose wake-up delivery failed.")

(defvar mevedel-schedule-clock #'float-time
  "Function returning the current time in seconds since the epoch.
Every scheduling decision reads this one clock.")

(defvar mevedel-schedule--buffers nil
  "Live root data buffers whose sessions hold pending wake-ups.")

(defvar mevedel-schedule--timer nil
  "The one timer armed for the earliest pending wake-up, or nil.")

(defvar-local mevedel-schedule--retry-at nil
  "Clock seconds before which this root buffer's wake-ups are not retried.")


;;
;;; Timing

(defun mevedel-schedule-now ()
  "Return the scheduler clock's current whole seconds."
  (truncate (funcall mevedel-schedule-clock)))

(defun mevedel-schedule-format-time (seconds)
  "Return SECONDS as an ISO 8601 local time with its UTC offset."
  (format-time-string "%FT%T%:z" seconds))

(defun mevedel-schedule--parse-duration (text)
  "Return the seconds in duration TEXT such as \"1h30m\", or nil."
  (let ((case-fold-search nil)
        (start 0)
        (total 0))
    (when (string-match-p "\\`\\(?:in +\\)?\\(?:[0-9]+ *[dhms] *\\)+\\'" text)
      (while (string-match "\\([0-9]+\\) *\\([dhms]\\)" text start)
        (cl-incf total (* (string-to-number (match-string 1 text))
                          (pcase (match-string 2 text)
                            ("d" 86400) ("h" 3600) ("m" 60) (_ 1))))
        (setq start (match-end 0)))
      total)))

(defun mevedel-schedule--parse-absolute (text)
  "Return the seconds of local time TEXT, \"YYYY-MM-DD HH:MM[:SS]\", or nil.
An ISO 8601 offset or Z selects another zone."
  (when-let* ((decoded
               (condition-case nil
                   (iso8601-parse
                    (replace-regexp-in-string
                     "\\`\\([0-9]+-[0-9]+-[0-9]+\\) +" "\\1T" text))
                 (error nil)))
              ((decoded-time-year decoded))
              ((decoded-time-hour decoded)))
    (unless (decoded-time-second decoded)
      (setf (decoded-time-second decoded) 0))
    (truncate (float-time (encode-time decoded)))))

(defun mevedel-schedule-parse-timing (text now)
  "Return the timing TEXT names at clock seconds NOW, or signal `user-error'.
TEXT is a duration such as \"20m\" or \"1h30m\" (units d, h, m, s), or a
local time \"YYYY-MM-DD HH:MM[:SS]\".  A one-shot timing is the plist
\(:kind once :at SECONDS)."
  (unless (and (stringp text) (string-match-p "\\S-" text))
    (user-error "Schedule timing must not be blank"))
  (let* ((text (string-trim text))
         (seconds (mevedel-schedule--parse-duration text))
         (at (cond
              ((null seconds)
               (or (mevedel-schedule--parse-absolute text)
                   (user-error "Unrecognized schedule timing %S; use a duration such as 20m or a local time such as 2026-11-21 10:00"
                               text)))
              ((> seconds 0) (+ now seconds))
              (t (user-error "Schedule duration must be positive: %s" text)))))
    (unless (> at now)
      (user-error "Schedule time %s is not in the future"
                  (mevedel-schedule-format-time at)))
    (list :kind 'once :at at)))

(defun mevedel-schedule-next-run (timing after)
  "Return TIMING's first fire time later than clock seconds AFTER, or nil."
  (pcase (plist-get timing :kind)
    ('once (let ((at (plist-get timing :at))) (and (> at after) at)))
    (kind (error "Unknown schedule timing: %S" kind))))


;;
;;; Store

(defun mevedel-schedule-entry-p (entry)
  "Return non-nil when ENTRY is a well-formed persisted wake-up."
  (and (proper-list-p entry)
       (cl-evenp (length entry))
       (stringp (plist-get entry :id))
       (stringp (plist-get entry :note))
       (eq 'once (plist-get (plist-get entry :timing) :kind))
       (integerp (plist-get (plist-get entry :timing) :at))
       (integerp (plist-get entry :created-at))
       (integerp (plist-get entry :due))
       (let ((expires (plist-get entry :expires-at)))
         (or (null expires) (integerp expires)))
       (pcase (plist-get entry :status)
         ('pending t)
         ('expired (integerp (plist-get entry :expired-at))))))

(defun mevedel-schedule-pending (session)
  "Return SESSION's pending wake-ups, oldest first."
  (cl-remove-if-not (lambda (entry) (eq 'pending (plist-get entry :status)))
                    (mevedel-session-schedules session)))

(defun mevedel-schedule--new-id (session)
  "Return a wake-up identity unused in SESSION."
  (let (id)
    (while (or (null id)
               (cl-find id (mevedel-session-schedules session)
                        :key (lambda (entry) (plist-get entry :id))
                        :test #'equal))
      (setq id (format "w-%06x" (random #x1000000))))
    id))

(defun mevedel-schedule--commit (session previous)
  "Commit SESSION's schedules, restoring PREVIOUS and re-signalling on failure."
  (condition-case err
      (mevedel-agent-control-commit-session session)
    ((error quit)
     (setf (mevedel-session-schedules session) previous)
     (signal (car err) (cdr err)))))

(defun mevedel-schedule--changed (session buffer)
  "Re-arm the scheduler for SESSION's root BUFFER and refresh its cockpit."
  (when (mevedel-schedule-pending session)
    (mevedel-schedule--register buffer))
  (mevedel-schedule--rearm)
  (when (fboundp 'mevedel-background-list-refresh-session)
    (mevedel-background-list-refresh-session session buffer)))

(defun mevedel-schedule-create (session buffer when note &optional expires)
  "Create a one-shot wake-up in SESSION, whose root data buffer is BUFFER.
WHEN is the timing text accepted by `mevedel-schedule-parse-timing'.  NOTE
is the text the woken turn receives.  EXPIRES, timing text of the same
form, is when a still-undelivered wake-up stops mattering.  The wake-up is
committed with the session before this returns it."
  (unless (and (stringp note) (string-match-p "\\S-" note))
    (user-error "Wake-up note must not be blank"))
  (when (> (length note) mevedel-schedule-max-note-length)
    (user-error "Wake-up note exceeds %d characters"
                mevedel-schedule-max-note-length))
  (when (>= (length (mevedel-schedule-pending session))
            mevedel-schedule-max-pending)
    (user-error "This session already has %d pending schedules; cancel one first"
                mevedel-schedule-max-pending))
  (let* ((now (mevedel-schedule-now))
         (timing (mevedel-schedule-parse-timing when now))
         (due (mevedel-schedule-next-run timing now))
         (expires-at (and expires
                          (plist-get (mevedel-schedule-parse-timing expires now) :at)))
         (previous (mevedel-session-schedules session))
         (entry (list :id (mevedel-schedule--new-id session)
                      :note (string-trim note)
                      :timing timing
                      :created-at now
                      :due due
                      :expires-at expires-at
                      :status 'pending)))
    (when (and expires-at (<= expires-at due))
      (user-error "Wake-up expiry %s is not after its time %s"
                  (mevedel-schedule-format-time expires-at)
                  (mevedel-schedule-format-time due)))
    (setf (mevedel-session-schedules session) (append previous (list entry)))
    (mevedel-schedule--commit session previous)
    (mevedel-schedule--changed session buffer)
    entry))

(defun mevedel-schedule-cancel (session buffer id)
  "Remove wake-up ID from SESSION, whose root data buffer is BUFFER.
Return the removed entry.  Expired wake-ups are removed the same way."
  (let* ((previous (mevedel-session-schedules session))
         (entry (cl-find id previous :key (lambda (entry) (plist-get entry :id))
                         :test #'equal)))
    (unless entry
      (user-error "No schedule with id %s" id))
    (setf (mevedel-session-schedules session) (remq entry previous))
    (mevedel-schedule--commit session previous)
    (mevedel-schedule--changed session buffer)
    entry))


;;
;;; Scheduler

(defun mevedel-schedule--register (buffer)
  "Track root data BUFFER until it is killed or holds no pending wake-up."
  (unless (memq buffer mevedel-schedule--buffers)
    (push buffer mevedel-schedule--buffers)
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook #'mevedel-schedule--forget-current nil t))))

(defun mevedel-schedule-restore-session (session buffer)
  "Schedule SESSION's persisted pending wake-ups from its root data BUFFER.
Call only where this client holds the session's mutation authority."
  (when (mevedel-schedule-pending session)
    (mevedel-schedule--register buffer)
    (mevedel-schedule--rearm)))

(defun mevedel-schedule--forget-current ()
  "Stop scheduling the current buffer's wake-ups; they stay persisted."
  (setq mevedel-schedule--buffers (delq (current-buffer) mevedel-schedule--buffers))
  (mevedel-schedule--rearm))

(defun mevedel-schedule--session (buffer)
  "Return root data BUFFER's session while this client may fire for it, or nil.
A buffer following a session whose lease another client holds never fires."
  (and (buffer-live-p buffer)
       (not (with-current-buffer buffer
              (bound-and-true-p mevedel-session--read-only-mode)))
       (buffer-local-value 'mevedel--session buffer)))

(defun mevedel-schedule--earliest (buffer)
  "Return when BUFFER's session next needs the scheduler, or nil."
  (when-let* ((session (mevedel-schedule--session buffer))
              (dues (mapcar (lambda (entry) (plist-get entry :due))
                            (mevedel-schedule-pending session))))
    (max (apply #'min dues)
         (or (buffer-local-value 'mevedel-schedule--retry-at buffer) 0))))

(defun mevedel-schedule--rearm ()
  "Arm one timer for the earliest pending wake-up, or none."
  (when (timerp mevedel-schedule--timer)
    (cancel-timer mevedel-schedule--timer))
  (setq mevedel-schedule--timer nil
        mevedel-schedule--buffers
        (cl-remove-if-not #'mevedel-schedule--earliest mevedel-schedule--buffers))
  (when mevedel-schedule--buffers
    (let ((due (apply #'min (mapcar #'mevedel-schedule--earliest
                                    mevedel-schedule--buffers))))
      (setq mevedel-schedule--timer
            (run-at-time (max 0 (- due (funcall mevedel-schedule-clock)))
                         nil #'mevedel-schedule-run-due)))))

(defun mevedel-schedule-run-due ()
  "Fire every wake-up due by `mevedel-schedule-clock', then re-arm."
  (let ((now (mevedel-schedule-now)))
    (dolist (buffer (copy-sequence mevedel-schedule--buffers))
      (when-let* ((session (mevedel-schedule--session buffer))
                  ((<= (or (buffer-local-value 'mevedel-schedule--retry-at buffer) 0)
                       now)))
        (dolist (entry (mevedel-schedule-pending session))
          (when (<= (plist-get entry :due) now)
            (mevedel-schedule--fire session buffer entry now)))
        (when (fboundp 'mevedel-background-list-refresh-session)
          (mevedel-background-list-refresh-session session buffer)))))
  (mevedel-schedule--rearm))

(defun mevedel-schedule--record-meta (entry now)
  "Return the WAKE_UP record facts for ENTRY firing at clock seconds NOW."
  (let ((late (- now (plist-get entry :due))))
    (append (list :schedule-id (plist-get entry :id)
                  :scheduled-at (mevedel-schedule-format-time (plist-get entry :due))
                  :fired-at (mevedel-schedule-format-time now))
            (when (>= late mevedel-schedule-overdue-threshold)
              (list :overdue-by (mevedel--duration-label late))))))

(defun mevedel-schedule--expire (session entry now)
  "Return SESSION's schedules with ENTRY recorded as expired at NOW."
  (let* ((all (append (remq entry (mevedel-session-schedules session))
                      (list (plist-put (plist-put (copy-sequence entry) :status 'expired)
                                       :expired-at now))))
         (expired (cl-remove-if-not (lambda (other) (eq 'expired (plist-get other :status)))
                                    all))
         ;; The oldest records beyond the retention limit are dropped.
         (dropped (butlast expired mevedel-schedule-retained-expired)))
    (cl-remove-if (lambda (other) (memq other dropped)) all)))

(defun mevedel-schedule--retry-later (session buffer entry err)
  "Restore ENTRY in SESSION after its delivery failed with ERR.
BUFFER waits `mevedel-schedule-retry-delay' before the next attempt."
  (setf (mevedel-session-schedules session)
        (append (cl-remove (plist-get entry :id) (mevedel-session-schedules session)
                           :key (lambda (other) (plist-get other :id)) :test #'equal)
                (list entry)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq mevedel-schedule--retry-at
            (+ (mevedel-schedule-now) mevedel-schedule-retry-delay))))
  (mevedel-diagnostic-warning
   (mevedel-diagnostic-scope session) 'mevedel
   (format "Wake-up %s could not be delivered (%s); retrying in %ds"
           (plist-get entry :id) (error-message-string err)
           mevedel-schedule-retry-delay)
   :warning))

(defun mevedel-schedule--fire (session buffer entry now)
  "Deliver or expire due ENTRY of SESSION at clock seconds NOW.
BUFFER is the root data buffer offered the background-event turn."
  (let ((previous (mevedel-session-schedules session))
        (expires (plist-get entry :expires-at)))
    (condition-case err
        (if (and expires (> now expires))
            (progn
              (setf (mevedel-session-schedules session)
                    (mevedel-schedule--expire session entry now))
              (mevedel-schedule--commit session previous))
          ;; Removal and the record share the mailbox commit, so a firing is
          ;; never both delivered and still pending in the persisted store.
          (setf (mevedel-session-schedules session)
                (if-let* ((next (mevedel-schedule-next-run (plist-get entry :timing) now)))
                    (cl-substitute (plist-put (copy-sequence entry) :due next) entry previous)
                  (remq entry previous)))
          (mevedel-agent-control-enqueue-wake-up
           session (plist-get entry :note) (mevedel-schedule--record-meta entry now)
           (lambda (success failure)
             (if success
                 (mevedel-agent-control-notify
                  session (list 'root-wake buffer)
                  (lambda ()
                    (unless (mevedel-session-publication-stopping-p session)
                      (mevedel-pending-inputs-offer-root-event session buffer))))
               (mevedel-schedule--retry-later session buffer entry failure)
               (mevedel-schedule--rearm)))))
      ((error quit)
       (mevedel-schedule--retry-later session buffer entry err)))))

(provide 'mevedel-schedule)
;;; mevedel-schedule.el ends here
