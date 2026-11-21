;;; niwa-schedule.el --- Session wake-ups and their scheduler -*- lexical-binding: t -*-

;;; Commentary:

;; A wake-up is a schedule whose target is its own session: a timing (one-shot,
;; fixed interval or local-time cron) and a note the root conversation wrote
;; for its future self.  The session's persisted state is the source of truth
;; for its wake-ups.  Next-run calculation is pure: it reads only the timing,
;; a reference time and the local time zone.
;;
;; One Emacs-wide timer is armed for the earliest pending wake-up across the
;; open root buffers and closed saved sessions that hold one, and re-armed
;; whenever that set changes; nothing polls while idle.  Every decision
;; reads `niwa-schedule-clock'.
;; A due wake-up becomes one WAKE_UP record in the root mailbox, committed
;; together with its removal from the store, or for a recurring one its next
;; due time computed from the firing time, and wakes an idle root through
;; the ordinary background-event path and its holds.  A wake-up past its
;; optional expiry is kept as `expired' instead, without a model turn.
;;
;; An Emacs-wide index under `niwa-user-dir' names every saved session
;; that holds pending wake-ups, one file per session.  At startup
;; `niwa-schedule-restore' reconciles it against the committed stores and
;; schedules the sessions that are not open; when one is due, it is reopened
;; without a window.  The session lease decides which Emacs fires.
;;
;; The same timer serves workspace schedules, whose store and runs
;; `niwa-workspace-schedule' owns; this module only knows when each
;; workspace next needs it.

;;; Code:

(require 'cl-lib)
(require 'iso8601)
(require 'niwa-notify)
(require 'niwa-structs)
(require 'subr-x)

;; `niwa-agent-control'
(declare-function niwa-agent-control-commit-session
                  "niwa-agent-control" (session &optional token))
(declare-function niwa-agent-control-context-mailbox
                  "niwa-agent-control" (context))
(declare-function niwa-agent-control-enqueue-wake-up
                  "niwa-agent-control" (session note meta acknowledge))
(declare-function niwa-agent-control-notify
                  "niwa-agent-control" (session key function))
(autoload 'niwa-agent-control-commit-session "niwa-agent-control")
(autoload 'niwa-agent-control-context-mailbox "niwa-agent-control")
(autoload 'niwa-agent-control-enqueue-wake-up "niwa-agent-control")
(autoload 'niwa-agent-control-notify "niwa-agent-control")

;; `niwa-background-list'
(declare-function niwa-background-list-refresh-session
                  "niwa-background-list" (session &optional data-buffer))

;; `niwa-models'
(declare-function niwa-model-parse-effort "niwa-models" (value))
(declare-function niwa-model-parse-selector "niwa-models" (value))
(declare-function niwa-model-resolve-workload "niwa-models"
                  (workload &optional explicit-selector explicit-effort))
(autoload 'niwa-model-parse-effort "niwa-models")
(autoload 'niwa-model-parse-selector "niwa-models")
(autoload 'niwa-model-resolve-workload "niwa-models")

;; `niwa-pending-inputs'
(declare-function niwa-pending-inputs-offer-root-event
                  "niwa-pending-inputs" (session &optional data-buffer))
(autoload 'niwa-pending-inputs-offer-root-event "niwa-pending-inputs")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-load-sidecar
                  "niwa-session-persistence" (path &optional scope))
(declare-function niwa-session-persistence-restore
                  "niwa-session-persistence"
                  (session-dir &optional lifecycle-source session-override workspace))
(defvar niwa-session--read-only-mode)
(autoload 'niwa-session-persistence-load-sidecar "niwa-session-persistence")
(autoload 'niwa-session-persistence-restore "niwa-session-persistence")

;; `niwa-session-publication'
(declare-function niwa-session-publication-read
                  "niwa-session-publication" (session-dir &optional head names))
(autoload 'niwa-session-publication-read "niwa-session-publication")

;; `niwa-utilities'
(declare-function niwa--duration-label "niwa-utilities" (seconds))
(declare-function niwa-diagnostic-scope "niwa-utilities" (context))
(declare-function niwa-diagnostic-warning "niwa-utilities"
                  (scope type message &optional level buffer-name))
(autoload 'niwa--duration-label "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")

;; `niwa-workspace-schedule'
(declare-function niwa-workspace-schedule-restore "niwa-workspace-schedule" ())
(declare-function niwa-workspace-schedule-run-due "niwa-workspace-schedule" (root now))
(autoload 'niwa-workspace-schedule-restore "niwa-workspace-schedule")
(autoload 'niwa-workspace-schedule-run-due "niwa-workspace-schedule")

(defconst niwa-schedule-max-pending 50
  "Most pending wake-ups one session may hold.")

(defconst niwa-schedule-max-note-length 4000
  "Most characters a wake-up note may contain.")

(defconst niwa-schedule-overdue-threshold 60
  "Seconds a wake-up may fire late before it is annotated as overdue.")

(defconst niwa-schedule-retained-expired 20
  "Most expired wake-ups a session keeps on record.")

(defconst niwa-schedule-run-history-length 20
  "Most recent runs a wake-up keeps on record.")

(defconst niwa-schedule-max-failures 3
  "Consecutive failed runs after which a recurring schedule pauses.")

(defconst niwa-schedule-retry-delay 60
  "Seconds before retrying a session whose wake-up delivery failed.")

(defconst niwa-schedule-recurring-expiry (* 7 86400)
  "Seconds after creation a recurring wake-up expires by default.")

(defconst niwa-schedule-max-recurring-expiry (* 30 86400)
  "Most seconds after creation a recurring wake-up may run.")

(defvar niwa-schedule-clock #'float-time
  "Function returning the current time in seconds since the epoch.
Every scheduling decision reads this one clock.")

(defvar niwa-schedule--buffers nil
  "Live root data buffers whose sessions hold pending wake-ups.")

(defvar niwa-schedule--closed nil
  "Saved sessions that are not open, as (DIRECTORY . DUE) pairs.
DUE is the clock second their earliest pending wake-up needs attention.")

(defvar niwa-schedule--workspaces nil
  "Workspaces holding active workspace schedules, as (ROOT . DUE) pairs.
DUE is the clock second their earliest schedule needs attention.")

(defvar niwa-schedule--timer nil
  "The one timer armed for the earliest pending wake-up, or nil.")

(defvar-local niwa-schedule--retry-at nil
  "Clock seconds before which this root buffer's wake-ups are not retried.")


;;
;;; Timing

(defun niwa-schedule-now ()
  "Return the scheduler clock's current whole seconds."
  (truncate (funcall niwa-schedule-clock)))

(defun niwa-schedule-format-time (seconds)
  "Return SECONDS as an ISO 8601 local time with its UTC offset."
  (format-time-string "%FT%T%:z" seconds))

(defun niwa-schedule-timing-label (timing)
  "Describe schedule TIMING as a local time, repeating interval, or cron."
  (pcase (plist-get timing :kind)
    ('once (niwa-schedule-format-time (plist-get timing :at)))
    ('interval (concat "Every " (niwa--duration-label (plist-get timing :seconds))))
    ('cron (plist-get timing :expression))))

(defun niwa-schedule--resolve-local (wall)
  "Return the absolute seconds of local wall-clock time WALL.
WALL counts seconds as if local time were UTC.  A time in an hour that
daylight saving repeats resolves to its first occurrence; a time in an hour
it skips resolves to the first valid minute after the gap."
  (let (time)
    (while (not (setq time
                      (cl-loop for offset in (list (car (current-time-zone (- wall 86400)))
                                                   (car (current-time-zone (+ wall 86400))))
                               for candidate = (- wall offset)
                               when (= offset (car (current-time-zone candidate)))
                               minimize candidate)))
      (setq wall (* 60 (1+ (floor wall 60)))))
    time))

(defconst niwa-schedule--cron-fields
  '(("minute" 0 59) ("hour" 0 23) ("day-of-month" 1 31) ("month" 1 12)
    ("day-of-week" 0 7))
  "Name, lowest and highest value of each cron field, in order.")

(defconst niwa-schedule--cron-horizon-days 146097
  "Days in 400 Gregorian years, a whole number of weeks.
Dates and weekdays repeat after it, so a cron matching no day in this span
never fires.  Shorter cycles fail: February 29 on a given weekday can be
40 years apart across a century that is not a leap year.")

(defun niwa-schedule--parse-cron-field (text name low high)
  "Return the values cron field TEXT allows, or t for \"*\".
NAME, LOW and HIGH describe the field for validation and errors."
  (if (equal text "*")
      t
    (let (values)
      (dolist (part (split-string text ","))
        (unless (string-match
                 "\\`\\(?:\\*\\|\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?\\)\\(?:/\\([0-9]+\\)\\)?\\'"
                 part)
          (user-error "Invalid cron %s field %S" name text))
        (let* ((from (if (match-beginning 1) (string-to-number (match-string 1 part)) low))
               (step (if (match-beginning 3) (string-to-number (match-string 3 part)) 1))
               (to (cond ((match-beginning 2) (string-to-number (match-string 2 part)))
                         ((and (match-beginning 1) (not (match-beginning 3))) from)
                         (t high))))
          (unless (and (<= low from to high) (> step 0))
            (user-error "Cron %s field %S is outside %d-%d or has a zero step"
                        name text low high))
          (cl-loop for value from from to to by step do (push value values))))
      ;; Day of week 7 is Sunday, like 0.
      (when (equal name "day-of-week")
        (setq values (cl-substitute 0 7 values)))
      (sort (delete-dups values) #'<))))

(defun niwa-schedule--parse-cron (text)
  "Return 5-field cron expression TEXT as a list of field values.
Signal `user-error' for syntax this scheduler does not support."
  (let ((case-fold-search nil)
        (fields (split-string text)))
    (cond
     ((/= 5 (length fields))
      (user-error "Cron expression %S needs 5 fields" text))
     ((string-match-p "[A-Za-z]\\{3\\}" text)
      (user-error "Cron names such as MON or JAN are not supported; use numbers: %s" text))
     ((string-match-p "[LW?]" text)
      (user-error "Cron L, W and ? are not supported: %s" text)))
    (cl-mapcar (lambda (field spec) (apply #'niwa-schedule--parse-cron-field field spec))
               fields niwa-schedule--cron-fields)))

(defun niwa-schedule--cron-next (text after)
  "Return cron expression TEXT's first local fire time later than AFTER, or nil.
A day matches when its month does and, if both day fields are restricted,
either of them does; otherwise both.  As in cronie and Vixie cron, a day
field starting with * (including */2) is unrestricted."
  (pcase-let* ((`(,minutes ,hours ,days ,months ,weekdays)
                (niwa-schedule--parse-cron text))
               (fields (split-string text))
               (either (not (or (string-prefix-p "*" (nth 2 fields))
                                (string-prefix-p "*" (nth 4 fields)))))
               (allows (lambda (values value) (or (eq values t) (memq value values))))
               (start (* 60 (1+ (floor (+ after (car (current-time-zone after))) 60))))
               (day (* 86400 (floor start 86400))))
    ;; One extra day: the first day's matching times may all precede START.
    (cl-loop repeat (1+ niwa-schedule--cron-horizon-days)
             for date = (decode-time day t)
             when (and (funcall allows months (decoded-time-month date))
                       (funcall (if either (lambda (a b) (or a b)) (lambda (a b) (and a b)))
                                (funcall allows days (decoded-time-day date))
                                (funcall allows weekdays (decoded-time-weekday date))))
             thereis (cl-loop for hour in (if (eq hours t) (number-sequence 0 23) hours)
                              thereis
                              (cl-loop for minute in (if (eq minutes t)
                                                         (number-sequence 0 59)
                                                       minutes)
                                       for wall = (+ day (* 3600 hour) (* 60 minute))
                                       for time = (and (>= wall start)
                                                       (niwa-schedule--resolve-local wall))
                                       thereis (and time (> time after) time)))
             do (cl-incf day 86400))))

(defun niwa-schedule--parse-duration (text)
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

(defun niwa-schedule--parse-absolute (text)
  "Return the seconds of local time TEXT, \"YYYY-MM-DD HH:MM[:SS]\", or nil.
An ISO 8601 offset or Z selects another zone.  An impossible date or time
such as February 31 is nil."
  (when-let* ((decoded
               (condition-case nil
                   (iso8601-parse
                    (replace-regexp-in-string
                     "\\`\\([0-9]+-[0-9]+-[0-9]+\\) +" "\\1T" text))
                 (error nil)))
              ((decoded-time-year decoded))
              ((decoded-time-hour decoded)))
    (let* ((zone (decoded-time-zone decoded))
           (fields (list (or (decoded-time-second decoded) 0) (decoded-time-minute decoded)
                         (decoded-time-hour decoded) (decoded-time-day decoded)
                         (decoded-time-month decoded) (decoded-time-year decoded)))
           (wall (time-convert (encode-time (append fields '(nil nil t))) 'integer)))
      ;; `encode-time' silently normalizes an impossible date to a later one.
      (when (equal fields (take 6 (decode-time wall t)))
        (if zone (- wall zone) (niwa-schedule--resolve-local wall))))))

(defun niwa-schedule-parse-timing (text now)
  "Return the timing TEXT names at clock seconds NOW, or signal `user-error'.
TEXT is a duration such as \"20m\" or \"1h30m\" (units d, h, m, s) or a
local time \"YYYY-MM-DD HH:MM[:SS]\", both one-shot \(:kind once :at
SECONDS); \"every\" and a duration of at least a minute, \(:kind interval
:seconds SECONDS); or a 5-field cron expression in local time, \(:kind cron
:expression TEXT)."
  (unless (and (stringp text) (string-match-p "\\S-" text))
    (user-error "Schedule timing must not be blank"))
  (let* ((text (string-trim text))
         (seconds (niwa-schedule--parse-duration text)))
    (cond
     ((string-match "\\`every +\\(.*\\)\\'" text)
      (let ((interval (niwa-schedule--parse-duration (match-string 1 text))))
        (unless (and interval (>= interval 60))
          (user-error "Schedule interval must be a duration of at least one minute: %s" text))
        (list :kind 'interval :seconds interval)))
     ((and (null seconds) (= 5 (length (split-string text))))
      (unless (niwa-schedule--cron-next text now)
        (user-error "Cron expression %S never fires" text))
      (list :kind 'cron :expression text))
     (t
      (let ((at (cond
                 ((null seconds)
                  (or (niwa-schedule--parse-absolute text)
                      (user-error "Unrecognized schedule timing %S; use a duration such as 20m, a local time such as 2026-11-21 10:00, every 20m, or a cron expression such as 0 9 * * 1-5"
                                  text)))
                 ((> seconds 0) (+ now seconds))
                 (t (user-error "Schedule duration must be positive: %s" text)))))
        (unless (> at now)
          (user-error "Schedule time %s is not in the future"
                      (niwa-schedule-format-time at)))
        (list :kind 'once :at at))))))

(defun niwa-schedule-next-run (timing after)
  "Return TIMING's first fire time later than clock seconds AFTER, or nil.
An interval is timed from AFTER, so runs missed in between never catch up."
  (pcase (plist-get timing :kind)
    ('once (let ((at (plist-get timing :at))) (and (> at after) at)))
    ('interval (+ after (plist-get timing :seconds)))
    ('cron (niwa-schedule--cron-next (plist-get timing :expression) after))
    (kind (error "Unknown schedule timing: %S" kind))))

(defun niwa-schedule-parse-expiry (text now)
  "Return the expiry TEXT names at clock seconds NOW, or nil for \"never\".
TEXT is a duration or local time, as for a one-shot timing; signal
`user-error' otherwise."
  (unless (equal "never" (string-trim text))
    (or (plist-get (niwa-schedule-parse-timing text now) :at)
        (user-error "Schedule expiry must be a duration, a local time or never: %s" text))))

(defun niwa-schedule-repeat-label (timing)
  "Return how TIMING repeats, such as \"every 20m 00s\" or its cron text.
Return nil for a one-shot timing."
  (pcase (plist-get timing :kind)
    ('interval (concat "every " (niwa--duration-label (plist-get timing :seconds))))
    ('cron (plist-get timing :expression))))


;;
;;; Store

(defun niwa-schedule-entry-p (entry)
  "Return non-nil when ENTRY is a well-formed persisted wake-up."
  (and (proper-list-p entry)
       (cl-evenp (length entry))
       (stringp (plist-get entry :id))
       (stringp (plist-get entry :note))
       (integerp (plist-get entry :due))
       (natnump (or (plist-get entry :failures) 0))
       (let ((runs (plist-get entry :runs)))
         (and (proper-list-p runs) (<= (length runs) niwa-schedule-run-history-length)
              (cl-every (lambda (run)
                          (and (proper-list-p run) (integerp (plist-get run :time))
                               (memq (plist-get run :outcome) '(done skipped failed))
                               (let ((reason (plist-get run :reason)))
                                 (or (null reason) (stringp reason))))) runs)))
       (let ((model (plist-get entry :model)) (effort (plist-get entry :effort)))
         (and (or (null model) (stringp model)) (or (null effort) (stringp effort))))
       (let ((expires (plist-get entry :expires-at)))
         (or (null expires) (integerp expires)))
       ;; Retained one-shot results have no timing or creation timestamp.
       (or (memq (plist-get entry :status) '(failed skipped))
           (let ((created-at (plist-get entry :created-at)))
             (and (integerp created-at)
                  (niwa-schedule-timing-p (plist-get entry :timing) created-at))))
       (pcase (plist-get entry :status)
         ('pending t)
         ('paused (and (not (eq 'once (plist-get (plist-get entry :timing) :kind)))
                       (stringp (plist-get entry :pause-reason))))
         ('expired (integerp (plist-get entry :expired-at)))
         ((or 'failed 'skipped)
          (eq (plist-get entry :status) (plist-get (car (plist-get entry :runs)) :outcome))))))

(defun niwa-schedule-timing-p (timing created-at)
  "Return non-nil when TIMING, stored at CREATED-AT, is one creation accepts."
  (pcase (plist-get timing :kind)
    ('once (integerp (plist-get timing :at)))
    ('interval (let ((seconds (plist-get timing :seconds)))
                 (and (integerp seconds) (>= seconds 60))))
    ;; Supported syntax that ever fires.  A creation time outside the
    ;; representable range signals from the time functions.
    ('cron (let ((expression (plist-get timing :expression)))
             (and (stringp expression)
                  (condition-case nil
                      (niwa-schedule--cron-next expression (or created-at 0))
                    (error nil)))))))

(defun niwa-schedule-pending (session)
  "Return SESSION's pending wake-ups, oldest first."
  (cl-remove-if-not (lambda (entry) (eq 'pending (plist-get entry :status)))
                    (niwa-session-schedules session)))

(defun niwa-schedule--new-id (session)
  "Return a wake-up identity unused in SESSION."
  (let (id)
    (while (or (null id)
               (niwa-schedule--find id (niwa-session-schedules session)))
      (setq id (format "w-%06x" (random #x1000000))))
    id))

(defun niwa-schedule--find (id entries)
  "Return the wake-up in ENTRIES whose id is ID, or nil."
  (cl-find id entries :key (lambda (entry) (plist-get entry :id)) :test #'equal))

(defun niwa-schedule--commit (session previous)
  "Commit SESSION's schedules, restoring PREVIOUS and re-signalling on failure."
  (condition-case err
      (niwa-agent-control-commit-session session)
    ((error quit)
     (setf (niwa-session-schedules session) previous)
     (signal (car err) (cdr err)))))

(defun niwa-schedule--changed (session buffer)
  "Re-arm the scheduler for SESSION's root BUFFER and refresh its cockpit."
  (when (niwa-schedule-pending session)
    (niwa-schedule--register buffer))
  (niwa-schedule--index session)
  (niwa-schedule--rearm)
  (when (fboundp 'niwa-background-list-refresh-session)
    (niwa-background-list-refresh-session session buffer)))

(defun niwa-schedule--assert-capacity (session)
  "Signal a user error when SESSION cannot admit another pending schedule."
  (when (>= (length (niwa-schedule-pending session)) niwa-schedule-max-pending)
    (user-error "This session already has %d pending schedules; cancel one first"
                niwa-schedule-max-pending)))

(defun niwa-schedule--propose (session when note expires model effort &optional previous)
  "Return the wake-up of SESSION described by WHEN, NOTE, EXPIRES, MODEL, EFFORT.
They are as for `niwa-schedule-create', and invalid ones signal.
PREVIOUS, a pending wake-up being changed, supplies whatever they omit; a
new WHEN is timed from now."
  (let* ((now (niwa-schedule-now))
         (note (or note (plist-get previous :note)))
         (timing (if when (niwa-schedule-parse-timing when now) (plist-get previous :timing)))
         (recurring (not (eq 'once (plist-get timing :kind))))
         (due (if when (niwa-schedule-next-run timing now) (plist-get previous :due)))
         ;; Changes keep the creation time, so they cannot extend the cap.
         (cap (+ (or (plist-get previous :created-at) now)
                 niwa-schedule-max-recurring-expiry))
         (expires-at (cond (expires (niwa-schedule-parse-expiry expires now))
                           ((plist-get previous :expires-at))
                           (recurring (min cap (+ now niwa-schedule-recurring-expiry)))))
         (entry (or (copy-sequence previous)
                    (list :id (niwa-schedule--new-id session) :note nil :timing nil
                          :created-at now :due nil :expires-at nil :status 'pending))))
    (unless (and (stringp note) (string-match-p "\\S-" note))
      (user-error "Wake-up note must not be blank"))
    (when (> (length note) niwa-schedule-max-note-length)
      (user-error "Wake-up note exceeds %d characters"
                  niwa-schedule-max-note-length))
    (when (or model effort)
      (setq model (or model (plist-get previous :model))
            effort (or effort (plist-get previous :effort)))
      (niwa-model-resolve-workload
       nil (niwa-model-parse-selector model) (niwa-model-parse-effort effort)))
    (when (and expires-at (<= expires-at due))
      (user-error "Wake-up expiry %s is not after its time %s"
                  (niwa-schedule-format-time expires-at)
                  (niwa-schedule-format-time due)))
    (when (and recurring
               (not (and expires-at (<= expires-at cap))))
      (user-error "A recurring wake-up must expire within 30 days of its creation, not at %s"
                  (if expires-at (niwa-schedule-format-time expires-at) "never")))
    (setq entry (plist-put entry :note (string-trim note))
          entry (plist-put entry :timing timing)
          entry (plist-put entry :due due)
          entry (plist-put entry :expires-at expires-at))
    (when model (setq entry (plist-put entry :model model)))
    (when effort (setq entry (plist-put entry :effort effort)))
    entry))

(defun niwa-schedule-create (session buffer when note &optional expires model effort)
  "Create a wake-up in SESSION, whose root data buffer is BUFFER.
WHEN is the timing text accepted by `niwa-schedule-parse-timing'.  NOTE
is the text the woken turn receives.  EXPIRES, as for
`niwa-schedule-parse-expiry', is when a still-undelivered wake-up stops
mattering; a recurring wake-up expires after
`niwa-schedule-recurring-expiry' by default and at most
`niwa-schedule-max-recurring-expiry' after creation.  The wake-up is
committed with the session before this returns it.
MODEL (a tier or BACKEND:MODEL) and EFFORT run the woken turn on another
model or effort; both are checked now, in the current buffer, so an
unavailable model or unsupported effort is an error here, not at fire time."
  (niwa-schedule--assert-capacity session)
  (let ((previous (niwa-session-schedules session))
        (entry (niwa-schedule--propose session when note expires model effort)))
    (setf (niwa-session-schedules session) (append previous (list entry)))
    (niwa-schedule--commit session previous)
    (niwa-schedule--changed session buffer)
    entry))

(defun niwa-schedule-change (session buffer id &optional when note expires model effort)
  "Change pending wake-up ID of SESSION, whose root data buffer is BUFFER.
WHEN, NOTE, EXPIRES, MODEL and EFFORT are as for `niwa-schedule-create'
and checked the same way; omitted ones keep the wake-up's values, and a new
WHEN is timed from now.  The recurring expiry cap still counts from the
wake-up's creation.  Return the committed entry."
  (let* ((previous (niwa-session-schedules session))
         (entry (niwa-schedule--find id previous)))
    (unless (eq 'pending (plist-get entry :status))
      (user-error "No pending wake-up with id %s" id))
    (let ((changed (niwa-schedule--propose session when note expires model effort entry)))
      (setf (niwa-session-schedules session) (cl-substitute changed entry previous))
      (niwa-schedule--commit session previous)
      (niwa-schedule--changed session buffer)
      changed)))

(defun niwa-schedule-cancel (session buffer id)
  "Remove wake-up ID from SESSION, whose root data buffer is BUFFER.
Return the removed entry.  Expired wake-ups are removed the same way."
  (let* ((previous (niwa-session-schedules session))
         (entry (niwa-schedule--find id previous)))
    (unless entry
      (user-error "No schedule with id %s" id))
    (setf (niwa-session-schedules session) (remq entry previous))
    (niwa-schedule--commit session previous)
    (niwa-schedule--changed session buffer)
    entry))

(defun niwa-schedule-resume (session buffer id)
  "Resume paused wake-up ID for SESSION in BUFFER, resetting its failures."
  (let* ((previous (niwa-session-schedules session))
         (entry (niwa-schedule--find id previous))
         (now (niwa-schedule-now)))
    (unless (eq (plist-get entry :status) 'paused) (user-error "Wake-up %s is not paused" id))
    (let ((next (niwa-schedule-next-run (plist-get entry :timing) now))
          (resumed (copy-sequence entry)))
      (if (and (plist-get entry :expires-at) (> next (plist-get entry :expires-at)))
          (setf (niwa-session-schedules session) (niwa-schedule--expire session entry now))
        (niwa-schedule--assert-capacity session)
        (setf (plist-get resumed :status) 'pending (plist-get resumed :due) next
              (plist-get resumed :failures) 0 (plist-get resumed :pause-reason) nil
              (niwa-session-schedules session) (cl-substitute resumed entry previous)))
      (niwa-schedule--commit session previous)
      (niwa-schedule--changed session buffer))))

(defun niwa-schedule-hand-over (session)
  "Commit SESSION's store and index entry without its wake-ups.
Save As calls this before its copy is written, so a wake-up is durable in
at most one session.  SESSION keeps its wake-ups in memory; a commit
failure signals and leaves the store as it was.  Return non-nil when
there was something to hand over."
  (when-let* ((schedules (niwa-session-schedules session)))
    (setf (niwa-session-schedules session) nil)
    (unwind-protect
        (progn
          (niwa-agent-control-commit-session session)
          (niwa-schedule--index session)
          t)
      (setf (niwa-session-schedules session) schedules))))

(defun niwa-schedule-hand-back (session)
  "Commit SESSION's in-memory wake-ups again after an abandoned hand-over."
  (niwa-agent-control-commit-session session)
  (niwa-schedule--index session))


;;
;;; Index

(defun niwa-schedule--directory-key (directory)
  "Return DIRECTORY in the one spelling the index compares."
  (directory-file-name (expand-file-name directory)))

(defun niwa-schedule--index-file (directory)
  "Return the index file naming session DIRECTORY."
  (file-name-concat niwa-user-dir "wake-ups"
                    (concat (md5 (niwa-schedule--directory-key directory)) ".el")))

(defun niwa-schedule-workspace-index-directory ()
  "Return the index directory naming workspaces with active schedules.
It is not under the wake-up index, whose restore drops entries it cannot read."
  (file-name-concat niwa-user-dir "workspace-schedules"))

(defun niwa-schedule--index-directory (directory pending)
  "Record in the index whether session DIRECTORY holds PENDING wake-ups."
  (let ((file (niwa-schedule--index-file directory)))
    (condition-case err
        (if pending
            (unless (file-exists-p file)
              (make-directory (file-name-directory file) t)
              (with-temp-file file
                (prin1 (niwa-schedule--directory-key directory) (current-buffer))))
          (when (file-exists-p file)
            (delete-file file)))
      (error
       (niwa-diagnostic-warning
        'global 'niwa
        (format "Wake-up index for %s could not be updated: %s"
                directory (error-message-string err))
        :warning)))))

(defun niwa-schedule--index (session)
  "Record in the index whether saved SESSION holds pending wake-ups."
  (when-let* ((directory (niwa-session-save-path session)))
    (niwa-schedule--index-directory
     directory (niwa-schedule-pending session))))

(defun niwa-schedule--absent-p (directory)
  "Return non-nil when DIRECTORY provably does not exist.
Only a missing path below an existing ancestor this Emacs can search is
proof; a permission or transport failure is not."
  (let* ((key (niwa-schedule--directory-key directory))
         (path key)
         parent)
    (while (and (not (file-exists-p path))
                (not (equal path (setq parent (directory-file-name
                                               (file-name-directory path))))))
      (setq path parent))
    (and (not (equal path key))
         (file-accessible-directory-p path))))

(defun niwa-schedule--stored-due (directory)
  "Return when session DIRECTORY's committed store next needs the scheduler.
Return nil only when the session is provably gone or its readable store
holds no pending wake-up; signal when the store cannot be read."
  (unless (niwa-schedule--absent-p directory)
    (let* ((publication (or (niwa-session-publication-read directory)
                            (error "No readable session publication")))
           (sidecar (or (niwa-session-persistence-load-sidecar
                         (plist-get publication :sidecar) 'global)
                        (error "Session sidecar is unreadable")))
           (dues (cl-loop for entry in (plist-get sidecar :schedules)
                          when (and (niwa-schedule-entry-p entry)
                                    (eq 'pending (plist-get entry :status)))
                          collect (plist-get entry :due))))
      (and dues (apply #'min dues)))))

(defun niwa-schedule--open-p (directory)
  "Return non-nil when a scheduled buffer holds session DIRECTORY."
  (let ((key (niwa-schedule--directory-key directory)))
    (cl-some (lambda (buffer)
               (when-let* ((session (buffer-local-value 'niwa--session buffer))
                           (path (niwa-session-save-path session)))
                 (equal key (niwa-schedule--directory-key path))))
             (cl-remove-if-not #'buffer-live-p niwa-schedule--buffers))))

(defun niwa-schedule--prune (directory)
  "Remove session DIRECTORY's index entry after its store read as stale.
Return the store's due time when a wake-up appeared meanwhile, else nil.

   Pruning Emacs              Lease holder
   reads store: none
                              commits a wake-up
                              sees the entry, keeps it
   deletes the entry
   re-reads store: pending -> recreates the entry

The holder commits before it looks at the entry, so deleting first and
then re-reading never loses a wake-up whichever way the two interleave.
Only a re-read that succeeds and finds nothing pending leaves the entry
deleted; when it signals, the entry is put back before the error
propagates."
  (niwa-schedule--index-directory directory nil)
  (let (due confirmed)
    (unwind-protect
        (setq due (niwa-schedule--stored-due directory)
              confirmed t)
      (when (or due (not confirmed))
        (niwa-schedule--index-directory directory t)))
    due))

(defun niwa-schedule--forget-closed (directory)
  "Stop scheduling closed session DIRECTORY."
  (let ((key (niwa-schedule--directory-key directory)))
    (setq niwa-schedule--closed
          (cl-remove key niwa-schedule--closed :key #'car :test #'equal))))

;;;###autoload
(defun niwa-schedule-restore ()
  "Schedule the pending wake-ups of saved sessions that are not open.
Run at startup.  Only entries proven stale are removed: malformed ones,
and those whose session is gone or whose readable store holds no pending
wake-up.  An entry that cannot be read now is kept for the next restore.
Indexed workspaces' schedules are restored by the same rule."
  (let ((inhibit-interaction t)
        (index (file-name-concat niwa-user-dir "wake-ups")))
    (dolist (file (and (file-directory-p index)
                       (directory-files index t "\\.el\\'")))
      (let (directory)
        (condition-case err
            (progn
              ;; An unreadable entry file signals here and is kept.
              (setq directory (with-temp-buffer
                                (insert-file-contents file)
                                (ignore-errors (read (current-buffer)))))
              (cond
               ((not (stringp directory)) (delete-file file))
               ((niwa-schedule--open-p directory))
               (t (when-let* ((due (or (niwa-schedule--stored-due directory)
                                       (niwa-schedule--prune directory)))
                              ((not (niwa-schedule--open-p directory))))
                    (niwa-schedule--forget-closed directory)
                    (push (cons directory due) niwa-schedule--closed)))))
          (error
           (niwa-diagnostic-warning
            'global 'niwa
            (format "Wake-ups of %s were not restored: %s"
                    (or directory file) (error-message-string err))
            :warning))))))
  (when (file-directory-p (niwa-schedule-workspace-index-directory))
    (niwa-workspace-schedule-restore))
  (niwa-schedule--rearm))

(defun niwa-schedule--wake-closed (directory now)
  "Reopen closed session DIRECTORY without a window when it is due at NOW.
Once open, the session schedules and fires its own wake-ups."
  (condition-case err
      (let ((due (or (niwa-schedule--stored-due directory)
                     (niwa-schedule--prune directory))))
        (cond
         ((null due))
         ((niwa-schedule--open-p directory))
         ((> due now) (push (cons directory due) niwa-schedule--closed))
         (t
          (let* ((existing (buffer-list))
                 (buffer (let ((inhibit-interaction t))
                           (niwa-session-persistence-restore directory))))
            (if (buffer-local-value 'niwa-session--read-only-mode buffer)
                ;; Another Emacs holds the lease, so it fires for the session.
                (unless (memq buffer existing)
                  (let ((kill-buffer-query-functions nil))
                    (kill-buffer buffer)))
              (message "niwa: reopened session %s for a wake-up"
                       (niwa-session-name
                        (buffer-local-value 'niwa--session buffer))))))))
    (error
     (push (cons directory (+ now niwa-schedule-retry-delay))
           niwa-schedule--closed)
     (niwa-diagnostic-warning
      'global 'niwa
      (format "Session %s could not be reopened for a wake-up (%s); retrying in %ds"
              directory (error-message-string err) niwa-schedule-retry-delay)
      :warning))))


;;
;;; Scheduler

(defun niwa-schedule--register (buffer)
  "Track root data BUFFER until it is killed or holds no pending wake-up."
  (unless (memq buffer niwa-schedule--buffers)
    (push buffer niwa-schedule--buffers)
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook #'niwa-schedule--forget-current nil t))))

(defun niwa-schedule-restore-session (session buffer)
  "Schedule SESSION's persisted pending wake-ups from its root data BUFFER.
Call only where this client holds the session's mutation authority."
  (when-let* ((directory (niwa-session-save-path session)))
    (niwa-schedule--forget-closed directory))
  (when (niwa-schedule-pending session)
    (niwa-schedule--register buffer))
  (niwa-schedule--index session)
  (niwa-schedule--rearm))

(defun niwa-schedule--forget-current ()
  "Schedule the current buffer's wake-ups as those of a closed session.
The session is reopened without a window when the earliest one is due."
  (when-let* ((session (niwa-schedule--session (current-buffer)))
              (directory (niwa-session-save-path session))
              (due (niwa-schedule--earliest-due session)))
    (niwa-schedule--forget-closed directory)
    (push (cons (niwa-schedule--directory-key directory) due)
          niwa-schedule--closed))
  (setq niwa-schedule--buffers (delq (current-buffer) niwa-schedule--buffers))
  (niwa-schedule--rearm))

(defun niwa-schedule--session (buffer)
  "Return root data BUFFER's session while this client may fire for it, or nil.
A buffer following a session whose lease another client holds never fires."
  (and (buffer-live-p buffer)
       (not (with-current-buffer buffer
              (bound-and-true-p niwa-session--read-only-mode)))
       (buffer-local-value 'niwa--session buffer)))

(defun niwa-schedule--earliest-due (session)
  "Return the earliest due time of SESSION's pending wake-ups, or nil."
  (when-let* ((dues (mapcar (lambda (entry) (plist-get entry :due))
                            (niwa-schedule-pending session))))
    (apply #'min dues)))

(defun niwa-schedule--earliest (buffer)
  "Return when BUFFER's session next needs the scheduler, or nil."
  (when-let* ((session (niwa-schedule--session buffer))
              (due (niwa-schedule--earliest-due session)))
    (max due
         (or (buffer-local-value 'niwa-schedule--retry-at buffer) 0))))

(defun niwa-schedule--rearm ()
  "Arm one timer for the earliest pending wake-up, or none."
  (when (timerp niwa-schedule--timer)
    (cancel-timer niwa-schedule--timer))
  (setq niwa-schedule--timer nil
        niwa-schedule--buffers
        (cl-remove-if-not #'niwa-schedule--earliest niwa-schedule--buffers))
  (when-let* ((dues (append (mapcar #'cdr niwa-schedule--closed)
                            (mapcar #'cdr niwa-schedule--workspaces)
                            (mapcar #'niwa-schedule--earliest
                                    niwa-schedule--buffers))))
    (let ((due (apply #'min dues)))
      (setq niwa-schedule--timer
            (run-at-time (max 0 (- due (funcall niwa-schedule-clock)))
                         nil #'niwa-schedule-run-due)))))

(defun niwa-schedule-run-due ()
  "Fire every wake-up and workspace schedule due by `niwa-schedule-clock'.
Then re-arm."
  (let ((now (niwa-schedule-now)))
    (dolist (workspace (copy-sequence niwa-schedule--workspaces))
      (when (<= (cdr workspace) now)
        (setq niwa-schedule--workspaces (delq workspace niwa-schedule--workspaces))
        (niwa-workspace-schedule-run-due (car workspace) now)))
    ;; A reopened session registers its buffer, which the loop below fires.
    (dolist (closed (copy-sequence niwa-schedule--closed))
      (when (<= (cdr closed) now)
        (setq niwa-schedule--closed (delq closed niwa-schedule--closed))
        (niwa-schedule--wake-closed (car closed) now)))
    (dolist (buffer (copy-sequence niwa-schedule--buffers))
      (when-let* ((session (niwa-schedule--session buffer))
                  ((<= (or (buffer-local-value 'niwa-schedule--retry-at buffer) 0)
                       now)))
        (dolist (entry (niwa-schedule-pending session))
          (when (<= (plist-get entry :due) now)
            (niwa-schedule--fire session buffer entry now)))
        (when (fboundp 'niwa-background-list-refresh-session)
          (niwa-background-list-refresh-session session buffer)))))
  (niwa-schedule--rearm))

(defun niwa-schedule--record-meta (entry now)
  "Return the WAKE_UP record facts for ENTRY firing at clock seconds NOW."
  (let ((late (- now (plist-get entry :due))))
    (append (list :schedule-id (plist-get entry :id)
                  :scheduled-at (niwa-schedule-format-time (plist-get entry :due))
                  :fired-at (niwa-schedule-format-time now))
            (and (plist-get entry :model) (list :model (plist-get entry :model)))
            (and (plist-get entry :effort) (list :effort (plist-get entry :effort)))
            (when (>= late niwa-schedule-overdue-threshold)
              (list :overdue-by (niwa--duration-label late))))))

(defun niwa-schedule-wake-request-context (session)
  "Return the request context for SESSION's woken turn, or nil.
The turn runs on a wake-up's model and effort only when every unread root
record is a WAKE_UP asking for that same override; any other mail answered
by the same turn keeps the session's selection.  The result is for
`niwa-skills--pending-request-context'."
  (let* ((metas (mapcar (lambda (record)
                          (and (eq 'WAKE_UP (plist-get record :type))
                               (or (plist-get record :meta) '(:none t))))
                        (niwa-agent-control-context-mailbox session)))
         (first (car metas)))
    (when (and first
               (or (plist-get first :model) (plist-get first :effort))
               (cl-every (lambda (meta)
                           (and meta
                                (equal (plist-get meta :model) (plist-get first :model))
                                (equal (plist-get meta :effort) (plist-get first :effort))))
                         metas))
      (list :model (niwa-model-parse-selector (plist-get first :model))
            :effort (niwa-model-parse-effort (plist-get first :effort))))))

(defun niwa-schedule--with-run (entry run)
  "Return ENTRY with RUN prepended to its bounded recent history."
  (plist-put (copy-sequence entry) :runs
             (seq-take (cons run (plist-get entry :runs)) niwa-schedule-run-history-length)))

(defun niwa-schedule-settle-wake (session buffer context outcome reason)
  "Record OUTCOME and REASON for the wake records in BUFFER's request CONTEXT.
An ordinary user turn owns no wake runs, even if it consumed SESSION's mail."
  (let ((previous (niwa-session-schedules session))
        (mail (niwa-session-messages session)) retired notices)
    (dolist (record (plist-get context :wake-runs))
      (when-let* ((id (plist-get (plist-get record :meta) :schedule-id)))
        (let* ((entries (niwa-session-schedules session))
               (entry (niwa-schedule--find id entries))
               (status (pcase outcome ('success 'done) ((or 'abort 'aborted) 'skipped) (_ 'failed)))
               (failed (eq status 'failed))
               (count (+ (or (plist-get entry :failures) 0) (if failed 1 0)))
               (pause (and failed (= count niwa-schedule-max-failures)
                           (eq (plist-get entry :status) 'pending)))
               (run (list :id (plist-get record :id) :time (niwa-schedule-now)
                          :outcome status :reason (if (eq status 'skipped) "Interrupted" reason))))
          (push (plist-get record :id) retired)
          (if entry
            (let ((updated (niwa-schedule--with-run entry run)))
              (setf (plist-get updated :failures) (if (eq status 'done) 0 count))
              (when pause
                (setf (plist-get updated :status) 'paused
                      (plist-get updated :pause-reason)
                      (format "%d consecutive failures: %s" niwa-schedule-max-failures
                              (or reason outcome))))
              (setf (niwa-session-schedules session) (cl-substitute updated entry entries)))
            ;; One-shots are already removed by the mailbox commit.  Retain
            ;; their failed or interrupted run without recreating a schedule.
            (unless (eq status 'done)
              (let ((due (plist-get (plist-get record :meta) :scheduled-at)))
                (setf (niwa-session-schedules session)
                      (niwa-schedule--retain-history
                       (append entries (list (list :id id :status status :note (plist-get record :payload)
                                                   :due (floor (float-time (date-to-time due)))
                                                   :runs (list run)))))))))
          (when failed
            (push (format "Wake-up %s failed: %s%s" id (or reason outcome)
                          (if pause
                              (format "; paused after %d consecutive failures"
                                      niwa-schedule-max-failures)
                            ""))
                  notices)))))
    ;; Preflight may have failed before either engine injected the mail.
    ;; Retire only accounted wake records, together with their outcome.
    (niwa-session--set-messages
     session (cl-remove-if (lambda (r) (member (plist-get r :id) retired)) mail))
    (condition-case err
        (niwa-schedule--commit session previous)
      ((error quit) (niwa-session--set-messages session mail) (signal (car err) (cdr err))))
    (setf (plist-get context :wake-runs) nil)
    (dolist (notice (nreverse notices)) (niwa-notify session 'failure notice))
    (niwa-schedule--changed session buffer)))

(defun niwa-schedule--retain-history (all)
  "Retain active schedules and the most recent terminal entries from ALL."
  (let* ((expired (cl-remove-if-not (lambda (other) (memq (plist-get other :status) '(expired failed skipped))) all))
         ;; The oldest records beyond the retention limit are dropped.
         (dropped (butlast expired niwa-schedule-retained-expired)))
    (cl-remove-if (lambda (other) (memq other dropped)) all)))

(defun niwa-schedule--expire (session entry now &optional updated)
  "Return SESSION's schedules with ENTRY expired at NOW, retaining UPDATED history."
  (niwa-schedule--retain-history
   (append (remq entry (niwa-session-schedules session))
           (list (plist-put (plist-put (copy-sequence (or updated entry)) :status 'expired)
                            :expired-at now)))))

(defun niwa-schedule--retry-later (session buffer entry err)
  "Restore ENTRY in SESSION after its delivery failed with ERR.
BUFFER waits `niwa-schedule-retry-delay' before the next attempt."
  (setf (niwa-session-schedules session)
        (append (remq (niwa-schedule--find (plist-get entry :id)
                                              (niwa-session-schedules session))
                      (niwa-session-schedules session))
                (list entry)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq niwa-schedule--retry-at
            (+ (niwa-schedule-now) niwa-schedule-retry-delay))))
  (niwa-diagnostic-warning
   (niwa-diagnostic-scope session) 'niwa
   (format "Wake-up %s could not be delivered (%s); retrying in %ds"
           (plist-get entry :id) (error-message-string err)
           niwa-schedule-retry-delay)
   :warning))

(defun niwa-schedule--fire (session buffer entry now)
  "Deliver or expire due ENTRY of SESSION at clock seconds NOW.
BUFFER is the root data buffer offered the background-event turn."
  (let ((previous (niwa-session-schedules session))
        (expires (plist-get entry :expires-at)))
    (condition-case err
        (if (and expires (> now expires))
            (progn
              (setf (niwa-session-schedules session)
                    (niwa-schedule--expire session entry now))
              (niwa-schedule--commit session previous)
              (niwa-schedule--index session))
          ;; Removal and the record share the mailbox commit, so a firing is
          ;; never both delivered and still pending in the persisted store.
          (setf (niwa-session-schedules session)
                (let* ((timing (plist-get entry :timing))
                       (due (plist-get entry :due))
                       (next (niwa-schedule-next-run timing now))
                       (missed (and next (< due now) (niwa-schedule-next-run timing due)))
                       (updated (if (and missed (<= missed now))
                                    (niwa-schedule--with-run
                                     entry (list :time missed :outcome 'skipped
                                                 :reason "Missed cadence before overdue firing"))
                                  (copy-sequence entry))))
                  (cond ((null next) (remq entry previous))
                        ;; A run past expiry would only be dropped; free the slot now.
                        ((and expires (> next expires))
                         (niwa-schedule--expire session entry now updated))
                        (t (cl-substitute (plist-put updated :due next)
                                          entry previous)))))
          (niwa-agent-control-enqueue-wake-up
           session (plist-get entry :note) (niwa-schedule--record-meta entry now)
           (lambda (success failure)
             (if success
                 (progn
                   (niwa-schedule--index session)
                   (niwa-agent-control-notify
                    session (list 'root-wake buffer)
                    (lambda ()
                      (unless (niwa-session-publication-stopping-p session)
                        (niwa-pending-inputs-offer-root-event session buffer)))))
               (niwa-schedule--retry-later session buffer entry failure)
               (niwa-schedule--rearm)))))
      ((error quit)
       (niwa-schedule--retry-later session buffer entry err)))))

(provide 'niwa-schedule)
;;; niwa-schedule.el ends here
