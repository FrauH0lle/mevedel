;;; niwa-sleep.el -- Keep the machine awake while niwa works -*- lexical-binding: t -*-

;;; Commentary:

;; Holds one operating-system sleep inhibitor while this Emacs has work that
;; system sleep would interrupt: a running or settling turn, a live managed
;; command, or an open collaboration share within its idle allowance.  Only
;; sleep is blocked; screen blanking and locking are unaffected.
;;
;; Work owners call `niwa-sleep-update' when work begins.  End edges need
;; no call: while any reason holds, a slow poll re-evaluates them and releases
;; the inhibitor once none remains, so no teardown path can leak it.  The poll
;; also refreshes the announced reasons, so short-lived work such as a quick
;; command does not replace the inhibitor process twice.  The inhibitor is a
;; child process whose stdin is a pipe from Emacs, so it also exits, releasing
;; the lock, when Emacs exits or crashes.

;;; Code:

(require 'niwa-utilities)

;; `niwa-collaboration'
(declare-function niwa-collaboration-sleep-reasons "niwa-collaboration"
                  (now))

;; `niwa-execution'
(declare-function niwa-execution-working-p "niwa-execution" ())

;; `niwa-transport'
(declare-function niwa-transport-run-at-time
                  "niwa-transport" (seconds function &rest args))
(autoload 'niwa-transport-run-at-time "niwa-transport")

;; `niwa-turn'
(declare-function niwa-busy-p "niwa-turn" ())


;;
;;; Customization

(defun niwa-sleep--set-inhibit (symbol value)
  "Set SYMBOL to VALUE and apply it to the current work at once."
  (set-default symbol value)
  (if value
      (niwa-sleep-update)
    (niwa-sleep-stop)))

(defcustom niwa-sleep-inhibit nil
  "Non-nil means block system sleep while niwa has work in progress.
The work that counts is a running or settling turn, a live managed command,
and an open collaboration share within its idle allowance.  Only sleep is
blocked, not screen blanking or locking.  The block is held by
`niwa-sleep-inhibit-command'.  Setting this through Customize applies at
once; a plain `setq' takes effect at the next evaluation."
  :type 'boolean
  :initialize #'custom-initialize-default
  :set #'niwa-sleep--set-inhibit
  :group 'niwa)

(defcustom niwa-sleep-inhibit-command
  '("systemd-inhibit" "--what=sleep" "--who=niwa" "--why=%s"
    "--mode=block" "cat")
  "Command that blocks system sleep for as long as it runs.
Used while `niwa-sleep-inhibit' is non-nil.  The default uses systemd.
Each \"%s\" in an argument is replaced by the reasons sleep is blocked.  The
command must exit when its standard input reaches end of file, so that it
cannot outlive Emacs."
  :type '(repeat :tag "Command and arguments" string)
  :group 'niwa)


;;
;;; State

(defconst niwa-sleep--poll-interval 30
  "Seconds between re-evaluations while sleep is inhibited.")

(defvar niwa-sleep--timer nil
  "Timer for the next re-evaluation, or nil while no reason holds.")

(defvar niwa-sleep--process nil
  "Live inhibitor process, or nil.")

(defvar niwa-sleep--held-reasons nil
  "Reasons the live inhibitor process announces.")

(defvar niwa-sleep--failed nil
  "Non-nil after the inhibitor failed; cleared once no reason holds.")


;;
;;; Reasons

(defun niwa-sleep--reasons ()
  "Return the current reasons to block sleep, as a list of strings.
A failing reason source counts as no reason: a lock that cannot be
evaluated must not keep the machine awake indefinitely."
  (condition-case err
      (append
       (and (featurep 'niwa-turn) (niwa-busy-p)
            (list "request running"))
       (and (featurep 'niwa-execution) (niwa-execution-working-p)
            (list "command running"))
       (and (fboundp 'niwa-collaboration-sleep-reasons)
            (niwa-collaboration-sleep-reasons (float-time))))
    (error
     (niwa--warn-once 'global 'sleep-reasons
                         "Cannot determine whether to block system sleep: %s"
                         (error-message-string err))
     nil)))


;;
;;; Inhibitor process

(defun niwa-sleep--sentinel (process event)
  "Report PROCESS ending with EVENT while it should still hold the lock."
  (when (and (eq process niwa-sleep--process)
             (memq (process-status process) '(exit signal)))
    (setq niwa-sleep--process nil
          niwa-sleep--held-reasons nil
          niwa-sleep--failed t)
    (niwa--warn-once
     'global 'sleep-inhibitor "Cannot block system sleep: %s%s"
     (string-trim event)
     (let ((output (string-trim (or (process-get process 'output) ""))))
       (if (string-empty-p output) "" (concat ": " output))))))

(defun niwa-sleep--spawn (reasons)
  "Start an inhibitor process announcing REASONS, or return nil on failure."
  (let ((why (string-join reasons ", ")))
    (condition-case err
        ;; The inhibitor guards this machine, so it never runs through a
        ;; remote `default-directory'.
        (let ((default-directory temporary-file-directory))
          (make-process
           :name "niwa-sleep-inhibitor"
           :command (mapcar (lambda (argument)
                              (string-replace "%s" why argument))
                            niwa-sleep-inhibit-command)
           :connection-type 'pipe
           :noquery t
           :filter (lambda (process output)
                     (process-put process 'output
                                  (concat (process-get process 'output)
                                          output)))
           :sentinel #'niwa-sleep--sentinel))
      (error
       (setq niwa-sleep--failed t)
       (niwa--warn-once 'global 'sleep-inhibitor "Cannot block system sleep: %s"
                           (error-message-string err))
       nil))))

(defun niwa-sleep--hold (reasons)
  "Make the inhibitor announce REASONS, releasing it when REASONS is nil.
A replacement is acquired before the old inhibitor is released."
  (let ((old niwa-sleep--process))
    (setq niwa-sleep--process
          (and reasons (not niwa-sleep--failed)
               (niwa-sleep--spawn reasons)))
    (setq niwa-sleep--held-reasons
          (and niwa-sleep--process reasons))
    (when old
      (delete-process old))))


;;
;;; Reconciliation

(defun niwa-sleep--schedule (seconds)
  "Schedule a reconciliation SECONDS from now as the only current one.
Execution state changes arrive from sentinels that can run inside a TRAMP
handler, where a plain timer would be discarded, so the timer goes through
the transport.  A held timer still fires after cancellation, so each
callback checks that its timer is still current."
  (let ((cell (list nil)))
    (setcar cell (niwa-transport-run-at-time
                  seconds #'niwa-sleep--scheduled cell))
    (setq niwa-sleep--timer (car cell))))

(defun niwa-sleep--scheduled (cell)
  "Reconcile when CELL holds the current timer; a superseded one is inert."
  (when (eq (car cell) niwa-sleep--timer)
    (niwa-sleep--reconcile)))

(defun niwa-sleep--reconcile ()
  "Match the inhibitor to the current reasons and schedule the next check."
  (setq niwa-sleep--timer nil)
  (let ((reasons (and niwa-sleep-inhibit
                      (niwa-sleep--reasons))))
    (unless reasons
      (setq niwa-sleep--failed nil))
    (unless (equal reasons niwa-sleep--held-reasons)
      (niwa-sleep--hold reasons))
    (when reasons
      (niwa-sleep--schedule niwa-sleep--poll-interval))))

;;;###autoload
(defun niwa-sleep-update (&rest _)
  "Re-evaluate soon whether system sleep must be blocked.
Call this when work begins.  While an inhibitor is already held, the poll
picks up new work, so this does nothing.  Repeated calls coalesce into one
evaluation.  Arguments are ignored, so this can serve directly as a hook
function."
  (when (and niwa-sleep-inhibit
             (not niwa-sleep--process))
    (when (timerp niwa-sleep--timer)
      (cancel-timer niwa-sleep--timer))
    (niwa-sleep--schedule 0)))

(defun niwa-sleep-stop ()
  "Release any sleep inhibitor and stop re-evaluating.
A later `niwa-sleep-update' starts again."
  (when (timerp niwa-sleep--timer)
    (cancel-timer niwa-sleep--timer))
  (setq niwa-sleep--timer nil
        niwa-sleep--failed nil)
  (niwa-sleep--hold nil))

(provide 'niwa-sleep)
;;; niwa-sleep.el ends here
