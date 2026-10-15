;;; mevedel-sleep.el -- Keep the machine awake while mevedel works -*- lexical-binding: t -*-

;;; Commentary:

;; Holds one operating-system sleep inhibitor while this Emacs has work that
;; system sleep would interrupt: a running or settling turn, a live managed
;; command, or an open collaboration share within its idle allowance.  Only
;; sleep is blocked; screen blanking and locking are unaffected.
;;
;; Work owners call `mevedel-sleep-update' when work begins.  End edges need
;; no call: while any reason holds, a slow poll re-evaluates them and releases
;; the inhibitor once none remains, so no teardown path can leak it.  The poll
;; also refreshes the announced reasons, so short-lived work such as a quick
;; command does not replace the inhibitor process twice.  The inhibitor is a
;; child process whose stdin is a pipe from Emacs, so it also exits, releasing
;; the lock, when Emacs exits or crashes.

;;; Code:

(require 'mevedel-utilities)

;; `mevedel-collaboration'
(declare-function mevedel-collaboration-sleep-reasons "mevedel-collaboration"
                  (now))

;; `mevedel-execution'
(declare-function mevedel-execution-working-p "mevedel-execution" ())

;; `mevedel-transport'
(declare-function mevedel-transport-run-at-time
                  "mevedel-transport" (seconds function &rest args))
(autoload 'mevedel-transport-run-at-time "mevedel-transport")

;; `mevedel-turn'
(declare-function mevedel-busy-p "mevedel-turn" ())


;;
;;; Customization

(defun mevedel-sleep--set-inhibit (symbol value)
  "Set SYMBOL to VALUE and apply it to the current work at once."
  (set-default symbol value)
  (if value
      (mevedel-sleep-update)
    (mevedel-sleep-stop)))

(defcustom mevedel-sleep-inhibit nil
  "Non-nil means block system sleep while mevedel has work in progress.
The work that counts is a running or settling turn, a live managed command,
and an open collaboration share within its idle allowance.  Only sleep is
blocked, not screen blanking or locking.  The block is held by
`mevedel-sleep-inhibit-command'.  Setting this through Customize applies at
once; a plain `setq' takes effect at the next evaluation."
  :type 'boolean
  :initialize #'custom-initialize-default
  :set #'mevedel-sleep--set-inhibit
  :group 'mevedel)

(defcustom mevedel-sleep-inhibit-command
  '("systemd-inhibit" "--what=sleep" "--who=mevedel" "--why=%s"
    "--mode=block" "cat")
  "Command that blocks system sleep for as long as it runs.
Used while `mevedel-sleep-inhibit' is non-nil.  The default uses systemd.
Each \"%s\" in an argument is replaced by the reasons sleep is blocked.  The
command must exit when its standard input reaches end of file, so that it
cannot outlive Emacs."
  :type '(repeat :tag "Command and arguments" string)
  :group 'mevedel)


;;
;;; State

(defconst mevedel-sleep--poll-interval 30
  "Seconds between re-evaluations while sleep is inhibited.")

(defvar mevedel-sleep--timer nil
  "Timer for the next re-evaluation, or nil while no reason holds.")

(defvar mevedel-sleep--process nil
  "Live inhibitor process, or nil.")

(defvar mevedel-sleep--held-reasons nil
  "Reasons the live inhibitor process announces.")

(defvar mevedel-sleep--failed nil
  "Non-nil after the inhibitor failed; cleared once no reason holds.")


;;
;;; Reasons

(defun mevedel-sleep--reasons ()
  "Return the current reasons to block sleep, as a list of strings.
A failing reason source counts as no reason: a lock that cannot be
evaluated must not keep the machine awake indefinitely."
  (condition-case err
      (append
       (and (featurep 'mevedel-turn) (mevedel-busy-p)
            (list "request running"))
       (and (featurep 'mevedel-execution) (mevedel-execution-working-p)
            (list "command running"))
       (and (fboundp 'mevedel-collaboration-sleep-reasons)
            (mevedel-collaboration-sleep-reasons (float-time))))
    (error
     (mevedel--warn-once 'sleep-reasons
                         "Cannot determine whether to block system sleep: %s"
                         (error-message-string err))
     nil)))


;;
;;; Inhibitor process

(defun mevedel-sleep--sentinel (process event)
  "Report PROCESS ending with EVENT while it should still hold the lock."
  (when (and (eq process mevedel-sleep--process)
             (memq (process-status process) '(exit signal)))
    (setq mevedel-sleep--process nil
          mevedel-sleep--held-reasons nil
          mevedel-sleep--failed t)
    (mevedel--warn-once
     'sleep-inhibitor "Cannot block system sleep: %s%s"
     (string-trim event)
     (let ((output (string-trim (or (process-get process 'output) ""))))
       (if (string-empty-p output) "" (concat ": " output))))))

(defun mevedel-sleep--spawn (reasons)
  "Start an inhibitor process announcing REASONS, or return nil on failure."
  (let ((why (string-join reasons ", ")))
    (condition-case err
        ;; The inhibitor guards this machine, so it never runs through a
        ;; remote `default-directory'.
        (let ((default-directory temporary-file-directory))
          (make-process
           :name "mevedel-sleep-inhibitor"
           :command (mapcar (lambda (argument)
                              (string-replace "%s" why argument))
                            mevedel-sleep-inhibit-command)
           :connection-type 'pipe
           :noquery t
           :filter (lambda (process output)
                     (process-put process 'output
                                  (concat (process-get process 'output)
                                          output)))
           :sentinel #'mevedel-sleep--sentinel))
      (error
       (setq mevedel-sleep--failed t)
       (mevedel--warn-once 'sleep-inhibitor "Cannot block system sleep: %s"
                           (error-message-string err))
       nil))))

(defun mevedel-sleep--hold (reasons)
  "Make the inhibitor announce REASONS, releasing it when REASONS is nil.
A replacement is acquired before the old inhibitor is released."
  (let ((old mevedel-sleep--process))
    (setq mevedel-sleep--process
          (and reasons (not mevedel-sleep--failed)
               (mevedel-sleep--spawn reasons)))
    (setq mevedel-sleep--held-reasons
          (and mevedel-sleep--process reasons))
    (when old
      (delete-process old))))


;;
;;; Reconciliation

(defun mevedel-sleep--schedule (seconds)
  "Schedule a reconciliation SECONDS from now as the only current one.
Execution state changes arrive from sentinels that can run inside a TRAMP
handler, where a plain timer would be discarded, so the timer goes through
the transport.  A held timer still fires after cancellation, so each
callback checks that its timer is still current."
  (let ((cell (list nil)))
    (setcar cell (mevedel-transport-run-at-time
                  seconds #'mevedel-sleep--scheduled cell))
    (setq mevedel-sleep--timer (car cell))))

(defun mevedel-sleep--scheduled (cell)
  "Reconcile when CELL holds the current timer; a superseded one is inert."
  (when (eq (car cell) mevedel-sleep--timer)
    (mevedel-sleep--reconcile)))

(defun mevedel-sleep--reconcile ()
  "Match the inhibitor to the current reasons and schedule the next check."
  (setq mevedel-sleep--timer nil)
  (let ((reasons (and mevedel-sleep-inhibit
                      (mevedel-sleep--reasons))))
    (unless reasons
      (setq mevedel-sleep--failed nil))
    (unless (equal reasons mevedel-sleep--held-reasons)
      (mevedel-sleep--hold reasons))
    (when reasons
      (mevedel-sleep--schedule mevedel-sleep--poll-interval))))

;;;###autoload
(defun mevedel-sleep-update (&rest _)
  "Re-evaluate soon whether system sleep must be blocked.
Call this when work begins.  While an inhibitor is already held, the poll
picks up new work, so this does nothing.  Repeated calls coalesce into one
evaluation.  Arguments are ignored, so this can serve directly as a hook
function."
  (when (and mevedel-sleep-inhibit
             (not mevedel-sleep--process))
    (when (timerp mevedel-sleep--timer)
      (cancel-timer mevedel-sleep--timer))
    (mevedel-sleep--schedule 0)))

(defun mevedel-sleep-stop ()
  "Release any sleep inhibitor and stop re-evaluating.
A later `mevedel-sleep-update' starts again."
  (when (timerp mevedel-sleep--timer)
    (cancel-timer mevedel-sleep--timer))
  (setq mevedel-sleep--timer nil
        mevedel-sleep--failed nil)
  (mevedel-sleep--hold nil))

(provide 'mevedel-sleep)
;;; mevedel-sleep.el ends here
