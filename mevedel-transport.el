;;; mevedel-transport.el -- Remote transport reentrancy state -*- lexical-binding: t -*-

;;; Commentary:

;; Answers one question: is this Emacs already inside a remote operation?
;;
;; Durable target I/O started from a timer, a process filter, or redisplay
;; nests inside whatever remote command happens to be in flight.  TRAMP refuses
;; the nested call, or the nested command consumes the outer command's pending
;; output and returns an answer belonging to something else.  A control
;; operation that reads another command's exit status reports a file that is
;; not there, and durable state derived from that is wrong.
;;
;; Callers that can be reached from a timer, a filter, or redisplay ask
;; `mevedel-transport-busy-p' before target I/O, and defer through
;; `mevedel-transport-run-when-idle' when it answers yes.

;;; Code:

;; `tramp'
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-get-connection-process "tramp" (vec))
(declare-function tramp-get-connection-property
                  "tramp-cache" (key property &optional default))
(autoload 'tramp-dissect-file-name "tramp")
(autoload 'tramp-get-connection-process "tramp")
(autoload 'tramp-get-connection-property "tramp-cache")


;;
;;; Customization

(defcustom mevedel-transport-retry-seconds 0.05
  "Seconds before a deferred target operation re-tests the transport.

Nesting is always transient: the operation already in flight completes and
releases the stack.  The interval only decides how promptly deferred work
notices, so it trades a little latency against idle polling."
  :type 'number
  :group 'mevedel)


;;
;;; Nesting depth

(defvar mevedel-transport--depth 0
  "Count of TRAMP file-name handler frames below this point on the stack.

TRAMP holds its connection lock for one `accept-process-output' and one send,
and its wait loop yields the CPU with `sit-for' between them -- lock released,
timers live -- so a command is in flight far longer than the lock reports.
This counter spans the whole handler call instead.  It is therefore the only
signal that covers that window, and the only one that also sees remote
operations started by other packages, which is what a mode line or a version
control check does during redisplay.")

(defvar mevedel-transport--enabled-p nil
  "Whether transport integration should return when TRAMP reloads.")

(defun mevedel-transport--handler-advice (original &rest args)
  "Count one TRAMP handler frame around ORIGINAL applied to ARGS.

Purely observational: the frame is counted with a dynamic binding, so a
handler that exits through `throw', `keyboard-quit', or any signal still
uncounts itself."
  (let ((mevedel-transport--depth (1+ mevedel-transport--depth)))
    (apply original args)))

(defun mevedel-transport-install ()
  "Begin counting TRAMP handler frames."
  (setq mevedel-transport--enabled-p t)
  (unless (advice-member-p #'mevedel-transport--handler-advice
                           'tramp-file-name-handler)
    (advice-add 'tramp-file-name-handler :around
                #'mevedel-transport--handler-advice))
  (when (boundp 'tramp-unload-hook)
    (add-hook 'tramp-unload-hook #'mevedel-transport--detach)))

(defun mevedel-transport--detach ()
  "Detach transport integration and cancel work that cannot safely run."
  (mevedel-transport-cancel-pending)
  (advice-remove 'tramp-file-name-handler
                 #'mevedel-transport--handler-advice)
  (setq mevedel-transport--depth 0))

(defun mevedel-transport-uninstall ()
  "Stop counting TRAMP handler frames and cancel deferred work."
  (setq mevedel-transport--enabled-p nil)
  (mevedel-transport--detach)
  (when (boundp 'tramp-unload-hook)
    (remove-hook 'tramp-unload-hook #'mevedel-transport--detach)))

(defun mevedel-transport-nested-p ()
  "Return non-nil when a TRAMP file operation is already on the stack."
  (> mevedel-transport--depth 0))


;;
;;; Busy predicate

(defun mevedel-transport--connection-locked-p (path)
  "Return non-nil when PATH's TRAMP connection holds its operation lock.

This catches a caller that reached the connection without going through the
file-name handler, which is what TRAMP's own internal command senders do."
  (when (and (stringp path) (file-remote-p path))
    (when-let* ((process (tramp-get-connection-process
                          (tramp-dissect-file-name path))))
      (and (process-live-p process)
           (tramp-get-connection-property process "locked")
           t))))

(defun mevedel-transport-busy-p (&optional path)
  "Return non-nil when starting a target operation now would nest.

PATH names the target whose connection is examined; a local or absent PATH
still consults the handler depth, because that covers every connection.

Two signals, with complementary blind spots.  The handler depth sees the whole
of any operation this Emacs started through a file name, including operations
belonging to other packages, but not one issued straight to TRAMP's internal
command senders.  The connection lock sees those, but only during the instants
TRAMP holds it.

Neither sees a classic remote process created by `make-process' on the
shared connection, nor operations on another thread.  Direct-async Bash
executions run on their own connection and no longer occupy this one, so
the blind spot covers only classic spawns -- TTY executions, oversized
commands, and targets without direct-async.  A caller that must not
corrupt durable state therefore treats this as necessary, not sufficient,
and fails closed when a target operation misbehaves anyway."
  (or (mevedel-transport-nested-p)
      (mevedel-transport--connection-locked-p path)))


;;
;;; Exclusive use

(defun mevedel-transport--unsuspend-timeouts (suspended elapsed)
  "Restart SUSPENDED `with-timeout' clocks, charging them ELAPSED seconds.

`with-timeout-unsuspend' re-arms each timeout with the delay it had left when
the clock stopped, because it exists for a debugger, where the time a person
spends reading a backtrace is not the program's to answer for.  A remote
operation is the program's, and a caller that bounded its work at thirty
seconds means thirty seconds of its own waiting, not thirty seconds of
whatever is left after this section.  Charging the section makes a deadline
that passed inside it fire as soon as the connection is free again."
  (dolist (entry suspended)
    (let* ((timer (car entry))
           (delay (max 0 (- (float-time (cadr entry)) elapsed))))
      (timer-set-time timer (time-add nil delay))
      (timer-activate timer))))

(defun mevedel-transport--call-with-exclusive-connection (thunk)
  "Call THUNK with foreign timers suspended, re-arming any it schedules.

Binding the timer lists away is what stops a foreign timer, but it also means
a timer THUNK arms lands on a binding that is about to be discarded.  The
durable path does arm one -- the lease renewal timer -- so the timers scheduled
inside are collected and re-armed against the restored lists instead of being
dropped.

A held timer keeps its absolute deadline and so fires overdue once it is
back.  A held `with-timeout' would not: its clock stops, and the deadline
would silently move by however long this section ran.  Both are therefore
charged for the section."
  (let ((suspended (with-timeout-suspend))
        (started (float-time))
        (scheduled nil)
        (scheduled-idle nil))
    (unwind-protect
        (let (timer-list timer-idle-list)
          (unwind-protect (funcall thunk)
            ;; Collect here: the outer lists come back when this `let' exits,
            ;; taking anything armed inside with them.
            (setq scheduled timer-list
                  scheduled-idle timer-idle-list)))
      (mevedel-transport--unsuspend-timeouts
       suspended (- (float-time) started))
      (dolist (timer scheduled)
        (ignore-errors (timer-activate timer)))
      (dolist (timer scheduled-idle)
        (ignore-errors (timer-activate-when-idle timer))))))

(defmacro mevedel-transport-with-exclusive-connection (&rest body)
  "Run BODY without letting a timer start another remote operation.

`mevedel-transport-busy-p' keeps this package from nesting inside somebody
else's remote operation.  It cannot do the reverse: an idle timer belonging to
a syntax checker, a mode line, or anything else does not consult it, and the
TRAMP wait loop yields to timers with a command in flight.  Such a timer sends
its own command on the same connection and consumes the reply we were waiting
for -- our records arrive at its parser, and the answer we read belongs to it.

Suspending timers for the duration is what TRAMP itself does around its
critical sections.  A timer BODY arms is re-armed on exit rather than lost, so
BODY may schedule one; a suspended timer whose deadline passed meanwhile fires
overdue, which for everything on this path is a latency cost and not a
correctness one.  A `with-timeout' a caller opened around BODY is charged for
the time BODY took, so bounding remote work still bounds it.

A timer BODY arms is armed on the bound lists, which are the ones Emacs
consults while BODY runs, so it fires normally -- including the one a
`with-timeout' opened inside BODY installs, which keeps a bounded probe
bounded.  Only timers that existed beforehand are held.

One limit BODY must respect: `cancel-timer' inside BODY cannot reach a timer
this macro suspended, because it is not on the bound list; that timer is
restored on exit as though the cancel never happened."
  (declare (indent 0) (debug t))
  `(mevedel-transport--call-with-exclusive-connection (lambda () ,@body)))


;;
;;; Deferral

(defvar mevedel-transport--pending (make-hash-table :test #'equal)
  "Timers for work waiting on an idle transport, keyed by coalescing key.")

(defvar mevedel-transport--pending-cancellers (make-hash-table :test #'equal)
  "Cancellation callbacks for deferred transport work.")

(defun mevedel-transport--retry (key path thunk on-cancel)
  "Re-attempt KEY's THUNK for PATH, preserving ON-CANCEL."
  (remhash key mevedel-transport--pending)
  (remhash key mevedel-transport--pending-cancellers)
  (mevedel-transport-run-when-idle key path thunk on-cancel))

(defun mevedel-transport-run-when-idle (key path thunk &optional on-cancel)
  "Call THUNK once no remote operation for PATH is in flight.

KEY coalesces repeated scheduling of the same logical work, so a caller that
re-arms on every event queues one retry rather than a growing fan of timers.
THUNK runs immediately when the transport is already idle, because a filter or
sentinel is only unsafe when it nests.  ON-CANCEL runs if queued work is
cancelled before THUNK starts.  Disabled transport drops late work."
  (when mevedel-transport--enabled-p
    (if (mevedel-transport-busy-p path)
        (unless (gethash key mevedel-transport--pending)
          (puthash key
                   (run-at-time mevedel-transport-retry-seconds nil
                                #'mevedel-transport--retry key path thunk
                                on-cancel)
                   mevedel-transport--pending)
          (when on-cancel
            (puthash key on-cancel
                     mevedel-transport--pending-cancellers)))
      (remhash key mevedel-transport--pending)
      (remhash key mevedel-transport--pending-cancellers)
      (funcall thunk))))

(defun mevedel-transport-cancel-pending (&optional key)
  "Cancel deferred transport work for KEY, or all of it when KEY is nil."
  (if key
      (when-let* ((timer (gethash key mevedel-transport--pending)))
        (when (timerp timer) (cancel-timer timer))
        (remhash key mevedel-transport--pending)
        (when-let* ((on-cancel
                     (gethash key mevedel-transport--pending-cancellers)))
          (remhash key mevedel-transport--pending-cancellers)
          (ignore-errors (funcall on-cancel))))
    (maphash (lambda (_key timer)
               (when (timerp timer) (cancel-timer timer)))
             mevedel-transport--pending)
    (clrhash mevedel-transport--pending)
    (maphash (lambda (_key on-cancel)
               (ignore-errors (funcall on-cancel)))
             mevedel-transport--pending-cancellers)
    (clrhash mevedel-transport--pending-cancellers)))

(mevedel-transport-install)

(with-eval-after-load 'tramp
  (when mevedel-transport--enabled-p
    (mevedel-transport-install)))

(provide 'mevedel-transport)

;;; mevedel-transport.el ends here
