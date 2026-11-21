;;; niwa-transport.el -- Remote transport reentrancy state -*- lexical-binding: t -*-

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
;; `niwa-transport-busy-p' before target I/O, and defer through
;; `niwa-transport-run-when-idle' when it answers yes.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'tramp-cache))

(require 'niwa-utilities)

;; `tramp'
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-get-connection-process "tramp" (vec))
(declare-function tramp-get-connection-property
                  "tramp-cache" (key property &optional default))
(autoload 'tramp-dissect-file-name "tramp")
(autoload 'tramp-get-connection-process "tramp")
(declare-function tramp-set-connection-property
                  "tramp-cache" (key property value))
(autoload 'tramp-get-connection-property "tramp-cache")
(defvar tramp-current-connection)
(defvar tramp-dont-suspend-timers)


;;
;;; Customization

(defcustom niwa-transport-retry-seconds 0.05
  "Seconds before a deferred target operation re-tests the transport.

Nesting is always transient: the operation already in flight completes and
releases the stack.  The interval only decides how promptly deferred work
notices, so it trades a little latency against idle polling."
  :type 'number
  :group 'niwa)

(defcustom niwa-transport-remote-direct-async t
  "When non-nil, eligible remote spawns use a private TRAMP channel.
This covers remote Bash children and oversized control programs."
  :type 'boolean
  :group 'niwa)


;;
;;; Nesting depth

(defvar niwa-transport--depth 0
  "Count of TRAMP file-name handler frames below this point on the stack.

TRAMP holds its connection lock for one `accept-process-output' and one send,
and its wait loop yields the CPU with `sit-for' between them -- lock released,
timers live -- so a command is in flight far longer than the lock reports.
This counter spans the whole handler call instead.  It is therefore the only
signal that covers that window, and the only one that also sees remote
operations started by other packages, which is what a mode line or a version
control check does during redisplay.")

(defvar niwa-transport--enabled-p nil
  "Whether transport integration should return when TRAMP reloads.")

(defun niwa-transport-call-as-remote-operation (thunk)
  "Call THUNK counted as one remote operation in flight.

The frame is counted with a dynamic binding, so a THUNK that exits through
`throw', `keyboard-quit', or any signal still uncounts itself.  After the
outermost frame restores TRAMP's suspended timer lists, re-arm pending retries
and held continuations.  Remote I/O that
bypasses TRAMP's file-name handler, such as a direct process pipe, uses this so
that `niwa-transport-busy-p' sees it exactly as it sees a handler call."
  (let ((outermost (zerop niwa-transport--depth))
        (niwa-transport--depth (1+ niwa-transport--depth)))
    (unwind-protect (funcall thunk)
      (when outermost
        (niwa-transport--rearm-pending-timers)
        (niwa-transport--release-held-timers)))))

(defun niwa-transport--handler-advice (original &rest args)
  "Count one TRAMP handler frame around ORIGINAL applied to ARGS."
  (niwa-transport-call-as-remote-operation
   (lambda () (apply original args))))

(defun niwa-transport-call-with-spawn-channel (remote direct-async thunk)
  "Call THUNK, which spawns a process, through REMOTE's selected channel.

With DIRECT-ASYNC, TRAMP starts the process on its own channel instead of
through the connection's shell.  A list supplies the method's command
shell; t selects ssh.  With nil, force the classic spawn."
  (if (and remote (fboundp 'tramp-direct-async-process-p))
      (let ((shim (and direct-async
                       (not (fboundp 'tramp-ssh-controlmaster-options))
                       (fboundp 'tramp-ssh-or-plink-options))))
        (cl-letf (((symbol-function 'tramp-direct-async-process-p)
                   (if direct-async
                       (lambda (&rest _) t)
                     (lambda (&rest _) nil)))
                  ((symbol-function 'tramp-ssh-controlmaster-options)
                   (if shim
                       (symbol-function 'tramp-ssh-or-plink-options)
                     (symbol-function 'tramp-ssh-controlmaster-options))))
          (if direct-async
              (let ((vec (tramp-dissect-file-name remote)))
                (with-tramp-saved-connection-property vec "direct-async"
                  (tramp-set-connection-property vec "direct-async" direct-async)
                  (funcall thunk)))
            (funcall thunk))))
    (funcall thunk)))

(defun niwa-transport-install ()
  "Begin counting TRAMP handler frames."
  (setq niwa-transport--enabled-p t)
  (unless (advice-member-p #'niwa-transport--handler-advice
                           'tramp-file-name-handler)
    (advice-add 'tramp-file-name-handler :around
                #'niwa-transport--handler-advice))
  (when (boundp 'tramp-unload-hook)
    (add-hook 'tramp-unload-hook #'niwa-transport--detach)))

(defun niwa-transport--detach ()
  "Detach transport integration and cancel work that cannot safely run."
  (niwa-transport-cancel-pending)
  (advice-remove 'timer-event-handler #'niwa-transport--guard-deadline-timer)
  (advice-remove 'tramp-file-name-handler
                 #'niwa-transport--handler-advice)
  (setq niwa-transport--depth 0))

(defun niwa-transport-uninstall ()
  "Stop counting TRAMP handler frames and cancel deferred work.
Paced processes continue."
  (niwa-transport-resume-paused)
  (when niwa-transport--pause-guard
    (delete-process niwa-transport--pause-guard)
    (setq niwa-transport--pause-guard nil))
  (setq niwa-transport--enabled-p nil)
  (niwa-transport--detach)
  (when (boundp 'tramp-unload-hook)
    (remove-hook 'tramp-unload-hook #'niwa-transport--detach)))

(defun niwa-transport-unload-function ()
  "Remove transport integration before unloading the feature."
  (niwa-transport-uninstall)
  nil)

(defun niwa-transport-nested-p ()
  "Return non-nil when a TRAMP file operation is already on the stack."
  (> niwa-transport--depth 0))

(defvar niwa-transport--section nil
  "Non-nil while this process holds or is taking control locks.")

(defun niwa-transport-call-in-section (thunk)
  "Call THUNK as a durable section holding control locks.
Deferred work consults this, not `niwa-transport-busy-p': code inside the
section must still run its own target operations, while an unrelated timer
that fires in one of the section's waits must not queue behind the locks."
  (let ((niwa-transport--section t))
    (funcall thunk)))


;;
;;; Busy predicate

(defun niwa-transport-target (path)
  "Return PATH resolved for repeated busy checks, or nil when it is local.
A caller asking about one target per event resolves it once with this and
passes the result as PATH to `niwa-transport-busy-p'."
  (and (stringp path) (file-remote-p path) (tramp-dissect-file-name path)))

(defun niwa-transport--connection-locked-p (path)
  "Return non-nil when PATH's TRAMP connection holds its operation lock.
PATH is a file name or a target from `niwa-transport-target'.

This catches a caller that reached the connection without going through the
file-name handler, which is what TRAMP's own internal command senders do."
  (when-let* ((target (if (stringp path) (niwa-transport-target path) path))
              (process (tramp-get-connection-process target)))
    (and (process-live-p process)
         (tramp-get-connection-property process "locked")
         t)))

(defun niwa-transport-busy-p (&optional path)
  "Return non-nil when starting a target operation now would nest.

PATH names the target whose connection is examined, as a file name or as
resolved by `niwa-transport-target'; a local or absent PATH still
consults the handler depth, because that covers every connection.

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
  (or (niwa-transport-nested-p)
      (niwa-transport--connection-locked-p path)))


;;
;;; Exclusive use

(defun niwa-transport--unsuspend-timeouts (suspended elapsed)
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

(defvar niwa-transport--suspended-timers nil
  "Timers enclosing exclusive sections suspended, still armed on their lists.
Activating one of them again inside the section would give it a second copy
on the section's list; one that fires there leaves the suspended copy marked
triggered, which Emacs then skips forever.")

(defvar niwa-transport--deadline-timer nil
  "The only timer allowed to fire during a bounded exclusive operation.")

(defun niwa-transport--call-with-exclusive-connection (thunk)
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
        (deadline (car (memq niwa-transport--deadline-timer timer-list)))
        (scheduled nil)
        (scheduled-idle nil))
    (unwind-protect
        (let ((niwa-transport--suspended-timers
               (append timer-list niwa-transport--suspended-timers))
              (timer-list (and deadline (list deadline)))
              timer-idle-list)
          ;; A collection inside the section is paid twice: it lengthens
          ;; the section, and the section is the window where the
          ;; connection is held and foreign timers are stopped, so the
          ;; whole of Emacs waits out the collection.  A profiled remote
          ;; turn sat at the 800KB idle floor `gcmh' leaves behind and
          ;; spent 18% of its samples in automatic GC.
          (unwind-protect (niwa--with-gc-batched (funcall thunk))
            ;; Collect here: the outer lists come back when this `let' exits,
            ;; taking anything armed inside with them.
            (setq scheduled (delq deadline timer-list)
                  scheduled-idle timer-idle-list)))
      (niwa-transport--unsuspend-timeouts
       suspended (- (float-time) started))
      (dolist (timer scheduled)
        (ignore-errors (timer-activate timer)))
      (dolist (timer scheduled-idle)
        ;; `run-with-idle-timer' allows this idle period.  Reactivating with
        ;; the default marks it triggered and stalls an idle continuation
        ;; until another user command starts a new idle period.
        (ignore-errors
          (timer-activate-when-idle timer (not (timer--triggered timer))))))))

(defmacro niwa-transport-with-exclusive-connection (&rest body)
  "Run BODY without letting a timer start another remote operation.

`niwa-transport-busy-p' keeps this package from nesting inside somebody
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
the time BODY took, but cannot interrupt a synchronous TRAMP wait.

A timer BODY arms fires normally unless TRAMP temporarily suspends it.
Use `niwa-transport-call-with-deadline' to bound a synchronous TRAMP wait.

One limit BODY must respect: `cancel-timer' inside BODY cannot reach a timer
this macro suspended, because it is not on the bound list; that timer is
restored on exit as though the cancel never happened."
  (declare (indent 0) (debug t))
  `(niwa-transport--call-with-exclusive-connection (lambda () ,@body)))

(defvar niwa-transport--deadline-held nil
  "Timers that became due during a bounded exclusive operation.")

(defun niwa-transport--guard-deadline-timer (original timer)
  "Dispatch TIMER through ORIGINAL unless a private deadline holds it."
  (if (and niwa-transport--deadline-timer
           (not (eq timer niwa-transport--deadline-timer)))
      ;; Emacs marks a due timer triggered before dispatching it.  Leave it
      ;; on its queue so cancellation still works, but do not run its callback.
      (cl-pushnew timer niwa-transport--deadline-held)
    (funcall original timer)))

(defun niwa-transport-call-with-deadline (seconds thunk on-timeout)
  "Call THUNK exclusively with a private deadline of SECONDS.
Call ON-TIMEOUT after aborting a synchronous wait, or instead of returning
an overdue result.  Hold both pre-existing timers and timers armed by
process filters during the wait; only the private deadline can fire.
Use one deadline around the whole operation, rather than nesting deadlines."
  (niwa-transport-with-exclusive-connection
    (let* ((outermost (not niwa-transport--deadline-timer))
           (tag (make-symbol "transport-deadline"))
           (deadline (+ (float-time) seconds))
           (tramp-dont-suspend-timers t)
           (connection (bound-and-true-p tramp-current-connection))
           (niwa-transport--deadline-held nil)
           (niwa-transport--deadline-timer
            (run-at-time
             seconds nil
             (lambda ()
               (let ((current (bound-and-true-p tramp-current-connection)))
                 (throw tag (unless (eq current connection)
                              (tramp-get-connection-process (car current))))))))
           completed result)
      (unwind-protect
          (progn
            (when outermost
              (advice-add 'timer-event-handler :around
                          #'niwa-transport--guard-deadline-timer '((depth . -90))))
            (let ((interrupted (catch tag (setq result (funcall thunk) completed t))))
              (if (and completed (< (float-time) deadline))
                  result
                ;; The aborted request's reply must not reach the next command.
                (when (processp interrupted) (delete-process interrupted))
                (funcall on-timeout))))
        (when outermost
          (advice-remove 'timer-event-handler #'niwa-transport--guard-deadline-timer))
        (cancel-timer niwa-transport--deadline-timer)
        (dolist (timer niwa-transport--deadline-held)
          (setf (timer--triggered timer) nil))))))


;;
;;; Continuation timers

(defvar niwa-transport--held-timers nil
  "Timers requested inside a TRAMP handler, newest first, not yet armed.")

(defun niwa-transport-call-with-retained-timers (thunk)
  "Call THUNK without losing the timers it schedules inside a remote operation.
Use this around a process filter whose library schedules ordinary one-shot
continuations with `run-at-time'.  Inside a TRAMP handler frame those timers can
land on a temporarily bound `timer-list' and vanish with it; they are held and
armed once the outermost frame returns.  Outside one THUNK runs unchanged, so
its `cancel-timer' calls still reach the real timer list.  The shared
host timer of `niwa--ui-timer-activate' is left armed: it lives on the
top-level list, and holding it froze every coalesced callback, the
telemetry heartbeat included, for the whole remote operation."
  (if (not (niwa-transport-nested-p))
      (funcall thunk)
    (let ((before (copy-sequence timer-list)))
      (unwind-protect (funcall thunk)
        (dolist (timer timer-list)
          (unless (or (memq timer before)
                      (eq timer niwa--coalesced-timer))
            (cancel-timer timer)
            (push timer niwa-transport--held-timers)))))))

(defun niwa-transport-run-at-time (seconds function &rest args)
  "Call FUNCTION with ARGS once, SECONDS from now, even from inside TRAMP.

A process filter or sentinel can run inside a remote command while TRAMP has
let-bound `timer-list' to nil, and a timer `run-at-time' arms there is
discarded with that binding: whatever continuation it carried never runs.
Inside a TRAMP handler frame the timer is therefore held and armed only once
the outermost frame has returned and the real list is back, which also keeps
FUNCTION from running nested inside the remote command.  Outside one this is
plain `run-at-time'.  Return the timer.

A held timer cancelled before it is armed still fires once, as a suspended
timer does after `niwa-transport-with-exclusive-connection', so FUNCTION
must check that its work is still current."
  (if (not (niwa-transport-nested-p))
      (apply #'run-at-time seconds nil function args)
    (let ((timer (timer-create)))
      (timer-set-time timer (time-add nil seconds))
      (timer-set-function timer function args)
      (push timer niwa-transport--held-timers)
      timer)))

(defun niwa-transport-held-timer-p (timer)
  "Return non-nil when TIMER is held for arming after the TRAMP handler."
  (and (memq timer niwa-transport--held-timers) t))

(defun niwa-transport--release-held-timers ()
  "Arm the timers held while a TRAMP handler was on the stack, oldest first."
  (let ((held (nreverse niwa-transport--held-timers)))
    (setq niwa-transport--held-timers nil)
    (dolist (timer held)
      (timer-activate timer))))


;;
;;; Output pacing

(defvar niwa-transport--paused nil
  "Local processes stopped by `niwa-transport-pause-process'.")

(defvar niwa-transport--pause-guard nil
  "Process that continues paused processes once this Emacs is gone.")

(defconst niwa-transport--pause-guard-script
  "trap '' HUP; echo; while read -r pids; do last=$pids; done; kill -CONT $last 2>/dev/null"
  "Shell program of `niwa-transport--pause-guard'.
Each line it reads lists the process IDs stopped at that moment.  Emacs
starts every child in its own session, so no kernel rule continues a
stopped child when Emacs dies, however it dies; the guard's input pipe
closes then, and it continues the last listed set.  Emacs hangs up its
children on a normal exit, so the guard ignores SIGHUP and announces
that with an empty line: hung up before that, it would die first.")

(defun niwa-transport--report-paused ()
  "Hand the paused process IDs to the guard, starting it on first use.
Return nil when the guard cannot be reached: nothing may stop then."
  (condition-case nil
      (let ((guard niwa-transport--pause-guard))
        (unless (process-live-p guard)
          (setq guard (let ((default-directory "/"))
                        (make-process
                         :name "niwa-pause-guard"
                         :command (list "/bin/sh" "-c"
                                        niwa-transport--pause-guard-script)
                         :connection-type 'pipe :noquery t
                         :filter #'ignore :sentinel #'ignore))
                niwa-transport--pause-guard guard)
          (unless (accept-process-output guard 1 nil 1)
            (delete-process guard)
            (error "Pause guard did not start")))
        (process-send-string
         guard (concat (mapconcat (lambda (process)
                                    (number-to-string (process-id process)))
                                  niwa-transport--paused " ")
                       "\n"))
        t)
    (error nil)))

(defconst niwa-transport--max-pause 0.5
  "Longest pause, whatever its caller's batch delay.
A long display batch must not stall the producing process.")

(defun niwa-transport-pause-process (process seconds)
  "Stop local PROCESS for SECONDS so its output arrives in one burst.
Emacs redisplays after every read of process output, and a pgtk frame
repaints its whole surface each time, so a stream's cost follows its
chunk rate.  Stopped, the process leaves its input waiting -- in a socket
or another process's pipe -- and Emacs reads what accumulated at once.
Only a real subprocess is stopped, on a system with job control, and
only once `niwa-transport--pause-guard' knows it: a stopped child
outlives a crashed Emacs.  The caller resumes it early with
`niwa-transport-resume-process' before writing to it or when its
output is complete.  Return non-nil when PROCESS was stopped."
  (when (and (numberp seconds) (> seconds 0)
             (not (memq system-type '(windows-nt ms-dos)))
             (eq (process-type process) 'real)
             (eq (process-status process) 'run)
             (not (niwa--timer-pending-p
                   (process-get process 'niwa-transport--resume-timer))))
    (push process niwa-transport--paused)
    (if (not (niwa-transport--report-paused))
        (progn (pop niwa-transport--paused) nil)
      (signal-process process 'SIGSTOP)
      (process-put process 'niwa-transport--resume-timer
                   (niwa-transport-run-at-time
                    (min seconds niwa-transport--max-pause)
                    #'niwa-transport-resume-process process))
      t)))

(defun niwa-transport-process-paused-p (process)
  "Return non-nil while `niwa-transport-pause-process' holds PROCESS."
  (and (memq process niwa-transport--paused) t))

(defun niwa-transport-resume-process (process)
  "Continue PROCESS if `niwa-transport-pause-process' stopped it."
  (when-let* ((timer (process-get process 'niwa-transport--resume-timer)))
    (cancel-timer timer)
    (process-put process 'niwa-transport--resume-timer nil))
  (when (memq process niwa-transport--paused)
    (setq niwa-transport--paused (delq process niwa-transport--paused))
    ;; Not by status: a stop Emacs has yet to notice still needs it.
    (when (process-live-p process)
      (signal-process process 'SIGCONT))
    ;; A stale ID could name an unrelated process by the time Emacs dies.
    (niwa-transport--report-paused)))

(defun niwa-transport-resume-paused (&optional within)
  "Continue every paced process, or those due to continue WITHIN seconds."
  (let ((until (and within (+ (float-time) within))))
    (dolist (process (copy-sequence niwa-transport--paused))
      (when (or (null until)
                (when-let* ((timer (process-get
                                    process 'niwa-transport--resume-timer)))
                  (<= (float-time (timer--time timer)) until)))
        (niwa-transport-resume-process process)))))


;;
;;; Deferral

(defvar niwa-transport--pending (make-hash-table :test #'equal)
  "Deferred work keyed by coalescing key.
Each entry is (TIMER . ON-CANCEL), keeping the retry and its optional
cancellation callback under one owner.")

(defun niwa-transport--rearm-pending-timers ()
  "Re-arm retry timers dropped by TRAMP's suspended timer binding.

The outermost file-name handler calls this after TRAMP has restored the
real timer list.  Pending entries still name their discarded timer, so
they can be activated without duplicating or losing the deferred work."
  (maphash
   (lambda (_key entry)
     (let ((timer (car entry)))
       (when (and (timerp timer) (not (niwa--timer-pending-p timer)))
         (timer-activate timer))))
   niwa-transport--pending))

(defun niwa-transport--retry (timer key path thunk on-cancel)
  "Re-attempt KEY's THUNK for PATH only while TIMER owns the pending entry.
Preserve ON-CANCEL when the retry must wait again.  A cancelled timer can
still be delivered after a suspended timer list is restored."
  (when (eq timer (car (gethash key niwa-transport--pending)))
    (remhash key niwa-transport--pending)
    (niwa-transport-run-when-idle key path thunk on-cancel)))

;; Every deferred continuation runs through this one timer function, so
;; event-loop lag telemetry names the THUNK argument it runs instead.
(put 'niwa-transport--retry 'niwa-telemetry-delegate 3)

(defun niwa-transport-run-when-idle (key path thunk &optional on-cancel delay)
  "Call THUNK once PATH's transport is idle and no control locks are held here.

Control locks held by a durable section of this process count as busy, so
work a timer starts inside the section's waits queues instead of blocking
on those locks.

KEY coalesces repeated scheduling of the same logical work, so a caller that
re-arms on every event queues one retry rather than a growing fan of timers.
THUNK runs immediately when the transport is already idle, because a filter or
sentinel is only unsafe when it nests.  ON-CANCEL runs if queued work is
cancelled before THUNK starts. Non-nil DELAY queues the first attempt by that
many seconds even when transport is idle, retaining the same cancellation owner.
Return non-nil when work was accepted; disabled transport drops late work."
  (when niwa-transport--enabled-p
    (if (or delay (niwa-transport-busy-p path) niwa-transport--section)
        (unless (gethash key niwa-transport--pending)
          (let ((timer (timer-create)))
            (timer-set-time timer (time-add nil (or delay niwa-transport-retry-seconds)))
            (timer-set-function timer #'niwa-transport--retry
                                (list timer key path thunk on-cancel))
            (timer-activate timer)
            (puthash key (cons timer on-cancel) niwa-transport--pending)))
      ;; An idle call fulfills the coalesced work with its current thunk.
      ;; Retire the old timer without notifying cancellation: the work runs.
      (when-let* ((timer (car (gethash key niwa-transport--pending))))
        (when (timerp timer) (cancel-timer timer)))
      (remhash key niwa-transport--pending)
      (funcall thunk))
    t))

(defvar niwa-transport--background-running nil
  "Non-nil while a scheduled background opportunity is executing.")

(defvar niwa-transport--background-resume-at 0
  "Earliest client time for the next scheduled background opportunity.")

(defun niwa-transport-schedule-idle (table key tag path thunk)
  "Run THUNK once for KEY after the current command, when PATH's transport is idle.
TABLE maps KEY to its pending timer until THUNK starts or the work is
cancelled, so repeated calls coalesce into one opportunity.  TAG names the
transport queue entry `(TAG KEY)'.  A superseded timer, a cancelled queue
entry, or a disabled transport removes KEY without calling THUNK.  Return the
timer, or nil when KEY already has pending work. Already-due background jobs
leave at least 10 ms between calls and wait for pending input instead of
running as one batch."
  (unless (gethash key table)
    (let (timer)
      (cl-labels
          ((forget ()
             (when (eq timer (gethash key table)) (remhash key table)))
           (run ()
             (when (eq timer (gethash key table))
               (if (or niwa-transport--background-running
                       (input-pending-p)
                       (< (float-time) niwa-transport--background-resume-at))
                   (progn
                     (timer-set-time timer (time-add nil .01))
                     (timer-activate timer))
                 (forget)
                 (unwind-protect
                     (let ((niwa-transport--background-running t)) (funcall thunk))
                   (setq niwa-transport--background-resume-at (+ (float-time) .01))))))
           (attempt ()
             (when (eq timer (gethash key table))
               (unless (niwa-transport-run-when-idle
                        (list tag key) path #'run #'forget)
                 (forget)))))
        (setq timer (niwa-transport-run-at-time 0 #'attempt))
        (puthash key timer table)
        timer))))

(defun niwa-transport-cancel-idle (table tag)
  "Cancel every timer in TABLE and its queued `(TAG KEY)' transport work."
  (let (keys)
    (maphash (lambda (key timer)
               (cancel-timer timer)
               (push key keys))
             table)
    (clrhash table)
    (dolist (key keys)
      (niwa-transport-cancel-pending
       (list tag key)))))

(defun niwa-transport-cancel-pending (&optional key)
  "Cancel deferred transport work for KEY, or all of it when KEY is nil."
  (if key
      (when-let* ((entry (gethash key niwa-transport--pending)))
        (when (timerp (car entry)) (cancel-timer (car entry)))
        (remhash key niwa-transport--pending)
        (when-let* ((on-cancel (cdr entry)))
          (ignore-errors (funcall on-cancel))))
    (let (cancellers)
      (maphash (lambda (_key entry)
                 (when (timerp (car entry)) (cancel-timer (car entry)))
                 (when (cdr entry) (push (cdr entry) cancellers)))
               niwa-transport--pending)
      ;; Retire the entire batch before callbacks can schedule replacement
      ;; work, including a new canceller under the same key.
      (clrhash niwa-transport--pending)
      (dolist (on-cancel cancellers)
        (ignore-errors (funcall on-cancel))))))

(niwa-transport-install)

(with-eval-after-load 'tramp
  (when niwa-transport--enabled-p
    (niwa-transport-install)))

(provide 'niwa-transport)

;;; niwa-transport.el ends here
