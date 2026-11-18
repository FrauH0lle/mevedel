;;; mevedel-session-durability.el -- Portable session durability -*- lexical-binding: t -*-

;;; Commentary:

;; Owns project and file session leases and the storage primitives used by
;; durable session publication.  Legacy pre-lease `.lock' files are refused.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-structs))
(require 'seq)

;; Every control operation in this file runs through the session control
;; filesystem, so its feature is a hard load-time dependency rather than a
;; lazily reachable one.
(require 'mevedel-session-control-fs)
(require 'mevedel-utilities)

;; `files'
(defvar remote-file-name-inhibit-cache)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-identity
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))

;; `mevedel-journal-pins'
(declare-function mevedel-journal-pins-present-p "mevedel-journal-pins" (session-dir))
(autoload 'mevedel-journal-pins-present-p "mevedel-journal-pins")

;; `mevedel-session-collection'
(declare-function mevedel-session-collection-cancel
                  "mevedel-session-collection" (session))

;; `mevedel-session-control-fs'
(declare-function mevedel-session-control-fs-create-file
                  "mevedel-session-control-fs"
                  (path content &optional coding-system))
(declare-function mevedel-session-control-fs-delete-file
                  "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-directory-p
                  "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-list-directory
                  "mevedel-session-control-fs" (directory regexp))
(declare-function mevedel-session-control-fs-make-directory
                  "mevedel-session-control-fs" (path &optional parents))
(declare-function mevedel-session-control-fs-path-exists-p
                  "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-physical-path
                  "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-read-file
                  "mevedel-session-control-fs"
                  (path &optional coding-system max-bytes))
(declare-function mevedel-session-control-fs-target-time
                  "mevedel-session-control-fs" (directory))
(declare-function mevedel-session-control-fs-write-file
                  "mevedel-session-control-fs"
                  (path content &optional coding-system))

;; `mevedel-session-control-transfer'
(declare-function mevedel-session-control-transfer-taken-over
                  "mevedel-session-control-transfer" (session lease))
(autoload 'mevedel-session-control-transfer-taken-over
  "mevedel-session-control-transfer")

;; `mevedel-session-publication'
(declare-function mevedel-session-publication--record-pending
                  "mevedel-session-publication" (session batches err))
(declare-function mevedel-session-publication-clear-transient
                  "mevedel-session-publication" (session))
(declare-function mevedel-session-publication-logical-path-p
                  "mevedel-session-publication" (path))
(declare-function mevedel-session-publication-valid-head-p
                  "mevedel-session-publication" (head))
(autoload 'mevedel-session-publication--record-pending
  "mevedel-session-publication")
(autoload 'mevedel-session-publication-clear-transient
  "mevedel-session-publication")
(autoload 'mevedel-session-publication-logical-path-p
  "mevedel-session-publication")
(autoload 'mevedel-session-publication-valid-head-p
  "mevedel-session-publication")
;; Cold lease acquisition loads publication before it can arm a heartbeat.
(declare-function mevedel-session-publication-barrier
                  "mevedel-session-publication" (session &optional token))
(declare-function mevedel-session-publication-busy-p
                  "mevedel-session-publication" (session))
(declare-function mevedel-session-publication-settle-taken-over
                  "mevedel-session-publication" (session))
(declare-function mevedel-session-publication--schedule
                  "mevedel-session-publication" (session))

;; `mevedel-session-recovery'
(declare-function mevedel-session-recovery--root
                  "mevedel-session-recovery" (session-dir))
(declare-function mevedel-session-recovery-read "mevedel-session-recovery" (session-dir &optional root-observation))
(declare-function mevedel-session-recovery-refresh
                  "mevedel-session-recovery" (session &optional root-observation))
(autoload 'mevedel-session-recovery--root "mevedel-session-recovery")
(autoload 'mevedel-session-recovery-read "mevedel-session-recovery")
(autoload 'mevedel-session-recovery-refresh "mevedel-session-recovery")

;; `mevedel-session-transfer'
(declare-function mevedel-session-transfer--path
                  "mevedel-session-transfer" (directory name))
(declare-function mevedel-session-transfer--requests-operation
                  "mevedel-session-transfer" (directory))
(declare-function mevedel-session-transfer--valid-request-p
                  "mevedel-session-transfer" (record &optional now))
(declare-function mevedel-session-transfer-release-fence
                  "mevedel-session-transfer" (directory generation))
(autoload 'mevedel-session-transfer--path "mevedel-session-transfer")
(autoload 'mevedel-session-transfer--requests-operation
  "mevedel-session-transfer")
(autoload 'mevedel-session-transfer--valid-request-p
  "mevedel-session-transfer")
(autoload 'mevedel-session-transfer-release-fence
  "mevedel-session-transfer")

;; `mevedel-structs'
(declare-function mevedel-session-authority-mode-for-session
                  "mevedel-structs" (session))
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-lease "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-lease-renewal-timer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-lease-renewal-job
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-control-transfer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-active-p
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-job
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-preparation-token
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-queue
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-uncommitted-batches
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-stopping-p
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-root-buffer "mevedel-structs" (cl-x) t)
(declare-function mevedel-session--create "mevedel-structs" (&rest slots))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-transport'
(declare-function mevedel-transport-busy-p
                  "mevedel-transport" (&optional path))
(declare-function mevedel-transport-run-when-idle
                  "mevedel-transport"
                  (key path thunk &optional on-cancel delay))

;; `mevedel-utilities'
(declare-function mevedel--truncate-display
                  "mevedel-utilities" (text width &optional ellipsis))
(declare-function mevedel-diagnostic-scope "mevedel-utilities" (owner))
(declare-function mevedel-diagnostic-warning "mevedel-utilities" (scope type message &optional level buffer-name))
(autoload 'mevedel--truncate-display "mevedel-utilities")
(autoload 'mevedel-diagnostic-scope "mevedel-utilities")
(autoload 'mevedel-diagnostic-warning "mevedel-utilities")


;; `mevedel-workspace'
(declare-function mevedel-workspace-state-dir "mevedel-workspace" (workspace))
(autoload 'mevedel-workspace-state-dir "mevedel-workspace")

;;
;;; Customization

(defcustom mevedel-session-lease-seconds 90
  "Seconds for which one portable session lease grants mutation authority.

Renewal runs from a timer, which cannot fire while a durable action is inside
blocking target I/O, so a slow remote action can outlive the lease.  The
owning client then reclaims its own expired lease without a prompt; raise this
only to widen the window in which another client may not take over.  Expiry is
measured with the target clock at whole-second resolution."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-session-lease-renewal-seconds 30
  "Seconds between portable session lease renewal attempts."
  :type 'integer
  :group 'mevedel)

(cl-defstruct (mevedel-session-durability--heartbeat
               (:constructor mevedel-session-durability--heartbeat-create))
  session shadow path lease buffer cancelled)

(defvar mevedel-session-durability--heartbeat-inhibit nil
  "Non-nil while a synchronous control operation excludes heartbeat ticks.")

(defvar mevedel-session-durability--async-valid-p nil
  "Optional predicate fencing an asynchronous lease operation's continuations.")

(defcustom mevedel-session-publication-lease-seconds 3600
  "Seconds reserved for each uninterrupted publication artifact operation.

The serialized publisher renews this window before and after every artifact.
A single target filesystem operation that outlasts it loses mutation authority
and the publication fails closed at the next ownership check."
  :type 'integer
  :group 'mevedel)


;;
;;; Target disclosure and leases

(defvar mevedel-session-durability--client-id
  (secure-hash
   'sha256
   (format "%S"
           (list (current-time) (random most-positive-fixnum)
                 (emacs-pid) (system-name))))
  "Opaque identity of this live mevedel client.")

(defun mevedel-session-durability--client-host ()
  "Return this client's bounded display host name.

The value is written into durable lease records, so it is trimmed to the
same shape a transfer label must satisfy: one line, no control characters,
short enough that a picker row stays readable."
  (let ((name (replace-regexp-in-string
               "[[:cntrl:]]" "" (or (system-name) ""))))
    (when (string-match-p "\\S-" name)
      (mevedel--truncate-display (string-trim name) 64))))

(defun mevedel-session-durability-holder-label (label)
  "Return LABEL as plain display text of at most 80 characters.
Strip properties, control and format characters without changing lease records."
  (let ((text (concat
               (seq-remove
                (lambda (char)
                  (memq (get-char-code-property char 'general-category) '(Cc Cf)))
                (substring-no-properties (format "%s" label))))))
    (substring text 0 (min 80 (length text)))))

(defun mevedel-session-durability--pid-alive-p (pid)
  "Return non-nil unless PID is provably absent from the current host.
An unavailable process list is not evidence that a holder is dead."
  ;; Process inspection follows `default-directory' through file handlers.
  ;; These PIDs belong to the Emacs host, not a remote workspace.
  (let ((default-directory "/"))
    (condition-case nil
        (let ((pids (list-system-processes)))
          (or (not (and (integerp pid) (> pid 0)))
              (null pids)
              (memq pid pids)))
      (error t))))

(defvar mevedel-session-durability--host-cache nil
  "Cons of the PID that computed `mevedel-session-durability--host' and its value.
Keyed by PID so a dumped or forked image never reuses its creator's identity.")

(defun mevedel-session-durability--host ()
  "Return this Emacs host's boot, machine and PID-namespace identity, or nil.
Only within one such identity can a recorded PID be checked for liveness."
  (unless (eql (car mevedel-session-durability--host-cache) (emacs-pid))
    (setq mevedel-session-durability--host-cache
          (cons (emacs-pid)
                (let ((default-directory "/"))
                  (ignore-errors
                    (let ((namespace (file-symlink-p "/proc/self/ns/pid"))
                          (read (lambda (file)
                                  (with-temp-buffer
                                    (insert-file-contents file)
                                    (string-trim (buffer-string))))))
                      (and (stringp namespace)
                           (format "%s/%s/%s"
                                   (funcall read "/proc/sys/kernel/random/boot_id")
                                   (funcall read "/etc/machine-id")
                                   namespace))))))))
  (cdr mevedel-session-durability--host-cache))

(defun mevedel-session-durability--process-holder (pid)
  "Return this user's PID as a host-scoped holder identity, or nil.
Nil means the identity is unprovable on this host."
  (when-let* ((host (mevedel-session-durability--host))
              (process (mevedel--process-identity pid)))
    (list :host host :uid (user-uid) :pid pid :process process)))

(defvar mevedel-session-durability--holder-cache nil
  "Cons of this Emacs's PID and its holder identity.")

(defun mevedel-session-durability--holder ()
  "Return this client's holder identity, recorded in every lease it writes."
  (unless (eql (car mevedel-session-durability--holder-cache) (emacs-pid))
    (setq mevedel-session-durability--holder-cache
          (cons (emacs-pid)
                (mevedel-session-durability--process-holder (emacs-pid)))))
  (cdr mevedel-session-durability--holder-cache))

(defun mevedel-session-durability--holder-dead-p (lease)
  "Return non-nil when LEASE's holder process provably died on this host.
The record must name this host's boot, machine and PID namespace and this
user, and its PID must be gone or reused.  The process list is complete only
for one's own processes: `hidepid' or `ProtectProc' hide other users'.
Missing, foreign or unreadable evidence is uncertain liveness and returns
nil, so such leases wait for expiry."
  (let* ((holder (plist-get lease :holder))
         (holder (and (proper-list-p holder) holder))
         (pid (plist-get holder :pid))
         (process (plist-get holder :process))
         (host (mevedel-session-durability--host)))
    (and host
         (equal host (plist-get holder :host))
         (eql (user-uid) (plist-get holder :uid))
         ;; 4194304 is Linux's largest pid_max; a larger PID is forged.
         (natnump pid) (> pid 0) (<= pid 4194304)
         ;; Malformed identity evidence cannot prove reuse of a live PID.
         (stringp process)
         (let ((case-fold-search nil))
           (string-match-p mevedel--process-identity-regexp
                           process))
         ;; The identity must come from the boot the host names.
         (string-prefix-p (concat (car (split-string host "/")) ":") process)
         (or (not (mevedel-session-durability--pid-alive-p pid))
             (let ((current (mevedel--process-identity pid)))
               (and current (not (equal current process))))))))

(defcustom mevedel-session-durability-accept-target-storage nil
  "Non-nil stores project state on remote targets without asking first.
Before its first write to a remote execution target, mevedel asks once
per target and Emacs process whether to keep the project's state there.
A host nobody sits at, such as an Emacs daemon serving collaboration
guests, cannot answer the question; set this there to accept in
advance."
  :type 'boolean
  :group 'mevedel)

(defvar mevedel-session-durability--disclosed-targets
  (make-hash-table :test #'equal)
  "Execution targets disclosed to the user in this Emacs process.")


(defun mevedel-session-durability-disclose (session)
  "Confirm SESSION's target-side durable storage before its first write.

The acknowledgement is once per target for this Emacs process, or given
in advance by `mevedel-session-durability-accept-target-storage'.  Local
project sessions need no disclosure."
  (let ((target (mevedel-session-execution-target session)))
    (when (and target (mevedel-execution-target-remote-p target))
      (let ((key (mevedel-execution-target-identity target)))
        (unless (gethash key mevedel-session-durability--disclosed-targets)
          (unless
              (or mevedel-session-durability-accept-target-storage
                  (yes-or-no-p
               (format
                (concat
                 "Store this project's mevedel state on the target at %s? "
                 "This includes conversations, permissions, agent state, "
                 "checkpoints, snapshots, plans, durable tool results, and "
                 "required logs. The data is not encrypted by mevedel. ")
                (mevedel-workspace-state-dir
                 (mevedel-session-workspace session)))))
            (user-error "Portable project session storage was not accepted"))
          (puthash key t mevedel-session-durability--disclosed-targets)))))
  t)

(defun mevedel-session-durability--lease-path (session-dir)
  "Return SESSION-DIR's portable lease path."
  (let* ((session-dir (mevedel-session-control-fs-physical-path
                       session-dir))
         (path (file-name-concat session-dir ".lease")))
    (mevedel-session-control-fs-physical-path path)))

(defvar mevedel-session-durability--observed-time nil
  "Target clock seconds already observed for the current transaction, or nil.

A batched observation reads the target clock in the same process as the lease
records it inspects, so the value is target-authoritative and needs no second
round trip.  It is still delivered through
`mevedel-session-durability--target-time', which stays the single seam for
the target clock.")

(defconst mevedel-session-durability--clock-reuse-seconds 1
  "How long one observed target clock reading may answer for a transaction.

Every reading is the target's own, never a client clock.  Reusing one for at
most this long keeps a transaction that renews its lease several times from
paying a round trip per reading, while bounding how stale a reading can be to
less than the whole-second resolution the deadlines are written at.  Local
elapsed time only decides when to read the target again; it never becomes a
time value.")

(defvar mevedel-session-durability--transaction-clock nil
  "Cons cell caching one transaction's target clock readings, or nil.

Its car maps control directories to (TARGET-SECONDS . LOCAL-FLOAT-TIME).
Nested transactions on different targets must not share a clock.  A caller
that spans one durable transaction binds this to a fresh `(list nil)';
outside such a binding every reading reaches the target.")

(define-error 'mevedel-session-durability-prelude-failed
  "Operations preceding a lease commit in its program failed")

(defvar mevedel-session-durability--commit-riders nil
  "Operations the next lease commit carries in its own program, or nil.

A plist (:before OPS :after OPS :consumed nil :before-results nil
:after-results nil), mutated in place by the first commit that runs, which
then marks it consumed; later commits under the same binding carry nothing.
BEFORE runs ahead of the commit's `verify', AFTER behind its listing, so each
keeps the order separate programs had, minus their process starts.  When a
non-optional BEFORE operation fails, nothing of the commit ran and the
commit signals `mevedel-session-durability-prelude-failed' with the BEFORE
results.  AFTER results are left for the binder, which must handle a
`skipped' one: the commit's own proof can stop the program.")

(defun mevedel-session-durability--note-target-time (directory seconds)
  "Record SECONDS as this transaction's clock reading for DIRECTORY."
  (when (and mevedel-session-durability--transaction-clock (numberp seconds))
    (setf (alist-get directory (car mevedel-session-durability--transaction-clock)
                     nil nil #'equal)
          (cons seconds (float-time))))
  seconds)

(defun mevedel-session-durability--target-time (directory)
  "Return the target filesystem's current time for control DIRECTORY.

The modification timestamp of a freshly created marker is supplied by the
target filesystem, including through TRAMP.  It is therefore shared by
clients whose local wall clocks disagree.  Failure to obtain that timestamp
fails closed instead of falling back to a client clock."
  (or mevedel-session-durability--observed-time
      (let ((cached (alist-get directory
                               (car mevedel-session-durability--transaction-clock)
                               nil nil #'equal)))
        (and cached
             (< (- (float-time) (cdr cached))
                mevedel-session-durability--clock-reuse-seconds)
             (car cached)))
      (mevedel-session-durability--note-target-time
       directory (mevedel-session-control-fs-target-time directory))))

(defvar mevedel-session-durability--asserted-directories nil
  "Cons cell listing directories proved free of legacy `.lock' files, or nil.

A portable session cannot grow a `.lock' while this client holds its lease,
and one transaction reaches several entry points that each prove its absence.
A caller that spans one transaction binds this to a fresh `(list nil)';
outside such a binding every call proves it against the target.")

;; `mevedel-session-recovery' declares this cache; the transaction macro
;; below binds it together with this file's own transaction scopes.
(defvar mevedel-session-recovery--mutation-cache)

(defmacro mevedel-session-durability-with-transaction (&rest body)
  "Run BODY as one durable transaction sharing clock, probe, and recovery reads.

The target clock reading, legacy `.lock' absence proofs, and the recovery
mutation lookups repeat across the entry points one user action reaches;
inside this scope each is paid once.  The bindings nest: an inner
transaction joins the outer one instead of starting fresh, and the reads
are tolerant so the scope works before the lazily loaded modules that own
the variables are in.  `dlet' keeps the bindings dynamic in callers that do
not declare the variables; a lexical binding would silently share nothing."
  (declare (indent 0) (debug t))
  `(dlet ((mevedel-session-durability--transaction-clock
          (or (bound-and-true-p mevedel-session-durability--transaction-clock)
              (list nil)))
         (mevedel-session-durability--asserted-directories
          (or (bound-and-true-p
               mevedel-session-durability--asserted-directories)
              (list nil)))
         (mevedel-session-recovery--mutation-cache
          (or (bound-and-true-p mevedel-session-recovery--mutation-cache)
              (list nil))))
     ,@body))

(defun mevedel-session-durability--assert-no-pid-lock (session-dir)
  "Signal when portable SESSION-DIR contains a legacy pre-lease `.lock'."
  (setq session-dir (mevedel-session-control-fs-physical-path
                     session-dir))
  (unless (member session-dir
                  (car mevedel-session-durability--asserted-directories))
    (when (mevedel-session-control-fs-path-exists-p
           (file-name-concat session-dir ".lock"))
      (error "Pre-lease session requires migration with scripts/migrate-sessions.el: %s"
             session-dir))
    (when mevedel-session-durability--asserted-directories
      (push session-dir
            (car mevedel-session-durability--asserted-directories)))))

(defun mevedel-session-durability--pid-lock-operation (session-dir)
  "Return the program operation proving no legacy `.lock' in SESSION-DIR.

Nil means this transaction already proved it.  A caller that runs the
operation inside its own program settles it with
`mevedel-session-durability--settle-pid-lock', which raises exactly as
`mevedel-session-durability--assert-no-pid-lock' would."
  (let ((physical (mevedel-session-control-fs-physical-path session-dir)))
    (unless (member physical
                    (car mevedel-session-durability--asserted-directories))
      (list :op 'path-exists-p
            :path (file-name-concat physical ".lock")
            :optional t))))

(defun mevedel-session-durability--settle-pid-lock (session-dir result)
  "Interpret RESULT of SESSION-DIR's legacy `.lock' absence operation."
  (pcase (plist-get result :status)
    ('absent (mevedel-session-durability-note-no-pid-lock session-dir))
    ('ok (error "Pre-lease session requires migration with scripts/migrate-sessions.el: %s"
                (mevedel-session-control-fs-physical-path session-dir)))
    (_ (mevedel-session-control-fs-program-value result))))

(defun mevedel-session-durability--clock-operation (directory)
  "Return an operation reading DIRECTORY's target clock for this transaction.

Nil outside a transaction, where the reading would have nowhere to be kept,
and while the transaction's reading is still fresh.  The reading comes from
the target in the same process as the caller's other operations, so it is as
authoritative as a separate clock program."
  (and mevedel-session-durability--transaction-clock
       (not (mevedel-session-durability--target-time-cached-p directory))
       (list :op 'target-time :path directory :optional t)))

(defun mevedel-session-durability--settle-clock (directory result)
  "Record RESULT of DIRECTORY's clock operation when the target answered."
  (when (eq 'ok (plist-get result :status))
    (mevedel-session-durability--note-target-time
     directory (plist-get result :value))))

(defun mevedel-session-durability-note-no-pid-lock (session-dir)
  "Record SESSION-DIR as already proved free of a legacy pre-lease `.lock'.

A caller that probes many candidates at once -- enumerating a workspace
probes every one of them in a single program -- seeds its proofs here, so
the per-entry assertions inside
`mevedel-session-durability-with-transaction' cost no further round trip.
Outside such a scope there is nowhere to record the proof and this does
nothing, which leaves the assertion proving it against the target."
  (when mevedel-session-durability--asserted-directories
    (let ((physical (mevedel-session-control-fs-physical-path session-dir)))
      (unless (member physical
                      (car mevedel-session-durability--asserted-directories))
        (push physical
              (car mevedel-session-durability--asserted-directories))))))

(defun mevedel-session-durability--read-plist (path)
  "Return the plist stored at PATH, or nil when PATH is absent or unreadable.

Control records live on the target, so a truncated or planted record must
read as \"nothing usable here\" rather than raising out of every lease
operation.  Callers skip a nil record and keep looking, and every record they
accept still passes its own validator."
  (setq path (mevedel-session-control-fs-physical-path path))
  (condition-case nil
      (with-temp-buffer
        (insert (mevedel-session-control-fs-read-file path))
        (goto-char (point-min))
        (read (current-buffer)))
    (mevedel-session-control-fs-absent nil)
    (end-of-file nil)
    (invalid-read-syntax nil)))

(defun mevedel-session-durability--write-plist (path plist)
  "Atomically replace PATH with PLIST through a nearby temporary file."
  (mevedel-session-control-fs-write-file
   (mevedel-session-control-fs-physical-path path)
   (mevedel-session-durability--record-bytes plist)))

(defun mevedel-session-durability--create-plist (path plist)
  "Atomically create PATH with PLIST, returning non-nil on success."
  (mevedel-session-control-fs-create-file
   (mevedel-session-control-fs-physical-path path)
   (mevedel-session-durability--record-bytes plist)))

(defun mevedel-session-durability--valid-relative-path-p (path)
  "Return non-nil when PATH is a normalized session-relative file path."
  (and (stringp path)
       (not (string-empty-p path))
       (not (file-name-absolute-p path))
       (not (file-remote-p path))
       (not (string-prefix-p "~" path))
       (not (string-suffix-p "/" path))
       (not (string-match-p "//" path))
       (not (string-match-p
             "\\(?:\\`\\|/\\)\\.\\.?\\(?:/\\|\\'\\)" path))))

(defun mevedel-session-durability--portable-session-p (session)
  "Return non-nil when SESSION has portable durable workspace authority."
  (eq (mevedel-session-authority-mode-for-session session) 'portable))

(defun mevedel-session-durability--lease-record
    (buffer-name generation &optional status publication-head
                 unsettled-mutation now transfer-generation)
  "Return a fresh lease record for BUFFER-NAME and GENERATION.
STATUS defaults to `active'.  PUBLICATION-HEAD is an immutable manifest path.
UNSETTLED-MUTATION records target mutation whose outcome is not yet proved.
TRANSFER-GENERATION identifies the request round open for this owner and
defaults to GENERATION."
  (unless (numberp now)
    (error "Lease record requires target-authoritative time"))
  (list :generation generation
        :transfer-generation (or transfer-generation generation)
        :status (or status 'active)
        :publication-head publication-head
        :unsettled-mutation (and unsettled-mutation t)
        :client-id mevedel-session-durability--client-id
        ;; The client id is opaque and per-process: it answers "is this me,
        ;; now" and nothing else.  One session directory is reached from
        ;; several machines, so name the holder in terms the user shares.
        :host (mevedel-session-durability--client-host)
        ;; Lets a later client on this host reclaim a crashed holder at once.
        :holder (mevedel-session-durability--holder)
        :renewed-at now
        :expires-at (+ now mevedel-session-lease-seconds)
        :buffer buffer-name))

(defun mevedel-session-durability--generation-path (directory generation)
  "Return GENERATION's immutable record path below DIRECTORY."
  (let ((path
         (file-name-concat
          (mevedel-session-control-fs-physical-path directory)
          (format "%020d.el" generation))))
    (mevedel-session-control-fs-physical-path path)))

(defconst mevedel-session-durability--generation-name-regexp
  "\\`[0-9]\\{20\\}\\.el\\'"
  "Name shape of a lease generation record.")

(defun mevedel-session-durability--validate-generation-names (names)
  "Refuse unknown entries among observed lease directory NAMES."
  (let ((case-fold-search nil))
    (dolist (name names)
      (unless (or (member name '("requests" "fences"))
                  (string-match-p mevedel-session-control-fs-scratch-name-regexp name)
                  (string-match-p mevedel-session-durability--generation-name-regexp name))
        (error "Unknown portable lease entry: %s" name)))))

(defun mevedel-session-durability--generation-paths (directory &optional names)
  "Return DIRECTORY's generation record paths in descending order.

NAMES, when non-nil, is a listing already observed for DIRECTORY and
replaces the list round trip: a list carries the observed entries and
the symbol `none' records that the observation saw an empty directory,
which a bare nil could not distinguish from no observation at all.
Only entries shaped like a generation record survive either way."
  (let ((physical (mevedel-session-control-fs-physical-path directory)))
    (sort (cond
           ((eq names 'none) nil)
           (names
            (let (paths)
              (dolist (name names paths)
                (when (string-match-p
                       mevedel-session-durability--generation-name-regexp
                       (file-name-nondirectory name))
                  (push (expand-file-name name physical) paths)))))
           (t
            (mevedel-session-control-fs-list-directory
             directory
             mevedel-session-durability--generation-name-regexp)))
          #'string>)))

(defun mevedel-session-durability--generation (path)
  "Return the generation number encoded by lease record PATH."
  (string-to-number (file-name-base path)))

(defun mevedel-session-durability--observed-names (observed)
  "Return OBSERVED's listing as a names argument for the path helpers.
An observation that saw an empty directory answers `none', so its
consumer does not list again; a failed observation answers nil, which
falls back to a fresh listing."
  (and (plist-get observed :listed)
       (or (plist-get observed :names) 'none)))

(defun mevedel-session-durability--read-operations (paths)
  "Return the program reading every lease record among PATHS."
  (mapcar (lambda (path)
            (list :op 'read :path path :optional t))
          paths))

(defun mevedel-session-durability--parse-records (results &optional strict)
  "Return the lease records the read RESULTS carry, nil where unreadable.
With STRICT, reject any bytes after the record except trailing whitespace."
  (mapcar
   (lambda (result)
     (and (eq 'ok (plist-get result :status))
          (condition-case nil
              (let* ((bytes (plist-get result :value))
                     (parsed (read-from-string bytes)))
                (and (or (not strict)
                         (string-match-p "\\`[ \t\r\n]*\\'"
                                         (substring bytes (cdr parsed))))
                     (car parsed)))
            (error nil))))
   results))

(defun mevedel-session-durability--read-records (paths &optional strict)
  "Read every lease record among PATHS in one target program.

Return parsed records or nil in PATHS order.  An absent or unreadable
record is nil rather than the program's end, so a vanished newest
record can never hide an older live one.  STRICT rejects trailing bytes as
`mevedel-session-durability--parse-records' does."
  (when paths
    (mevedel-session-durability--parse-records
     (mevedel-session-control-fs-run-program
      (mevedel-session-durability--read-operations paths)) strict)))

(defun mevedel-session-durability--head-of-records (records)
  "Return the first non-aborted record among RECORDS, newest first."
  (catch 'head
    (dolist (record records)
      (when (and record (not (eq 'aborted (plist-get record :status))))
        (throw 'head record)))))

(defun mevedel-session-durability--validated-head (paths records)
  "Return RECORDS' head only when every generation matches its observed PATHS."
  (unless (= (length paths) (length records))
    (error "Incomplete portable lease generation reads"))
  (cl-mapc
   (lambda (path record)
     (unless (and (mevedel-session-durability--valid-lease-p record)
                  (= (mevedel-session-durability--generation path)
                     (plist-get record :generation)))
       (error "Unreadable or invalid portable lease generation")))
   paths records)
  (mevedel-session-durability--head-of-records records))

(defun mevedel-session-durability--resolve-lease-head (directory paths records)
  "Resolve DIRECTORY's head from RECORDS read at the observed PATHS.
If no head survives, refresh a changed name set once: a successor may have
pruned every listed generation.  Unchanged unreadable records stay nil."
  (let ((head (mevedel-session-durability--head-of-records records)))
    (when (and (null head) paths)
      (let ((fresh-paths (mevedel-session-durability--generation-paths directory)))
        (unless (equal fresh-paths paths)
          (setq head (mevedel-session-durability--head-of-records
                      (mevedel-session-durability--read-records fresh-paths)))
          (when (and fresh-paths (null head))
            (error "Portable session lease changed during discovery")))))
    head))

(defun mevedel-session-durability--lease-head (directory &optional names)
  "Return DIRECTORY's latest non-aborted lease record.

NAMES, when non-nil, is an already-observed listing that replaces the
list round trip; all records are then read in one program.  A
generation whose record cannot be read is skipped rather than reported
as \"no lease\": a vanished or unreadable newest record must never hide
an older live one, because the generation compare-and-set is what keeps
two writers apart."
  (let ((paths (mevedel-session-durability--generation-paths directory names)))
    (mevedel-session-durability--resolve-lease-head
     directory paths (mevedel-session-durability--read-records paths))))

(defun mevedel-session-durability--ensure-lease-directory (directory)
  "Ensure that lease generation DIRECTORY exists."
  (mevedel-session-control-fs-physical-path directory)
  (cond
   ((mevedel-session-control-fs-directory-p directory))
   ((mevedel-session-control-fs-path-exists-p directory)
    (error "Invalid portable session lease: %s" directory))
   (t
    (condition-case nil
        (mevedel-session-control-fs-make-directory directory)
      (file-already-exists nil))
    (mevedel-session-control-fs-physical-path directory)
    (unless (mevedel-session-control-fs-directory-p directory)
      (error "Could not create portable session lease: %s" directory)))))

(defun mevedel-session-durability--write-generation (directory record)
  "Atomically replace RECORD's own generation below DIRECTORY."
  (mevedel-session-durability--write-plist
   (mevedel-session-durability--generation-path
    directory (plist-get record :generation))
   record))

(defun mevedel-session-durability--record-bytes (record)
  "Return the complete printed content of control RECORD."
  (with-temp-buffer
    (let ((print-length nil)
          (print-level nil))
      (prin1 record (current-buffer))
      (buffer-string))))

(defun mevedel-session-durability--newest-generation (names)
  "Return the highest generation among lease record NAMES, or zero.

Only names shaped like a generation record count, so an unrelated entry in
the lease directory cannot present itself as a newer generation."
  (let ((best 0))
    (dolist (name names best)
      (when (string-match-p mevedel-session-durability--generation-name-regexp
                            (file-name-nondirectory name))
        (let ((generation (mevedel-session-durability--generation name)))
          (when (> generation best)
            (setq best generation)))))))

(defun mevedel-session-durability--target-time-cached-p (directory)
  "Return non-nil when DIRECTORY has a fresh clock reading without I/O."
  (or (numberp mevedel-session-durability--observed-time)
      (let ((cached (alist-get directory
                               (car mevedel-session-durability--transaction-clock)
                               nil nil #'equal)))
        (and cached
             (< (- (float-time) (cdr cached))
                mevedel-session-durability--clock-reuse-seconds)))))

(defun mevedel-session-durability--strip-assumption (lease)
  "Return LEASE without the remembered on-target bytes."
  (let ((copy (copy-sequence lease)))
    (plist-put copy :bytes nil)))

(defun mevedel-session-durability--observe-lease-plan
    (directory generation extra &optional force-clock)
  "Return the program observing DIRECTORY's lease, as a plist.

`:operations' is the program, `:clocked' and `:reading' record which optional
operations it carries.  FORCE-CLOCK reads the target clock even when the
transaction holds a fresh reading, for a caller that cannot read the target
again if that reading ages out before the result is used.  GENERATION and
EXTRA are as for `mevedel-session-durability--observe-lease'."
  (let* ((clocked (or force-clock
                      (not (mevedel-session-durability--target-time-cached-p
                            directory))))
         (reading (and (natnump generation)
                       (list :op 'read
                             :path (mevedel-session-durability--generation-path
                                    directory generation)
                             :optional t))))
    (list :clocked clocked
          :reading reading
          :operations
          (append
           ;; A transaction whose reading is still fresh answers `:now'
           ;; through the clock seam instead.
           (when clocked (list (list :op 'target-time :path directory)))
           (list (list :op 'list-directory :path directory))
           (when reading (list reading))
           extra))))

(defun mevedel-session-durability--observe-lease-settle
    (directory plan results)
  "Interpret RESULTS of the observation program PLAN for DIRECTORY.
Return the plist `mevedel-session-durability--observe-lease' documents."
  (let* ((clocked (plist-get plan :clocked))
         (reading (plist-get plan :reading))
         (results (copy-sequence results))
         (clock (and clocked (pop results)))
         (listing (pop results))
         (record (and reading (pop results)))
         (bytes (and record
                     (eq 'ok (plist-get record :status))
                     (plist-get record :value))))
    ;; A clock the target could not supply is left unanswered rather than
    ;; raised: the seam falls back, and a caller with no usable clock fails
    ;; closed there.
    (list :now (and (eq 'ok (plist-get clock :status))
                    (mevedel-session-durability--note-target-time
                     directory (plist-get clock :value)))
          :listed (eq 'ok (plist-get listing :status))
          :names (and (eq 'ok (plist-get listing :status))
                      (plist-get listing :value))
          :bytes bytes
          :record
          (and bytes
               (condition-case nil
                   (car (read-from-string bytes))
                 (error nil)))
          :extra results)))

(defun mevedel-session-durability--observe-lease
    (directory generation &optional extra)
  "Observe DIRECTORY's clock, records, and GENERATION's own record at once.

Return a plist with `:now', `:names', `:bytes', `:record', and `:extra'.  The
three observations are one target process rather than four, and the record
read is optional so an absent record still leaves the clock, the listing, and
the rest answered.  EXTRA is a list of further optional observations riding in
the same process; `:extra' holds their results in order."
  (let ((plan (mevedel-session-durability--observe-lease-plan
               directory generation extra)))
    (mevedel-session-durability--observe-lease-settle
     directory plan
     (mevedel-session-control-fs-run-program
      (plist-get plan :operations)))))

(defun mevedel-session-durability--commit-lease-plan
    (directory generation expected record deadline)
  "Return the program committing RECORD over GENERATION's EXPECTED bytes.
A plist: `:operations' is the program and the other keys carry what
`mevedel-session-durability--commit-lease-settle' needs.  DIRECTORY,
GENERATION, EXPECTED, RECORD, and DEADLINE are as for
`mevedel-session-durability--commit-lease'."
  (let* ((riders (and mevedel-session-durability--commit-riders
                      (not (plist-get mevedel-session-durability--commit-riders
                                      :consumed))
                      mevedel-session-durability--commit-riders))
         (before (append
                  (plist-get riders :before)
                  (when (and (plist-get riders :before) (numberp deadline))
                    (list (list :op 'before-time :path directory
                                :content (number-to-string (floor deadline)))))))
         (after (plist-get riders :after))
         (path (mevedel-session-durability--generation-path
                directory generation))
         (bytes (mevedel-session-durability--record-bytes record))
         (changed (not (equal expected bytes)))
         (refresh-time (not (mevedel-session-durability--target-time-cached-p directory)))
         ;; `verify' opens the commit's own operations: the takeover race
         ;; tests key on the commit program by it.  A fresh transaction clock
         ;; needs no new marker here.  Never extend its reuse window without
         ;; reading the target again; a slow operation expires it naturally.
         (own
          (append
           (list (list :op 'verify :path path :content expected))
           ;; Several renewals can share one target-clock second.  Prove
           ;; the unchanged bytes without replacing them with themselves;
           ;; the head observation and clock refresh still follow.
           (when changed (list (list :op 'write :path path :content bytes)))
           (list (list :op 'list-directory :path directory))
           (when refresh-time
             (list (list :op 'target-time :path directory :optional t))))))
    (list :operations (append before own after)
          :riders riders :before before :own own :after after
          :changed changed :refresh-time refresh-time)))

(defun mevedel-session-durability--commit-lease-settle
    (directory plan all)
  "Interpret ALL, the results of commit program PLAN for DIRECTORY.
Return what `mevedel-session-durability--commit-lease' documents."
  (let* ((riders (plist-get plan :riders))
         (before (plist-get plan :before))
         (own (plist-get plan :own))
         (changed (plist-get plan :changed))
         (refresh-time (plist-get plan :refresh-time))
         (before-results (seq-take all (length before)))
         (results (seq-take (nthcdr (length before) all) (length own)))
         (proof (nth 0 results))
         (write (and changed (nth 1 results)))
         (listing (nth (if changed 2 1) results))
         (clock (and refresh-time (nth (if changed 3 2) results))))
    (when riders
      (setf (plist-get riders :consumed) t
            (plist-get riders :before-results) before-results
            (plist-get riders :after-results)
            (nthcdr (+ (length before) (length own)) all)))
    ;; Nothing of the commit ran: the program stopped in its prelude.
    (when (cl-some (lambda (pair)
                     (and (not (plist-get (car pair) :optional))
                          (not (eq 'ok (plist-get (cdr pair) :status)))))
                   (cl-mapcar #'cons before before-results))
      (signal 'mevedel-session-durability-prelude-failed (list before-results)))
    (when (eq 'ok (plist-get clock :status))
      (mevedel-session-durability--note-target-time
       directory (plist-get clock :value)))
    (cond
     ((memq (plist-get proof :status) '(mismatch absent)) nil)
     ((not (eq 'ok (plist-get proof :status)))
      (mevedel-session-control-fs-program-value proof))
     ((and changed (not (eq 'ok (plist-get write :status))))
      (mevedel-session-control-fs-program-value write))
     (t (or (plist-get listing :value) t)))))

(defun mevedel-session-durability--commit-lease
    (directory generation expected record &optional deadline)
  "Replace GENERATION's record below DIRECTORY only while it still is EXPECTED.

EXPECTED is the content observed on the target; RECORD is the replacement.
The proof and the write share one target process, which narrows the window
between them but does not remove it: another client can exclusively create
the next generation in between, so `verify' is a precondition and not an
election, and the caller decides who won from what it observes afterwards.
Return the record names observed after the write, or nil when the proof
failed.

Operations in `mevedel-session-durability--commit-riders' share the program:
see there.  A prelude delays the commit past the clock reading it was decided
from, so with one the program also proves, between prelude and commit, that
the target clock is still before DEADLINE: the earliest of the held lease's
expiry and the replacement's own.  A lapsed deadline fails the prelude."
  (let ((plan (mevedel-session-durability--commit-lease-plan
               directory generation expected record deadline)))
    (mevedel-session-durability--commit-lease-settle
     directory plan
     (mevedel-session-control-fs-run-program
      (plist-get plan :operations)))))

(defun mevedel-session-durability--same-generation-p (left right)
  "Return non-nil when lease records LEFT and RIGHT name one generation."
  (and left right
       (equal (plist-get left :generation) (plist-get right :generation))
       (equal (plist-get left :client-id) (plist-get right :client-id))))

(defun mevedel-session-durability--cancel-renewal (session)
  "Cancel SESSION's lease-renewal timer, if any."
  (when (fboundp 'mevedel-session-collection-cancel)
    (mevedel-session-collection-cancel session))
  (when-let* ((job (mevedel-session-lease-renewal-job session)))
    (setf (mevedel-session-durability--heartbeat-cancelled job) t))
  (when-let* ((timer (mevedel-session-lease-renewal-timer session))
              ((timerp timer)))
    (cancel-timer timer))
  (setf (mevedel-session-lease-renewal-timer session) nil))

(defun mevedel-session-durability--bind-lease (session lease state &optional bytes predecessor)
  "Record LEASE and STATE on SESSION and maintain its renewal timer.

BYTES is the exact content LEASE occupies on the target when that is known.
A later renewal states it as the precondition of its own write, which saves
re-reading a record this client just wrote.
PREDECESSOR is the observed record before acquisition, when different from the
bound lease.  Foreign ownership advances SESSION's local ownership epoch;
same-owner generation rotations and reclaiming one's own expired lease do not.

An `owned' state runs the renewal heartbeat and a `lost' one stops it,
except while takeover still needs to stop producers: ticks retry that stop.
A `contested' state keeps it: another client has a generation outstanding
but has not activated it, and that claim aborts if this client's write
landed first, so stopping the heartbeat here would let a lease this client
still holds expire for real."
  (when session
    (let ((previous (mevedel-session-lease session))
          (live (and lease (copy-sequence lease)))
          (release-pending
           (plist-get (mevedel-session-lease session) :release-pending)))
      (when live
        (setq live (plist-put live :state state))
        (setq live (plist-put live :bytes bytes)))
      (when release-pending
        (setq live (plist-put live :release-pending t)))
      (when (and (eq state 'owned)
                 (or (and predecessor
                          (not (equal mevedel-session-durability--client-id
                                      (plist-get predecessor :client-id))))
                     (and (not (eq 'owned (plist-get previous :state)))
                          (or (memq (plist-get (mevedel-session-control-transfer session) :state)
                                    '(taken-over following))
                              (and (memq (plist-get previous :status) '(active publishing released))
                                   (not (equal mevedel-session-durability--client-id
                                               (plist-get previous :client-id))))))))
        (cl-incf (mevedel-session-ownership-epoch session)))
      (setf (mevedel-session-lease session) live)
      ;; The async updater runs on a private session projection.  Its bindings
      ;; are provisional: neither timers nor collection belong to that object.
      (unless (when-let* ((job (mevedel-session-lease-renewal-job session)))
                (eq session (mevedel-session-durability--heartbeat-shadow job)))
        (when (and (memq state '(lost foreign))
                   (mevedel-session-durability--valid-lease-p lease)
                   (memq (plist-get lease :status) '(active publishing released))
                   (not (equal mevedel-session-durability--client-id
                               (plist-get lease :client-id)))
                   (natnump (plist-get previous :generation))
                   (or (and (equal mevedel-session-durability--client-id
                                   (plist-get previous :client-id))
                            (> (plist-get lease :generation)
                               (plist-get previous :generation)))
                       (and (eq 'claiming (plist-get previous :status))
                            (= (plist-get lease :generation)
                               (plist-get previous :generation)))))
          (mevedel-session-control-transfer-taken-over session lease))
        (cond
         ((or (eq state 'owned)
              (eq 'taken-over
                  (plist-get (mevedel-session-control-transfer session) :state)))
          (unless (timerp (mevedel-session-lease-renewal-timer session))
            (setf (mevedel-session-lease-renewal-timer session)
                  (run-at-time
                   mevedel-session-lease-renewal-seconds
                   mevedel-session-lease-renewal-seconds
                   #'mevedel-session-durability-heartbeat session))))
         ;; Contested keeps the timer: see this function's docstring.
         ((eq state 'contested))
         (t (mevedel-session-durability--cancel-renewal session)))))))

(defun mevedel-session-durability--finite-nonnegative-number-p (value)
  "Return non-nil when VALUE is a finite nonnegative number.
A durable record is file bytes some other client wrote, so a timestamp
read back from one is only authority if it can be compared: NaN fails
every comparison, and an infinity never expires."
  (and (numberp value)
       ;; NaN fails every comparison, so the lower bound rejects it; the upper
       ;; one rejects an infinity and a bignum that would never expire.
       (>= value 0)
       (<= value most-positive-fixnum)))

(defun mevedel-session-durability--valid-lease-p (lease)
  "Return non-nil when LEASE has the current portable representation."
  (and (proper-list-p lease)
       (natnump (plist-get lease :generation))
       (> (plist-get lease :generation) 0)
       (natnump (plist-get lease :transfer-generation))
       (> (plist-get lease :transfer-generation) 0)
       (<= (plist-get lease :transfer-generation)
           (plist-get lease :generation))
       (memq (plist-get lease :status)
             '(claiming active publishing released aborted))
       (stringp (plist-get lease :client-id))
       (string-match-p "\\`[0-9a-f]\\{64\\}\\'"
                       (plist-get lease :client-id))
       (mevedel-session-durability--finite-nonnegative-number-p
        (plist-get lease :renewed-at))
       (mevedel-session-durability--finite-nonnegative-number-p
        (plist-get lease :expires-at))
       (<= (plist-get lease :renewed-at) (plist-get lease :expires-at))
       (plist-member lease :publication-head)
       (plist-member lease :unsettled-mutation)
       (booleanp (plist-get lease :unsettled-mutation))
       (or (null (plist-get lease :publication-head))
           (mevedel-session-publication-valid-head-p
            (plist-get lease :publication-head)))))

(defun mevedel-session-durability-publication-head (session-dir &optional names)
  "Return SESSION-DIR's validated relative publication head, or nil.

NAMES, when non-nil, is a listing of SESSION-DIR's lease directory already
observed by the caller, and replaces the list round trip exactly as it does
for `mevedel-session-durability--lease-head'.  Only a caller that took the
observation for this read may supply it.

This read-only operation bypasses remote file caches and does not acquire or
otherwise mutate the session lease."
  (let* ((remote-file-name-inhibit-cache t)
         (_ (mevedel-session-durability--assert-no-pid-lock session-dir))
         (directory (mevedel-session-durability--lease-path session-dir))
         (lease (mevedel-session-durability--lease-head directory names)))
    (cond
     ((null lease) nil)
     ((mevedel-session-durability--valid-lease-p lease)
      (plist-get lease :publication-head))
     (t
      (error "Invalid portable session lease: %s" directory)))))

(defun mevedel-session-durability--lease-lock-directory (directory)
  "Return the stable sessions parent lock shared by lease DIRECTORY's writers.
The session and its lease directory may be deleted; their parent must survive."
  (mevedel-session-control-fs-physical-path
   (file-name-concat directory ".." "..")))

(defun mevedel-session-durability--claim-next
    (directory expected buffer-name
               &optional status unsettled-mutation-p unsettled-mutation
               transfer-generation observed)
  "Claim DIRECTORY's next generation after EXPECTED for BUFFER-NAME.

The candidate fences older writers as soon as its exclusive generation file
appears.  It becomes active only if EXPECTED remained byte-for-byte unchanged;
otherwise it is marked aborted and removed best-effort.  STATUS defaults to
`active'.  When UNSETTLED-MUTATION-P is non-nil, the successor records
UNSETTLED-MUTATION instead of preserving EXPECTED's value.
TRANSFER-GENERATION explicitly opens that request round; otherwise a
same-owner successor preserves EXPECTED's round.  OBSERVED is a lease
observation the caller already took of DIRECTORY, which replaces this
function's own."
  (let* ((status (or status 'active))
         (lock-directory (mevedel-session-durability--lease-lock-directory directory))
         ;; One observation answers the clock and the known generations.
         ;; Any record created after it is a foreign claim at the same
         ;; next generation, which collides with the exclusive create
         ;; below and fails it -- no generation older than the candidate
         ;; can appear in between, so the observed name set is the
         ;; complete predecessor universe.  That holds for any observation
         ;; taken before the create, including the caller's.
         (observed (or observed
                       (mevedel-session-durability--observe-lease
                        directory nil)))
         ;; The observed reading flows through the clock seam, so a test
         ;; that stubs the seam still governs every deadline.
         (now (let ((mevedel-session-durability--observed-time
                     (plist-get observed :now)))
                (mevedel-session-durability--target-time directory)))
         (names (progn
                  (unless (plist-get observed :listed)
                    (error "Unreadable portable lease directory"))
                  (mevedel-session-durability--validate-generation-names
                   (plist-get observed :names))
                  (mevedel-session-durability--observed-names observed)))
         (generation
          (1+ (mevedel-session-durability--newest-generation
               (plist-get observed :names))))
         (candidate-path (mevedel-session-durability--generation-path
                          directory generation))
         (older (seq-filter
                 (lambda (path)
                   (< (mevedel-session-durability--generation path)
                      generation))
                 (mevedel-session-durability--generation-paths
                  directory names)))
         (candidate
          (let ((mevedel-session-lease-seconds
                 (if (eq status 'publishing)
                     mevedel-session-publication-lease-seconds
                   mevedel-session-lease-seconds)))
             (mevedel-session-durability--lease-record
              buffer-name generation 'claiming
              (plist-get expected :publication-head)
              (if unsettled-mutation-p
                  unsettled-mutation
                (plist-get expected :unsettled-mutation))
              now
              (or transfer-generation
                  (and expected
                       (equal mevedel-session-durability--client-id
                              (plist-get expected :client-id))
                       (plist-get expected :transfer-generation))))))
         (claiming-bytes (mevedel-session-durability--record-bytes candidate))
         (expected-bytes
          ;; One strictly validated predecessor needs only an exact byte proof;
          ;; multiple predecessors must all be read before any can be pruned.
          (and expected observed
               (= (length older) 1)
               (not (eq 'aborted (plist-get expected :status)))
               (equal expected (plist-get observed :record))
               (eql (plist-get expected :generation) (1- generation))
               (equal expected
                      (mevedel-session-durability--validated-head
                       older (mevedel-session-durability--parse-records
                              (list (list :status 'ok :value (plist-get observed :bytes))) t)))
               (plist-get observed :bytes)))
         (settle
          (lambda ()
            (setq candidate (plist-put candidate :status status))
            ;; Settling the claim and pruning superseded generations share
            ;; one program; a prune that fails is best-effort, but a failed
            ;; settle write must still raise.
            (append (list (list :op 'verify :path candidate-path :content claiming-bytes)
                          (list :op 'write :path candidate-path
                        :content (mevedel-session-durability--record-bytes
                                  candidate)))
                  (mapcar (lambda (path)
                            (list :op 'delete-file :path path :optional t))
                          older))))
         (abort
          (lambda ()
            (setq candidate (plist-put candidate :status 'aborted))
            (let* ((results
                    (mevedel-session-control-fs-run-program
                   (list (list :op 'verify :path candidate-path :content claiming-bytes)
                         (list :op 'write :path candidate-path
                               :content
                               (mevedel-session-durability--record-bytes
                                candidate))
                         (list :op 'delete-file :path candidate-path
                               :optional t)) lock-directory))
                   (proof (car results)))
              (unless (memq (plist-get proof :status) '(absent mismatch))
                (mevedel-session-control-fs-program-value proof)
                (mevedel-session-control-fs-program-value (cadr results))))
            nil))
         (create (list :op 'create :path candidate-path
                       :content (mevedel-session-durability--record-bytes
                                 candidate))))
    (if expected-bytes
        ;; Fence, prove, settle and prune in one program.  A failed proof
        ;; stops it before the settle write, leaving the claim to abort.
        (let* ((results
                (mevedel-session-control-fs-run-program
                 (append (list create
                               (list :op 'verify
                                     :path (mevedel-session-durability--generation-path
                                            directory (plist-get expected :generation))
                                     :content expected-bytes))
                         (funcall settle)) lock-directory))
               (created (nth 0 results))
               (proved (nth 1 results)))
          (cond
           ((eq 'conflict (plist-get created :status)) nil)
           ((not (eq 'ok (plist-get created :status)))
            (mevedel-session-control-fs-program-value created))
           ((not (eq 'ok (plist-get proved :status)))
            (if (memq (plist-get proved :status) '(mismatch absent))
                (funcall abort)
              (mevedel-session-control-fs-program-value proved)))
           (t
            (mevedel-session-control-fs-program-value (nth 2 results))
            (mevedel-session-control-fs-program-value (nth 3 results))
            candidate)))
      ;; The fencing create and the predecessor reads are one program: the
      ;; create is first, so a lost race skips the reads.
      (let* ((results
              (mevedel-session-control-fs-run-program
               (cons create
                     (mapcar (lambda (path)
                               (list :op 'read :path path :optional t))
                             older)) lock-directory))
             (created (car results)))
        (cond
         ((eq 'conflict (plist-get created :status)) nil)
         ((not (eq 'ok (plist-get created :status)))
          (mevedel-session-control-fs-program-value created))
         ((equal expected
                 (condition-case failure
                     (mevedel-session-durability--validated-head
                      older (mevedel-session-durability--parse-records (cdr results) t))
                   (error
                    (funcall abort)
                    (signal (car failure) (cdr failure)))))
          (let ((settled (mevedel-session-control-fs-run-program
                          (funcall settle) lock-directory)))
            (mevedel-session-control-fs-program-value (car settled))
            (mevedel-session-control-fs-program-value (cadr settled)))
          candidate)
         (t (funcall abort)))))))

(defun mevedel-session-durability--owned-lease-record-p
    (lease directory &optional now)
  "Return non-nil when LEASE is live authority owned by this client.
DIRECTORY supplies the target-authoritative clock.  NOW may reuse a reading
already taken for the surrounding operation."
  (let ((now (or now
                 (mevedel-session-durability--target-time directory))))
    (and (mevedel-session-durability--valid-lease-p lease)
         (memq (plist-get lease :status) '(active publishing))
         (equal mevedel-session-durability--client-id
                (plist-get lease :client-id))
         (> (plist-get lease :expires-at) now))))

(defun mevedel-session-durability-lease-acquire
    (session-dir buffer-name &optional session token force)
  "Acquire SESSION-DIR's portable lease for BUFFER-NAME.

Return non-nil when this client owns mutation authority and nil when another
unexpired client owns it.  Expired takeover requires explicit confirmation,
except from a holder that provably died on this host, which is reclaimed at
once even before expiry (see `mevedel-session-durability--holder-dead-p').
When SESSION is non-nil, record the resulting lease state on it, after
serializing behind its asynchronous publication: see
`mevedel-session-durability--gate' for TOKEN.
FORCE offers explicit confirmation even for an unexpired foreign holder.
Only an explicit user takeover command should pass FORCE."
  (require 'mevedel-session-publication)
  (mevedel-session-durability--gate session token)
  (let* ((remote-file-name-inhibit-cache t)
         (_ (mevedel-session-durability--assert-no-pid-lock session-dir))
         (directory (mevedel-session-durability--lease-path session-dir))
         (_ (mevedel-session-durability--ensure-lease-directory directory))
         ;; One observation answers the clock and the listing the head
         ;; reads work from.
         (observed (mevedel-session-durability--observe-lease directory nil))
         (now (let ((mevedel-session-durability--observed-time
                     (plist-get observed :now)))
                (mevedel-session-durability--target-time directory)))
         (existing (mevedel-session-durability--lease-head
                    directory
                    (mevedel-session-durability--observed-names observed)))
         (lease
          (cond
           ((null existing)
            (mevedel-session-durability--claim-next
             directory nil buffer-name))
           ((not (mevedel-session-durability--valid-lease-p existing))
            (error "Invalid portable session lease: %s" directory))
           ((and (equal mevedel-session-durability--client-id
                        (plist-get existing :client-id))
                 (eq 'active (plist-get existing :status))
                 (> (plist-get existing :expires-at) now))
            (let ((record
                   (mevedel-session-durability--lease-record
                    buffer-name (plist-get existing :generation) 'active
                    (plist-get existing :publication-head)
                    (plist-get existing :unsettled-mutation)
                    now
                    (plist-get existing :transfer-generation))))
              (mevedel-session-durability--write-generation directory record)
              (and (mevedel-session-durability--same-generation-p
                    record
                    (mevedel-session-durability--lease-head directory))
                   record)))
           ((and (equal mevedel-session-durability--client-id
                        (plist-get existing :client-id))
                 (eq 'publishing (plist-get existing :status))
                 (> (plist-get existing :expires-at) now))
            existing)
           ((and (memq (plist-get existing :status)
                       '(claiming active publishing))
                 (> (plist-get existing :expires-at) now)
                 (not force))
            ;; A holder that provably died on this host protects no write in
            ;; flight.  A publishing holder's child writer may outlive it, so
            ;; only expiry ends that lease.
            (and (not (eq 'publishing (plist-get existing :status)))
                 (mevedel-session-durability--holder-dead-p existing)
                 (mevedel-session-durability--claim-next
                  directory existing buffer-name)))
           ((eq 'released (plist-get existing :status))
            (let ((fence
                   (mevedel-session-transfer-release-fence
                    directory (plist-get existing :generation))))
              (if (or (null fence)
                      (equal mevedel-session-durability--client-id
                             (plist-get fence :requester-client-id)))
                  (mevedel-session-durability--claim-next
                   directory existing buffer-name)
                nil)))
           ;; Reclaim this client's own expired lease without a prompt.  A
           ;; single durable action on a remote target can outlast the lease
           ;; while blocking target I/O keeps the renewal timer from running,
           ;; and asking a client to confirm taking over from itself is both
           ;; meaningless and, in a non-interactive client, fatal.  The
           ;; generation compare-and-set below still refuses the reclaim if
           ;; another client claimed the lease in the meantime.
           ((and (equal mevedel-session-durability--client-id
                        (plist-get existing :client-id))
                 (memq (plist-get existing :status) '(claiming active)))
            (mevedel-session-durability--claim-next
             directory existing buffer-name))
           ;; An expired claim never activated; fence its delayed activation
           ;; with a successor instead of waiting forever for that client.
           ((and (not force) (eq 'claiming (plist-get existing :status)))
            (mevedel-session-durability--claim-next
             directory existing buffer-name))
           ;; Nobody needs to confirm a takeover from a provably dead holder.
           ((and (memq (plist-get existing :status) '(claiming active))
                 (mevedel-session-durability--holder-dead-p existing))
            (mevedel-session-durability--claim-next
             directory existing buffer-name))
           ((not
             (let ((expired-at
                    (format-time-string
                     "%FT%T%z"
                     (seconds-to-time
                      (plist-get existing :expires-at)))))
               (funcall
                (if force #'yes-or-no-p #'y-or-n-p)
                (if force
                    (format
                     (concat
                      "Take over portable session held by %s, buffer %s, "
                      "last renewal %s, status %s? "
                      "%sUnpublished changes on that client will not be kept. ")
                     (mevedel-session-durability-holder-label
                      (plist-get existing :host))
                     (mevedel-session-durability-holder-label
                      (plist-get existing :buffer))
                     (format-time-string
                      "%FT%T%z" (seconds-to-time
                                   (plist-get existing :renewed-at)))
                     (plist-get existing :status)
                     (if (eq 'publishing (plist-get existing :status))
                         (concat "A critical write may still be in flight; "
                                 "confirm that the prior client is stopped. ")
                       ""))
                  (if (eq 'publishing (plist-get existing :status))
                    (format
                     (concat
                      "Portable session publishing lease expired at %s. "
                      "A critical write may still be in flight; confirm that "
                      "the prior client is stopped before takeover? ")
                     expired-at)
                  (format
                   "Portable session lease expired at %s; take it over? "
                   expired-at))))))
            nil)
           (t
            (mevedel-session-durability--claim-next
             directory existing buffer-name)))))
    (let* ((current (mevedel-session-durability--lease-head directory))
           (current-now (mevedel-session-durability--target-time directory))
           (owned
            (and lease
                 (equal lease current)
                 (memq (plist-get current :status) '(active publishing))
                 (equal mevedel-session-durability--client-id
                        (plist-get current :client-id))
                 (> (plist-get current :expires-at) current-now))))
      (when session
        (mevedel-session-durability--bind-lease
         session current
         (cond
          (owned
           'owned)
          ((and current
                (memq (plist-get current :status)
                      '(claiming active publishing))
                (> (plist-get current :expires-at) current-now))
           'foreign)
          ((and current
                (eq (plist-get current :status) 'released)
                (progn
                  (mevedel-session-transfer-release-fence
                   directory (plist-get current :generation))))
           'foreign)
          (t 'expired))
         ;; An owned head is the record this client's claim just wrote.
         (and owned (mevedel-session-durability--record-bytes current))
         (and owned existing)))
      (and owned t))))

(defun mevedel-session-durability-cleanup-assessment (session-dir)
  "Assess portable SESSION-DIR without acquiring or modifying authority.
Return :state, :host, validated :head, symbolic :reasons, and :recoverable.
Recovery blockers are publishing, unsettled-mutation, recovery and journal-pins;
live-owner, transfer-reserved and invalid-authority prohibit explicit discard.
Unreadable generation listings or records are unknown authority, not absence.
The returned observations belong to this call only, never a status cache."
  (let* ((remote-file-name-inhibit-cache t)
         (session-dir (mevedel-session-control-fs-physical-path session-dir))
         (directory (mevedel-session-durability--lease-path session-dir))
         (pins-dir (file-name-concat session-dir ".journal-pins"))
         (recovery-dir (mevedel-session-recovery--root session-dir))
         head now names pins recovery recovery-names fence reasons
         (state 'unknown))
    (condition-case nil
        (let* ((results
                (mevedel-session-control-fs-run-program
                 (list (list :op 'absent :path (file-name-concat session-dir ".lock"))
                       (list :op 'observation-time :path session-dir)
                       (list :op 'list-directory :path directory :optional t)
                       (list :op 'list-directory :path pins-dir :optional t)
                       (list :op 'directory-p :path recovery-dir :optional t))))
               (listing (nth 2 results))
               (pin-result (nth 3 results))
               (recovery-result (nth 4 results)))
          (mevedel-session-control-fs-program-value (nth 0 results))
          (setq now
                (let ((mevedel-session-durability--observed-time
                       (floor (mevedel-session-control-fs-program-value (nth 1 results)))))
                  (mevedel-session-durability--target-time directory)))
          (unless (memq (plist-get listing :status) '(ok absent))
            (error "Unreadable portable lease directory"))
          (when (eq 'absent (plist-get listing :status))
            (error "Session has no portable lease; run scripts/migrate-sessions.el"))
          (setq names (plist-get listing :value))
          (mevedel-session-durability--validate-generation-names names)
          (let* ((paths (mevedel-session-durability--generation-paths
                         directory (or names 'none)))
                 (records (mevedel-session-durability--read-records paths t)))
            (setq head (mevedel-session-durability--validated-head paths records)))
          (when (eq (plist-get head :status) 'released)
            (let ((mevedel-session-durability--observed-time now))
              (setq fence (mevedel-session-transfer-release-fence
                           directory (plist-get head :generation)))))
          (setq state (cond ((and fence
                                  (not (equal mevedel-session-durability--client-id
                                              (plist-get fence :requester-client-id))))
                             'foreign)
                            ((or (null head) (eq (plist-get head :status) 'released))
                             'available)
                            ((or (<= (plist-get head :expires-at) now)
                                 (and (memq (plist-get head :status) '(active claiming))
                                      (mevedel-session-durability--holder-dead-p head)))
                             'expired)
                            ((equal mevedel-session-durability--client-id
                                    (plist-get head :client-id)) 'owned)
                            (t 'foreign)))
          (when (memq state '(owned foreign)) (push 'live-owner reasons))
          (when fence (push 'transfer-reserved reasons))
          (when (eq 'publishing (plist-get head :status)) (push 'publishing reasons))
          (when (plist-get head :unsettled-mutation) (push 'unsettled-mutation reasons))
          (setq pins (if (memq (plist-get pin-result :status) '(ok absent))
                         (plist-get pin-result :value)
                       'unknown))
          (when pins (push 'journal-pins reasons))
          (condition-case nil
              (progn
                (setq recovery (mevedel-session-recovery-read session-dir recovery-result))
                (when (eq 'ok (plist-get recovery-result :status))
                  (setq recovery-names
                        (mevedel-session-control-fs-list-directory recovery-dir "."))))
            (error (setq recovery 'unknown)))
          (when (and recovery-names (null recovery))
            (setq recovery 'unknown))
          (when recovery (push 'recovery reasons)))
      (error (setq state 'unknown head nil reasons '(invalid-authority))))
    (list :state state :host (plist-get head :host) :head head
          :reasons (nreverse reasons)
          :recoverable (and (memq state '(available expired))
                            (not fence) (and reasons t))
          :pins pins :recovery recovery :now now
          :names names :recovery-names recovery-names)))

(defun mevedel-session-durability--abandoned-head-p (directory head now)
  "Return non-nil when lease HEAD of DIRECTORY has no owner at target time NOW.
A missing head, a release not reserved for control transfer, or an expired
active or claiming head qualifies, as does one whose holder provably died on
this host.  Unresolved mutation and publishing never do: their evidence needs
an explicit takeover."
  (or (null head)
      (and (not (plist-get head :unsettled-mutation))
           (or (and (eq 'released (plist-get head :status))
                    (not (mevedel-session-transfer-release-fence
                          directory (plist-get head :generation))))
               (and (memq (plist-get head :status) '(active claiming))
                    (or (<= (plist-get head :expires-at) now)
                        (mevedel-session-durability--holder-dead-p head)))))))

(defun mevedel-session-durability--delete-owned-session (session-dir lease)
  "Delete SESSION-DIR only under LEASE's exact, newest, unexpired authority.
Reobserve all blockers, then prove their absence under the election lock."
  (when (mevedel-session-control-fs-async-busy-p session-dir)
    (error "Session deletion refused: a local control program is still pending"))
  (let* ((assessment (mevedel-session-durability-cleanup-assessment session-dir))
         (head (plist-get assessment :head))
         (directory (mevedel-session-durability--lease-path session-dir))
         (path (mevedel-session-durability--generation-path
                directory (plist-get lease :generation)))
         (bytes (or (plist-get lease :bytes)
                    (mevedel-session-durability--record-bytes lease))))
    (unless (and (eq 'owned (plist-get assessment :state))
                 (equal bytes (mevedel-session-durability--record-bytes head))
                 (null (remq 'live-owner (copy-sequence (plist-get assessment :reasons)))))
      (error "Session deletion blocked: %S" (plist-get assessment :reasons)))
    (let* ((results
            (mevedel-session-control-fs-run-program
             (list (list :op 'verify :path path :content bytes)
                   (list :op 'verify-latest :path path :content ".el")
                   (mevedel-session-control-fs-listing-proof
                    directory (plist-get assessment :names))
                   (list :op 'absent :path (file-name-concat session-dir ".lock"))
                   (list :op 'verify-list :path (file-name-concat session-dir ".journal-pins")
                         :content "")
                   (list :op 'verify-list :path (mevedel-session-recovery--root session-dir)
                         :content "")
                   ;; Fresh target time is checked last, never a transaction cache.
                   (list :op 'before-time :path directory
                         :content (number-to-string (floor (plist-get head :expires-at))))
                   (list :op 'delete-directory :path session-dir))
             (mevedel-session-durability--lease-lock-directory directory))))
      (dolist (result results)
        (unless (eq 'ok (plist-get result :status))
          (error "Session deletion refused: authority or retained data changed (%s)"
                 (plist-get result :status))))
      t)))

(defun mevedel-session-durability-discard-session (session prepare-function)
  "Discard confirmed portable SESSION after PREPARE-FUNCTION settles its data.
The caller confirms permanent loss and verifies target/session identity.
Acquire through generation election without full restoration or another prompt.
Call PREPARE-FUNCTION with no arguments while owned, preserving mutation
evidence.
No lock spans Lisp processing.  Delete only after all blockers are resolved;
errors retain partial preparation progress and release the claim, not rollback.
The caller must ensure the previous client and its child writers have stopped."
  (require 'mevedel-session-publication)
  (when (or (mevedel-session-publication-busy-p session)
            (mevedel-session-lease-renewal-job session))
    (error "Session discard refused: local work is still active"))
  (let* ((session-dir (mevedel-session-save-path session))
         (_ (when (mevedel-session-control-fs-async-busy-p session-dir)
              (error "Session discard refused: a local control program is still pending")))
         (assessment (mevedel-session-durability-cleanup-assessment session-dir))
         (directory (mevedel-session-durability--lease-path session-dir))
         deleted lease)
    (unless (and (memq (plist-get assessment :state) '(available expired))
                 (not (memq 'transfer-reserved (plist-get assessment :reasons))))
      (error "Session discard refused: %S" (plist-get assessment :reasons)))
    (mevedel-session-durability--ensure-lease-directory directory)
    (setq lease (mevedel-session-durability--claim-next
                 directory (plist-get assessment :head) "session discard"))
    (unless lease (error "Session discard refused: another client claimed authority"))
    (unwind-protect
        (progn
          (mevedel-session-durability--bind-lease
           session lease 'owned (mevedel-session-durability--record-bytes lease))
          (unless (mevedel-session-durability-lease-owned-p session)
            (error "Session discard lost authority before preparation"))
          (funcall prepare-function)
          (setq deleted (mevedel-session-durability--delete-owned-session
                         session-dir (mevedel-session-lease session))))
      (unwind-protect
          (unless deleted
            (mevedel-session-durability-lease-release session-dir session))
        (mevedel-session-durability--cancel-renewal session)
        (setf (mevedel-session-lease session) nil)))
    deleted))

(defun mevedel-session-durability-delete-abandoned (session-dir max-age-seconds)
  "Delete portable SESSION-DIR when no client held it for MAX-AGE-SECONDS.
Unknown authority, publishing, unresolved mutation, recovery, journal pins and
reserved transfers always retain storage.  Claim through the same generation
election as takeover, then use the explicit discard's locked deletion boundary.
Return non-nil only when deleted; automatic cleanup never prompts."
  (let* ((assessment (mevedel-session-durability-cleanup-assessment session-dir))
         (head (plist-get assessment :head))
         (directory (mevedel-session-durability--lease-path session-dir))
         deleted)
    (when (and (memq (plist-get assessment :state) '(available expired))
               (null (plist-get assessment :reasons))
               ;; A zero cap deletes a session released this very second.
               (<= (or (plist-get head :renewed-at) 0)
                   (- (plist-get assessment :now) max-age-seconds)))
      (mevedel-session-durability--ensure-lease-directory directory)
      (when-let* ((lease (mevedel-session-durability--claim-next
                          directory head "session cleanup")))
        (unwind-protect
            (setq deleted (condition-case nil
                              (mevedel-session-durability--delete-owned-session session-dir lease)
                            (error nil)))
          (unless deleted
            (mevedel-session-durability-lease-release session-dir)))))
    deleted))

(defun mevedel-session-durability-call-with-abandoned-lease
    (session function &optional token)
  "Call FUNCTION under temporary authority for abandoned SESSION storage.
This is for recovering already frozen evidence, not resuming a conversation.
Return nil without prompting for live owners, unresolved mutation, publishing
leases, or a release reserved for control transfer.  Expired ordinary owners
are fenced by the existing generation election.  Always release the temporary
lease, including on nonlocal exit; never publish a new session head."
  (mevedel-session-durability--gate session token)
  (let* ((session-dir (mevedel-session-save-path session))
         (_ (mevedel-session-durability--assert-no-pid-lock session-dir))
         (directory (mevedel-session-durability--lease-path session-dir))
         (_ (mevedel-session-durability--ensure-lease-directory directory))
         (existing (mevedel-session-durability--lease-head directory))
         (now (mevedel-session-durability--target-time directory)))
    (when (and existing (not (mevedel-session-durability--valid-lease-p existing)))
      (error "Invalid portable session lease: %s" directory))
    (when (mevedel-session-durability--abandoned-head-p directory existing now)
      (when-let* ((lease (mevedel-session-durability--claim-next
                         directory existing "journal recovery")))
        (unwind-protect
            (progn
              (mevedel-session-durability--bind-lease session lease 'owned)
              (mevedel-session-durability-call-with-reserved-lease session function))
          (mevedel-session-durability-lease-release session-dir session))))))

(defun mevedel-session-durability--observed-head (record names)
  "Return RECORD when the listed NAMES alone prove it is the lease head.
That holds when it is not aborted and no newer generation is listed; nil
means the head must be read."
  (and record
       (not (eq 'aborted (plist-get record :status)))
       (eql (plist-get record :generation)
            (mevedel-session-durability--newest-generation names))
       record))

(defun mevedel-session-durability--update-decline
    (existing head now accepted-status &optional expected-head-p expected-head)
  "Return the first reason EXISTING may not be updated in place, or nil.
HEAD is the lease head and NOW the target clock.  EXISTING must be this
client's valid, unexpired head in one of ACCEPTED-STATUS; with
EXPECTED-HEAD-P its publication head must also equal EXPECTED-HEAD.  The
reason is a key of `mevedel-session-durability--lease-declines'."
  (cond
   ((not (mevedel-session-durability--valid-lease-p existing)) 'record)
   ((not (memq (plist-get existing :status) accepted-status)) 'status)
   ((not (equal mevedel-session-durability--client-id
                (plist-get existing :client-id)))
    'client)
   ((not (equal existing head)) 'head)
   ((and expected-head-p
         (not (equal expected-head (plist-get existing :publication-head))))
    'publication-head)
   ((not (> (plist-get existing :expires-at) now)) 'expired)))

(defun mevedel-session-durability--updated-record
    (existing status seconds publication-head now)
  "Return EXISTING renewed at NOW for SECONDS in STATUS.
A non-nil PUBLICATION-HEAD replaces its preserved head."
  (let ((mevedel-session-lease-seconds seconds))
    (mevedel-session-durability--lease-record
     (plist-get existing :buffer)
     (plist-get existing :generation)
     status
     (or publication-head (plist-get existing :publication-head))
     (plist-get existing :unsettled-mutation)
     now
     (plist-get existing :transfer-generation))))

(defun mevedel-session-durability--update-committed (committed generation)
  "Classify COMMITTED, the commit result of an update of GENERATION.
`adopt' when the write landed with no newer generation listed, `check-head'
when a newer one appeared beside it (it won only if it is not aborted), and
nil when the write did not land."
  (cond
   ((null committed) nil)
   ((or (not (listp committed))
        (= generation
           (mevedel-session-durability--newest-generation committed)))
    'adopt)
   (t 'check-head)))

(defun mevedel-session-durability--bind-latest (session latest directory now)
  "Bind LATEST, the lease head read after a declined update, to SESSION.
Return the state bound: `owned' while it is this client's live authority at
NOW, `contested' for a claim that has not activated and so has not won
\(the heartbeat keeps running), otherwise `lost'.  DIRECTORY is the lease
directory."
  (let ((state
         (cond
          ((mevedel-session-durability--owned-lease-record-p
            latest directory now)
           'owned)
          ((eq 'claiming (plist-get latest :status)) 'contested)
          (t 'lost))))
    (mevedel-session-durability--bind-lease session latest state)
    state))

(defun mevedel-session-durability--update-owned-lease
    (session accepted-status status seconds
             &optional publication-head expected-publication-head-p
             expected-publication-head)
  "Update SESSION's owned lease from ACCEPTED-STATUS to STATUS for SECONDS.
Replace its preserved head with non-nil PUBLICATION-HEAD.  When
EXPECTED-PUBLICATION-HEAD-P is non-nil, require the current head to equal
EXPECTED-PUBLICATION-HEAD.  A decline signals a named `user-error', as the
asynchronous update reports; the latest authority is still bound to SESSION."
  (if-let* ((session-dir (mevedel-session-save-path session)))
    (let* ((remote-file-name-inhibit-cache t)
           (directory (mevedel-session-durability--lease-path session-dir))
           (bound (mevedel-session-lease session))
           (generation (plist-get bound :generation))
           ;; A transaction that renews repeatedly already knows the bytes it
           ;; last wrote and has a fresh target clock, so it states them as
           ;; the write's precondition instead of observing again.  A
           ;; precondition that no longer holds falls back to observation
           ;; below, so a stale belief costs a round trip and never a wrong
           ;; answer.
           (assumed
            (and (natnump generation)
                 (eq 'owned (plist-get bound :state))
                 (plist-get bound :bytes)
                 (mevedel-session-durability--target-time-cached-p directory)
                 bound))
           (observed (unless assumed
                       (condition-case nil
                           (mevedel-session-durability--observe-lease
                            directory generation)
                         (error nil))))
           ;; The clock arrives from the same process as the records, and still
           ;; through the single target-clock seam.
           (now (let ((mevedel-session-durability--observed-time
                       (plist-get observed :now)))
                  (mevedel-session-durability--target-time directory)))
           (existing (if assumed assumed (plist-get observed :record)))
           (existing-bytes (if assumed
                               (plist-get assumed :bytes)
                             (plist-get observed :bytes)))
           ;; The bound generation is the head whenever no newer record was
           ;; allocated, which is the steady state; only a foreign claim costs
           ;; the extra reads to derive the head.
           (head
            (or (and assumed existing)
                (mevedel-session-durability--observed-head
                 existing (plist-get observed :names))
                (condition-case nil
                    (mevedel-session-durability--lease-head directory)
                  (error nil))))
           (decline (mevedel-session-durability--update-decline
                     existing head now accepted-status
                     expected-publication-head-p expected-publication-head))
           (record (unless decline
                     (mevedel-session-durability--updated-record
                      existing status seconds publication-head now)))
           (verdict
            (and record
                 (mevedel-session-durability--update-committed
                  (mevedel-session-durability--commit-lease
                   directory (plist-get existing :generation)
                   existing-bytes record
                   (min (plist-get existing :expires-at)
                        (plist-get record :expires-at)))
                  (plist-get existing :generation))))
           (latest (cond ((eq verdict 'adopt) record)
                         ;; Retried below without reading anything.
                         ((and assumed (null verdict)) nil)
                         (decline (or head existing))
                         (t (mevedel-session-durability--lease-head
                             directory)))))
      (cond
       ;; An assumed precondition that no longer holds is not a lost lease:
       ;; drop the assumption and decide from the target.
       ((and assumed (null verdict))
        (mevedel-session-durability--bind-lease
         session (mevedel-session-durability--strip-assumption bound)
         (plist-get bound :state))
        (mevedel-session-durability--update-owned-lease
         session accepted-status status seconds
         publication-head expected-publication-head-p
         expected-publication-head))
       ;; A newer generation beside the write won unless it is aborted.
       ((and verdict (equal record latest))
        (mevedel-session-durability--bind-lease
         session record 'owned
         (mevedel-session-durability--record-bytes record))
        t)
       (t
        (mevedel-session-durability--bind-latest session latest directory now)
        (let ((failure (mevedel-session-durability--lease-decline
                        (or decline 'changed))))
          (signal (car failure) (cdr failure))))))
    (let ((failure (mevedel-session-durability--lease-decline 'directory)))
      (signal (car failure) (cdr failure)))))

(defun mevedel-session-durability--gate (session token &optional reentrant)
  "Serialize a lease-changing operation on SESSION behind its async publication.

Outside publication nothing is queued, so this does nothing until
`mevedel-session-publication-barrier' exists.  TOKEN is the preparation token
the barrier accepts from a synchronous operation a queued preparation runs;
without it the operation drains the job, or fails when it cannot.  With
REENTRANT, a window an enclosing synchronous operation already holds on this
stack (publication active, no job) passes: the reserved and held wrappers
nest, and the outer operation was gated on entry.  While a queued preparation
runs, only its own TOKEN passes that way: an unrelated timer firing inside
the preparation's synchronous publication has no token and reaches the
barrier, which refuses it."
  (when (and session
             (fboundp 'mevedel-session-publication-barrier)
             (not (and reentrant
                       (mevedel-session-publication-active-p session)
                       (not (mevedel-session-publication-job session))
                       (let ((preparation
                              (mevedel-session-publication-preparation-token
                               session)))
                         (or (null preparation) (eq preparation token))))))
    (mevedel-session-publication-barrier session token)))

(defun mevedel-session-durability-set-unsettled-mutation (session value &optional token)
  "Set SESSION's durable unsettled-mutation latch to boolean VALUE.
Return non-nil only when the current owned lease generation commits VALUE."
  (unless (booleanp value)
    (error "Unsettled mutation latch must be boolean"))
  ;; An asynchronous publication owns the lease until it settles.
  (mevedel-session-durability--gate session token)
  (when-let* ((session-dir (mevedel-session-save-path session)))
    (let* ((remote-file-name-inhibit-cache t)
           (directory (mevedel-session-durability--lease-path session-dir))
           (bound (mevedel-session-lease session))
           ;; One observation answers the clock, the listing, and the
           ;; bound generation's record.
           (observed (mevedel-session-durability--observe-lease
                      directory (plist-get bound :generation)))
           (now (let ((mevedel-session-durability--observed-time
                       (plist-get observed :now)))
                  (mevedel-session-durability--target-time directory)))
           (existing (plist-get observed :record))
           (head
            (or (mevedel-session-durability--observed-head
                 existing (plist-get observed :names))
                (mevedel-session-durability--lease-head
                 directory
                 (mevedel-session-durability--observed-names observed)))))
      (if (mevedel-session-durability--update-decline
           existing head now '(active publishing))
          (progn
            (mevedel-session-durability--bind-latest
             session (or head existing) directory now)
            nil)
          (if-let* ((successor
                    (mevedel-session-durability--claim-next
                     directory existing (plist-get existing :buffer)
                     (plist-get existing :status) t value nil
                     ;; Its listing is only a predecessor universe when
                     ;; the observation actually listed the directory.
                     (and (plist-get observed :listed) observed))))
              (progn
                ;; The claim wrote exactly these bytes, so a later
                ;; renewal or ownership proof can state them.
                (mevedel-session-durability--bind-lease
                 session successor 'owned
                 (mevedel-session-durability--record-bytes successor))
                t)
            (mevedel-session-durability--bind-latest
             session (mevedel-session-durability--lease-head directory)
             directory now)
            nil)))))

(defun mevedel-session-durability-unsettled-mutation-p (session)
  "Return non-nil when SESSION's bound lease records unsettled mutation."
  (and (plist-get (mevedel-session-lease session) :unsettled-mutation) t))

(defvar mevedel-session-durability--final-commit nil
  "Non-nil while committing a head that ends its publication window.")

(defun mevedel-session-durability-commit-publication-head (session head)
  "Commit immutable session-relative HEAD while SESSION is the current owner.

Return non-nil when SESSION's publishing generation remains the exact
lease head through the commit; otherwise signal the refusal.  A commit bound
as final by `mevedel-session-durability--final-commit' also returns the lease to
`active' with its ordinary expiry: nothing follows it in the window, so a
separate finishing write only repeated the proof the commit just made."
  (unless (mevedel-session-publication-valid-head-p head)
    (error "Publication head must name an immutable manifest"))
  (let ((expected
         (plist-get (mevedel-session-lease session) :publication-head)))
    (if mevedel-session-durability--final-commit
        (mevedel-session-durability--update-owned-lease
         session '(publishing) 'active
         mevedel-session-lease-seconds head t expected)
      (mevedel-session-durability--update-owned-lease
       session '(publishing) 'publishing
       mevedel-session-publication-lease-seconds head t expected))))

(defun mevedel-session-durability--reclaim-own-lease (session)
  "Retake SESSION's own expired lease, returning non-nil on success.

Only a record this client wrote and never released is reclaimed.  A lease
another client holds, or one this client released on purpose, is genuinely
lost and stays that way."
  (let ((lease (mevedel-session-lease session)))
    (and (eq 'lost (plist-get lease :state))
         (not (plist-get lease :release-pending))
         (equal mevedel-session-durability--client-id
                (plist-get lease :client-id))
         (memq (plist-get lease :status) '(claiming active))
         (mevedel-session-durability-lease-acquire
          (mevedel-session-save-path session)
          (plist-get lease :buffer)
          session))))

(defun mevedel-session-durability-lease-renew (session &optional token)
  "Renew SESSION's unexpired owned lease, returning non-nil on success.

Explicit renewal may run wherever the main loop happens to
be waiting, including inside a TRAMP operation.  Target I/O from there is a
reentrant TRAMP call: TRAMP refuses it and the connection can be left wedged
for the operation that was already running.  Renewal therefore performs no
target I/O while another TRAMP operation is in progress, or while a
publication owns the bounded window, and reports the state it already knows.
The next tick renews once the transport is free.  After serialization exits,
renewal also normalizes this client's live `publishing\=' generation back to
`active\='.

A suspended machine stops the timer but not the clock, so a standby longer
than `mevedel-session-lease-seconds' wakes up with this client's own record
already expired and nobody else claiming it.  Renewal reclaims it instead of
reporting a lost lease: taking over from oneself is prompt-free, the
generation compare-and-set still refuses the reclaim if another client won
meanwhile, and binding `lost\=' here would cancel the heartbeat for a
condition that the next mutation heals anyway.

A queued or running asynchronous publication owns the lease too, so a tick
without its preparation TOKEN reports the known state instead of draining it:
the installed timer uses `mevedel-session-durability-heartbeat' instead."
  (when (mevedel-session-lease-renewal-job session)
    (mevedel-session-durability--gate session token))
  (if (or (mevedel-session-publication-active-p session)
          (and (not (and token
                         (eq token
                             (mevedel-session-publication-preparation-token
                              session))))
               (fboundp 'mevedel-session-publication-busy-p)
               (mevedel-session-publication-busy-p session))
          (mevedel-transport-busy-p (mevedel-session-save-path session)))
      (eq 'owned (plist-get (mevedel-session-lease session) :state))
    (condition-case err
        (or (mevedel-session-durability--update-owned-lease
             session '(active publishing) 'active
             mevedel-session-lease-seconds)
            (mevedel-session-durability--reclaim-own-lease session))
      (error
       (if (mevedel-session-durability--lease-decline-p err)
           (mevedel-session-durability--reclaim-own-lease session)
         (mevedel-session-durability--bind-lease
          session (mevedel-session-lease session) 'lost)
         (mevedel-diagnostic-warning (mevedel-diagnostic-scope session)
          'mevedel
          (format "Portable session lease renewal failed: %s" err)
          :warning)
         nil)))))

(defun mevedel-session-durability-lease-owned-p (session)
  "Return non-nil when SESSION has this client's unexpired lease."
  (let* ((lease (mevedel-session-lease session))
         (session-dir (mevedel-session-save-path session)))
    (and session-dir
         (eq 'owned (plist-get lease :state))
         (memq (plist-get lease :status) '(active publishing))
         (equal mevedel-session-durability--client-id
                (plist-get lease :client-id))
         (mevedel-session-durability--finite-nonnegative-number-p
          (plist-get lease :expires-at))
         (condition-case nil
             (> (plist-get lease :expires-at)
                (mevedel-session-durability--target-time
                 (mevedel-session-durability--lease-path session-dir)))
           (error nil)))))

(defun mevedel-session-durability-lease-state (session-dir)
  "Return this client's read-only lease state for portable SESSION-DIR."
  (plist-get (mevedel-session-durability-lease-status session-dir) :state))

(defun mevedel-session-durability-lease-status (session-dir)
  "Return `(:state STATE :host HOST)' for portable SESSION-DIR.

STATE is this client's view of the lease.  HOST names the machine holding
it, or the last one to hold it when the lease is free, and is nil for a
session whose lease predates recorded hosts or was never held at all.  Both
answers come from one head read: a picker asks this per candidate, and on a
remote target each extra read is a round trip the user waits through."
  (mevedel-session-durability--assert-no-pid-lock session-dir)
  (let ((lease
         (mevedel-session-durability--lease-head
          (mevedel-session-durability--lease-path session-dir))))
    (list :state (mevedel-session-durability--lease-state-of session-dir lease)
          :host (plist-get lease :host))))

(defun mevedel-session-durability--lease-state-of (session-dir lease)
  "Return this client's lease state for SESSION-DIR given its head LEASE."
  (cond
   ((null lease) 'available)
   ((not (mevedel-session-durability--valid-lease-p lease))
    (error "Invalid portable session lease: %s" session-dir))
   ((eq (plist-get lease :status) 'released)
    (let ((fence
           (mevedel-session-transfer-release-fence
            (mevedel-session-durability--lease-path session-dir)
            (plist-get lease :generation))))
      (if (and fence
               (not (equal mevedel-session-durability--client-id
                           (plist-get fence :requester-client-id))))
          'foreign
        'available)))
   ((<= (plist-get lease :expires-at)
        (mevedel-session-durability--target-time
         (mevedel-session-durability--lease-path session-dir)))
    'expired)
   ((equal mevedel-session-durability--client-id
           (plist-get lease :client-id))
    'owned)
   (t 'foreign)))

(defun mevedel-session-durability-lease-release (session-dir &optional session token)
  "Release this client's portable lease for SESSION-DIR.
An inactive owned `publishing' generation is releasable, but a live publisher
is never interrupted.  When SESSION is non-nil, cancel its renewal and clear
its lease state."
  (mevedel-session-durability--gate session token)
  (mevedel-session-durability--assert-no-pid-lock session-dir)
  (if (and session (mevedel-session-publication-active-p session))
      (let ((lease (copy-sequence (mevedel-session-lease session))))
        (setf (mevedel-session-lease session)
              (plist-put lease :release-pending t)))
    (let* ((remote-file-name-inhibit-cache t)
           (directory (mevedel-session-durability--lease-path session-dir))
           (lease
            (condition-case nil
                (mevedel-session-durability--lease-head directory)
              (error nil))))
      (when (and (mevedel-session-durability--valid-lease-p lease)
                 (memq (plist-get lease :status) '(active publishing))
                 (equal mevedel-session-durability--client-id
                        (plist-get lease :client-id)))
        (let ((now (mevedel-session-durability--target-time directory)))
          (setq lease (plist-put lease :status 'released)
                lease (plist-put lease :expires-at now)))
        (mevedel-session-durability--write-generation directory lease))
      (when session
        (mevedel-session-durability--cancel-renewal session)
        (setf (mevedel-session-lease session) nil)
        (when-let* ((queued
                    (and (not (mevedel-session-pending-publication session))
                         (mevedel-session-publication-queue session))))
          (let ((batches
                 (append
                  (mevedel-session-publication-uncommitted-batches session)
                  queued)))
            (setf (mevedel-session-publication-uncommitted-batches session) nil
                  (mevedel-session-publication-queue session) nil)
            (mevedel-session-publication--record-pending
             session batches
             '(user-error
               "Session lease released before queued publication completed"))))
        (unless (mevedel-session-pending-publication session)
          (mevedel-session-publication-clear-transient session))))))


;;
;;; Critical publication

(defun mevedel-session-durability--renew-publication-lease (session)
  "Reserve SESSION's bounded lease window for serialized publication I/O."
  (condition-case nil
      (mevedel-session-durability--update-owned-lease
       session '(active publishing) 'publishing
       mevedel-session-publication-lease-seconds)
    (error nil)))

(defun mevedel-session-durability--finish-publication-lease (session)
  "Return SESSION's publishing lease to its ordinary renewable state.
A final head commit already did, and that commit is the latest proof."
  (let ((lease (mevedel-session-lease session)))
    (if (and (eq 'owned (plist-get lease :state))
             (eq 'active (plist-get lease :status)))
        t
      (condition-case nil
          (mevedel-session-durability--update-owned-lease
           session '(publishing) 'active mevedel-session-lease-seconds)
        (error nil)))))

(defun mevedel-session-durability-call-with-reserved-lease
    (session function &optional token)
  "Call FUNCTION while SESSION owns a bounded synchronous mutation lease.

Timer renewal performs no target I/O while FUNCTION runs.  The final ownership
check fails closed.  This wrapper does not publish artifacts, commit a
manifest, or drain SESSION's publication queue.  It first serializes behind
SESSION's asynchronous publication, as `mevedel-session-durability--gate'
describes for TOKEN, and nests inside an enclosing synchronous window."
  (mevedel-session-durability--gate session token t)
  (let ((mevedel-session-durability--transaction-clock
         (or mevedel-session-durability--transaction-clock (list nil)))
        (mevedel-session-durability--asserted-directories
         (or mevedel-session-durability--asserted-directories (list nil)))
        entered failure result)
    (setf (mevedel-session-publication-active-p session) t)
    (unwind-protect
        (condition-case err
            (progn
              (unless (mevedel-session-durability--renew-publication-lease
                       session)
                (user-error
                 "Portable session lease could not reserve publication"))
              (setq entered t
                    result (funcall function)))
          (error (setq failure err)))
      (when (and entered
                 (not (mevedel-session-durability--finish-publication-lease
                       session))
                 (not failure))
        (setq failure
              '(user-error
                "Portable session lease was lost during publication")))
      (setf (mevedel-session-publication-active-p session) nil)
      ;; The pending-release drain lives in the cleanup: a quit or throw
      ;; out of FUNCTION is not an `error' the condition-case sees, and
      ;; it must not strand a lease another client already asked for.
      ;; It runs after the active flag clears, because release defers
      ;; itself behind that flag.
      (when (plist-get (mevedel-session-lease session) :release-pending)
        (condition-case err
            (mevedel-session-durability-lease-release
             (mevedel-session-save-path session) session)
          (error
           (unless failure
             (setq failure err))))))
    (when failure
      (signal (car failure) (cdr failure)))
    result))

(defun mevedel-session-durability-call-with-held-lease
    (session function &optional token)
  "Call FUNCTION while SESSION's publication is held active, reserving nothing.

Like `mevedel-session-durability-call-with-reserved-lease', timer renewal
performs no target I/O and a critical publisher reached meanwhile queues
instead of nesting.  No lease record is written on entry or exit: FUNCTION
proves ownership inside each of its own programs through
`mevedel-session-durability-run-owned-program', so the window it holds needs
no bounded extension.  A release requested meanwhile runs on exit."
  (mevedel-session-durability--gate session token t)
  (let (failure result)
    (setf (mevedel-session-publication-active-p session) t)
    (unwind-protect
        (condition-case err
            (setq result (funcall function))
          (error (setq failure err)))
      (setf (mevedel-session-publication-active-p session) nil)
      (when (plist-get (mevedel-session-lease session) :release-pending)
        (condition-case err
            (mevedel-session-durability-lease-release
             (mevedel-session-save-path session) session)
          (error
           (unless failure
             (setq failure err))))))
    (when failure
      (signal (car failure) (cdr failure)))
    result))

(defun mevedel-session-durability-run-owned-program (session operations)
  "Run OPERATIONS behind a proof that SESSION still owns its lease generation.

The proof is a `verify' of the exact bytes this client last committed for
its bound generation, first in the same target process, so a program whose
ownership no longer holds stops before any of OPERATIONS runs.  Like a lease
commit, the proof is a precondition and not an election: another client can
claim the next generation in between, so the same program lists the lease
directory and a newer live generation ends this client's assumption
afterwards.  Return the results of OPERATIONS, or nil when this client knows
no committed bytes or the proof failed; a failed proof drops the remembered
bytes so the next renewal observes the target instead of assuming."
  (let* ((lease (mevedel-session-lease session))
         (bytes (plist-get lease :bytes))
         (generation (plist-get lease :generation))
         (session-dir (mevedel-session-save-path session)))
    (when (and session-dir bytes (natnump generation)
               (eq 'owned (plist-get lease :state)))
      (let* ((directory (mevedel-session-durability--lease-path session-dir))
             (results
              (mevedel-session-control-fs-run-program
               (append
                (list (list :op 'verify
                            :path (mevedel-session-durability--generation-path
                                   directory generation)
                            :content bytes)
                      (list :op 'list-directory :path directory :optional t))
                operations)))
             (proof (car results))
             (listing (cadr results)))
        (pcase (plist-get proof :status)
          ('ok
           (when (and (eq 'ok (plist-get listing :status))
                      (> (mevedel-session-durability--newest-generation
                          (plist-get listing :value))
                         generation))
             ;; A newer generation appeared.  An aborted claim leaves this
             ;; client the head; anything else is decided by the next
             ;; renewal, which observes instead of assuming.
             (let ((head (mevedel-session-durability--lease-head
                          directory (plist-get listing :value))))
               (unless (mevedel-session-durability--same-generation-p
                        head lease)
                 (mevedel-session-durability--bind-lease
                  session (mevedel-session-durability--strip-assumption lease)
                  (plist-get lease :state)))))
           (cddr results))
          ((or 'mismatch 'absent)
           (mevedel-session-durability--bind-lease
            session (mevedel-session-durability--strip-assumption lease)
            (plist-get lease :state))
           nil)
          (_ (mevedel-session-control-fs-program-value proof)))))))

(defun mevedel-session-durability-run-owned-program-async
    (session operations callback)
  "Run OPERATIONS behind SESSION's ownership proof, then call CALLBACK.

As `mevedel-session-durability-run-owned-program', through
`mevedel-session-control-fs-run-program-async': where the target allows it
nothing waits for the program.  CALLBACK receives the RESULTS of OPERATIONS,
or nil when this client knows no committed bytes or the proof failed, and
ERROR for a program that failed as a whole.

The callback can run inside another remote command, so it changes nothing
but the remembered bytes: a failed proof, or a newer generation in the
listing, strips them from the lease -- when it still carries the bytes the
program proved -- and the next renewal observes the target instead of
assuming."
  (let* ((lease (mevedel-session-lease session))
         (bytes (plist-get lease :bytes))
         (generation (plist-get lease :generation))
         (session-dir (mevedel-session-save-path session)))
    (if (not (and session-dir bytes (natnump generation)
                  (eq 'owned (plist-get lease :state))))
        (funcall callback nil nil)
      (let ((directory (mevedel-session-durability--lease-path session-dir)))
        (mevedel-session-control-fs-run-program-async
         (append
          (list (list :op 'verify
                      :path (mevedel-session-durability--generation-path
                             directory generation)
                      :content bytes)
                (list :op 'list-directory :path directory :optional t))
          operations)
         (lambda (results error)
           (let* ((proof (car results))
                  (listing (cadr results))
                  (proved (eq 'ok (plist-get proof :status)))
                  (superseded
                   (and proved
                        (eq 'ok (plist-get listing :status))
                        (> (mevedel-session-durability--newest-generation
                            (plist-get listing :value))
                           generation))))
             (when (and (not error) (or (not proved) superseded)
                        (equal bytes (plist-get (mevedel-session-lease session)
                                                :bytes)))
               (setf (mevedel-session-lease session)
                     (mevedel-session-durability--strip-assumption
                      (mevedel-session-lease session))))
             (funcall callback (and proved (cddr results)) error)))
         (mevedel-diagnostic-scope session))))))


;;
;;; Asynchronous lease operations

;; Each operation below mirrors a synchronous one, reusing its program
;; planning and result interpretation, and runs every target program through
;; `mevedel-session-control-fs-run-program-async'.  None falls back to a
;; synchronous target program: a step that cannot be answered from its own
;; observation reports an error and leaves the lease binding as it was.
;; CALLBACK receives (SUCCESS ERROR) exactly once and never signals into its
;; caller's continuation; it may run before the operation returns when the
;; carrier is synchronous, and from a sentinel otherwise.

(defvar mevedel-session-durability-async-process-function nil
  "Function called with each process an asynchronous lease step launches, or nil.
Bound by a job around a launch to own the running process, for stopping it at
teardown.  It is part of the captured context, so every later step of the
same operation reports through the function in effect at launch.  A step the
carrier runs synchronously has no process and does not call it.")

(defun mevedel-session-durability-capture-context ()
  "Return the dynamic transaction state an asynchronous operation keeps.

The clock, legacy `.lock' absence, and recovery cells and commit riders are
dynamic bindings that do not survive a returned callback.  Every asynchronous
operation here captures them at launch and rebinds them through
`mevedel-session-durability-call-in-context' while it advances, so a job can
do the same for its own continuations."
  (list mevedel-session-durability--transaction-clock
        mevedel-session-durability--asserted-directories
        (bound-and-true-p mevedel-session-recovery--mutation-cache)
        mevedel-session-durability--commit-riders
        mevedel-session-durability-async-process-function
        mevedel-session-durability--async-valid-p))

(defun mevedel-session-durability-call-in-context (context function)
  "Call FUNCTION with the transaction state CONTEXT captured."
  (dlet ((mevedel-session-durability--transaction-clock (nth 0 context))
         (mevedel-session-durability--asserted-directories (nth 1 context))
         (mevedel-session-recovery--mutation-cache (nth 2 context))
         (mevedel-session-durability--commit-riders (nth 3 context))
         (mevedel-session-durability-async-process-function (nth 4 context))
         (mevedel-session-durability--async-valid-p (nth 5 context))
         ;; Heartbeat continuations cannot borrow a surrounding publisher's flags.
         (mevedel-session-durability--observed-time
          (and (null (nth 5 context)) mevedel-session-durability--observed-time))
         (mevedel-session-durability--final-commit
          (and (null (nth 5 context)) mevedel-session-durability--final-commit))
         ;; An explicit drain may force ordinary async callers synchronous.
         ;; A heartbeat keeps yielding between every phase, even in that drain.
         (mevedel-session-control-fs--async-wait
          (and (null (nth 5 context)) mevedel-session-control-fs--async-wait)))
    (funcall function)))

(defun mevedel-session-durability-fresh-context (process-function)
  "Return a new transaction context for a job that advances over many turns.

Unlike `mevedel-session-durability-capture-context' inside
`mevedel-session-durability-with-transaction', it never joins a surrounding
transaction: its own clock, legacy `.lock' absence, and recovery cells are
shared by every `mevedel-session-durability-call-in-context' with it.  It
carries no commit riders, so a phase sees only riders it binds itself,
never ones of a publisher whose binding a timer happened to run inside.
PROCESS-FUNCTION
owns each process a lease step launches; see
`mevedel-session-durability-async-process-function'.  With no async validity
predicate, observed time, final commit, and the drain flag stay inherited."
  (list (list nil) (list nil) (list nil) nil process-function nil))

(defun mevedel-session-durability-call-outside-transaction (function)
  "Call FUNCTION with no transaction state of the caller in effect.

The clock, legacy `.lock' absence and recovery cells, commit riders, final
commit, and process function are all nil, so arbitrary code called back from
inside a job or timer opens its own transactions.  Callers, to audit
whenever these cells change: the publication acknowledgement in
`mevedel-session-publication--job-complete', and
`mevedel-agent-control-notify', `mevedel-agent-control--flush-notifications',
and `mevedel-session-control-transfer-taken-over'."
  (dlet ((mevedel-session-durability--transaction-clock nil)
         (mevedel-session-durability--asserted-directories nil)
         (mevedel-session-durability--commit-riders nil)
         (mevedel-session-durability--final-commit nil)
         (mevedel-session-durability-async-process-function nil)
         (mevedel-session-recovery--mutation-cache nil))
    (funcall function)))

(defun mevedel-session-durability--async-job (callback &optional scope)
  "Return (FINISH GUARD SCOPE) delivering one result to CALLBACK.
SCOPE is captured diagnostic ownership, or `unavailable' for ownerless work.
FINISH calls CALLBACK with (SUCCESS ERROR) the first time only.  GUARD runs
a thunk and reports its error or quit through FINISH, unless FINISH already
ran: then an error came from CALLBACK itself and propagates.  A quit never
escapes: it finishes the job with the error `(quit)' and GUARD returns, so no
step leaves its job waiting and nothing is re-signalled from the timer or
sentinel that ran it.  A quit inside CALLBACK itself, or after FINISH ran, is
reported as a warning.  A thunk handed to GUARD after FINISH ran, such as the
continuation of a program whose launch was interrupted, does not run."
  (let* (done
         (finish
          (lambda (success error)
            (unless done
              (setq done t)
              (condition-case nil
                  (funcall callback success error)
                (quit
                 (mevedel-diagnostic-warning (or scope 'unavailable)
                  'mevedel "Asynchronous lease callback was interrupted"
                  :warning)))))))
    (list finish
          (lambda (thunk)
            (unless done
              (condition-case err
                  (funcall thunk)
                ((error quit)
                 (cond
                  ((not done)
                   (funcall finish nil err))
                  ((eq 'quit (car err))
                   (mevedel-diagnostic-warning (or scope 'unavailable)
                    'mevedel "Asynchronous lease step was interrupted"
                    :warning))
                  (t (signal (car err) (cdr err))))))))
          scope)))

(defun mevedel-session-durability--async-program
    (directory operations job context continue &optional lock-directory)
  "Run OPERATIONS asynchronously, then call CONTINUE with (RESULTS ERROR).

JOB is `mevedel-session-durability--async-job''s result and CONTEXT the
captured transaction state, which CONTINUE runs under.  The program starts at
once unless a remote operation is in flight for DIRECTORY, in which case it
waits for the transport: the previous step's callback can run inside another
remote command.  A transport that refuses the wait fails the job.
LOCK-DIRECTORY is the shared target lock for the entire program, or nil."
  (let* ((finish (car job))
         (guard (cadr job))
         (owner (nth 4 context))
         (launch
          (lambda ()
            (mevedel-session-durability-call-in-context
             context
             (lambda ()
               (cond
                ((and (nth 5 context) (not (funcall (nth 5 context))))
                 (funcall finish nil '(error "Lease heartbeat superseded")))
                ((and (nth 5 context)
                      (not (mevedel-session-durability--heartbeat-carrier-p directory)))
                 (funcall finish nil '(file-error "Lease heartbeat carrier unavailable")))
                (t
                 (let ((process
                        (apply #'mevedel-session-control-fs-run-program-async
                         operations
                         (lambda (results error)
                           (funcall
                            guard
                            (lambda ()
                              (mevedel-session-durability-call-in-context
                               context
                               (lambda ()
                                 (if (and (nth 5 context)
                                          (not (funcall (nth 5 context))))
                                     (funcall finish nil '(error "Lease heartbeat superseded"))
                                   (funcall continue results error)))))))
                         (append (list (nth 2 job))
                                 (when lock-directory (list lock-directory))))))
                   (when (and process owner)
                     (funcall owner process))))))))))
    (if (mevedel-transport-busy-p directory)
        (unless (mevedel-transport-run-when-idle
                 (list 'mevedel-session-durability-async (make-symbol "step"))
                 directory
                 (lambda () (funcall guard launch))
                 (lambda ()
                   (funcall finish nil
                            '(user-error "Portable lease operation cancelled"))))
          (funcall finish nil
                   '(user-error "Portable lease operation could not wait")))
      (funcall guard launch))))

(defun mevedel-session-durability--known-target-time (directory &optional observed)
  "Return a target clock reading for DIRECTORY without target I/O, or nil.
OBSERVED is a reading taken by the caller's own program.  Otherwise only the
transaction's still-fresh reading answers."
  (let ((seconds (or observed mevedel-session-durability--observed-time)))
    (if (numberp seconds)
        seconds
      (let ((cached (alist-get directory
                               (car mevedel-session-durability--transaction-clock)
                               nil nil #'equal)))
        (and cached
             (< (- (float-time) (cdr cached))
                mevedel-session-durability--clock-reuse-seconds)
             (car cached))))))

(defun mevedel-session-durability--observe-lease-async
    (directory generation extra job context continue)
  "Observe DIRECTORY as `mevedel-session-durability--observe-lease' does.
CONTINUE receives its plist.  The program always reads the target clock: the
transaction's reading can age out before the result is used, and the
synchronous reading it would fall back to is not available here."
  (let ((plan (mevedel-session-durability--observe-lease-plan
               directory generation extra t)))
    (mevedel-session-durability--async-program
     directory (plist-get plan :operations) job context
     (lambda (results error)
       (if error
           (funcall (car job) nil error)
         (funcall continue
                  (mevedel-session-durability--observe-lease-settle
                   directory plan results)))))))

(defun mevedel-session-durability--lease-head-async
    (directory names job context continue &optional previous-paths)
  "Pass CONTINUE DIRECTORY's latest non-aborted lease record, or nil.
NAMES is an observed listing, as for `mevedel-session-durability--lease-head'.
JOB and CONTEXT belong to `mevedel-session-durability--async-program'.
PREVIOUS-PATHS marks the one refresh after those paths yielded no head."
  (if (null names)
      (mevedel-session-durability--async-program
       directory
       (list (list :op 'list-directory :path directory :optional t))
       job context
       (lambda (results error)
         (if error
             (funcall (car job) nil error)
           (let ((listing (car results)))
             (mevedel-session-durability--lease-head-async
              directory
              (if (eq 'absent (plist-get listing :status))
                  'none
                (or (mevedel-session-control-fs-program-value listing) 'none))
              job context continue previous-paths)))))
    (let ((paths (mevedel-session-durability--generation-paths
                  directory names)))
      (if (or (null paths) (equal paths previous-paths))
          (funcall continue nil)
        (mevedel-session-durability--async-program
         directory (mevedel-session-durability--read-operations paths)
         job context
         (lambda (results error)
           (if error
               (funcall (car job) nil error)
             (let ((head (mevedel-session-durability--head-of-records
                          (mevedel-session-durability--parse-records results))))
               (cond
                (head (funcall continue head))
                (previous-paths
                 (error "Portable session lease changed during discovery"))
                (t
                 (mevedel-session-durability--lease-head-async
                  directory nil job context continue paths)))))))))))

(defconst mevedel-session-durability--lease-declines
  '((directory . "Portable session has no save directory")
    (record . "Portable session lease record is not valid")
    (status . "Portable session lease is not in a state this update accepts")
    (client . "Portable session lease belongs to another client")
    (head . "Portable session has a newer lease generation")
    (publication-head . "Portable session publication head changed")
    (expired . "Portable session lease has expired")
    ;; Only the change is observed, not who made it.
    (changed . "Portable session lease changed before this update committed"))
  "Named causes of a lease update's decline.
Messages name the condition only, never paths or lease bytes.")

(defun mevedel-session-durability--lease-decline (cause)
  "Return the `user-error' condition data naming decline CAUSE."
  (list 'user-error
        (alist-get cause mevedel-session-durability--lease-declines)))

(defun mevedel-session-durability--lease-decline-p (error)
  "Return non-nil when ERROR names a lease-update decline, not a failure."
  (and (eq 'user-error (car-safe error))
       (rassoc (cadr error) mevedel-session-durability--lease-declines)))

(defun mevedel-session-durability--update-async
    (session accepted-status status seconds publication-head
             expected-head-p expected-head job context)
  "Advance an asynchronous `mevedel-session-durability--update-owned-lease'.
JOB and CONTEXT are as for `mevedel-session-durability--async-program'."
  (let ((finish (car job)))
    (if-let* ((session-dir (mevedel-session-save-path session)))
        (let* ((directory (mevedel-session-durability--lease-path session-dir))
               (bound (mevedel-session-lease session))
               (generation (plist-get bound :generation))
               ;; As in the synchronous update, a transaction that renews
               ;; repeatedly states the bytes it last wrote instead of
               ;; observing, while its clock reading is fresh.
               (assumed-now
                (and (natnump generation)
                     (eq 'owned (plist-get bound :state))
                     (plist-get bound :bytes)
                     (mevedel-session-durability--known-target-time
                      directory)))
               (retry
                (lambda ()
                  (mevedel-session-durability--update-async
                   session accepted-status status seconds publication-head
                   expected-head-p expected-head job context)))
               (decide
                (lambda (observed)
                  (let* ((assumed (and assumed-now bound))
                         (now (or assumed-now (plist-get observed :now)))
                         (existing (or assumed (plist-get observed :record)))
                         (existing-bytes
                          (if assumed
                              (plist-get assumed :bytes)
                            (plist-get observed :bytes))))
                    (unless now
                      (error "Portable session target clock unavailable"))
                    (mevedel-session-durability--update-async-decide
                     session accepted-status status seconds publication-head
                     expected-head-p expected-head job context
                     directory assumed now existing existing-bytes
                     (and (not assumed)
                          (not (mevedel-session-durability--observed-head
                                existing (plist-get observed :names))))
                     retry)))))
          (if assumed-now
              (funcall decide nil)
            (mevedel-session-durability--observe-lease-async
             directory generation nil job context decide)))
      (funcall finish nil (mevedel-session-durability--lease-decline
                           'directory)))))

(defun mevedel-session-durability--update-async-decide
    (session accepted-status status seconds publication-head
             expected-head-p expected-head job context
             directory assumed now existing existing-bytes read-head retry)
  "Decide and commit an asynchronous lease update from its observation.
EXISTING is the observed (or assumed) bound record and EXISTING-BYTES its
target content.  READ-HEAD means the head must be read, because the bound
generation is not known to be the newest.  RETRY observes afresh after an
assumption that no longer held.  The rest is as for
`mevedel-session-durability--update-async'."
  (let* ((finish (car job))
         (bound (mevedel-session-lease session))
         (settle-lost
          (lambda (latest)
            (mevedel-session-durability--bind-latest
             session latest directory now)
            (funcall finish nil (mevedel-session-durability--lease-decline
                                 'changed))))
         (drop-assumption
          (lambda ()
            (mevedel-session-durability--bind-lease
             session (mevedel-session-durability--strip-assumption bound)
             (plist-get bound :state))
            (funcall retry)))
         (proceed
          (lambda (head)
            (if-let* ((cause (mevedel-session-durability--update-decline
                              existing head now accepted-status
                              expected-head-p expected-head)))
                (if assumed
                    (funcall drop-assumption)
                  (mevedel-session-durability--bind-latest
                   session (or head existing) directory now)
                  (funcall finish nil
                           (mevedel-session-durability--lease-decline cause)))
                (let* ((record
                        (mevedel-session-durability--updated-record
                         existing status seconds publication-head now))
                       (plan
                        (mevedel-session-durability--commit-lease-plan
                         directory (plist-get existing :generation)
                         existing-bytes record
                         (min (plist-get existing :expires-at)
                              (plist-get record :expires-at))))
                       (adopt
                        (lambda ()
                          (mevedel-session-durability--bind-lease
                           session record 'owned
                           (mevedel-session-durability--record-bytes record))
                          (funcall finish t nil))))
                  (mevedel-session-durability--async-program
                   directory (plist-get plan :operations) job context
                   (lambda (results error)
                     (if error
                         (funcall finish nil error)
                       (pcase (mevedel-session-durability--update-committed
                               (mevedel-session-durability--commit-lease-settle
                                directory plan results)
                               (plist-get existing :generation))
                         ('adopt (funcall adopt))
                         ;; A newer generation beside the write won unless
                         ;; it is aborted.
                         ('check-head
                          (mevedel-session-durability--lease-head-async
                           directory nil job context
                           (lambda (latest)
                             (if (equal record latest)
                                 (funcall adopt)
                               (funcall settle-lost latest)))))
                         ;; An assumed precondition that no longer holds is
                         ;; not a lost lease: decide from the target.
                         (_ (if assumed
                                (funcall drop-assumption)
                              (mevedel-session-durability--lease-head-async
                               directory nil job context
                               settle-lost))))))))))))
    (if read-head
        (mevedel-session-durability--lease-head-async
         directory nil job context proceed)
      (funcall proceed existing))))

(defun mevedel-session-durability--update-owned-lease-async
    (session accepted-status status seconds callback
             &optional publication-head expected-head-p expected-head)
  "Update SESSION's owned lease asynchronously, then call CALLBACK.

As `mevedel-session-durability--update-owned-lease' with the arguments of the
same names: CALLBACK receives SUCCESS, non-nil only when the update
committed, and ERROR, condition data when a program failed, the commit's
prelude failed, or no target clock could be had.  Such an error leaves the
lease binding unchanged.  A decline carries a `user-error' naming the first
condition observed (see `mevedel-session-durability--lease-declines'); the
binding is then rebound as the synchronous update does when the lease is
lost.  A nil SUCCESS always carries ERROR.  Commit riders and the
transaction cells in effect at launch stay in effect across the asynchronous
steps."
  (let ((job (mevedel-session-durability--async-job callback (mevedel-diagnostic-scope session)))
        (context (mevedel-session-durability-capture-context)))
    (funcall (cadr job)
             (lambda ()
               (mevedel-session-durability--update-async
                session accepted-status status seconds publication-head
                expected-head-p expected-head job context)))))

(defun mevedel-session-durability-reserve-publication-lease-async
    (session callback)
  "Reserve SESSION's bounded publication window, then call CALLBACK.
As `mevedel-session-durability--renew-publication-lease': SUCCESS is non-nil
when the lease now publishes over SESSION's cached publication head.
The caller marks publication active."
  (mevedel-session-durability--update-owned-lease-async
   session '(active publishing) 'publishing
   mevedel-session-publication-lease-seconds callback
   nil t (plist-get (mevedel-session-publication session) :head)))

(defun mevedel-session-durability-finish-publication-lease-async
    (session callback)
  "Return SESSION's publishing lease to `active', then call CALLBACK.
As `mevedel-session-durability--finish-publication-lease'.  CALLBACK runs
before this returns when the lease is already owned and active."
  (let ((lease (mevedel-session-lease session)))
    (if (and (eq 'owned (plist-get lease :state))
             (eq 'active (plist-get lease :status)))
        (funcall callback t nil)
      (mevedel-session-durability--update-owned-lease-async
       session '(publishing) 'active mevedel-session-lease-seconds callback))))

(defun mevedel-session-durability-commit-publication-head-async
    (session head callback)
  "Commit immutable HEAD for SESSION asynchronously, then call CALLBACK.
As `mevedel-session-durability-commit-publication-head', including a final
commit bound at launch by `mevedel-session-durability--final-commit'.
SUCCESS is non-nil only when the publishing generation stayed the exact head
through the commit."
  (if (not (mevedel-session-publication-valid-head-p head))
      (funcall callback nil
               '(error "Publication head must name an immutable manifest"))
    (let ((expected
           (plist-get (mevedel-session-lease session) :publication-head)))
      (if mevedel-session-durability--final-commit
          (mevedel-session-durability--update-owned-lease-async
           session '(publishing) 'active mevedel-session-lease-seconds
           callback head t expected)
        (mevedel-session-durability--update-owned-lease-async
         session '(publishing) 'publishing
         mevedel-session-publication-lease-seconds callback head t expected)))))

(defun mevedel-session-durability-observe-publication-head-async
    (session callback)
  "Observe SESSION's authoritative lease head asynchronously.

CALLBACK receives a plist, or ERROR: `:lease' is the newest non-aborted lease
record on the target whoever owns it, `:publication-head' its publication
head, `:now' the target clock, and `:owned' non-nil when that record is this
client's live authority.  The lease binding is not changed.  Use it to decide
an uncertain commit: nothing is rolled back from a head this has not read."
  (let ((job (mevedel-session-durability--async-job callback (mevedel-diagnostic-scope session)))
        (context (mevedel-session-durability-capture-context)))
    (funcall
     (cadr job)
     (lambda ()
       (if-let* ((session-dir (mevedel-session-save-path session)))
           (let* ((directory (mevedel-session-durability--lease-path
                              session-dir))
                  (finish (car job))
                  (generation
                   (plist-get (mevedel-session-lease session) :generation)))
             (mevedel-session-durability--observe-lease-async
              directory generation nil job context
              (lambda (observed)
                (let* ((now (plist-get observed :now))
                       (record (plist-get observed :record))
                       (report
                        (lambda (head)
                          (funcall
                           finish
                           (list :lease head
                                 :publication-head
                                 (plist-get head :publication-head)
                                 :now now
                                 :owned
                                 (and now
                                      (mevedel-session-durability--owned-lease-record-p
                                       head directory now)))
                           nil))))
                  (if (mevedel-session-durability--observed-head
                       record (plist-get observed :names))
                      (funcall report record)
                    (mevedel-session-durability--lease-head-async
                     directory
                     (mevedel-session-durability--observed-names observed)
                     job context report))))))
         (funcall (car job) nil '(error "Session has no save path")))))))

(defun mevedel-session-durability-bind-observed-lease (session observed)
  "Bind the head OBSERVED by the publication-head observation to SESSION.
Return the state bound: `owned' when it is this client's live authority,
`contested' for an unactivated claim beside it, otherwise `lost'.  No target
I/O.  An owned head is bound with the bytes this client writes for it, which
is exactly what a commit of that record put on the target."
  (let* ((head (plist-get observed :lease))
         (state (cond
                 ((plist-get observed :owned) 'owned)
                 ((eq 'claiming (plist-get head :status)) 'contested)
                 (t 'lost))))
    (mevedel-session-durability--bind-lease
     session head state
     (and (eq state 'owned)
          (mevedel-session-durability--record-bytes head)))
    state))

(defun mevedel-session-durability-admit-async (session callback)
  "Admit a mutation of SESSION from one asynchronous target program.

The program is the one the synchronous admission runs: target clock, lease
listing, the bound generation's record, the transfer request listing, the
legacy `.lock' absence probe, and the recovery-root probe.  Its absence,
clock, and recovery observations are recorded in the transaction cells as the
synchronous admission records them.

CALLBACK receives SUCCESS: t when SESSION's bound lease is this client's live
newest generation, no recovery marker exists, and no live transfer request
names this client.  Remembered lease bytes, when present, must match the
target; otherwise the observed record proves ownership without rebinding
SESSION's lease.  The target head must match SESSION's cached publication.
A nil SUCCESS always carries ERROR, as condition data:
a `user-error' naming the first observed cause when the warm path declines
-- a missing save path, session ID, bound lease generation or
ownership, quiescing or released state, an
unobserved lease directory, a lease record that is not this client's live
lease, target bytes that differ from the bound ones, a changed publication
head, a newer generation, a recovery marker, or a live transfer request --
and otherwise the error of a
failed program, an invalid request, or a legacy `.lock'.  Local checks (pending
publication, target incarnation, disclosure, materialization) stay with the
caller."
  (let* ((job (mevedel-session-durability--async-job callback (mevedel-diagnostic-scope session)))
         (finish (car job))
         (context (mevedel-session-durability-capture-context)))
    (funcall
     (cadr job)
     (lambda ()
       (let* ((session-dir (mevedel-session-save-path session))
              (session-id (mevedel-session-session-id session))
              (bound (mevedel-session-lease session))
              (generation (plist-get bound :generation))
              (decline
               (cond
                ((not session-dir) "Session has no save path")
                ((not session-id) "Session has no session ID")
                ((not (natnump generation))
                 "Portable session has no bound lease generation")
                ((not (eq 'owned (plist-get bound :state)))
                 "Portable session lease is not owned by this client")
                ((memq (plist-get (mevedel-session-control-transfer session)
                                  :state)
                       '(quiescing released))
                 "Session is quiescing for cooperative control transfer"))))
         (if decline
             (funcall finish nil (list 'user-error decline))
           (let* ((directory (mevedel-session-durability--lease-path
                              session-dir))
                  (requests-directory
                   (mevedel-session-transfer--path directory "requests"))
                  (pid-lock (mevedel-session-durability--pid-lock-operation
                             session-dir)))
             (mevedel-session-durability--observe-lease-async
              directory generation
              (append
               (list (mevedel-session-transfer--requests-operation directory))
               (and pid-lock (list pid-lock))
               (list (list :op 'directory-p
                           :path (mevedel-session-recovery--root session-dir)
                           :optional t)))
              job context
              (lambda (observed)
                (mevedel-session-durability--admit-async-settle
                 session session-dir session-id bound directory
                 requests-directory pid-lock observed job context)))))))))
  nil)

(defun mevedel-session-durability--admit-async-settle
    (session session-dir session-id bound directory requests-directory
             pid-lock observed job context)
  "Settle the admission program's OBSERVED results for SESSION.
The arguments are what `mevedel-session-durability-admit-async' derived
before launch; JOB and CONTEXT are as for
`mevedel-session-durability--async-program'."
  (let* ((finish (car job))
         (results (plist-get observed :extra))
         (now (plist-get observed :now))
         (record (plist-get observed :record))
         (generation (plist-get bound :generation)))
    (if (or (null now)
            (cl-some (lambda (result) (eq 'skipped (plist-get result :status)))
                     results))
        ;; An absent lease directory stops the program before its probes.
        (funcall finish nil
                 '(user-error
                   "Portable session lease directory could not be observed"))
      (let* ((requests (pop results))
             (_ (when pid-lock
                  (mevedel-session-durability--settle-pid-lock
                   session-dir (pop results))))
             (root (car results))
             (unowned
              (cond
               ((not (mevedel-session-durability--owned-lease-record-p
                      record directory now))
                "Portable session lease record is not this client's live lease")
               ((and (plist-get bound :bytes)
                     (not (equal (plist-get bound :bytes)
                                 (plist-get observed :bytes))))
                "Portable session lease on the target differs from the bound lease")
               ((not (equal (plist-get record :publication-head)
                            (plist-get (mevedel-session-publication session) :head)))
                "Portable session publication head changed")
               ((not (equal generation
                            (mevedel-session-durability--newest-generation
                             (plist-get observed :names))))
                "A newer portable session lease generation exists"))))
        (pcase (plist-get root :status)
          ('absent
           (mevedel-session-recovery-refresh session root))
          ('ok nil)
          (_ (mevedel-session-control-fs-program-value root)
             (error "Invalid specialized recovery root")))
        (let ((newest
               (and (not (eq 'absent (plist-get requests :status)))
                    (car (sort
                          (delq nil
                                (mapcar
                                 (lambda (name)
                                   (and (string-match-p
                                         "\\`request-[0-9]\\{20\\}\\.el\\'"
                                         name)
                                        (expand-file-name
                                         name requests-directory)))
                                 (mevedel-session-control-fs-program-value
                                  requests)))
                          #'string>)))))
          (cond
           (unowned (funcall finish nil (list 'user-error unowned)))
           ((not (eq 'absent (plist-get root :status)))
            (funcall finish nil
                     '(user-error
                       "Portable session has a pending recovery marker")))
           ((null newest) (funcall finish t nil))
           (t
            (mevedel-session-durability--async-program
             directory
             (list (list :op 'read :path newest :optional t))
             job context
             (lambda (read-results error)
               (if error
                   (funcall finish nil error)
                 (let* ((result (car read-results))
                        (request
                         (and (eq 'ok (plist-get result :status))
                              (condition-case nil
                                  (car (read-from-string
                                        (plist-get result :value)))
                                (error nil)))))
                   (unless (mevedel-session-transfer--valid-request-p
                            request now)
                     (error "Invalid portable control-transfer request: %s"
                            newest))
                   ;; A request naming this client at or below its
                   ;; generation is live.
                   (if (and (<= (plist-get request :generation) generation)
                            (equal session-id
                                   (plist-get request :session-id))
                            (equal mevedel-session-durability--client-id
                                   (plist-get request :owner-client-id)))
                       (funcall
                        finish nil
                        '(user-error
                          "Portable session has a live control-transfer request"))
                     (funcall finish t nil)))))))))))))

(defun mevedel-session-durability--heartbeat-carrier-p (path)
  "Return non-nil when PATH can launch a heartbeat without synchronous fallback.
Remote interpreter lookup must already be warm.  This qualifies a launch,
not a completed result: process failure itself invalidates that cache."
  (let ((remote (file-remote-p path)))
    (or (null remote)
        (and (gethash remote mevedel-session-control-fs--programs)
             (mevedel-session-control-fs--pipe-capable-p remote)))))

(defun mevedel-session-durability--heartbeat-valid-p (job)
  "Return non-nil while JOB still owns its original live session projection."
  (let ((session (mevedel-session-durability--heartbeat-session job))
        (path (mevedel-session-durability--heartbeat-path job)))
    (and (eq job (mevedel-session-lease-renewal-job session))
         (not (mevedel-session-durability--heartbeat-cancelled job))
         (equal path (mevedel-session-save-path session))
         (eq (mevedel-session-durability--heartbeat-lease job)
             (mevedel-session-lease session))
         (eq (mevedel-session-durability--heartbeat-buffer job)
             (mevedel-session-root-buffer session))
         (or (null (mevedel-session-durability--heartbeat-buffer job))
             (buffer-live-p (mevedel-session-durability--heartbeat-buffer job))))))

(defun mevedel-session-durability--heartbeat-finish (job error)
  "Retire JOB, adopting its verified projection unless superseded, or ERROR."
  (let* ((session (mevedel-session-durability--heartbeat-session job))
         (shadow (mevedel-session-durability--heartbeat-shadow job))
         (valid (mevedel-session-durability--heartbeat-valid-p job)))
    (when (eq job (mevedel-session-lease-renewal-job session))
      (setf (mevedel-session-lease-renewal-job session) nil)
      (when valid
        (let ((lease (mevedel-session-lease shadow)))
          (mevedel-session-durability--bind-lease
           session lease (if error 'lost (plist-get lease :state))
           (and (not error) (plist-get lease :bytes))))
        (when error
          (mevedel-diagnostic-warning (mevedel-diagnostic-scope session) 'mevedel
                           (format "Portable session heartbeat failed: %s" error)
                           :warning)))
      (when (mevedel-session-publication-busy-p session)
        (mevedel-session-publication--schedule session)))))

(defun mevedel-session-durability--heartbeat-verify (shadow callback)
  "Observe SHADOW's final authority afresh, then call CALLBACK with an error."
  (let* ((directory (mevedel-session-durability--lease-path
                     (mevedel-session-save-path shadow)))
         (proposed (mevedel-session-lease shadow))
         (job (mevedel-session-durability--async-job
               (lambda (_success error) (funcall callback error))
               (mevedel-diagnostic-scope shadow)))
         (context (mevedel-session-durability-capture-context)))
    (mevedel-session-durability--observe-lease-async
     directory (plist-get proposed :generation) nil job context
     (lambda (observed)
       (unless (numberp (plist-get observed :now))
         (error "Portable session target clock unavailable"))
       (let ((settle
              (lambda (head)
                (let ((owned
                       (and (equal (plist-get proposed :bytes)
                                   (mevedel-session-durability--record-bytes head))
                            (mevedel-session-durability--owned-lease-record-p
                             head directory (plist-get observed :now)))))
                  (mevedel-session-durability--bind-lease
                   shadow head
                   (cond (owned 'owned)
                         ((eq 'claiming (plist-get head :status)) 'contested)
                         (t 'lost))
                   (and owned (mevedel-session-durability--record-bytes head)))
                  (funcall (car job) owned nil)))))
         (if (eql (plist-get proposed :generation)
                  (mevedel-session-durability--newest-generation
                   (plist-get observed :names)))
             (funcall settle (plist-get observed :record))
           (mevedel-session-durability--lease-head-async
            directory (mevedel-session-durability--observed-names observed)
            job context settle)))))))

(defun mevedel-session-durability--heartbeat-reclaim (shadow callback)
  "Reclaim SHADOW's expired active or claiming lease for this client only.
CALLBACK receives SUCCESS and ERROR.  Election, predecessor comparison and
abort follow `mevedel-session-durability--claim-next', using real programs."
  (let* ((directory (mevedel-session-durability--lease-path
                     (mevedel-session-save-path shadow)))
         (job (mevedel-session-durability--async-job callback (mevedel-diagnostic-scope shadow)))
         (lock-directory (mevedel-session-durability--lease-lock-directory directory))
         (context (mevedel-session-durability-capture-context)))
    (mevedel-session-durability--observe-lease-async
     directory nil nil job context
     (lambda (observed)
       (unless (numberp (plist-get observed :now))
         (error "Portable session target clock unavailable"))
       (unless (plist-get observed :listed)
         (error "Unreadable portable lease directory"))
       (mevedel-session-durability--validate-generation-names (plist-get observed :names))
       (mevedel-session-durability--lease-head-async
        directory (mevedel-session-durability--observed-names observed) job context
        (lambda (head)
          (if (not (and (mevedel-session-durability--valid-lease-p head)
                        (equal mevedel-session-durability--client-id
                               (plist-get head :client-id))
                        (memq (plist-get head :status) '(claiming active))
                        (<= (plist-get head :expires-at) (plist-get observed :now))
                        (not (plist-get (mevedel-session-lease shadow) :release-pending))))
              (funcall (car job) nil nil)
            (let* ((generation (1+ (mevedel-session-durability--newest-generation
                                    (plist-get observed :names))))
                   (path (mevedel-session-durability--generation-path directory generation))
                   (older (mevedel-session-durability--generation-paths
                           directory (mevedel-session-durability--observed-names observed)))
                   (candidate (mevedel-session-durability--lease-record
                               (plist-get head :buffer) generation 'claiming
                               (plist-get head :publication-head)
                               (plist-get head :unsettled-mutation)
                               (plist-get observed :now)
                               (plist-get head :transfer-generation))))
              (mevedel-session-durability--async-program
               directory
               (cons (list :op 'create :path path
                           :content (mevedel-session-durability--record-bytes candidate))
                     (mevedel-session-durability--read-operations older))
               job context
               (lambda (results error)
                 (when error (signal (car error) (cdr error)))
                 (let ((created (car results)))
                   (if (eq 'conflict (plist-get created :status))
                       (funcall (car job) nil nil)
                     (mevedel-session-control-fs-program-value created)
                     (let* (validation-error
                            (same (condition-case failure
                                      (equal head
                                             (mevedel-session-durability--validated-head
                                              older (mevedel-session-durability--parse-records
                                                     (cdr results) t)))
                                    (error (setq validation-error failure) nil)))
                            (record (plist-put (copy-sequence candidate) :status
                                               (if same 'active 'aborted))))
                       (mevedel-session-durability--async-program
                        directory
                        (append (list (list :op 'verify :path path
                                            :content (mevedel-session-durability--record-bytes candidate))
                                      (list :op 'write :path path
                                            :content (mevedel-session-durability--record-bytes record)))
                              (mapcar (lambda (old)
                                        (list :op 'delete-file :path old :optional t))
                                      (if same older (list path))))
                        job context
                        (lambda (settled failure)
                          (when failure (signal (car failure) (cdr failure)))
                          (mevedel-session-control-fs-program-value (car settled))
                          (mevedel-session-control-fs-program-value (cadr settled))
                          (when same
                            (mevedel-session-durability--bind-lease
                             shadow record 'owned
                             (mevedel-session-durability--record-bytes record)))
                          (funcall (car job) same validation-error)) lock-directory)))))
               lock-directory)))))))))

(defun mevedel-session-durability-heartbeat (session)
  "Renew SESSION from its timer without waiting for target programs.
Single-flight ownership shares the publication gate.  Explicit mutation and
teardown drain it; subsequent ticks never drain.  Unsupported remote carriers
and synchronous-drain contexts skip a tick instead of blocking the editor.
After takeover, retry stopping producers instead of renewing lost authority."
  (if (eq 'taken-over
          (plist-get (mevedel-session-control-transfer session) :state))
      (unless (or mevedel-session-durability--heartbeat-inhibit
                  (eq 'owned (plist-get (mevedel-session-lease session) :state))
                  (mevedel-transport-busy-p (mevedel-session-save-path session)))
        (if (buffer-live-p (mevedel-session-root-buffer session))
            (progn
              (mevedel-session-publication-settle-taken-over session)
              (mevedel-session-control-transfer-taken-over
               session (mevedel-session-lease session)))
          (mevedel-session-durability--cancel-renewal session)))
    (let ((path (mevedel-session-save-path session)))
      (unless (or (null path) mevedel-session-durability--heartbeat-inhibit
                  mevedel-session-control-fs--async-wait
                  (plist-get (mevedel-session-lease session) :release-pending)
                  (mevedel-session-publication-stopping-p session)
                  (mevedel-session-publication-active-p session)
                  (mevedel-transport-busy-p path)
                  (not (mevedel-session-durability--heartbeat-carrier-p path)))
        (unless (mevedel-session-publication-busy-p session)
          (let* ((job (mevedel-session-durability--heartbeat-create
                       :session session :path path :lease (mevedel-session-lease session)
                       :buffer (mevedel-session-root-buffer session)))
                 (shadow (mevedel-session--create
                          :save-path path :lease (copy-sequence (mevedel-session-lease session))
                          :lease-renewal-job job))
                 (mevedel-session-durability--async-valid-p
                  (lambda () (mevedel-session-durability--heartbeat-valid-p job))))
            (setf (mevedel-session-durability--heartbeat-shadow job) shadow
                  (mevedel-session-lease-renewal-job session) job)
            ;; A timer may interrupt another session's publisher.  Its cells,
            ;; riders and process owner must never become this job's context.
            (dlet ((mevedel-session-durability--transaction-clock (list nil))
                   (mevedel-session-durability--asserted-directories (list nil))
                   (mevedel-session-recovery--mutation-cache (list nil))
                   (mevedel-session-durability--observed-time nil)
                   (mevedel-session-durability--commit-riders nil)
                   (mevedel-session-durability--final-commit nil)
                   (mevedel-session-durability-async-process-function nil))
              (let* ((verify (lambda ()
                               (mevedel-session-durability--heartbeat-verify
                                shadow (lambda (error)
                                         (mevedel-session-durability--heartbeat-finish job error)))))
                     (finish (lambda (success error)
                               ;; A named decline is a lost lease, not a failure.
                               (if (and error
                                        (not (mevedel-session-durability--lease-decline-p
                                              error)))
                                   (mevedel-session-durability--heartbeat-finish job error)
                                 (if success (funcall verify)
                                   (mevedel-session-durability--heartbeat-reclaim
                                    shadow (lambda (_reclaimed failure)
                                             (if failure
                                                 (mevedel-session-durability--heartbeat-finish job failure)
                                               (funcall verify)))))))))
                (mevedel-session-durability--update-owned-lease-async
                 shadow '(active publishing) 'active mevedel-session-lease-seconds finish)))))))))

(defun mevedel-session-durability-adopt-owned-lease (session source &optional token)
  "Move SOURCE's verified owned lease and path into SESSION.

Verify SOURCE against its target before changing either object.  Renewal timer
creation also precedes mutation, so failure leaves SESSION and SOURCE intact."
  (mevedel-session-durability--gate session token)
  (mevedel-session-durability-call-with-reserved-lease source #'ignore token)
  (unless (mevedel-session-durability-lease-owned-p source)
    (error "Committed child lease is not owned"))
  (let ((lease (copy-sequence (mevedel-session-lease source)))
        (save-path (mevedel-session-save-path source))
        (timer
         (run-at-time
          mevedel-session-lease-renewal-seconds
          mevedel-session-lease-renewal-seconds
          #'mevedel-session-durability-heartbeat session)))
    (mevedel-session-durability--cancel-renewal session)
    (mevedel-session-durability--cancel-renewal source)
    (setf (mevedel-session-save-path session) save-path
          (mevedel-session-lease session) lease
          (mevedel-session-lease-renewal-timer session) timer
          (mevedel-session-lease source) nil
          (mevedel-session-lease-renewal-timer source) nil)
    t))


(defun mevedel-session-durability-forget-removed-session (session &optional token)
  "Clear local durability state after SESSION's owned directory was removed.

The caller removes the target directory while its lease is still held.  This
function performs no target I/O and must run only after that removal."
  (mevedel-session-durability--gate session token)
  (mevedel-session-durability--cancel-renewal session)
  (setf (mevedel-session-lease session) nil
        (mevedel-session-publication session) nil
        (mevedel-session-publication-active-p session) nil)
  (mevedel-session-publication-clear-transient session))


(provide 'mevedel-session-durability)

;;; mevedel-session-durability.el ends here
