;;; mevedel-session-durability.el -- Portable session durability -*- lexical-binding: t -*-

;;; Commentary:

;; Owns portable project-session leases and the storage primitives used by
;; durable session publication.  File-workspace sessions retain their PID lock.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-structs))

;; Every control operation in this file runs through the session control
;; filesystem, so its feature is a hard load-time dependency rather than a
;; lazily reachable one.
(require 'mevedel-session-control-fs)

;; `files'
(defvar remote-file-name-inhibit-cache)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-identity
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))

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
                  (path &optional coding-system))
(declare-function mevedel-session-control-fs-target-time
                  "mevedel-session-control-fs" (directory))
(declare-function mevedel-session-control-fs-write-file
                  "mevedel-session-control-fs"
                  (path content &optional coding-system))

;; `mevedel-session-publication'
(declare-function mevedel-session-publication--record-pending
                  "mevedel-session-publication" (session batches err))
(declare-function mevedel-session-publication-clear-transient
                  "mevedel-session-publication" (session))
(declare-function mevedel-session-publication-logical-path-p
                  "mevedel-session-publication" (path))
(declare-function mevedel-session-publication-valid-head-p
                  "mevedel-session-publication" (head))

;; `mevedel-session-transfer'
(declare-function mevedel-session-transfer-release-fence
                  "mevedel-session-transfer" (directory generation))

;; `mevedel-structs'
(declare-function mevedel-session-authority-mode-for-session
                  "mevedel-structs" (session))
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-lease "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-lease-renewal-timer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-control-transfer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-active-p
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-queue
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-uncommitted-batches
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-transport'
(declare-function mevedel-transport-busy-p
                  "mevedel-transport" (&optional path))


;; `mevedel-workspace'
(declare-function mevedel-workspace-state-dir "mevedel-workspace" (workspace))

;;
;;; Customization

(defcustom mevedel-session-lease-seconds 90
  "Seconds for which one portable project lease grants mutation authority.

Renewal runs from a timer, which cannot fire while a durable action is inside
blocking target I/O, so a slow remote action can outlive the lease.  The
owning client then reclaims its own expired lease without a prompt; raise this
only to widen the window in which another client may not take over.  Expiry is
measured with the target clock at whole-second resolution."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-session-lease-renewal-seconds 30
  "Seconds between portable project lease renewal attempts."
  :type 'integer
  :group 'mevedel)

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
      (truncate-string-to-width (string-trim name) 64))))

(defvar mevedel-session-durability--disclosed-targets
  (make-hash-table :test #'equal)
  "Execution targets disclosed to the user in this Emacs process.")


(defun mevedel-session-durability-disclose (session)
  "Confirm SESSION's target-side durable storage before its first write.

The acknowledgement is once per target for this Emacs process.  Local
project sessions need no disclosure."
  (require 'mevedel-workspace)
  (let ((target (mevedel-session-execution-target session)))
    (when (and target (mevedel-execution-target-remote-p target))
      (let ((key (mevedel-execution-target-identity target)))
        (unless (gethash key mevedel-session-durability--disclosed-targets)
          (unless
              (yes-or-no-p
               (format
                (concat
                 "Store this project's mevedel state on the target at %s? "
                 "This includes conversations, permissions, agent state, "
                 "checkpoints, snapshots, plans, durable tool results, and "
                 "required logs. The data is not encrypted by mevedel. ")
                (mevedel-workspace-state-dir
                 (mevedel-session-workspace session))))
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
  "Cons cell caching one transaction's target clock reading, or nil.

Its car is (TARGET-SECONDS . LOCAL-FLOAT-TIME).  A caller that spans one
durable transaction binds this to a fresh `(list nil)'; outside such a binding
every reading reaches the target.")

(defun mevedel-session-durability--note-target-time (seconds)
  "Record SECONDS as this transaction's target clock reading."
  (when (and mevedel-session-durability--transaction-clock (numberp seconds))
    (setcar mevedel-session-durability--transaction-clock
            (cons seconds (float-time))))
  seconds)

(defun mevedel-session-durability--target-time (directory)
  "Return the target filesystem's current time for control DIRECTORY.

The modification timestamp of a freshly created marker is supplied by the
target filesystem, including through TRAMP.  It is therefore shared by
clients whose local wall clocks disagree.  Failure to obtain that timestamp
fails closed instead of falling back to a client clock."
  (or mevedel-session-durability--observed-time
      (let ((cached (car mevedel-session-durability--transaction-clock)))
        (and cached
             (< (- (float-time) (cdr cached))
                mevedel-session-durability--clock-reuse-seconds)
             (car cached)))
      (mevedel-session-durability--note-target-time
       (mevedel-session-control-fs-target-time directory))))

(defvar mevedel-session-durability--asserted-directories nil
  "Cons cell listing directories already proved free of a PID lock, or nil.

A portable session cannot grow a `.lock' while this client holds its lease,
and one transaction reaches several entry points that each prove its absence.
A caller that spans one transaction binds this to a fresh `(list nil)';
outside such a binding every call proves it against the target.")

;; `mevedel-session-recovery' declares this cache; the transaction macro
;; below binds it together with this file's own transaction scopes.
(defvar mevedel-session-recovery--mutation-cache)

(defmacro mevedel-session-durability-with-transaction (&rest body)
  "Run BODY as one durable transaction sharing clock, probe, and recovery reads.

The target clock reading, the pid-lock assertions, and the recovery
mutation lookups repeat across the entry points one user action reaches;
inside this scope each is paid once.  The bindings nest: an inner
transaction joins the outer one instead of starting fresh, and the reads
are tolerant so the scope works before the lazily loaded modules that own
the variables are in."
  (declare (indent 0) (debug t))
  `(let ((mevedel-session-durability--transaction-clock
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
  "Signal when portable SESSION-DIR also contains the obsolete PID lock."
  (setq session-dir (mevedel-session-control-fs-physical-path
                     session-dir))
  (unless (member session-dir
                  (car mevedel-session-durability--asserted-directories))
    (when (mevedel-session-control-fs-path-exists-p
           (file-name-concat session-dir ".lock"))
      (error "Portable session has a PID lock: %s" session-dir))
    (when mevedel-session-durability--asserted-directories
      (push session-dir
            (car mevedel-session-durability--asserted-directories)))))

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
  (with-temp-buffer
    (let ((print-length nil)
          (print-level nil))
      (prin1 plist (current-buffer))
      (mevedel-session-control-fs-write-file
       (mevedel-session-control-fs-physical-path path)
       (buffer-string)))))

(defun mevedel-session-durability--create-plist (path plist)
  "Atomically create PATH with PLIST, returning non-nil on success."
  (with-temp-buffer
    (let ((print-length nil)
          (print-level nil))
      (prin1 plist (current-buffer))
      (mevedel-session-control-fs-create-file
       (mevedel-session-control-fs-physical-path path)
       (buffer-string)))))

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
  "Return non-nil when SESSION uses portable project durability."
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
        :renewed-at now
        :expires-at (+ now mevedel-session-lease-seconds)
        :buffer buffer-name))

(defun mevedel-session-durability-lease-holder (session-dir)
  "Return a display name for SESSION-DIR's current lease holder, or nil.

Records written before hosts were recorded carry no name, which reads as
unknown rather than as an error."
  (let ((lease (mevedel-session-durability--lease-head
                (mevedel-session-durability--lease-path session-dir))))
    (when (and lease (mevedel-session-durability--valid-lease-p lease))
      (plist-get lease :host))))

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

(defun mevedel-session-durability--read-records (paths)
  "Read every lease record among PATHS in one target program.

Return parsed records or nil in PATHS order.  An absent or unreadable
record is nil rather than the program's end, so a vanished newest
record can never hide an older live one."
  (when paths
    (mapcar
     (lambda (result)
       (and (eq 'ok (plist-get result :status))
            (condition-case nil
                (car (read-from-string (plist-get result :value)))
              (error nil))))
     (mevedel-session-control-fs-run-program
      (mapcar (lambda (path)
                (list :op 'read :path path :optional t))
              paths)))))

(defun mevedel-session-durability--head-of-records (records)
  "Return the first non-aborted record among RECORDS, newest first."
  (catch 'head
    (dolist (record records)
      (when (and record (not (eq 'aborted (plist-get record :status))))
        (throw 'head record)))))

(defun mevedel-session-durability--lease-head (directory &optional names)
  "Return DIRECTORY's latest non-aborted lease record.

NAMES, when non-nil, is an already-observed listing that replaces the
list round trip; all records are then read in one program.  A
generation whose record cannot be read is skipped rather than reported
as \"no lease\": a vanished or unreadable newest record must never hide
an older live one, because the generation compare-and-set is what keeps
two writers apart."
  (mevedel-session-durability--head-of-records
   (mevedel-session-durability--read-records
    (mevedel-session-durability--generation-paths directory names))))

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

(defun mevedel-session-durability--create-generation (directory record)
  "Exclusively create RECORD below lease DIRECTORY."
  (mevedel-session-durability--create-plist
   (mevedel-session-durability--generation-path
    directory (plist-get record :generation))
   record))

(defun mevedel-session-durability--write-generation (directory record)
  "Atomically replace RECORD's own generation below DIRECTORY."
  (mevedel-session-durability--write-plist
   (mevedel-session-durability--generation-path
    directory (plist-get record :generation))
   record))

(defun mevedel-session-durability--record-bytes (record)
  "Return the exact content RECORD occupies as a lease generation file."
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

(defun mevedel-session-durability--target-time-cached-p ()
  "Return non-nil when a fresh target clock reading can answer without I/O."
  (or (numberp mevedel-session-durability--observed-time)
      (let ((cached (car mevedel-session-durability--transaction-clock)))
        (and cached
             (< (- (float-time) (cdr cached))
                mevedel-session-durability--clock-reuse-seconds)))))

(defun mevedel-session-durability--strip-assumption (lease)
  "Return LEASE without the remembered on-target bytes."
  (let ((copy (copy-sequence lease)))
    (plist-put copy :bytes nil)))

(defun mevedel-session-durability--observe-lease (directory generation)
  "Observe DIRECTORY's clock, records, and GENERATION's own record at once.

Return a plist with `:now', `:names', `:bytes', and `:record'.  The three
observations are one target process rather than four, and the record read is
last so an absent record still leaves the clock and the listing answered."
  (let* ((operations
          (append
           (list (list :op 'target-time :path directory)
                 (list :op 'list-directory :path directory))
           (when (natnump generation)
             (list (list :op 'read
                         :path (mevedel-session-durability--generation-path
                                directory generation))))))
         (results (mevedel-session-control-fs-run-program operations))
         (clock (nth 0 results))
         (listing (nth 1 results))
         (record (nth 2 results))
         (bytes (and record
                     (eq 'ok (plist-get record :status))
                     (plist-get record :value))))
    ;; A clock the target could not supply is left unanswered rather than
    ;; raised: the seam falls back, and a caller with no usable clock fails
    ;; closed there.
    (list :now (and (eq 'ok (plist-get clock :status))
                    (mevedel-session-durability--note-target-time
                     (plist-get clock :value)))
          :listed (eq 'ok (plist-get listing :status))
          :names (and (eq 'ok (plist-get listing :status))
                      (plist-get listing :value))
          :bytes bytes
          :record
          (and bytes
               (condition-case nil
                   (car (read-from-string bytes))
                 (error nil))))))

(defun mevedel-session-durability--commit-lease
    (directory generation expected record)
  "Replace GENERATION's record below DIRECTORY only while it still is EXPECTED.

EXPECTED is the content observed on the target; RECORD is the replacement.
The proof and the write share one target process, which narrows the window
between them but does not remove it: another client can exclusively create
the next generation in between, so `verify' is a precondition and not an
election, and the caller decides who won from what it observes afterwards.
Return the record names observed after the write, or nil when the proof
failed."
  (let* ((path (mevedel-session-durability--generation-path
                directory generation))
         ;; `verify' stays first: the takeover race tests key on the commit
         ;; program by its opening operation.  The clock rides last so a
         ;; successful renewal refreshes the transaction clock's reuse
         ;; window and the next renewal can assume instead of observing.
         (results
          (mevedel-session-control-fs-run-program
           (list (list :op 'verify :path path :content expected)
                 (list :op 'write :path path
                       :content (mevedel-session-durability--record-bytes
                                 record))
                 (list :op 'list-directory :path directory)
                 (list :op 'target-time :path directory :optional t))))
         (proof (nth 0 results))
         (write (nth 1 results))
         (listing (nth 2 results))
         (clock (nth 3 results)))
    (when (eq 'ok (plist-get clock :status))
      (mevedel-session-durability--note-target-time
       (plist-get clock :value)))
    (cond
     ((memq (plist-get proof :status) '(mismatch absent)) nil)
     ((not (eq 'ok (plist-get proof :status)))
      (mevedel-session-control-fs-program-value proof))
     ((not (eq 'ok (plist-get write :status)))
      (mevedel-session-control-fs-program-value write))
     (t (or (plist-get listing :value) t)))))

(defun mevedel-session-durability--same-generation-p (left right)
  "Return non-nil when lease records LEFT and RIGHT name one generation."
  (and left right
       (equal (plist-get left :generation) (plist-get right :generation))
       (equal (plist-get left :client-id) (plist-get right :client-id))))

(defun mevedel-session-durability--cancel-renewal (session)
  "Cancel SESSION's lease-renewal timer, if any."
  (when-let* ((timer (mevedel-session-lease-renewal-timer session))
              ((timerp timer)))
    (cancel-timer timer))
  (setf (mevedel-session-lease-renewal-timer session) nil))

(defun mevedel-session-durability--bind-lease (session lease state &optional bytes)
  "Record LEASE and STATE on SESSION and maintain its renewal timer.

BYTES is the exact content LEASE occupies on the target when that is known.
A later renewal states it as the precondition of its own write, which saves
re-reading a record this client just wrote.

An `owned' state runs the renewal heartbeat and a `lost' one stops it.
A `contested' state keeps it: another client has a generation outstanding
but has not activated it, and that claim aborts if this client's write
landed first, so stopping the heartbeat here would let a lease this client
still holds expire for real."
  (when session
    (let ((live (and lease (copy-sequence lease)))
          (release-pending
           (plist-get (mevedel-session-lease session) :release-pending)))
      (when live
        (setq live (plist-put live :state state))
        (setq live (plist-put live :bytes bytes)))
      (when release-pending
        (setq live (plist-put live :release-pending t)))
      (setf (mevedel-session-lease session) live)
      (cond
       ((eq state 'owned)
        (unless (timerp (mevedel-session-lease-renewal-timer session))
          (setf (mevedel-session-lease-renewal-timer session)
                (run-at-time
                 mevedel-session-lease-renewal-seconds
                 mevedel-session-lease-renewal-seconds
                 #'mevedel-session-durability-lease-renew session))))
       ;; Contested keeps the timer: see this function's docstring.
       ((eq state 'contested))
       (t
        (mevedel-session-durability--cancel-renewal session))))))

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
  (require 'mevedel-session-publication)
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

(defun mevedel-session-durability-publication-head (session-dir)
  "Return SESSION-DIR's validated relative publication head, or nil.

This read-only operation bypasses remote file caches and does not acquire or
otherwise mutate the session lease."
  (let* ((remote-file-name-inhibit-cache t)
         (_ (mevedel-session-durability--assert-no-pid-lock session-dir))
         (directory (mevedel-session-durability--lease-path session-dir))
         (lease (mevedel-session-durability--lease-head directory)))
    (cond
     ((null lease) nil)
     ((mevedel-session-durability--valid-lease-p lease)
      (plist-get lease :publication-head))
     (t
      (error "Invalid portable session lease: %s" directory)))))

(defun mevedel-session-durability--claim-next
    (directory expected buffer-name
               &optional status unsettled-mutation-p unsettled-mutation
               transfer-generation)
  "Claim DIRECTORY's next generation after EXPECTED for BUFFER-NAME.

The candidate fences older writers as soon as its exclusive generation file
appears.  It becomes active only if EXPECTED remained byte-for-byte unchanged;
otherwise it is marked aborted and removed best-effort.  STATUS defaults to
`active'.  When UNSETTLED-MUTATION-P is non-nil, the successor records
UNSETTLED-MUTATION instead of preserving EXPECTED's value.
TRANSFER-GENERATION explicitly opens that request round; otherwise a
same-owner successor preserves EXPECTED's round."
  (let* ((status (or status 'active))
         ;; One observation answers the clock and the known generations.
         ;; Any record created after it is a foreign claim at the same
         ;; next generation, which collides with the exclusive create
         ;; below and fails it -- no generation older than the candidate
         ;; can appear in between, so the observed name set is the
         ;; complete predecessor universe.
         (observed (mevedel-session-durability--observe-lease directory nil))
         ;; The observed reading flows through the clock seam, so a test
         ;; that stubs the seam still governs every deadline.
         (now (let ((mevedel-session-durability--observed-time
                     (plist-get observed :now)))
                (mevedel-session-durability--target-time directory)))
         (names (mevedel-session-durability--observed-names observed))
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
         ;; The fencing create and the predecessor reads are one program:
         ;; the create is first, so a lost race skips the reads.
         (results
          (mevedel-session-control-fs-run-program
           (cons (list :op 'create :path candidate-path
                       :content (mevedel-session-durability--record-bytes
                                 candidate))
                 (mapcar (lambda (path)
                           (list :op 'read :path path :optional t))
                         older))))
         (created (car results)))
    (cond
     ((eq 'conflict (plist-get created :status)) nil)
     ((not (eq 'ok (plist-get created :status)))
      (mevedel-session-control-fs-program-value created))
     (t
      (let ((predecessor
             (mevedel-session-durability--head-of-records
              (mapcar
               (lambda (result)
                 (and (eq 'ok (plist-get result :status))
                      (condition-case nil
                          (car (read-from-string
                                (plist-get result :value)))
                        (error nil))))
               (cdr results)))))
        (if (equal expected predecessor)
            (progn
              (setq candidate (plist-put candidate :status status))
              ;; Settling the claim and pruning superseded generations
              ;; share one program; a prune that fails is best-effort,
              ;; but a failed settle write must still raise.
              (mevedel-session-control-fs-program-value
               (car (mevedel-session-control-fs-run-program
                     (cons (list :op 'write :path candidate-path
                                 :content
                                 (mevedel-session-durability--record-bytes
                                  candidate))
                           (mapcar (lambda (path)
                                     (list :op 'delete-file :path path
                                           :optional t))
                                   older)))))
              candidate)
          (setq candidate (plist-put candidate :status 'aborted))
          (mevedel-session-control-fs-program-value
           (car (mevedel-session-control-fs-run-program
                 (list (list :op 'write :path candidate-path
                             :content
                             (mevedel-session-durability--record-bytes
                              candidate))
                       (list :op 'delete-file :path candidate-path
                             :optional t)))))
          nil))))))

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
    (session-dir buffer-name &optional session)
  "Acquire SESSION-DIR's portable lease for BUFFER-NAME.

Return non-nil when this client owns mutation authority and nil when another
unexpired client owns it.  Expired takeover requires explicit confirmation.
When SESSION is non-nil, record the resulting lease state on it."
  (when (and session
             (not (mevedel-session-durability--portable-session-p session)))
    (error "Portable lease requires a portable project session"))
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
                 (> (plist-get existing :expires-at) now))
            nil)
           ((eq 'released (plist-get existing :status))
            (require 'mevedel-session-transfer)
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
           ((not
             (let ((expired-at
                    (format-time-string
                     "%FT%T%z"
                     (seconds-to-time
                      (plist-get existing :expires-at)))))
               (y-or-n-p
                (if (eq 'publishing (plist-get existing :status))
                    (format
                     (concat
                      "Portable session publishing lease expired at %s. "
                      "A critical write may still be in flight; confirm that "
                      "the prior client is stopped before takeover? ")
                     expired-at)
                  (format
                   "Portable session lease expired at %s; take it over? "
                   expired-at)))))
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
                  (require 'mevedel-session-transfer)
                  (mevedel-session-transfer-release-fence
                   directory (plist-get current :generation))))
           'foreign)
          (t 'expired))))
      (and owned t))))

(defun mevedel-session-durability--update-owned-lease
    (session accepted-status status seconds
             &optional publication-head expected-publication-head-p
             expected-publication-head unsettled-mutation-p
             unsettled-mutation)
  "Update SESSION's owned lease from ACCEPTED-STATUS to STATUS for SECONDS.
Replace its preserved head with non-nil PUBLICATION-HEAD.  When
EXPECTED-PUBLICATION-HEAD-P is non-nil, require the current head to equal
EXPECTED-PUBLICATION-HEAD.  When UNSETTLED-MUTATION-P is non-nil, replace the
preserved unsettled-mutation flag with UNSETTLED-MUTATION."
  (when-let ((session-dir (mevedel-session-save-path session)))
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
                 (mevedel-session-durability--target-time-cached-p)
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
           (names (plist-get observed :names))
           ;; The bound generation is the head whenever no newer record was
           ;; allocated, which is the steady state; only a foreign claim costs
           ;; the extra reads to derive the head.
           (head
            (if (or assumed
                    (and existing
                         (natnump generation)
                         (= generation
                            (mevedel-session-durability--newest-generation
                             names))))
                existing
              (condition-case nil
                  (mevedel-session-durability--lease-head directory)
                (error nil)))))
      (if (and (mevedel-session-durability--valid-lease-p existing)
               (memq (plist-get existing :status) accepted-status)
               (equal mevedel-session-durability--client-id
                      (plist-get existing :client-id))
               (equal existing head)
               (or (not expected-publication-head-p)
                   (equal expected-publication-head
                          (plist-get existing :publication-head)))
               (> (plist-get existing :expires-at) now))
          (let* ((record
                  (let ((mevedel-session-lease-seconds seconds))
                    (mevedel-session-durability--lease-record
                     (plist-get existing :buffer)
                     (plist-get existing :generation)
                     status
                     (or publication-head
                         (plist-get existing :publication-head))
                     (if unsettled-mutation-p
                         unsettled-mutation
                       (plist-get existing :unsettled-mutation))
                     now
                     (plist-get existing :transfer-generation))))
                 (committed
                  (mevedel-session-durability--commit-lease
                   directory (plist-get existing :generation)
                   existing-bytes record)))
            (cond
             ((and committed
                   ;; A newer generation appearing means another client won,
                   ;; unless that record is itself aborted.
                   (or (not (listp committed))
                       (= (plist-get existing :generation)
                          (mevedel-session-durability--newest-generation
                           committed))
                       (equal record
                              (mevedel-session-durability--lease-head
                               directory))))
              (mevedel-session-durability--bind-lease
               session record 'owned
               (mevedel-session-durability--record-bytes record))
              t)
             ;; An assumed precondition that no longer holds is not a lost
             ;; lease: drop the assumption and decide from the target.
             ((and (null committed) assumed)
              (mevedel-session-durability--bind-lease
               session (mevedel-session-durability--strip-assumption bound)
               (plist-get bound :state))
              (mevedel-session-durability--update-owned-lease
               session accepted-status status seconds
               publication-head expected-publication-head-p
               expected-publication-head unsettled-mutation-p
               unsettled-mutation))
             (t
              (let ((latest
                     (mevedel-session-durability--lease-head directory)))
                (mevedel-session-durability--bind-lease
                 session latest
                 (cond
                  ((mevedel-session-durability--owned-lease-record-p
                    latest directory now)
                   'owned)
                  ;; The write landed; a generation appearing beside it that
                  ;; has not activated has not won, and it aborts once its
                  ;; own precondition check reads this record.  The next
                  ;; heartbeat reconverges, so keep it running.
                  ((eq 'claiming (plist-get latest :status))
                   'contested)
                  (t 'lost))))
              nil)))
        (if assumed
            ;; The assumption was the only reason this looked unusable.
            (progn
              (mevedel-session-durability--bind-lease
               session (mevedel-session-durability--strip-assumption bound)
               (plist-get bound :state))
              (mevedel-session-durability--update-owned-lease
               session accepted-status status seconds
               publication-head expected-publication-head-p
               expected-publication-head unsettled-mutation-p
               unsettled-mutation))
          (let ((latest (or head existing)))
            (mevedel-session-durability--bind-lease
             session latest
             (if (mevedel-session-durability--owned-lease-record-p
                  latest directory now)
                 'owned
               'lost)))
          nil)))))

(defun mevedel-session-durability-set-unsettled-mutation (session value)
  "Set SESSION's durable unsettled-mutation latch to boolean VALUE.
Return non-nil only when the current owned lease generation commits VALUE."
  (unless (booleanp value)
    (error "Unsettled mutation latch must be boolean"))
  (when-let ((session-dir (mevedel-session-save-path session)))
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
            (if (and existing
                     (not (eq 'aborted (plist-get existing :status)))
                     (equal (plist-get existing :generation)
                            (mevedel-session-durability--newest-generation
                             (plist-get observed :names))))
                existing
              (mevedel-session-durability--lease-head
               directory
               (mevedel-session-durability--observed-names observed))))
           (status (plist-get existing :status)))
      (if (and (equal existing head)
               (mevedel-session-durability--owned-lease-record-p
                existing directory now))
          (if-let ((successor
                    (mevedel-session-durability--claim-next
                     directory existing (plist-get existing :buffer)
                     status t value)))
              (progn
                (mevedel-session-durability--bind-lease
                 session successor 'owned)
                t)
            (let ((latest (mevedel-session-durability--lease-head directory)))
              (mevedel-session-durability--bind-lease
               session latest
               (if (mevedel-session-durability--owned-lease-record-p
                    latest directory now)
                   'owned
                 'lost))
              nil))
        (mevedel-session-durability--bind-lease
         session (or head existing) 'lost)
        nil))))

(defun mevedel-session-durability-unsettled-mutation-p (session)
  "Return non-nil when SESSION's bound lease records unsettled mutation."
  (and (plist-get (mevedel-session-lease session) :unsettled-mutation) t))

(defun mevedel-session-durability-commit-publication-head (session head)
  "Commit immutable session-relative HEAD while SESSION is the current owner.

Return non-nil only when SESSION's publishing generation remains the exact
lease head through the commit."
  (require 'mevedel-session-publication)
  (unless (mevedel-session-publication-valid-head-p head)
    (error "Publication head must name an immutable manifest"))
  (let ((expected
         (plist-get (mevedel-session-lease session) :publication-head)))
    (mevedel-session-durability--update-owned-lease
     session '(publishing) 'publishing
     mevedel-session-publication-lease-seconds head t expected)))

(defun mevedel-session-durability-lease-renew (session)
  "Renew SESSION's unexpired owned lease, returning non-nil on success.

Renewal runs from a timer, and a timer fires wherever the main loop happens to
be waiting, including inside a TRAMP operation.  Target I/O from there is a
reentrant TRAMP call: TRAMP refuses it and the connection can be left wedged
for the operation that was already running.  Renewal therefore performs no
target I/O while another TRAMP operation is in progress, or while a
publication owns the bounded window, and reports the state it already knows.
The next tick renews once the transport is free.  After serialization exits,
renewal also normalizes this client's live `publishing\=' generation back to
`active\='."
  (if (or (mevedel-session-publication-active-p session)
          (mevedel-transport-busy-p (mevedel-session-save-path session)))
      (eq 'owned (plist-get (mevedel-session-lease session) :state))
    (condition-case err
        (mevedel-session-durability--update-owned-lease
         session '(active publishing) 'active mevedel-session-lease-seconds)
      (error
       (mevedel-session-durability--bind-lease
        session (mevedel-session-lease session) 'lost)
       (display-warning
        'mevedel
        (format "Portable session lease renewal failed: %s" err)
        :warning)
       nil))))

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
    (require 'mevedel-session-transfer)
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

(defun mevedel-session-durability-lease-release (session-dir &optional session)
  "Release this client's portable lease for SESSION-DIR.
An inactive owned `publishing' generation is releasable, but a live publisher
is never interrupted.  When SESSION is non-nil, cancel its renewal and clear
its lease state."
  (when (and session
             (not (mevedel-session-durability--portable-session-p session)))
    (error "Portable lease requires a portable project session"))
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
        (when-let ((queued
                    (and (not (mevedel-session-pending-publication session))
                         (mevedel-session-publication-queue session))))
          (let ((batches
                 (append
                  (mevedel-session-publication-uncommitted-batches session)
                  queued)))
            (setf (mevedel-session-publication-uncommitted-batches session) nil
                  (mevedel-session-publication-queue session) nil)
            (require 'mevedel-session-publication)
            (mevedel-session-publication--record-pending
             session batches
             '(user-error
               "Session lease released before queued publication completed"))))
        (unless (mevedel-session-pending-publication session)
          (require 'mevedel-session-publication)
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
  "Return SESSION's publishing lease to its ordinary renewable state."
  (condition-case nil
      (mevedel-session-durability--update-owned-lease
       session '(publishing) 'active mevedel-session-lease-seconds)
    (error nil)))

(defun mevedel-session-durability-call-with-reserved-lease
    (session function)
  "Call FUNCTION while SESSION owns a bounded synchronous mutation lease.

Timer renewal performs no target I/O while FUNCTION runs.  The final ownership
check fails closed.  This wrapper does not publish artifacts, commit a
manifest, or drain SESSION's publication queue."
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

(defun mevedel-session-durability-adopt-owned-lease (session source)
  "Move SOURCE's verified owned lease and path into SESSION.

Verify SOURCE against its target before changing either object.  Renewal timer
creation also precedes mutation, so failure leaves SESSION and SOURCE intact."
  (mevedel-session-durability-call-with-reserved-lease source #'ignore)
  (unless (mevedel-session-durability-lease-owned-p source)
    (error "Committed child lease is not owned"))
  (let ((lease (copy-sequence (mevedel-session-lease source)))
        (save-path (mevedel-session-save-path source))
        (timer
         (run-at-time
          mevedel-session-lease-renewal-seconds
          mevedel-session-lease-renewal-seconds
          #'mevedel-session-durability-lease-renew session)))
    (mevedel-session-durability--cancel-renewal session)
    (mevedel-session-durability--cancel-renewal source)
    (setf (mevedel-session-save-path session) save-path
          (mevedel-session-lease session) lease
          (mevedel-session-lease-renewal-timer session) timer
          (mevedel-session-lease source) nil
          (mevedel-session-lease-renewal-timer source) nil)
    t))


(defun mevedel-session-durability-forget-removed-session (session)
  "Clear local durability state after SESSION's owned directory was removed.

The caller removes the target directory while its lease is still held.  This
function performs no target I/O and must run only after that removal."
  (mevedel-session-durability--cancel-renewal session)
  (setf (mevedel-session-lease session) nil
        (mevedel-session-publication session) nil
        (mevedel-session-publication-active-p session) nil)
  (require 'mevedel-session-publication)
  (mevedel-session-publication-clear-transient session))


(provide 'mevedel-session-durability)

;;; mevedel-session-durability.el ends here
