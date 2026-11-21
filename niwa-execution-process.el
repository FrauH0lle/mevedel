;;; niwa-execution-process.el -- Opaque child process lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Owns one operating-system child, its process group, timeout/termination
;; timers, and bounded raw output spool.  Callers receive output chunks and one
;; immutable terminal result; managed session state never crosses this module.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `niwa-execution-target'
(declare-function niwa-execution-target-create
                  "niwa-execution-target" (workspace-root))
(declare-function niwa-execution-target-direct-async-capable-p
                  "niwa-execution-target" (target))
(declare-function niwa-execution-target-native-path
                  "niwa-execution-target" (target path))
(autoload 'niwa-execution-target-create "niwa-execution-target")
(autoload 'niwa-execution-target-direct-async-capable-p
  "niwa-execution-target")
(autoload 'niwa-execution-target-native-path "niwa-execution-target")

;; `niwa-transport'
(declare-function niwa-transport-busy-p "niwa-transport" (&optional path))
(declare-function niwa-transport-call-with-spawn-channel
                  "niwa-transport" (remote direct-async thunk))
;; Settlement timers are armed from sentinels and filters, which can run
;; inside a remote command; there a plain `run-at-time' timer fires nested in
;; that command, where the settlement's own target operations are refused.
(declare-function niwa-transport-run-at-time
                  "niwa-transport" (seconds function &rest args))
(declare-function niwa-transport-run-when-idle
                  "niwa-transport" (key path thunk &optional on-cancel))
(defvar niwa-transport-remote-direct-async)
(autoload 'niwa-transport-busy-p "niwa-transport")
(autoload 'niwa-transport-call-with-spawn-channel "niwa-transport")
(autoload 'niwa-transport-run-at-time "niwa-transport")
(autoload 'niwa-transport-run-when-idle "niwa-transport")

;; `niwa-utilities'
(declare-function niwa--executable-find
                  "niwa-utilities" (name &optional remote))
(declare-function niwa--process-identity "niwa-utilities" (pid))
(declare-function niwa--run-periodic-timer
                  "niwa-utilities" (scope period function &rest arguments))
(declare-function niwa--timer-pending-p "niwa-utilities" (timer))
(declare-function niwa--ui-timer-cancel "niwa-utilities" (timer))
(autoload 'niwa--executable-find "niwa-utilities")
(autoload 'niwa--process-identity "niwa-utilities")
(autoload 'niwa--run-periodic-timer "niwa-utilities")
(autoload 'niwa--timer-pending-p "niwa-utilities")
(autoload 'niwa--ui-timer-cancel "niwa-utilities")

(defcustom niwa-execution-process-output-limit (* 64 1024 1024)
  "Maximum bytes retained from one child process."
  :type 'integer
  :group 'niwa)

(defconst niwa-execution-process--direct-async-command-limit 3584
  "Largest quoted remote command, in bytes, spawned direct-async.")

(defconst niwa-execution-process--child-kill-delay 2
  "Seconds to wait before force-killing a stopped child process group.")

(defconst niwa-execution-process--remote-control-timeout 5
  "Seconds allowed for one target process-control operation.")

(defconst niwa-execution-process--stderr-drain-timeout 0.1
  "Seconds allowed to drain stderr after the main child settles.")

(defconst niwa-execution-process--environment
  '(("NO_COLOR" . "1")
    ("TERM" . "dumb")
    ("LC_ALL" . "C.UTF-8")
    ("LANG" . "C.UTF-8")
    ("COLORTERM" . "")
    ("PAGER" . "cat")
    ("GIT_PAGER" . "cat")
    ("GH_PAGER" . "cat")
    ("NIWA_EXECUTION" . "1"))
  "Stable defaults for model-triggered child processes.")

(defconst niwa-execution-process--remote-group-script
  (string-join
   '("command=(\"$@\")"
     "read -r stat < /proc/$$/stat || exit 125"
     "rest=${stat##*) }"
     "set -- $rest"
     "printf '%s%s:%s\\n' \"$0\" \"$$\" \"${20}\""
     "trap ':' HUP INT TERM"
     "\"${command[@]}\""
     "status=$?"
     "group_has_members() { for path in /proc/[0-9]*/stat"
     "do read -r member 2>/dev/null < \"$path\" || continue"
     "pid=${member%% *}"
     "tail=${member##*) }"
     "set -- $tail"
     "[ \"$pid\" != \"$$\" ] && [ \"$3\" = \"$$\" ] && [ \"$1\" != \"Z\" ] && return 0"
     "done"
     "return 1"
     "}"
     "while group_has_members; do sleep 0.25; done"
     "exit \"$status\"")
   "; ")
  "Target-side launcher that reports a private process-group identity.")

(defconst niwa-execution-process--remote-group-status-script
  (string-join
   '("group=$1"
     "expected=$2"
     "live=0"
     "for path in /proc/[0-9]*/stat"
     "do read -r member 2>/dev/null < \"$path\" || continue"
     "rest=${member##*) }"
     "set -- $rest"
     "[ \"$3\" = \"$group\" ] || continue"
     "[ \"$1\" = \"Z\" ] && continue"
     "live=1"
     "break"
     "done"
     "[ \"$live\" = 1 ] || exit 1"
     "[ -r \"/proc/$group/stat\" ] || exit 2"
     "read -r stat < \"/proc/$group/stat\" || exit 2"
     "rest=${stat##*) }"
     "set -- $rest"
     "[ \"${20}\" = \"$expected\" ] || exit 2")
   "; ")
  "Target-side zombie-aware process-group identity probe.")

(cl-defstruct (niwa-execution-process--child
               (:constructor niwa-execution-process--child-create))
  "Private state for one operating-system child."
  coding
  confined-p
  direct-async-p
  diagnostic-scope
  error
  escaped
  exit-code
  finished-p
  force-timer
  filter-function
  group-id
  group-marker
  group-marker-buffer
  group-start-time
  interrupt-requested-p
  launch-attempted-p
  output-function
  output-limit-p
  pipeline-buffer
  pipeline-exit-codes
  pipeline-marker
  process
  service-p
  settle-timer
  spool-path
  started-at
  stderr-buffer
  stderr-function
  stderr-process
  stderr-remote-path
  stop-p
  terminal-function
  termination
  timed-out-p
  timeout-settle-timer
  timeout-timer
  tty-p
  watch-timer
  workdir)

(cl-defun niwa-execution-process-create
    (&key workdir tty spool-path filter-function output-function pipeline-marker
          stderr-function terminal-function diagnostic-scope service)
  "Create an opaque child for WORKDIR without launching it.
DIAGNOSTIC-SCOPE is its captured workspace scope for deferred reports.
OUTPUT-FUNCTION and STDERR-FUNCTION receive (CHILD RETAINED-CHUNK) after
the shared spool cap.  Separated stderr bypasses FILTER-FUNCTION and
OUTPUT-FUNCTION; otherwise both streams use the ordinary output path.
SERVICE keeps pipe stdin open, omits the spool and observes exits by sentinel."
  (niwa-execution-process--child-create
   :diagnostic-scope (or diagnostic-scope 'unavailable)
   :filter-function filter-function
   :output-function output-function
   :pipeline-marker pipeline-marker
   :service-p service
   :spool-path (unless service
                 (or spool-path (make-temp-file "niwa-execution-output-")))
   :started-at (float-time)
   :stderr-function stderr-function
   :terminal-function terminal-function
   :tty-p (and tty t)
   :workdir workdir))

(defun niwa-execution-process-spool-path (child)
  "Return CHILD's private raw output spool path."
  (niwa-execution-process--child-spool-path child))

(defun niwa-execution-process-launch-attempted-p (child)
  "Return non-nil when CHILD reached the operating-system spawn call."
  (niwa-execution-process--child-launch-attempted-p child))

(defun niwa-execution-process--refresh-terminal (child)
  "Reconcile CHILD when its exact process is already terminal."
  (when-let* (((not (niwa-execution-process--child-finished-p child)))
              (process (niwa-execution-process--child-process child))
              ((memq (process-status process) '(exit signal))))
    (niwa-execution-process--ended child process)))

(defun niwa-execution-process-live-p (child)
  "Return non-nil when CHILD still owns a live process."
  (when child (niwa-execution-process--refresh-terminal child))
  (and child
       (not (niwa-execution-process--child-finished-p child))
       (process-live-p (niwa-execution-process--child-process child))))

(defun niwa-execution-process-terminal-p (child)
  "Return non-nil when CHILD has emitted its terminal result."
  (when child (niwa-execution-process--refresh-terminal child))
  (and child (niwa-execution-process--child-finished-p child)))

(defun niwa-execution-process-status (child)
  "Return immutable current process facts for CHILD."
  (list :error (niwa-execution-process--child-error child)
        :direct-async-p
        (niwa-execution-process--child-direct-async-p child)
        :exit-code (niwa-execution-process--child-exit-code child)
        :pipeline-exit-codes
        (copy-sequence (niwa-execution-process--child-pipeline-exit-codes child))
        :group-id (niwa-execution-process--child-group-id child)
        :group-start-time
        (niwa-execution-process--child-group-start-time child)
        :launch-attempted-p
        (niwa-execution-process--child-launch-attempted-p child)
        :output-limit-p
        (niwa-execution-process--child-output-limit-p child)
        :stop-p (niwa-execution-process--child-stop-p child)
        :termination (niwa-execution-process--child-termination child)
        :timed-out-p (niwa-execution-process--child-timed-out-p child)
        :tty-p (niwa-execution-process--child-tty-p child)
        :workdir (niwa-execution-process--child-workdir child)))

(defun niwa-execution-process--process-environment (&optional remote)
  "Return stable child environment, preserving the target when REMOTE."
  (let ((process-environment
         (and (not remote) (copy-sequence process-environment))))
    (dolist (entry niwa-execution-process--environment)
      (setenv (car entry) (cdr entry)))
    process-environment))

(defun niwa-execution-process--localize-command (command workdir target)
  "Return COMMAND in TARGET's native path domain for WORKDIR."
  (let ((target (or target (niwa-execution-target-create workdir))))
    (mapcar (lambda (argument)
              (if (stringp argument)
                  (niwa-execution-target-native-path target argument)
                argument))
            command)))

(defun niwa-execution-process--remote-command (child command)
  "Wrap COMMAND with target-side process-group ownership for CHILD."
  (let ((marker (make-temp-name "NIWA_PROCESS_GROUP_")))
    (setf (niwa-execution-process--child-group-marker child) marker
          (niwa-execution-process--child-group-marker-buffer child) "")
    (append
     (when (niwa-execution-process--child-direct-async-p child)
       (cons "env"
             (mapcar (lambda (entry)
                       (format "%s=%s" (car entry) (cdr entry)))
                     niwa-execution-process--environment)))
     (list "setsid" "-f" "-w" "bash" "-c"
           niwa-execution-process--remote-group-script marker)
     command)))

(defun niwa-execution-process--direct-async-p (child command workdir)
  "Return non-nil when CHILD may spawn COMMAND direct-async."
  (and niwa-transport-remote-direct-async
       (not (niwa-execution-process--child-tty-p child))
       (fboundp 'tramp-direct-async-process-p)
       (niwa-execution-target-direct-async-capable-p
        (niwa-execution-target-create workdir))
       (< (+ (string-bytes
              (mapconcat #'shell-quote-argument command " "))
             (string-bytes niwa-execution-process--remote-group-script)
             256)
          niwa-execution-process--direct-async-command-limit)))

(defun niwa-execution-process--filter-group-marker (child chunk)
  "Strip CHILD's remote group marker and consume output CHUNK."
  (let ((marker (niwa-execution-process--child-group-marker child)))
    (if (or (null marker)
            (eq :done
                (niwa-execution-process--child-group-marker-buffer child)))
        (niwa-execution-process--consume-output child chunk)
      (setq chunk
            (concat
             (niwa-execution-process--child-group-marker-buffer child)
             chunk))
      (if-let* ((newline (string-search "\n" chunk)))
          (let ((line (string-trim-right (substring chunk 0 newline) "\r")))
            (setf (niwa-execution-process--child-group-marker-buffer child)
                  :done)
            (if (string-match
                 (concat "\\`" (regexp-quote marker)
                         "\\([0-9]+\\):\\([0-9]+\\)\\'")
                 line)
                (setf (niwa-execution-process--child-group-id child)
                      (string-to-number (match-string 1 line))
                      (niwa-execution-process--child-group-start-time child)
                      (match-string 2 line))
              (niwa-execution-process--consume-output
               child (substring chunk 0 (1+ newline))))
            (let ((rest (substring chunk (1+ newline))))
              (unless (string-empty-p rest)
                (niwa-execution-process--consume-output child rest))))
        (setf (niwa-execution-process--child-group-marker-buffer child)
              chunk)))))

(defun niwa-execution-process-remote-group-status
    (group-id start-time workdir)
  "Return the target status of GROUP-ID started at START-TIME under WORKDIR.

The value is `live', `dead', or `ambiguous'.  START-TIME pins the identity
so a recycled process-group number cannot read as the original group.
Incomplete identity, an unreachable target, or an unexpected exit is
`ambiguous': only an affirmative answer proves the group gone."
  (let ((process-environment nil))
    (if (not (and (integerp group-id) (> group-id 0)
                  (stringp start-time) (stringp workdir)
                  (file-remote-p workdir)))
        'ambiguous
      (niwa-transport-call-with-deadline
       niwa-execution-process--remote-control-timeout
       (lambda ()
          (with-temp-buffer
            (setq default-directory workdir)
            (pcase
                (process-file
                 "bash" nil nil nil "-c"
                 niwa-execution-process--remote-group-status-script
                 "niwa-process-group" (number-to-string group-id)
                 start-time)
              (0 'live)
              (1 'dead)
              (_ 'ambiguous))))
       (lambda () (error "Target process status probe timed out"))))))

(defun niwa-execution-process--remote-group-status (child)
  "Return CHILD's remote group status as `live', `dead', or `ambiguous'."
  (niwa-execution-process-remote-group-status
   (niwa-execution-process--child-group-id child)
   (niwa-execution-process--child-group-start-time child)
   (niwa-execution-process--child-workdir child)))

(defun niwa-execution-process--mark-unknown (child error-data)
  "Mark CHILD's target outcome unprovable with ERROR-DATA."
  (setf (niwa-execution-process--child-termination child) 'unknown
        (niwa-execution-process--child-error child) error-data)
  (unless (niwa--timer-pending-p
           (niwa-execution-process--child-settle-timer child))
    (setf (niwa-execution-process--child-settle-timer child)
          (niwa-transport-run-at-time
           niwa-execution-process--child-kill-delay
           #'niwa-execution-process--settle-after-kill child)))
  (when-let* ((process (niwa-execution-process--child-process child))
              ((process-live-p process)))
    (ignore-errors (delete-process process))))

(defun niwa-execution-process--process-table ()
  "Return a cons of process attributes and parent-to-children hash tables."
  (let ((attributes (make-hash-table :test #'eql))
        (children (make-hash-table :test #'eql)))
    (dolist (pid (list-system-processes))
      (when-let* ((attrs (process-attributes pid))
                  (parent (alist-get 'ppid attrs)))
        (puthash pid attrs attributes)
        (push pid (gethash parent children))))
    (cons attributes children)))

(defun niwa-execution-process--signal-confined-group (child signal)
  "Send SIGNAL to CHILD's foreground process group inside Bubblewrap."
  (pcase-let* ((default-directory "/")
               (outer-group-id (niwa-execution-process--child-group-id child))
               (`(,attributes . ,children)
                (niwa-execution-process--process-table))
               (pending (list outer-group-id))
               (group-id nil))
    (while (and pending (not group-id))
      (let ((pid (pop pending)))
        (dolist (child-pid (gethash pid children))
          (push child-pid pending)
          (let ((attrs (gethash child-pid attributes)))
            (unless (equal "bwrap" (alist-get 'comm attrs))
              (let ((candidate
                     (let ((foreground (alist-get 'tpgid attrs)))
                       (if (and (integerp foreground)
                                (> foreground 0)
                                (not (eql foreground outer-group-id)))
                           foreground
                         (alist-get 'pgrp attrs)))))
                (when (and (integerp candidate) (> candidate 0)
                           (not (eql candidate outer-group-id)))
                  (setq group-id candidate))))))))
    (when group-id
      (condition-case nil
          (progn (signal-process (- group-id) signal) t)
        (error nil)))))

(defun niwa-execution-process--start-identity (pid attributes)
  "Return a stable start identity for local PID with process ATTRIBUTES.
Linux derives the `start' attribute from the current uptime, so it differs
between calls; the start tick in /proc does not."
  (or (niwa--process-identity pid)
      (alist-get 'start attributes)))

(defun niwa-execution-process--refresh-escaped (child)
  "Record and return CHILD's live descendants outside its process group.
A descendant such as GNU `timeout' moves itself into a new process group, so
signalling the group alone never reaches it.  Entries are (PID . START);
START pins identity so a recycled PID is never signalled.  Earlier entries
stay roots because a stopped shell's escaped children are reparented away."
  (pcase-let* ((default-directory "/")
               (group-id (niwa-execution-process--child-group-id child))
               (own-group (alist-get 'pgrp (process-attributes (emacs-pid))))
               (`(,attributes . ,children)
                (niwa-execution-process--process-table))
               (seen (make-hash-table :test #'eql))
               (pending nil)
               (escaped nil))
    (maphash (lambda (pid attrs)
               (when (eql (alist-get 'pgrp attrs) group-id)
                 (push pid pending)))
             attributes)
    (dolist (entry (niwa-execution-process--child-escaped child))
      (when-let* ((attrs (gethash (car entry) attributes))
                  ((equal (cdr entry)
                          (niwa-execution-process--start-identity
                           (car entry) attrs))))
        (push (car entry) pending)))
    (while pending
      (let* ((pid (pop pending))
             (attrs (gethash pid attributes)))
        (unless (gethash pid seen)
          (puthash pid t seen)
          (unless (or (eql (alist-get 'pgrp attrs) group-id)
                      (eql (alist-get 'pgrp attrs) own-group)
                      (equal "Z" (alist-get 'state attrs)))
            (push (cons pid (niwa-execution-process--start-identity
                             pid attrs))
                  escaped))
          (setq pending (append (gethash pid children) pending)))))
    (setf (niwa-execution-process--child-escaped child) escaped)))

(defun niwa-execution-process--signal (child signal)
  "Send SIGNAL to CHILD's process group when available."
  (let* ((process (niwa-execution-process--child-process child))
         (group-id (niwa-execution-process--child-group-id child))
         (workdir (niwa-execution-process--child-workdir child))
         (remote (and workdir (file-remote-p workdir)))
         (confined-p (niwa-execution-process--child-confined-p child)))
    (condition-case err
        (cond
         ((and (not remote) confined-p (eq signal 'INT)
               (niwa-execution-process--signal-confined-group
                child signal)))
         ((and (not remote) (eq system-type 'windows-nt)
               (process-live-p process))
          (kill-process process t))
         ((and (eq signal 'INT)
               (niwa-execution-process--child-tty-p child)
               (process-live-p process))
          (process-send-string process (string 3)))
         ((and (or remote (not (eq system-type 'windows-nt)))
               (integerp group-id) (> group-id 0))
          (if remote
              (pcase (niwa-execution-process--remote-group-status child)
                ('live
                 (niwa-transport-call-with-deadline
                  niwa-execution-process--remote-control-timeout
                  (lambda () (signal-process (- group-id) signal workdir))
                  (lambda () (error "Target process signal timed out"))))
                ;; A dead group needs no signal, and an ambiguous one must
                ;; not be signalled: the pinned identity is the only thing
                ;; stopping a recycled group number from taking it.  Neither
                ;; answer proves the outcome unprovable, though -- a leader
                ;; that exits a moment before its last member reads as
                ;; ambiguous for exactly that moment.  The bounded
                ;; escalation re-probes, and settles the outcome as unknown
                ;; only once the ambiguity outlives it.
                (_ -1))
            ;; Walk before signalling: a stopped shell's escaped children
            ;; are reparented and no longer reachable from its group.
            (progn
              (dolist (entry (niwa-execution-process--refresh-escaped child))
                (ignore-errors (signal-process (car entry) signal)))
              (signal-process (- group-id) signal))))
         (remote (error "Remote process-group identity is unavailable"))
         ((process-live-p process) (signal-process process signal)))
      (error
       (if remote
           (niwa-execution-process--mark-unknown child err)
         (when (process-live-p process)
           (ignore-errors (signal-process process signal))))))))

(defun niwa-execution-process--group-live-p (child)
  "Return non-nil when CHILD's process group still has a live member."
  (let* ((workdir (niwa-execution-process--child-workdir child))
         (remote (and workdir (file-remote-p workdir)))
         (group-id (niwa-execution-process--child-group-id child)))
    (cond
     ((and remote (not group-id))
      (niwa-execution-process--mark-unknown
       child '(error "Remote process-group identity was not received"))
      nil)
     (remote
      (condition-case err
          (not (eq 'dead
                   (niwa-execution-process--remote-group-status child)))
        (error
         (niwa-execution-process--mark-unknown child err)
         nil)))
     ((not (eq system-type 'windows-nt))
      (or (and group-id
               (condition-case nil
                   (zerop (signal-process (- group-id) 0))
                 (error nil)))
          (and (niwa-execution-process--child-escaped child)
               (niwa-execution-process--refresh-escaped child)
               t))))))

(defun niwa-execution-process--append-spool (child chunk)
  "Append CHUNK to CHILD's spool within the output cap.
Return (RETAINED . OVER-LIMIT-P): the prefix of CHUNK that was written, and
whether CHUNK exceeded the remaining cap."
  (let* ((path (niwa-execution-process--child-spool-path child))
         (current (file-attribute-size (file-attributes path)))
         (remaining (- niwa-execution-process-output-limit current))
         (coding (niwa-execution-process--child-coding child))
         (length (string-bytes (encode-coding-string chunk coding t)))
         (retained
          (if (<= length remaining)
              chunk
            (let ((low 0)
                  (high (length chunk)))
              (while (< low high)
                (let ((middle (/ (+ low high 1) 2)))
                  (if (<= (string-bytes
                           (encode-coding-string
                            (substring chunk 0 middle) coding t))
                          remaining)
                      (setq low middle)
                    (setq high (1- middle)))))
              (substring chunk 0 low)))))
    (when (and (> remaining 0) (not (string-empty-p retained)))
      (let ((coding-system-for-write 'no-conversion))
        (write-region (encode-coding-string retained coding t)
                      nil path t 'silent)))
    (cons retained (> length remaining))))

(defun niwa-execution-process--filter-pipeline-marker (child chunk)
  "Remove CHILD's private final-pipeline status frame from CHUNK.
Keep only a bounded possible frame between chunks; all other bytes remain raw."
  (let ((marker (niwa-execution-process--child-pipeline-marker child)))
    (if (null marker)
        chunk
      (let ((text (concat (niwa-execution-process--child-pipeline-buffer child)
                          chunk))
            (prefix (concat "\036" marker ":"))
            pieces start)
        (setf (niwa-execution-process--child-pipeline-buffer child) nil)
        (while (setq start (string-search "\036" text))
          (push (substring text 0 start) pieces)
          (setq text (substring text start))
          (cond
           ((and (string-prefix-p prefix text)
                 (string-search "\037" text))
            (let* ((end (string-search "\037" text))
                   (payload (substring text (length prefix) end)))
              (if (string-match-p "\\`[0-9]+\\(?: [0-9]+\\)* ?\\'" payload)
                  (setf (niwa-execution-process--child-pipeline-exit-codes child)
                        (mapcar #'string-to-number (split-string payload " " t)))
                (push (substring text 0 (1+ end)) pieces))
              (setq text (substring text (1+ end)))))
           ((or (string-prefix-p text prefix)
                (and (string-prefix-p prefix text) (< (length text) 65536)))
            (setf (niwa-execution-process--child-pipeline-buffer child) text)
            (setq text ""))
           (t (push (substring text 0 1) pieces)
              (setq text (substring text 1)))))
        (apply #'concat (nreverse (cons text pieces)))))))

(defun niwa-execution-process--consume-output (child chunk &optional stderr)
  "Append bounded raw output CHUNK to CHILD and notify its consumer.
STDERR selects the separate consumer and bypasses stdout marker filtering."
  (unless (or (niwa-execution-process--child-finished-p child)
              (niwa-execution-process--child-output-limit-p child))
    (condition-case err
        (let* ((filter
                (unless stderr
                  (niwa-execution-process--child-filter-function child)))
               (chunk (or (if filter (funcall filter child chunk) chunk) ""))
               (chunk (if stderr chunk
                        (niwa-execution-process--filter-pipeline-marker
                         child chunk)))
               (appended (if (niwa-execution-process--child-service-p child)
                             (cons chunk nil)
                           (niwa-execution-process--append-spool child chunk)))
               (retained (car appended)))
          (when-let* ((function
                       (if stderr
                           (niwa-execution-process--child-stderr-function child)
                         (niwa-execution-process--child-output-function child))))
            (unless (string-empty-p retained)
              (funcall function child retained)))
          (when (cdr appended)
            (setf (niwa-execution-process--child-output-limit-p child) t)
            (niwa-execution-process-stop child 'output-limit)))
      (error
       (setf (niwa-execution-process--child-error child) err)
       (niwa-execution-process-stop child 'output-write-failed)))))

(defconst niwa-execution-process--stderr-redirect-script
  "exec 2> >(exec head -c \"$1\" >>\"$0\"); shift; exec \"$@\""
  "Target-side wrapper sending stderr to the file named by its first argument.
The second argument bounds the file, as the spool cap bounds native stderr:
once `head' has copied that many bytes it exits, and further stderr writes
fail instead of filling the target's disk.")

(defun niwa-execution-process-collect-stderr (child)
  "Move CHILD's target-side stderr file into its spool and delete the file.
Return (TEXT . TRUNCATED-P), or nil when CHILD keeps no such file.  TEXT was
appended to the spool; TRUNCATED-P reports stderr cut by the output cap.  The
file exists only for a remote launch through TRAMP's shell adapter, where the
child cannot get a native stderr pipe.  Reading it is target I/O, so callers
run this where remote operations may run, such as result delivery.  A read
failure signals after the file is deleted."
  (when-let* ((path (niwa-execution-process--child-stderr-remote-path child)))
    (setf (niwa-execution-process--child-stderr-remote-path child) nil)
    (unwind-protect
        (let* ((spool (niwa-execution-process--child-spool-path child))
               (remaining
                (max 0 (- niwa-execution-process-output-limit
                          (or (and (file-readable-p spool)
                                   (file-attribute-size (file-attributes spool)))
                              0))))
               (raw (with-temp-buffer
                      (set-buffer-multibyte nil)
                      ;; One byte more than fits reveals an over-limit file.
                      (insert-file-contents-literally path nil 0 (1+ remaining))
                      (buffer-string)))
               (text (decode-coding-string
                      raw (or (niwa-execution-process--child-coding child)
                              'utf-8-unix)
                      t))
               (appended (niwa-execution-process--append-spool child text)))
          (when (cdr appended)
            (setf (niwa-execution-process--child-output-limit-p child) t))
          appended)
      (niwa-execution-process--delete-spool path))))

(defun niwa-execution-process-discard-stderr (child)
  "Delete CHILD's uncollected target-side stderr file, if any."
  (when-let* ((path (niwa-execution-process--child-stderr-remote-path child)))
    (setf (niwa-execution-process--child-stderr-remote-path child) nil)
    (niwa-execution-process--delete-spool path)))

(defun niwa-execution-process--drain-stderr (child &optional buffer-only)
  "Drain CHILD's separate stderr buffer and any pending pipe output.
The buffer may contain startup output received before installing the
filter, or final output from an adapter that already deleted its pipe.
BUFFER-ONLY consumes startup text without waiting on a running producer.
Final pipe draining has a deadline: an escaped descendant can retain stderr
after the owned process group settles, so EOF is not guaranteed."
  (when-let* ((buffer (niwa-execution-process--child-stderr-buffer child))
              ((buffer-live-p buffer)))
    (cl-labels ((drain-buffer
                 ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (unless (zerop (buffer-size))
                       (let ((chunk (buffer-string))
                             (inhibit-read-only t))
                         (erase-buffer)
                         (niwa-execution-process--consume-output
                          child chunk t)))))))
      (drain-buffer)
      (unless buffer-only
        (when-let* ((process (niwa-execution-process--child-stderr-process child))
                    ((process-live-p process)))
          (let ((deadline (+ (float-time)
                             niwa-execution-process--stderr-drain-timeout))
                (filter (process-filter process)))
            ;; Once output arrives, a single accept-process-output can keep
            ;; reading until a gap, ignoring its timeout.  Enforce our budget
            ;; inside the filter too, closing the owned pipe to end that read.
            ;; Keep timers inhibited so settlement cannot reenter itself.
            (unwind-protect
                (progn
                  (set-process-filter
                   process
                   (lambda (_process chunk)
                     (niwa-execution-process--consume-output child chunk t)
                     (when (>= (float-time) deadline)
                       (set-process-sentinel process #'ignore)
                       (delete-process process))))
                  (while (and (< (float-time) deadline)
                              (process-live-p process)
                              (accept-process-output process 0.01 nil 1))))
              (when (process-live-p process)
                (set-process-filter process filter)))))))))

(defun niwa-execution-process--attach-stderr (child)
  "Attach CHILD's separate stderr filter without losing startup output."
  (when-let* ((buffer (niwa-execution-process--child-stderr-buffer child))
              ((buffer-live-p buffer))
              (process (get-buffer-process buffer)))
    (setf (niwa-execution-process--child-stderr-process child) process)
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process #'ignore)
    (set-process-filter
     process (lambda (_process chunk)
               (niwa-execution-process--consume-output child chunk t))))
  (niwa-execution-process--drain-stderr child t))

(defun niwa-execution-process-read (child)
  "Return CHILD's complete decoded spooled output."
  (let ((path (niwa-execution-process--child-spool-path child)))
    (if (not (and path (file-readable-p path)))
        ""
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (decode-coding-string (buffer-string) 'utf-8-unix t)))))

(defun niwa-execution-process-release (child &optional preserve-spool)
  "Release CHILD's runtime and delete its spool unless PRESERVE-SPOOL."
  (let ((process (niwa-execution-process--child-process child)))
    (dolist (timer
             (list (niwa-execution-process--child-timeout-timer child)
                   (niwa-execution-process--child-timeout-settle-timer child)
                   (niwa-execution-process--child-force-timer child)
                   (niwa-execution-process--child-settle-timer child)
                   (niwa-execution-process--child-watch-timer child)))
      (when (timerp timer) (niwa--ui-timer-cancel timer)))
    (setf (niwa-execution-process--child-process child) nil
          (niwa-execution-process--child-timeout-timer child) nil
          (niwa-execution-process--child-timeout-settle-timer child) nil
          (niwa-execution-process--child-force-timer child) nil
          (niwa-execution-process--child-settle-timer child) nil
          (niwa-execution-process--child-watch-timer child) nil)
    (let* ((buffer (niwa-execution-process--child-stderr-buffer child))
           (stderr (or (niwa-execution-process--child-stderr-process child)
                       (and (buffer-live-p buffer)
                            (get-buffer-process buffer)))))
      (when stderr
        (set-process-sentinel stderr #'ignore)
        (set-process-filter stderr #'ignore)
        (ignore-errors (delete-process stderr)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (setf (niwa-execution-process--child-stderr-buffer child) nil
            (niwa-execution-process--child-stderr-process child) nil))
    (when process
      (set-process-query-on-exit-flag process nil)
      (ignore-errors (delete-process process)))
    (unless preserve-spool
      (when-let* ((path (niwa-execution-process--child-spool-path child)))
        (niwa-execution-process--delete-spool path)))))

(defun niwa-execution-process--delete-spool (path)
  "Delete spool PATH, deferring the deletion while the transport is busy.

Release reaches here from the child\'s sentinel and from the settle timer,
so this deletion lands inside whatever remote command those interrupted:
TRAMP yields to timers between its send and its read, and a command issued
in that window is answered with the reply belonging to the one already in
flight.

Deferral is decided on the transport alone, never on how PATH is spelled.
Two attempts to decide it from the name were wrong -- `file-remote-p\' on
the name as passed, then on its expansion, and an absolute local name
expands to itself even under a remote `default-directory\'.  Meanwhile the
recorded chain showed this function reaching the target anyway.  A local
`delete-file\' is cheap enough that deferring one costs nothing, and
nothing waits on the spool being gone, so the question the guard has to
answer is only whether issuing any file operation right now can nest.

`niwa-transport-run-when-idle\' runs the deletion inline when the
transport is free, which is the ordinary case.  A disabled transport drops
deferred work, so that case deletes immediately rather than leak."
  (unless (and (niwa-transport-busy-p path)
               (niwa-transport-run-when-idle
                (list 'execution-spool path) path
                (lambda () (ignore-errors (delete-file path)))))
    (ignore-errors (delete-file path))))

(defun niwa-execution-process-relocate-spool (child destination)
  "Move CHILD's spool to DESTINATION and return the new path."
  (let ((source (niwa-execution-process--child-spool-path child)))
    (unless (equal source destination)
      (make-directory (file-name-directory destination) t)
      (rename-file source destination t)
      (setf (niwa-execution-process--child-spool-path child) destination))
    destination))

(defun niwa-execution-process-rebind-spool (child path)
  "Record PATH as CHILD's spool after an external tree relocation."
  (setf (niwa-execution-process--child-spool-path child) path))

(defun niwa-execution-process--finish
    (child status &optional error-data)
  "Emit CHILD's immutable terminal result once."
  (unless (niwa-execution-process--child-finished-p child)
    (when-let* ((process (niwa-execution-process--child-process child))
                ((memq (process-status process) '(exit signal))))
      (while (accept-process-output process 0.01 nil 1)))
    (niwa-execution-process--drain-stderr child)
    (when-let* ((pending (niwa-execution-process--child-pipeline-buffer child)))
      (setf (niwa-execution-process--child-pipeline-marker child) nil
            (niwa-execution-process--child-pipeline-buffer child) nil)
      (niwa-execution-process--consume-output child pending))
    (setf (niwa-execution-process--child-finished-p child) t
          (niwa-execution-process--child-exit-code child) status)
    (let* ((path (niwa-execution-process--child-spool-path child))
           (bytes (or (and path (file-readable-p path)
                           (file-attribute-size (file-attributes path)))
                      0))
           (result
            (list :error (or error-data
                             (niwa-execution-process--child-error child))
                  :exit-code status
                  :group-id (niwa-execution-process--child-group-id child)
                  :group-start-time
                  (niwa-execution-process--child-group-start-time child)
                  :output (niwa-execution-process-read child)
                  :output-bytes bytes
                  :output-limit-p
                  (niwa-execution-process--child-output-limit-p child)
                  :termination
                  (niwa-execution-process--child-termination child)
                  :timed-out-p
                  (niwa-execution-process--child-timed-out-p child)
                  :wall-time-seconds
                  (- (float-time)
                     (niwa-execution-process--child-started-at child))
                  :workdir
                  (niwa-execution-process--child-workdir child)))
           (terminal
            (niwa-execution-process--child-terminal-function child)))
      (niwa-execution-process-release child t)
      (when terminal (funcall terminal child result)))))

(defun niwa-execution-process--settle-after-kill (child)
  "Settle CHILD after its final process-group signal."
  (unless (niwa-execution-process--child-finished-p child)
    (when (and (file-remote-p
                (or (niwa-execution-process--child-workdir child) ""))
               (not (eq 'unknown
                        (niwa-execution-process--child-termination child)))
               (niwa-execution-process--group-live-p child))
      (niwa-execution-process--mark-unknown
       child '(error "Remote process group survived the final KILL signal")))
    (niwa-execution-process--finish
     child (or (niwa-execution-process--child-exit-code child) -1))))

(defun niwa-execution-process--force-kill (child)
  "Force-kill CHILD and schedule bounded settlement."
  (unless (niwa-execution-process--child-finished-p child)
    (setf (niwa-execution-process--child-force-timer child) nil)
    (niwa-execution-process--signal child 'KILL)
    (unless (niwa--timer-pending-p
             (niwa-execution-process--child-settle-timer child))
      (setf (niwa-execution-process--child-settle-timer child)
            (niwa-transport-run-at-time
             niwa-execution-process--child-kill-delay
             #'niwa-execution-process--settle-after-kill child)))))

(defun niwa-execution-process--start-stop (child)
  "Send CHILD's first stop signal outside its process filter."
  (setf (niwa-execution-process--child-force-timer child) nil)
  (unless (niwa-execution-process--child-finished-p child)
    (niwa-execution-process--signal child 'TERM)
    (unless (eq 'unknown
                (niwa-execution-process--child-termination child))
      (setf (niwa-execution-process--child-force-timer child)
            (niwa-transport-run-at-time
             niwa-execution-process--child-kill-delay
             #'niwa-execution-process--force-kill child)))))

(defun niwa-execution-process-stop (child reason)
  "Latch REASON and terminate CHILD with TERM then bounded KILL grace."
  (unless (or (niwa-execution-process--child-finished-p child)
              (niwa-execution-process--child-stop-p child))
    (setf (niwa-execution-process--child-stop-p child) t
          (niwa-execution-process--child-termination child) reason)
    (let* ((process (niwa-execution-process--child-process child))
           (remote
            (file-remote-p
             (or (niwa-execution-process--child-workdir child) ""))))
      (cond
       ((and (processp process)
             (memq (process-status process) '(exit signal))
             (not remote)
             (niwa-execution-process--group-live-p child))
        (niwa-execution-process--signal child 'TERM)
        (setf (niwa-execution-process--child-force-timer child)
              (niwa-transport-run-at-time
               niwa-execution-process--child-kill-delay
               #'niwa-execution-process--force-kill child)))
       ((and (processp process)
             (memq (process-status process) '(exit signal)))
        (niwa-execution-process--ended child process))
       ((not (process-live-p process))
        (niwa-execution-process--finish
         child (or (niwa-execution-process--child-exit-code child) -1)))
       (remote
        (setf (niwa-execution-process--child-force-timer child)
              (niwa-transport-run-at-time
               0 #'niwa-execution-process--start-stop child)))
       (t
        (niwa-execution-process--signal child 'TERM)
        (setf (niwa-execution-process--child-force-timer child)
              (niwa-transport-run-at-time
               niwa-execution-process--child-kill-delay
               #'niwa-execution-process--force-kill child)))))
    t))

(defun niwa-execution-process--settle-timed-out (child)
  "Forcibly settle CHILD when its timeout escalation did not finish."
  (unless (niwa-execution-process--child-finished-p child)
    (when (and (file-remote-p
                (or (niwa-execution-process--child-workdir child) ""))
               (not (eq 'unknown
                        (niwa-execution-process--child-termination child))))
      (niwa-execution-process--mark-unknown
       child '(error "Timed-out child did not settle")))
    (niwa-execution-process--finish
     child (or (niwa-execution-process--child-exit-code child) -1))))

(defun niwa-execution-process--time-out (child)
  "Mark CHILD timed out and terminate its process group."
  (unless (or (niwa-execution-process--child-finished-p child)
              (niwa-execution-process--child-stop-p child))
    (setf (niwa-execution-process--child-timed-out-p child) t)
    (niwa-execution-process-stop child 'timed-out)
    (unless (niwa-execution-process--child-finished-p child)
      (setf (niwa-execution-process--child-timeout-settle-timer child)
            (niwa-transport-run-at-time
             (+ (* 2 niwa-execution-process--child-kill-delay)
                (* 4 niwa-execution-process--remote-control-timeout))
             #'niwa-execution-process--settle-timed-out child)))))

(defun niwa-execution-process--settle-main-exit (child)
  "Drain descendants or settle CHILD after its main process exits."
  (let ((workdir (niwa-execution-process--child-workdir child)))
    (setf (niwa-execution-process--child-settle-timer child) nil)
    (if (and (not (eq system-type 'windows-nt))
             (not (file-remote-p (or workdir "")))
             (niwa-execution-process--group-live-p child))
        (niwa-execution-process-stop
         child
         (or (niwa-execution-process--child-termination child) 'exited))
      (niwa-execution-process--finish
       child (or (niwa-execution-process--child-exit-code child) -1)))))

(defun niwa-execution-process--settle-stop-main-exit (child)
  "Settle stopped remote CHILD early when its group already drained."
  (setf (niwa-execution-process--child-settle-timer child) nil)
  (unless (niwa-execution-process--child-finished-p child)
    (when (eq 'dead
              (condition-case nil
                  (niwa-execution-process--remote-group-status child)
                (error nil)))
      (when-let* ((timer
                   (niwa-execution-process--child-force-timer child)))
        (when (timerp timer) (cancel-timer timer)))
      (setf (niwa-execution-process--child-force-timer child) nil)
      (niwa-execution-process--finish
       child (or (niwa-execution-process--child-exit-code child) -1)))))

(defun niwa-execution-process--ended (child process)
  "Settle CHILD when its exact PROCESS reaches a terminal state."
  (when (eq process (niwa-execution-process--child-process child))
    (let ((status (process-status process)))
      (when (memq status '(exit signal))
        (setf (niwa-execution-process--child-exit-code child)
              (process-exit-status process))
        (when (and
               (not (niwa-execution-process--child-termination child))
               (eq status 'signal))
          (setf (niwa-execution-process--child-termination child)
                (if (and (niwa-execution-process--child-interrupt-requested-p child)
                         (= (process-exit-status process) 2))
                    'interrupted
                  'signaled)))
        (cond
         ((niwa--timer-pending-p
           (niwa-execution-process--child-settle-timer child)))
         ((and (niwa-execution-process--child-stop-p child)
               (niwa--timer-pending-p
                (niwa-execution-process--child-force-timer child)))
          (setf (niwa-execution-process--child-settle-timer child)
                (niwa-transport-run-at-time
                 0.02
                 (if (file-remote-p
                      (or (niwa-execution-process--child-workdir child) ""))
                     #'niwa-execution-process--settle-stop-main-exit
                   #'niwa-execution-process--settle-main-exit)
                 child)))
         (t
          (setf (niwa-execution-process--child-settle-timer child)
                (niwa-transport-run-at-time
                 0.02 #'niwa-execution-process--settle-main-exit
                 child))))))))

(defconst niwa-execution-process--watch-interval 1.0
  "Seconds between checks for an exit whose sentinel never ran.
The sentinel settles almost every child; this only bounds how late a lost
one is noticed.  Checks share periodic host wakeups with housekeeping.
The former 0.1 s timer repainted a whole pgtk frame ten times a second.")

(cl-defun niwa-execution-process-start
    (child &key name command target coding timeout confined separate-stderr)
  "Launch COMMAND in CHILD and return CHILD, or nil on failure.
SEPARATE-STDERR retains stderr in the shared bounded spool without stdout
filtering or notifications.  Local and direct-async launches use a native
buffer-backed stderr pipe.  A remote launch through TRAMP's sh adapter does
not: that adapter serves `:stderr' through a FIFO read on a second
connection, and the sentinel it installs to remove that FIFO issues remote
commands from inside whatever command is already running on the host.  There
a target-side wrapper appends stderr to a temporary file instead, which
`niwa-execution-process-collect-stderr' moves into the spool."
  (let* ((workdir (niwa-execution-process--child-workdir child))
         (remote (file-remote-p workdir))
         (command
          (niwa-execution-process--localize-command
           command workdir target))
         (direct-async
          (and remote
               (niwa-execution-process--direct-async-p
                child command workdir)))
         (file-stderr (and separate-stderr remote (not direct-async)))
         (separate-stderr (and separate-stderr (not file-stderr)))
         (_ (setf (niwa-execution-process--child-direct-async-p child)
                  direct-async))
         (process-environment
          (niwa-execution-process--process-environment remote)))
    (setf (niwa-execution-process--child-confined-p child)
          (and confined t)
          (niwa-execution-process--child-coding child) coding)
    (condition-case err
        (progn
          (with-temp-buffer
            (setq default-directory workdir)
            (when file-stderr
              (let ((path (make-nearby-temp-file "niwa-stderr-")))
                (setf (niwa-execution-process--child-stderr-remote-path child)
                      path)
                ;; Outside any confinement wrapper, which inherits fd 2.
                (setq command
                      (append (list "bash" "-c"
                                    niwa-execution-process--stderr-redirect-script
                                    (file-local-name path)
                                    (number-to-string
                                     (1+ niwa-execution-process-output-limit)))
                              command))))
            (when remote
              (setq command
                    (niwa-execution-process--remote-command child command)))
            (let ((executable (car-safe command)))
              (unless (and (stringp executable)
                           (if remote
                               (niwa--executable-find executable remote)
                             (executable-find executable)))
                (signal 'file-missing (list "Executable not found" executable))))
            (when separate-stderr
              (setf (niwa-execution-process--child-stderr-buffer child)
                    (generate-new-buffer " *niwa-execution-stderr*")))
            (setf (niwa-execution-process--child-launch-attempted-p child)
                  t
                  (niwa-execution-process--child-process child)
                  (niwa-transport-call-with-spawn-channel
                   remote direct-async
                   (lambda ()
                     ;; `make-process' does not pass CODING to the pipe it
                     ;; creates for `:stderr'; that pipe takes the default
                     ;; process coding, which follows the locale.
                     (let ((default-process-coding-system
                            (if coding (cons coding coding)
                              default-process-coding-system)))
                       (make-process
                        :name name :buffer nil :command command :coding coding
                        :stderr (niwa-execution-process--child-stderr-buffer child)
                        :connection-type
                        (if (niwa-execution-process--child-tty-p child)
                            'pty
                          'pipe)
                        :file-handler t
                        :filter
                        (lambda (_process chunk)
                          (niwa-execution-process--filter-group-marker
                           child chunk))
                        :noquery t
                        :sentinel
                        (lambda (process _event)
                          (niwa-execution-process--ended child process))))))))
          (unless remote
            (setf (niwa-execution-process--child-group-id child)
                  (process-id
                   (niwa-execution-process--child-process child))))
          (niwa-execution-process--attach-stderr child)
          (let ((process (niwa-execution-process--child-process child)))
            (when (and (not (niwa-execution-process--child-service-p child))
                       (not (niwa-execution-process--child-tty-p child))
                       (process-live-p process))
              (condition-case err
                  (process-send-eof process)
                (error
                 ;; A fast helper can exit between the liveness check and EOF.
                 (when (process-live-p process)
                   (signal (car err) (cdr err)))))))
          (unless (niwa-execution-process--child-service-p child)
            (setf (niwa-execution-process--child-watch-timer child)
                (niwa--run-periodic-timer
                 (niwa-execution-process--child-diagnostic-scope child)
                 niwa-execution-process--watch-interval
                 (lambda ()
                   (unless
                       (niwa-execution-process--child-finished-p child)
                     (let ((process
                            (niwa-execution-process--child-process child)))
                       (when (and
                              (processp process)
                              (memq (process-status process) '(exit signal)))
                         (niwa-execution-process--ended child process))))))))
          (when timeout
            (setf (niwa-execution-process--child-timeout-timer child)
                  (niwa-transport-run-at-time
                   timeout #'niwa-execution-process--time-out child)))
          child)
      (error
       (niwa-execution-process--finish child -1 err)
       nil))))

(defun niwa-execution-process-write (child input)
  "Write INPUT to live CHILD's writable stdin."
  (let ((process (niwa-execution-process--child-process child)))
    (when (process-live-p process)
      (process-send-string process input)
      t)))

(defun niwa-execution-process-interrupt (child)
  "Interrupt CHILD through its supported process-group boundary."
  (unless (niwa-execution-process--child-finished-p child)
    (setf (niwa-execution-process--child-interrupt-requested-p child) t)
    (niwa-execution-process--signal child 'INT)
    t))

(provide 'niwa-execution-process)
;;; niwa-execution-process.el ends here
