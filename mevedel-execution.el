;;; mevedel-execution.el --- Bounded child-process lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Owns model-triggered operating-system processes: stable environments,
;; process groups, optional PTYs, confinement, timeouts, bounded disk spooling,
;; and private process records.  During an explicit telemetry profiler run it
;; also wraps one full Eask suite with GNU time for native peak-resource data.
;; Callers receive result plists and never process objects.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `mevedel-execution-scheduler'
(declare-function mevedel-execution-scheduler-cancel
                  "mevedel-execution-scheduler" (lease))
(declare-function mevedel-execution-scheduler-create
                  "mevedel-execution-scheduler" ())
(declare-function mevedel-execution-scheduler-release
                  "mevedel-execution-scheduler" (lease))
(declare-function mevedel-execution-scheduler-submit
                  "mevedel-execution-scheduler" (scheduler mode start))

;; `mevedel-sandbox'
(declare-function mevedel-sandbox--record-launch-failure
                  "mevedel-sandbox" (child-result))
(declare-function mevedel-sandbox-cleanup "mevedel-sandbox" (preparation))
(declare-function mevedel-sandbox-launch-failed-p
                  "mevedel-sandbox" (preparation child-result))
(declare-function mevedel-sandbox-prepare
                  "mevedel-sandbox"
                  (command workdir writable-roots &optional
                           additional-permissions sandbox-permissions))
(declare-function mevedel-sandbox-strip-marker
                  "mevedel-sandbox" (preparation child-result))
(declare-function mevedel-sandbox-track-active
                  "mevedel-sandbox" (session token facts))

;; `mevedel-structs'
(declare-function mevedel-request-push-canceller
                  "mevedel-structs" (request canceller))
(declare-function mevedel-session--set-execution-state
                  "mevedel-structs" (session state))
(declare-function mevedel-session-execution-state "mevedel-structs" (cl-x) t)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-finish "mevedel-telemetry" (span &rest props))
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))
(declare-function mevedel-telemetry-start
                  "mevedel-telemetry" (session event &rest props))
(declare-function mevedel-telemetry-profiler-directory
                  "mevedel-telemetry" (session))

;; `mevedel-utilities'
(declare-function mevedel--head-tail-preview-parts
                  "mevedel-utilities"
                  (head tail total-length &optional preview-size))


;;
;;; Configuration and private state

(defcustom mevedel-execution-output-limit (* 64 1024 1024)
  "Maximum bytes retained from one child process.

The execution module terminates a child when its merged stdout and stderr
reach this limit.  Bytes already written to its spool remain available in the
terminal result."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-execution-inline-output-limit 2000
  "Maximum unread characters returned inline by one managed observation.

Larger unread ranges use the shared newline-aware head-and-tail preview while
the complete spool remains at the path in the execution facts."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-execution-progress-delay 2
  "Seconds before a managed Bash execution publishes live progress."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-execution-progress-interval 0.25
  "Seconds between managed Bash progress events.
Values below 0.25 are clamped so the UI receives at most four per second."
  :type 'number
  :group 'mevedel)

(defconst mevedel-execution-live-limit 64
  "Maximum number of live managed Bash processes in one session.")

(defconst mevedel-execution--child-kill-delay 2
  "Seconds to wait before force-killing a stopped child process group.")

(defconst mevedel-execution--environment
  '(("NO_COLOR" . "1")
    ("TERM" . "dumb")
    ("LC_ALL" . "C.UTF-8")
    ("LANG" . "C.UTF-8")
    ("COLORTERM" . "")
    ("PAGER" . "cat")
    ("GIT_PAGER" . "cat")
    ("GH_PAGER" . "cat")
    ("MEVEDEL_EXECUTION" . "1"))
  "Stable defaults for model-triggered child processes.")

(define-error 'mevedel-execution-error "Managed execution error")
(define-error 'mevedel-execution-input-error
  "Managed execution does not accept this input" 'mevedel-execution-error)
(define-error 'mevedel-execution-limit
  "Managed execution limit reached" 'mevedel-execution-error)
(define-error 'mevedel-execution-not-found
  "Managed execution not found" 'mevedel-execution-error)

(cl-defstruct (mevedel-execution--state
               (:constructor mevedel-execution--state-create))
  "Opaque per-session execution state."
  next-id
  records
  scheduler)

(cl-defstruct (mevedel-execution--origin
               (:constructor mevedel-execution--origin-create))
  "Immutable ownership and transcript correlation for one execution."
  data-buffer
  owner
  owner-context
  session
  tool-args
  tool-use-id)

(cl-defstruct (mevedel-execution--record
               (:constructor mevedel-execution--record-create))
  "Private state for one operating-system process."
  callback
  delivery-state
  error-data
  execution-id
  exit-code
  finished-p
  force-timer
  group-id
  last-byte-newline-p
  marker
  marker-buffer
  marker-seen-p
  newline-count
  observer
  observer-timer
  omitted-output-bytes
  origin
  outcome-function
  output-chars
  output-head
  output-limit-p
  output-tail
  process
  progress-timer
  read-offset
  resource-report-path
  retained-p
  sandbox-active-token
  sandbox-facts
  sandbox-preparation
  scheduler-lease
  settle-timer
  spool-path
  started-at
  stop-p
  termination
  timed-out-p
  teardown-function
  timeout
  timeout-timer
  token
  tty-p
  unread-chars
  unread-head
  unread-tail
  watch-timer
  workdir
  yield-time-ms
  yield-timer
  yielded-p)

(defvar mevedel-execution--orphan-state nil
  "Private state for children that have no chat session owner.")

(defvar mevedel-execution--sessions
  (make-hash-table :test #'eq :weakness 'key)
  "Weak set of sessions that have created execution state.")

(defvar mevedel-execution--resource-capture-claims
  (make-hash-table :test #'equal :weakness 'key)
  "Weak set of native resource report paths claimed by live runs.")

(defvar mevedel-execution-event-functions nil
  "Functions notified of immutable managed execution event plists.

Each function receives one event.  Return values are ignored and errors are
contained by the execution module.")

(defvar mevedel-execution-state-change-hook nil
  "Functions notified when a session's live execution set changes.

Each function receives the owning session and originating data buffer.  Errors
are contained by the execution module.")

(defvar mevedel-execution-mailbox-delivery-function nil
  "Function that synchronously secures one terminal event in its owner mailbox.
The function receives an immutable event plist and its private owner context,
then returns non-nil only after durable delivery.  This is deliberately
separate from passive event hooks; the context is never published to them.")


;;
;;; State and environment

(defun mevedel-execution--managed-live-p (record)
  "Return non-nil when RECORD is a user-visible live Bash execution."
  (and (mevedel-execution--record-execution-id record)
       (not (mevedel-execution--record-finished-p record))))

(defun mevedel-execution--new-state ()
  "Return an empty opaque execution state."
  (require 'mevedel-execution-scheduler)
  (mevedel-execution--state-create
   :next-id 0
   :records (make-hash-table :test #'equal)
   :scheduler (mevedel-execution-scheduler-create)))

(defun mevedel-execution--state-for-session (session)
  "Return the private execution state for SESSION.

When SESSION is nil, use the module-owned state for direct non-session calls."
  (if session
      (progn
        (puthash session t mevedel-execution--sessions)
        (or (mevedel-session-execution-state session)
            (let ((state (mevedel-execution--new-state)))
              (mevedel-session--set-execution-state session state)
              state)))
    (or mevedel-execution--orphan-state
        (setq mevedel-execution--orphan-state
              (mevedel-execution--new-state)))))

(defun mevedel-execution--process-environment ()
  "Return a child environment with stable execution defaults."
  (let ((process-environment (copy-sequence process-environment)))
    (dolist (entry mevedel-execution--environment)
      (setenv (car entry) (cdr entry)))
    process-environment))

(defun mevedel-execution--notify-state-change (record)
  "Notify observers that RECORD's session execution set changed."
  (when (mevedel-execution--record-execution-id record)
    (let* ((origin (mevedel-execution--record-origin record))
           (session (mevedel-execution--origin-session origin))
           (data-buffer (mevedel-execution--origin-data-buffer origin)))
      (when session
        (dolist (function mevedel-execution-state-change-hook)
          (when (functionp function)
            (condition-case err
                (funcall function session data-buffer)
              (error
               (display-warning
                'mevedel
                (format "Execution state consumer failed: %s"
                        (error-message-string err))
                :warning)))))))))


;;
;;; Process groups and output spooling

(defun mevedel-execution--signal-confined-group (record signal)
  "Send SIGNAL to RECORD's foreground process group inside Bubblewrap."
  (let ((outer-group-id (mevedel-execution--record-group-id record))
        (children (make-hash-table :test #'eql))
        (attributes (make-hash-table :test #'eql))
        (pending (list (mevedel-execution--record-group-id record)))
        group-id)
    (dolist (pid (list-system-processes))
      (when-let* ((attrs (process-attributes pid))
                  (parent (alist-get 'ppid attrs)))
        (puthash pid attrs attributes)
        (push pid (gethash parent children))))
    (while (and pending (not group-id))
      (let ((pid (pop pending)))
        (dolist (child (gethash pid children))
          (push child pending)
          (let ((attrs (gethash child attributes)))
            (unless (equal "bwrap" (alist-get 'comm attrs))
              (let ((candidate
                     (let ((foreground (alist-get 'tpgid attrs)))
                       (if (and (integerp foreground)
                                (> foreground 0)
                                (not (eql foreground outer-group-id)))
                           foreground
                         (alist-get 'pgrp attrs)))))
                (when (and (integerp candidate) (> candidate 0))
                  (unless (eql candidate outer-group-id)
                    (setq group-id candidate)))))))))
    (when group-id
      (condition-case nil
          (progn
            (signal-process (- group-id) signal)
            t)
        (error nil)))))

(defun mevedel-execution--signal-record (record signal)
  "Send SIGNAL to RECORD's process group when available."
  (let* ((process (mevedel-execution--record-process record))
         (group-id (mevedel-execution--record-group-id record))
         (confined-p
          (eq 'bubblewrap
              (plist-get (mevedel-execution--record-sandbox-facts record)
                         :sandbox))))
    (condition-case nil
        (cond
         ((and confined-p
               (eq signal 'INT)
               (mevedel-execution--signal-confined-group record signal)))
         ((and (eq signal 'INT)
               (mevedel-execution--record-tty-p record)
               (process-live-p process))
          (process-send-string process (string 3)))
         ((and (not (eq system-type 'windows-nt))
               (integerp group-id) (> group-id 0))
          (signal-process (- group-id) signal))
         ((process-live-p process)
          (signal-process process signal)))
      (error
       (when (process-live-p process)
         (ignore-errors (signal-process process signal)))))))

(defun mevedel-execution--group-live-p (record)
  "Return non-nil when RECORD's process group still has a member."
  (unless (eq system-type 'windows-nt)
    (when-let* ((group-id (mevedel-execution--record-group-id record)))
      (condition-case nil
          (zerop (signal-process (- group-id) 0))
        (error nil)))))

(defun mevedel-execution--append-output (record chunk)
  "Append bounded raw output CHUNK to RECORD's spool."
  (unless (or (mevedel-execution--record-finished-p record)
              (mevedel-execution--record-output-limit-p record))
    (let* ((path (mevedel-execution--record-spool-path record))
           (current (file-attribute-size (file-attributes path)))
           (remaining (- mevedel-execution-output-limit current))
           (length (string-bytes chunk)))
      (when (> remaining 0)
        (let ((coding-system-for-write 'no-conversion))
          (write-region
           (if (> length remaining)
               (substring chunk 0 remaining)
             chunk)
           nil path t 'silent)))
      (when (> length remaining)
        (setf (mevedel-execution--record-output-limit-p record) t)
        (mevedel-execution--begin-stop record 'output-limit)))))

(defun mevedel-execution--read-output (record)
  "Return RECORD's complete decoded spooled output."
  (let ((path (mevedel-execution--record-spool-path record)))
    (if (not (file-readable-p path))
        ""
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (decode-coding-string (buffer-string) 'utf-8-unix t)))))

(defun mevedel-execution--release-runtime (record)
  "Release RECORD's process, timers, group file, and active boundary."
  (dolist (timer (list (mevedel-execution--record-timeout-timer record)
                       (mevedel-execution--record-force-timer record)
                       (mevedel-execution--record-observer-timer record)
                       (mevedel-execution--record-progress-timer record)
                       (mevedel-execution--record-settle-timer record)
                       (mevedel-execution--record-watch-timer record)
                       (mevedel-execution--record-yield-timer record)))
    (when (timerp timer)
      (cancel-timer timer)))
  (when-let* ((process (mevedel-execution--record-process record)))
    (set-process-query-on-exit-flag process nil)
    (when (process-live-p process)
      (ignore-errors (delete-process process))))
  (when-let* ((active-token
              (mevedel-execution--record-sandbox-active-token record)))
    (mevedel-sandbox-track-active
     (mevedel-execution--origin-session
      (mevedel-execution--record-origin record))
     active-token nil))
  (setf (mevedel-execution--record-process record) nil
        (mevedel-execution--record-group-id record) nil
        (mevedel-execution--record-sandbox-active-token record) nil))

(defun mevedel-execution--cleanup-record (record &optional preserve-spool)
  "Release runtime and registry state for RECORD.

Delete its spool unless PRESERVE-SPOOL is non-nil."
  (let ((removed-live-p (mevedel-execution--managed-live-p record)))
    (mevedel-execution--release-runtime record)
    (unless preserve-spool
      (when-let* ((path (mevedel-execution--record-spool-path record)))
        (ignore-errors (delete-file path))))
    (let ((state
           (mevedel-execution--state-for-session
            (mevedel-execution--origin-session
             (mevedel-execution--record-origin record)))))
      (remhash (or (mevedel-execution--record-execution-id record)
                   (mevedel-execution--record-token record))
               (mevedel-execution--state-records state))
      (when removed-live-p
        (mevedel-execution--notify-state-change record)))))

(defun mevedel-execution--finish-record (record status &optional error-data)
  "Settle RECORD once with STATUS and optional ERROR-DATA."
  (unless (mevedel-execution--record-finished-p record)
    (setf (mevedel-execution--record-finished-p record) t)
    (let* ((path (mevedel-execution--record-spool-path record))
           (bytes (or (and (file-readable-p path)
                           (file-attribute-size (file-attributes path)))
                      0))
           (output (mevedel-execution--read-output record))
           (callback (mevedel-execution--record-callback record))
           (result
            (list :exit-code status
                  :output output
                  :output-bytes bytes
                  :timed-out-p
                  (mevedel-execution--record-timed-out-p record)
                  :output-limit-p
                  (mevedel-execution--record-output-limit-p record)
                  :wall-time-seconds
                  (- (float-time)
                     (mevedel-execution--record-started-at record))
                  :error error-data)))
      (mevedel-execution--cleanup-record record)
      (funcall callback result))))

(defun mevedel-execution--settle-after-kill (record)
  "Settle stopped RECORD after its final process-group signal."
  (unless (mevedel-execution--record-finished-p record)
    (if (mevedel-execution--record-execution-id record)
        (unless (and (mevedel-execution--record-stop-p record)
                     (timerp
                      (mevedel-execution--record-force-timer record)))
          (mevedel-execution--finish-managed record))
      (mevedel-execution--finish-record
       record (or (mevedel-execution--record-exit-code record) -1)))))

(defun mevedel-execution--force-kill (record)
  "Force-kill RECORD's process group and schedule bounded settlement."
  (unless (mevedel-execution--record-finished-p record)
    (setf (mevedel-execution--record-force-timer record) nil)
    (mevedel-execution--signal-record record 'KILL)
    (setf (mevedel-execution--record-settle-timer record)
          (run-at-time mevedel-execution--child-kill-delay nil
                       #'mevedel-execution--settle-after-kill record))))

(defun mevedel-execution--begin-stop (record reason)
  "Latch REASON and terminate RECORD with TERM then bounded KILL grace."
  (unless (or (mevedel-execution--record-finished-p record)
              (mevedel-execution--record-stop-p record))
    (setf (mevedel-execution--record-stop-p record) t
          (mevedel-execution--record-termination record) reason)
    (mevedel-execution--signal-record record 'TERM)
    (setf (mevedel-execution--record-force-timer record)
          (run-at-time mevedel-execution--child-kill-delay nil
                       #'mevedel-execution--force-kill record))
    t))

(defun mevedel-execution--time-out (record)
  "Mark RECORD timed out and terminate its process group."
  (when (mevedel-execution--begin-stop record 'timed-out)
    (setf (mevedel-execution--record-timed-out-p record) t)))

(defun mevedel-execution--settle-managed-main-exit (record)
  "Clean remaining descendants, or settle RECORD after its shell exits."
  (setf (mevedel-execution--record-settle-timer record) nil)
  (if (mevedel-execution--group-live-p record)
      (mevedel-execution--begin-stop
       record (or (mevedel-execution--record-termination record) 'exited))
    (mevedel-execution--finish-managed record)))

(defun mevedel-execution--process-ended (record process)
  "Settle RECORD when PROCESS reaches a terminal state."
  (let ((status (process-status process)))
    (when (memq status '(exit signal))
      (let ((exit-code (process-exit-status process)))
        (setf (mevedel-execution--record-exit-code record) exit-code)
        (when (and (not (mevedel-execution--record-termination record))
                   (eq status 'signal))
          (setf (mevedel-execution--record-termination record) 'signaled)))
      (if (mevedel-execution--record-execution-id record)
          (unless (or (and (mevedel-execution--record-stop-p record)
                           (timerp
                            (mevedel-execution--record-force-timer record)))
                      (timerp
                       (mevedel-execution--record-settle-timer record)))
            (setf (mevedel-execution--record-settle-timer record)
                  (run-at-time
                   0.02 nil
                   #'mevedel-execution--settle-managed-main-exit record)))
        (unless (or (and (mevedel-execution--record-stop-p record)
                         (timerp
                          (mevedel-execution--record-force-timer record)))
                    (timerp
                     (mevedel-execution--record-settle-timer record)))
          (setf (mevedel-execution--record-settle-timer record)
                (run-at-time
                 0.02 nil #'mevedel-execution--finish-record record
                 (mevedel-execution--record-exit-code record))))))))

(defun mevedel-execution--launch-record
    (record name command workdir coding filter)
  "Launch COMMAND into RECORD using NAME, WORKDIR, CODING, and FILTER."
  (let* ((executable (car-safe command))
         (_resolved
          (or (and (stringp executable) (executable-find executable))
              (signal 'file-missing
                      (list "Executable not found" executable))))
         (process-environment (mevedel-execution--process-environment))
         (default-directory workdir))
    (setf (mevedel-execution--record-process record)
          (make-process
           :name name :buffer nil :command command
           :coding coding
           :connection-type
           (if (mevedel-execution--record-tty-p record) 'pty 'pipe)
           :filter (lambda (_process chunk) (funcall filter record chunk))
           :noquery t
           :sentinel (lambda (process _event)
                       (mevedel-execution--process-ended record process))))
    (setf (mevedel-execution--record-group-id record)
          (process-id (mevedel-execution--record-process record)))
    (unless (mevedel-execution--record-tty-p record)
      (process-send-eof (mevedel-execution--record-process record)))
    ;; A terminal status can be visible while sentinels are inhibited.
    (setf (mevedel-execution--record-watch-timer record)
          (run-at-time
           0.1 0.1
           (lambda ()
             (unless (mevedel-execution--record-finished-p record)
               (let ((process (mevedel-execution--record-process record)))
                 (when (and (processp process)
                            (memq (process-status process) '(exit signal)))
                   (mevedel-execution--process-ended record process)))))))
    (mevedel-execution--record-process record)))

(defun mevedel-execution--start-process
    (callback name command workdir timeout session owner teardown-function)
  "Start raw COMMAND and call CALLBACK with its bounded terminal result."
  (let* ((record
          (mevedel-execution--record-create
           :callback callback
           :origin (mevedel-execution--origin-create
                    :owner (or owner "/root") :session session)
           :spool-path (make-temp-file "mevedel-execution-output-")
           :started-at (float-time)
           :teardown-function teardown-function
           :token (gensym "execution-process-")))
         (state (mevedel-execution--state-for-session session)))
    (puthash (mevedel-execution--record-token record) record
             (mevedel-execution--state-records state))
    (condition-case err
        (progn
          (mevedel-execution--launch-record
           record name command workdir 'no-conversion
           #'mevedel-execution--append-output)
          (when timeout
            (setf (mevedel-execution--record-timeout-timer record)
                  (run-at-time timeout nil
                               #'mevedel-execution--time-out record)))
          (mevedel-execution--record-process record))
      (error
       (mevedel-execution--finish-record record -1 err)
       nil))))


;;
;;; Managed Bash interface

(defun mevedel-execution--record-output-bytes (record)
  "Return the current spool size for RECORD."
  (let ((path (mevedel-execution--record-spool-path record)))
    (or (and (file-readable-p path)
             (file-attribute-size (file-attributes path)))
        0)))

(defun mevedel-execution--managed-count (state)
  "Return the number of live managed records in STATE."
  (let ((count 0))
    (maphash
     (lambda (_key record)
       (when (mevedel-execution--managed-live-p record)
         (setq count (1+ count))))
     (mevedel-execution--state-records state))
    count))

(defun mevedel-execution--telemetry-facts (facts)
  "Return the non-sensitive confinement subset of sandbox FACTS."
  (list :sandbox (plist-get facts :sandbox)
        :filesystem (plist-get facts :filesystem)
        :network (plist-get facts :network)
        :proc (plist-get facts :proc)
        :protected-path-count (plist-get facts :protected-paths)
        :additional-read-count (plist-get facts :additional-filesystem-read)
        :additional-write-count (plist-get facts :additional-filesystem-write)))

(defun mevedel-execution--eask-command-p (command)
  "Return non-nil when COMMAND invokes Eask directly or through npx."
  (and (stringp command)
       (string-match-p
        (concat
         "\\(?:^\\|[;&|][[:space:]]*\\)"
         "\\(?:[[:alpha:]_][[:alnum:]_]*=[^[:space:]]+[[:space:]]+\\)*"
         "\\(?:npx[ ]+@emacs-eask/cli\\|eask\\)\\(?:[ ]\\|$\\)")
        command)))

(defun mevedel-execution--eask-targets (command)
  "Return bounded test file targets named by Eask COMMAND.
Only repository-relative Emacs Lisp test paths are retained; the rest of the
command remains represented solely by its hash."
  (let ((start 0)
        targets)
    (while (and (< (length targets) 16)
                (string-match "test/[[:alnum:]_./*-]+\\.el" command start))
      (push (match-string 0 command) targets)
      (setq start (match-end 0)))
    (nreverse (delete-dups targets))))

(defun mevedel-execution--cache-identity ()
  "Return a hash identifying the parent package-cache environment."
  (secure-hash
   'sha256
   (mapconcat (lambda (name) (or (getenv name) ""))
              '("HOME" "XDG_CACHE_HOME" "XDG_CONFIG_HOME" "EASK_HOME")
              "\0")))

(defun mevedel-execution--full-eask-command-p (command)
  "Return non-nil when COMMAND appears to run the full Eask ERT suite."
  (and (mevedel-execution--eask-command-p command)
       (string-match-p "\\btest[ ]+ert\\b" command)
       (null (mevedel-execution--eask-targets command))))

(defun mevedel-execution--resource-capture (session command)
  "Return native resource-capture preparation for SESSION and COMMAND.
The result is a plist containing a wrapped command and report path.  Capture
is enabled only for one full Eask suite during an active telemetry profiler
run."
  (when-let* ((directory
               (and (fboundp 'mevedel-telemetry-profiler-directory)
                    (mevedel-telemetry-profiler-directory session)))
              ((mevedel-execution--full-eask-command-p command))
              (time-program (executable-find "time"))
              ((equal (file-truename time-program) "/usr/bin/time"))
              (report (file-name-concat directory "full-suite-time.txt"))
              ((not (or (file-exists-p report)
                        (gethash report
                                 mevedel-execution--resource-capture-claims)))))
    (make-directory directory t)
    (puthash report t mevedel-execution--resource-capture-claims)
    (list :command
          (mapconcat
           #'identity
           (list (shell-quote-argument time-program) "-v" "-o"
                 (shell-quote-argument report) "--"
                 (shell-quote-argument (or shell-file-name "/bin/sh"))
                 (shell-quote-argument shell-command-switch)
                 (shell-quote-argument command))
           " ")
          :report report)))

(defun mevedel-execution--telemetry (record event &rest props)
  "Record safe execution EVENT and PROPS for RECORD."
  (let* ((origin (mevedel-execution--record-origin record))
         (session (and (mevedel-execution--origin-p origin)
                       (mevedel-execution--origin-session origin))))
    (when (and session (fboundp 'mevedel-telemetry-record))
      (apply #'mevedel-telemetry-record
             session event
             :execution-id (mevedel-execution--record-execution-id record)
             :tool-use-id (mevedel-execution--origin-tool-use-id origin)
             :owner (mevedel-execution--origin-owner origin)
             props))))

(defun mevedel-execution--next-id (state)
  "Return the next opaque execution id in STATE."
  (let ((next (1+ (mevedel-execution--state-next-id state))))
    (setf (mevedel-execution--state-next-id state) next)
    (format "exec-%06d" next)))

(defun mevedel-execution--utf8-prefix (text maximum-bytes)
  "Return the longest prefix of TEXT no larger than MAXIMUM-BYTES in UTF-8."
  (if (<= (string-bytes text) maximum-bytes)
      text
    (let ((low 0)
          (high (length text)))
      (while (< low high)
        (let ((middle (/ (+ low high 1) 2)))
          (if (<= (string-bytes (substring text 0 middle)) maximum-bytes)
              (setq low middle)
            (setq high (1- middle)))))
      (substring text 0 low))))

(defun mevedel-execution--retain-output (record text)
  "Retain bounded whole and unread preview TEXT in RECORD."
  (cl-labels
      ((next (chars head tail)
         (let* ((limit mevedel-execution-inline-output-limit)
                (head (concat (or head "") text))
                (tail (concat (or tail "") text)))
           (list (+ (or chars 0) (length text))
                 (substring head 0 (min limit (length head)))
                 (substring tail (max 0 (- (length tail) limit)))))))
    (pcase-let ((`(,chars ,head ,tail)
                 (next (mevedel-execution--record-output-chars record)
                       (mevedel-execution--record-output-head record)
                       (mevedel-execution--record-output-tail record))))
      (setf (mevedel-execution--record-output-chars record) chars
            (mevedel-execution--record-output-head record) head
            (mevedel-execution--record-output-tail record) tail))
    (pcase-let ((`(,chars ,head ,tail)
                 (next (mevedel-execution--record-unread-chars record)
                       (mevedel-execution--record-unread-head record)
                       (mevedel-execution--record-unread-tail record))))
      (setf (mevedel-execution--record-unread-chars record) chars
            (mevedel-execution--record-unread-head record) head
            (mevedel-execution--record-unread-tail record) tail))))

(defun mevedel-execution--write-managed-output (record text)
  "Write a UTF-8-safe bounded prefix of TEXT and update RECORD."
  (let* ((path (mevedel-execution--record-spool-path record))
         (current (mevedel-execution--record-output-bytes record))
         (remaining (max 0 (- mevedel-execution-output-limit current)))
         (written-text (mevedel-execution--utf8-prefix text remaining)))
    (unless (string-empty-p written-text)
      (when (zerop current)
        (mevedel-execution--telemetry
         record 'execution-first-output
         :chunk-bytes (string-bytes written-text)))
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region written-text nil path t 'silent))
      (mevedel-execution--retain-output record written-text)
      (cl-incf (mevedel-execution--record-newline-count record)
               (cl-count ?\n written-text))
      (setf (mevedel-execution--record-last-byte-newline-p record)
            (eq (aref written-text (1- (length written-text))) ?\n)))
    (when (< (length written-text) (length text))
      (setf (mevedel-execution--record-output-limit-p record) t)
      (mevedel-execution--begin-stop record 'output-limit))))

(defun mevedel-execution--managed-append (record chunk)
  "Remove RECORD's private sandbox marker and spool CHUNK."
  (unless (mevedel-execution--record-finished-p record)
    (let ((marker (mevedel-execution--record-marker record)))
      (when (and marker
                 (not (eq (mevedel-execution--record-marker-buffer record)
                          :done)))
        (setq chunk
              (concat (or (mevedel-execution--record-marker-buffer record) "")
                      chunk))
        (if-let* ((newline (string-search "\n" chunk)))
            (let ((first (substring chunk 0 newline)))
              (setf (mevedel-execution--record-marker-buffer record) :done)
              (when (equal first marker)
                (setf (mevedel-execution--record-marker-seen-p record) t)
                (setq chunk (substring chunk (1+ newline)))))
          (setf (mevedel-execution--record-marker-buffer record) chunk)
          (setq chunk "")))
      (unless (string-empty-p chunk)
        (mevedel-execution--write-managed-output record chunk)))))

(defun mevedel-execution--managed-lines (record)
  "Return the retained logical line count for RECORD."
  (let ((bytes (mevedel-execution--record-output-bytes record))
        (newlines (or (mevedel-execution--record-newline-count record) 0)))
    (+ newlines
       (if (and (> bytes 0)
                (not (mevedel-execution--record-last-byte-newline-p record)))
           1
         0))))

(defun mevedel-execution--termination (record)
  "Return RECORD's canonical termination symbol, or nil while running."
  (when (mevedel-execution--record-finished-p record)
    (or (mevedel-execution--record-termination record)
        (and (mevedel-execution--record-error-data record) 'spawn-failed)
        'exited)))

(defun mevedel-execution--resolve-outcome
    (outcome-function exit-code termination)
  "Resolve a canonical outcome without risking execution settlement.

OUTCOME-FUNCTION may interpret EXIT-CODE and TERMINATION.  Without one,
preserve the default zero-success/nonzero-failure rule."
  (let ((default-outcome
         (if (and (integerp exit-code) (zerop exit-code))
             'success
           'failure)))
    (if (null outcome-function)
        default-outcome
      (condition-case err
          (let ((outcome (funcall outcome-function exit-code termination)))
            (unless (memq outcome '(success failure no-match different false))
              (error "Invalid execution outcome: %S" outcome))
            outcome)
        (error
         (display-warning
          'mevedel
          (format "Execution outcome resolver failed: %s"
                  (error-message-string err))
          :warning)
         default-outcome)))))

(defun mevedel-execution--lifecycle-state (record)
  "Return RECORD's canonical public lifecycle state."
  (cond
   ((mevedel-execution--record-finished-p record) 'completed)
   ((mevedel-execution--record-stop-p record) 'stopping)
   ((processp (mevedel-execution--record-process record)) 'running)
   (t 'queued)))

(defun mevedel-execution--facts (record)
  "Return an immutable public fact snapshot for RECORD."
  (let* ((finished (mevedel-execution--record-finished-p record))
         (origin (mevedel-execution--record-origin record))
         (exit-code (and finished
                         (mevedel-execution--record-exit-code record)))
         (termination (mevedel-execution--termination record))
         (outcome-function
          (mevedel-execution--record-outcome-function record)))
    (list :execution-id
          (and (mevedel-execution--record-yielded-p record)
               (mevedel-execution--record-execution-id record))
          :command
          (plist-get (mevedel-execution--origin-tool-args origin) :command)
          :state (mevedel-execution--lifecycle-state record)
          :termination termination
          :exit-code exit-code
          :outcome (and finished
                        (mevedel-execution--resolve-outcome
                         outcome-function exit-code termination))
          :wall-time-seconds
          (- (float-time) (mevedel-execution--record-started-at record))
          :output-bytes (mevedel-execution--record-output-bytes record)
          :output-lines (mevedel-execution--managed-lines record)
          :omitted-output-bytes
          (or (mevedel-execution--record-omitted-output-bytes record) 0)
          :tty (and (mevedel-execution--record-tty-p record) t)
          :output-path
          (and (mevedel-execution--record-retained-p record)
               (mevedel-execution--record-spool-path record)))))

(defun mevedel-execution--event (record type &rest properties)
  "Return an immutable TYPE event for RECORD with PROPERTIES."
  (append
   (list :type type
         :emitted-at (float-time)
         :session (mevedel-execution--origin-session
                   (mevedel-execution--record-origin record))
         :data-buffer (mevedel-execution--origin-data-buffer
                       (mevedel-execution--record-origin record))
         :owner (mevedel-execution--origin-owner
                 (mevedel-execution--record-origin record))
         :tool-args
         (copy-tree
          (mevedel-execution--origin-tool-args
           (mevedel-execution--record-origin record)))
         :tool-use-id (mevedel-execution--origin-tool-use-id
                       (mevedel-execution--record-origin record))
         :timeout-seconds (mevedel-execution--record-timeout record)
         :facts (mevedel-execution--facts record))
   properties))

(defun mevedel-execution--copy-event (event)
  "Return an isolated copy of EVENT, including mutable string leaves."
  (cl-labels
      ((copy-value (value)
         (cond
          ((stringp value) (copy-sequence value))
          ((consp value)
           (cons (copy-value (car value))
                 (copy-value (cdr value))))
          (t value))))
    (copy-value event)))

(defun mevedel-execution--emit-event (event)
  "Publish EVENT to passive consumers, ignoring their return values."
  (dolist (function mevedel-execution-event-functions)
    (when (functionp function)
      (condition-case err
          (funcall function (mevedel-execution--copy-event event))
        (error
         (display-warning
          'mevedel
          (format "Execution event consumer failed: %s"
                  (error-message-string err))
          :warning))))))

(defun mevedel-execution--deliver-mailbox-event (record event)
  "Return non-nil after the mailbox sink secures RECORD's EVENT."
  (when (functionp mevedel-execution-mailbox-delivery-function)
    (condition-case err
        (funcall mevedel-execution-mailbox-delivery-function
                 (mevedel-execution--copy-event event)
                 (mevedel-execution--origin-owner-context
                  (mevedel-execution--record-origin record)))
      (error
       (display-warning
        'mevedel
        (format "Execution mailbox delivery failed: %s"
                (error-message-string err))
        :warning)
       nil))))

(defun mevedel-execution--whole-preview (record)
  "Return RECORD's bounded whole-artifact head-and-tail preview."
  (require 'mevedel-utilities)
  (plist-get
   (mevedel--head-tail-preview-parts
    (or (mevedel-execution--record-output-head record) "")
    (or (mevedel-execution--record-output-tail record) "")
    (or (mevedel-execution--record-output-chars record) 0)
    mevedel-execution-inline-output-limit)
   :text))

(defun mevedel-execution--emit-progress (record)
  "Publish one bounded progress event for live RECORD."
  (unless (mevedel-execution--record-finished-p record)
    (mevedel-execution--emit-event
     (mevedel-execution--event
      record 'progress
      :output-tail (or (mevedel-execution--record-output-tail record) "")))))

(defun mevedel-execution--unread-preview (record unread-bytes)
  "Return RECORD's bounded unread preview for UNREAD-BYTES."
  (require 'mevedel-utilities)
  (let* ((parts
          (mevedel--head-tail-preview-parts
           (or (mevedel-execution--record-unread-head record) "")
           (or (mevedel-execution--record-unread-tail record) "")
           (or (mevedel-execution--record-unread-chars record) 0)
           mevedel-execution-inline-output-limit))
         (visible-bytes
          (+ (string-bytes (plist-get parts :head))
             (string-bytes (plist-get parts :tail)))))
    (list :output (plist-get parts :text)
          :omitted (max 0 (- unread-bytes visible-bytes)))))

(defun mevedel-execution--unread-range (record)
  "Return RECORD's next unread range without consuming it."
  (let* ((start (or (mevedel-execution--record-read-offset record) 0))
         (end (mevedel-execution--record-output-bytes record))
         (preview (mevedel-execution--unread-preview record (- end start))))
    (list :end end
          :omitted (plist-get preview :omitted)
          :output (plist-get preview :output))))

(defun mevedel-execution--consume-unread-range (record range)
  "Commit consumption of RECORD's unread RANGE."
  (let ((omitted (plist-get range :omitted)))
    (setf (mevedel-execution--record-read-offset record)
          (plist-get range :end)
          (mevedel-execution--record-unread-chars record) 0
          (mevedel-execution--record-unread-head record) ""
          (mevedel-execution--record-unread-tail record) ""
          (mevedel-execution--record-omitted-output-bytes record)
          (+ (or (mevedel-execution--record-omitted-output-bytes record) 0)
             omitted))
    (when (> omitted 0)
      (setf (mevedel-execution--record-retained-p record) t))))

(defun mevedel-execution--range-observation
    (record range &optional claimed project-unconsumed)
  "Return an observation of RECORD and unread RANGE.
CLAIMED non-nil marks it as the final model delivery.
PROJECT-UNCONSUMED includes RANGE's omission in facts before commitment."
  (let ((facts (mevedel-execution--facts record)))
    (when project-unconsumed
      (setq facts
            (plist-put
             facts :omitted-output-bytes
             (+ (or (plist-get facts :omitted-output-bytes) 0)
                (plist-get range :omitted)))))
    (list :output (plist-get range :output)
          :facts facts
          :sandbox-facts (mevedel-execution--record-sandbox-facts record)
          :error (mevedel-execution--record-error-data record)
          :claimed-final-p (and claimed t))))

(defun mevedel-execution--terminal-event (record delivery observation)
  "Return RECORD's terminal event for DELIVERY and OBSERVATION."
  (let ((event
         (mevedel-execution--event
          record 'terminal
          :delivery delivery
          :observation observation
          :whole-output (mevedel-execution--whole-preview record))))
    (plist-put event :facts (plist-get observation :facts))))

(defun mevedel-execution--observation (record &optional claim-final)
  "Return RECORD's next unread observation.

When CLAIM-FINAL is non-nil, mark this observation as the single final model
delivery and retire the private handle while preserving retained artifacts."
  (when (and claim-final
             (mevedel-execution--record-delivery-state record))
    (signal 'mevedel-execution-not-found
            (list "Execution terminal result is already claimed")))
  (when claim-final
    (setf (mevedel-execution--record-delivery-state record) 'model))
  (let ((range (mevedel-execution--unread-range record)))
    (mevedel-execution--consume-unread-range record range)
    (let ((observation
           (mevedel-execution--range-observation record range claim-final)))
      (when claim-final
        (mevedel-execution--emit-event
         (mevedel-execution--terminal-event record 'model observation))
        (mevedel-execution--cleanup-record
         record (mevedel-execution--record-retained-p record)))
      observation)))

(defun mevedel-execution--deliver-independent (record)
  "Secure RECORD's unread terminal result in its owner mailbox."
  (unless (mevedel-execution--record-delivery-state record)
    (setf (mevedel-execution--record-delivery-state record) 'mailbox)
    (let* ((range (mevedel-execution--unread-range record))
           (observation
            (mevedel-execution--range-observation record range t t))
           (event
            (mevedel-execution--terminal-event record 'mailbox observation)))
      (let ((delivered-p
             (mevedel-execution--deliver-mailbox-event record event)))
        (mevedel-execution--emit-event event)
        (if delivered-p
          (progn
            (mevedel-execution--consume-unread-range record range)
            (mevedel-execution--cleanup-record
             record (mevedel-execution--record-retained-p record))
            t)
          (display-warning
           'mevedel
           (format "Execution %s completion has no mailbox consumer"
                   (mevedel-execution--record-execution-id record))
           :warning)
          (setf (mevedel-execution--record-delivery-state record) nil)
          nil)))))

(defun mevedel-execution--deliver-observer (record)
  "Deliver RECORD's terminal observation to its waiting observer."
  (when-let* ((callback (mevedel-execution--record-observer record)))
    (setf (mevedel-execution--record-observer record) nil
          (mevedel-execution--record-observer-timer record) nil)
    (funcall callback (mevedel-execution--observation record t))))

(defun mevedel-execution--cancel-observer (record callback)
  "Detach CALLBACK from RECORD without consuming unread output."
  (when (eq callback (mevedel-execution--record-observer record))
    (when-let* ((timer (mevedel-execution--record-observer-timer record)))
      (cancel-timer timer))
    (setf (mevedel-execution--record-observer record) nil
          (mevedel-execution--record-observer-timer record) nil)))

(defun mevedel-execution--flush-observer (record)
  "Complete RECORD's pending poll before a control operation takes over."
  (when-let* ((callback (mevedel-execution--record-observer record)))
    (when-let* ((timer (mevedel-execution--record-observer-timer record)))
      (cancel-timer timer))
    (setf (mevedel-execution--record-observer record) nil
          (mevedel-execution--record-observer-timer record) nil)
    (condition-case err
        (funcall callback (mevedel-execution--observation record))
      (error
       (display-warning
        'mevedel
        (format "Execution poll callback failed: %s"
                (error-message-string err))
        :warning)))))

(defun mevedel-execution--poll-expired (record)
  "Deliver a running observation when RECORD's poll wait expires."
  (when-let* ((callback (mevedel-execution--record-observer record)))
    (setf (mevedel-execution--record-observer record) nil
          (mevedel-execution--record-observer-timer record) nil)
    (funcall callback (mevedel-execution--observation record))))

(defun mevedel-execution--sandbox-launch-result (record)
  "Return the terminal child-result subset used for sandbox launch checks."
  (list :exit-code (mevedel-execution--record-exit-code record)
        :output (mevedel-execution--read-output record)
        :timed-out-p (mevedel-execution--record-timed-out-p record)
        :error (mevedel-execution--record-error-data record)))

(defun mevedel-execution--restart-unconfined (record facts)
  "Restart RECORD's original command without confinement using FACTS."
  (let ((preparation (mevedel-execution--record-sandbox-preparation record)))
    (apply #'mevedel-execution--telemetry
           record 'sandbox-fallback
           :launch-failure-stage 'before-command-start
           :launch-failure-reason-class 'sandbox-launch-failure
           :fallback-offered t
           :full-execution-approval-offered nil
           (mevedel-execution--telemetry-facts facts))
    (apply #'mevedel-execution--telemetry
           record 'execution-unrestricted
           :reason-class 'sandbox-launch-failure
           :after-confined-launch-failure t
           (mevedel-execution--telemetry-facts facts))
    (mevedel-execution--release-runtime record)
    (mevedel-sandbox-cleanup preparation)
    (let ((coding-system-for-write 'no-conversion))
      (write-region "" nil (mevedel-execution--record-spool-path record)
                    nil 'silent))
    (setf (mevedel-execution--record-exit-code record) nil
          (mevedel-execution--record-error-data record) nil
          (mevedel-execution--record-marker record) nil
          (mevedel-execution--record-marker-buffer record) :done
          (mevedel-execution--record-last-byte-newline-p record) nil
          (mevedel-execution--record-newline-count record) 0
          (mevedel-execution--record-output-chars record) 0
          (mevedel-execution--record-output-head record) ""
          (mevedel-execution--record-output-tail record) ""
          (mevedel-execution--record-read-offset record) 0
          (mevedel-execution--record-sandbox-facts record) facts
          (mevedel-execution--record-sandbox-preparation record) nil
          (mevedel-execution--record-unread-chars record) 0
          (mevedel-execution--record-unread-head record) ""
          (mevedel-execution--record-unread-tail record) "")
    (when-let* ((session
                 (mevedel-execution--origin-session
                  (mevedel-execution--record-origin record))))
      (let ((active-token (gensym "sandbox-bash-fallback-")))
        (setf (mevedel-execution--record-sandbox-active-token record)
              active-token)
        (mevedel-sandbox-track-active session active-token facts)))
    (mevedel-execution--launch-managed
     record (plist-get preparation :original-command))
    (mevedel-execution--arm-managed-timers record)))

(defun mevedel-execution--release-scheduler (record)
  "Release RECORD's scheduler lease exactly once."
  (when-let* ((lease (mevedel-execution--record-scheduler-lease record)))
    (setf (mevedel-execution--record-scheduler-lease record) nil)
    (mevedel-execution-scheduler-release lease)))

(defun mevedel-execution--finish-managed (record)
  "Settle managed RECORD without delivering unsolicited model output."
  (unless (mevedel-execution--record-finished-p record)
    (let* ((preparation
            (mevedel-execution--record-sandbox-preparation record))
           (child-result (and (eq (plist-get preparation :state) 'confined)
                              (not (mevedel-execution--record-marker-seen-p
                                    record))
                              (mevedel-execution--sandbox-launch-result record)))
           (launch-failed
            (and child-result
                 (mevedel-sandbox-launch-failed-p preparation child-result))))
      (if (and launch-failed
               (plist-get preparation :fallback-p)
               (not (mevedel-execution--record-yielded-p record)))
          (mevedel-execution--restart-unconfined
           record (mevedel-sandbox--record-launch-failure child-result))
        (when launch-failed
          (setf (mevedel-execution--record-sandbox-facts record)
                (mevedel-sandbox--record-launch-failure child-result)))
        (when preparation
          (mevedel-sandbox-cleanup preparation))
        (setf (mevedel-execution--record-finished-p record) t)
        (apply #'mevedel-execution--telemetry
               record 'execution-finished
               :exit-code (mevedel-execution--record-exit-code record)
               :termination (mevedel-execution--termination record)
               :duration-ms
               (round (* 1000.0
                         (- (float-time)
                            (mevedel-execution--record-started-at record))))
               :output-bytes (mevedel-execution--record-output-bytes record)
               :output-limit (and (mevedel-execution--record-output-limit-p
                                   record)
                                  t)
               :timed-out (and (mevedel-execution--record-timed-out-p record)
                               t)
               :native-resource-report-bytes
               (when-let* ((report
                            (mevedel-execution--record-resource-report-path
                             record))
                           ((file-readable-p report)))
                 (file-attribute-size (file-attributes report)))
               (mevedel-execution--telemetry-facts
                (mevedel-execution--record-sandbox-facts record)))
        (mevedel-execution--notify-state-change record)
        (mevedel-execution--release-runtime record)
        (mevedel-execution--release-scheduler record)
        (if (mevedel-execution--record-yielded-p record)
            (if (mevedel-execution--record-observer record)
                (mevedel-execution--deliver-observer record)
              (when mevedel-execution-mailbox-delivery-function
                (mevedel-execution--deliver-independent record)))
          (let ((callback (mevedel-execution--record-callback record)))
            (funcall callback
                     (mevedel-execution--observation record t))))))))

(defun mevedel-execution--yield-managed (record)
  "Deliver RECORD's initial running observation and detach it from request."
  (unless (or (mevedel-execution--record-finished-p record)
              (mevedel-execution--record-yielded-p record))
    (setf (mevedel-execution--record-yielded-p record) t
          (mevedel-execution--record-retained-p record) t)
    (mevedel-execution--release-scheduler record)
    (mevedel-execution--emit-event
     (mevedel-execution--event record 'yield))
    (funcall (mevedel-execution--record-callback record)
             (mevedel-execution--observation record))))

(defun mevedel-execution--arm-managed-timers (record)
  "Arm RECORD's yield and optional timeout clocks."
  (when (and mevedel-execution-event-functions
             (mevedel-execution--origin-tool-use-id
              (mevedel-execution--record-origin record)))
    (setf (mevedel-execution--record-progress-timer record)
          (run-at-time
           (max 0 mevedel-execution-progress-delay)
           (max 0.25 mevedel-execution-progress-interval)
           #'mevedel-execution--emit-progress record)))
  (when-let* ((yield-time-ms
               (mevedel-execution--record-yield-time-ms record)))
    (setf (mevedel-execution--record-yield-timer record)
          (run-at-time
           (/ yield-time-ms 1000.0)
           nil #'mevedel-execution--yield-managed record)))
  (when-let* ((timeout (mevedel-execution--record-timeout record)))
    (setf (mevedel-execution--record-timeout-timer record)
          (run-at-time timeout nil #'mevedel-execution--time-out record))))

(defun mevedel-execution--launch-managed (record command)
  "Launch managed RECORD with raw COMMAND."
  (condition-case err
      (progn
        (mevedel-execution--launch-record
         record "mevedel-bash" command
         (mevedel-execution--record-workdir record)
         'utf-8-unix #'mevedel-execution--managed-append)
        (when (process-live-p (mevedel-execution--record-process record))
          (apply #'mevedel-execution--telemetry
                 record 'execution-started
                 (mevedel-execution--telemetry-facts
                  (mevedel-execution--record-sandbox-facts record)))))
    (error
     (setf (mevedel-execution--record-error-data record) err
           (mevedel-execution--record-exit-code record) -1
           (mevedel-execution--record-termination record) 'spawn-failed)
     (mevedel-execution--finish-managed record))))

(defun mevedel-execution--start-admitted (record preparation)
  "Start scheduler-admitted RECORD from PREPARATION."
  (apply #'mevedel-execution--telemetry
         record 'execution-admitted
         :queue-duration-ms
         (round (* 1000.0
                   (- (float-time)
                      (mevedel-execution--record-started-at record))))
         :preparation-state (plist-get preparation :state)
         :fallback-possible (and (plist-get preparation :fallback-p) t)
         (mevedel-execution--telemetry-facts
          (plist-get preparation :facts)))
  (when (eq (plist-get preparation :state) 'unrestricted)
    (apply #'mevedel-execution--telemetry
           record 'execution-unrestricted
           :reason-class (plist-get (plist-get preparation :facts) :sandbox)
           :after-confined-launch-failure nil
           (mevedel-execution--telemetry-facts
            (plist-get preparation :facts))))
  (pcase (plist-get preparation :state)
    ('refused
     (setf (mevedel-execution--record-error-data record)
           (list 'error (plist-get preparation :error))
           (mevedel-execution--record-exit-code record) -1
           (mevedel-execution--record-sandbox-facts record)
           (plist-get preparation :facts)
           (mevedel-execution--record-termination record) 'spawn-failed)
     (mevedel-execution--finish-managed record))
    ((or 'unrestricted 'confined)
     (let* ((session
             (mevedel-execution--origin-session
              (mevedel-execution--record-origin record)))
            (active-token (and session (gensym "sandbox-bash-"))))
       (setf (mevedel-execution--record-marker record)
             (plist-get preparation :marker)
             (mevedel-execution--record-marker-buffer record)
             (unless (plist-get preparation :marker) :done)
             (mevedel-execution--record-sandbox-active-token record)
             active-token
             (mevedel-execution--record-sandbox-facts record)
             (plist-get preparation :facts)
             (mevedel-execution--record-sandbox-preparation record)
             preparation)
       (when active-token
         (mevedel-sandbox-track-active
          session active-token (plist-get preparation :facts)))
       (mevedel-execution--launch-managed
        record (plist-get preparation :command))
       (unless (mevedel-execution--record-finished-p record)
         (mevedel-execution--arm-managed-timers record))))
    (_
     (mevedel-execution--cleanup-record record)
     (error "Unknown sandbox preparation state: %s"
            (plist-get preparation :state)))))

(defun mevedel-execution--abort-request-record (record)
  "Abort queued or foreground RECORD for its originating request."
  (unless (or (mevedel-execution--record-finished-p record)
              (mevedel-execution--record-yielded-p record))
    (if (mevedel-execution-scheduler-cancel
         (mevedel-execution--record-scheduler-lease record))
        (progn
          (setf (mevedel-execution--record-exit-code record) -1
                (mevedel-execution--record-termination record) 'aborted)
          (mevedel-execution--finish-managed record))
      (mevedel-execution--begin-stop record 'aborted))))

(cl-defun mevedel-execution-start-bash
    (callback &key session data-buffer owner owner-context request
              command workdir
              writable-roots timeout
              additional-permissions sandbox-permissions artifact-directory
              outcome-function read-only-p tool-args tool-use-id tty
              (yield-time-ms 10000))
  "Start managed Bash COMMAND and call CALLBACK at terminal or yield.

SESSION, canonical OWNER, and OWNER-CONTEXT fix the control boundary.
OWNER-CONTEXT is the durable mailbox object captured at spawn.
REQUEST owns the foreground lifetime only.  Remaining confinement arguments
match the one-shot interface.  ARTIFACT-DIRECTORY owns the spool after yield.
OUTCOME-FUNCTION derives canonical outcome from exit code and termination.
READ-ONLY-P selects the overlapping reader lane; all other calls are exclusive.
DATA-BUFFER and TOOL-USE-ID identify the authoritative transcript row.
TOOL-ARGS correlates immutable events with the original call.
TTY non-nil explicitly allocates a terminal and retains writable stdin.
YIELD-TIME-MS may be nil only for trusted internal callers that must wait for
terminal settlement."
  (require 'mevedel-sandbox)
  (unless session
    (signal 'mevedel-execution-error
            (list "Managed Bash requires an active session")))
  (let* ((state (mevedel-execution--state-for-session session)))
    (when (>= (mevedel-execution--managed-count state)
              mevedel-execution-live-limit)
      (signal 'mevedel-execution-limit
              (list "A session may have at most 64 live Bash processes")))
    (let* ((resource-capture
            (and (stringp command)
                 (mevedel-execution--resource-capture session command)))
           (command (or (plist-get resource-capture :command) command))
           (id (mevedel-execution--next-id state))
           (artifact-directory
            (or artifact-directory temporary-file-directory))
           (_ (make-directory artifact-directory t))
           (record
            (mevedel-execution--record-create
             :callback callback :execution-id id
             :marker-buffer nil :newline-count 0
             :origin
             (mevedel-execution--origin-create
              :data-buffer data-buffer
              :owner (or owner "/root")
              :owner-context owner-context
              :session session
              :tool-args tool-args
              :tool-use-id tool-use-id)
             :outcome-function outcome-function
             :output-chars 0 :output-head "" :output-tail ""
             :read-offset 0
             :resource-report-path (plist-get resource-capture :report)
             :spool-path
             (make-temp-file
              (file-name-concat artifact-directory "execution-") nil ".log")
             :started-at (float-time) :timeout timeout :token id
             :tty-p (and tty t) :workdir workdir
             :yield-time-ms yield-time-ms)))
      (puthash id record (mevedel-execution--state-records state))
      (let* ((raw-command (plist-get tool-args :command))
             (command-text (and (stringp raw-command) raw-command)))
        (mevedel-execution--telemetry
         record 'execution-enqueued
         :lane (if read-only-p 'read 'exclusive)
         :queue-depth (mevedel-execution--managed-count state)
         :overlap-count (max 0 (1- (mevedel-execution--managed-count state)))
         :workload (and (mevedel-execution--eask-command-p command-text)
                        'eask)
         :test-targets (and (mevedel-execution--eask-command-p command-text)
                            (mevedel-execution--eask-targets command-text))
         :test-scope (and (mevedel-execution--eask-command-p command-text)
                          (if (mevedel-execution--eask-targets command-text)
                              'focused
                            'full))
         :cache-identity (and (mevedel-execution--eask-command-p command-text)
                              (mevedel-execution--cache-identity))
         :native-resource-capture (and resource-capture t)
         :resource-report-relative-path
         (and resource-capture
              (file-name-concat "diagnostics"
                                (file-name-nondirectory
                                 (directory-file-name
                                  (file-name-directory
                                   (plist-get resource-capture :report))))
                                "full-suite-time.txt"))
         :command-hash (and command-text
                            (secure-hash 'sha256 command-text))
         :tty (and tty t)
         :timeout-seconds timeout
         :yield-time-ms yield-time-ms))
      (mevedel-execution--notify-state-change record)
      (let ((lease
             (mevedel-execution-scheduler-submit
              (mevedel-execution--state-scheduler state)
              (if read-only-p 'read 'exclusive)
              (lambda (admitted-lease)
                (setf (mevedel-execution--record-scheduler-lease record)
                      admitted-lease)
                (condition-case err
                    (mevedel-execution--start-admitted
                     record
                     (mevedel-sandbox-prepare
                      command workdir writable-roots additional-permissions
                      sandbox-permissions))
                  (error
                   (setf (mevedel-execution--record-error-data record) err
                         (mevedel-execution--record-exit-code record) -1
                         (mevedel-execution--record-termination record)
                         'spawn-failed)
                   (mevedel-execution--finish-managed record)))))))
        (unless (or (mevedel-execution--record-finished-p record)
                    (mevedel-execution--record-scheduler-lease record))
          (setf (mevedel-execution--record-scheduler-lease record) lease)))
      (when (and request
                 (not (mevedel-execution--record-finished-p record))
                 (not (mevedel-execution--record-yielded-p record)))
        (mevedel-request-push-canceller
         request
         (lambda () (mevedel-execution--abort-request-record record))))
      nil)))

(defun mevedel-execution--owned-yielded-record (session owner execution-id)
  "Return yielded EXECUTION-ID owned by OWNER in SESSION, or signal."
  (let* ((state (mevedel-execution--state-for-session session))
         (record (gethash execution-id
                          (mevedel-execution--state-records state))))
    (unless (and record
                 (mevedel-execution--record-execution-id record)
                 (mevedel-execution--record-yielded-p record)
                 (equal (mevedel-execution--origin-owner
                         (mevedel-execution--record-origin record))
                        (or owner "/root")))
      (signal 'mevedel-execution-not-found
              (list "No yielded execution with that id")))
    record))

(cl-defun mevedel-execution-observe
    (session owner execution-id callback &key chars (wait-ms 0) request)
  "Observe unread output from owner-scoped yielded EXECUTION-ID.

Ordinary non-empty CHARS require a PTY; a single Ctrl-C character interrupts
either process mode.  CALLBACK receives immediately for terminal state or
after WAIT-MS while the process remains live."
  (let* ((record
          (mevedel-execution--owned-yielded-record
           session owner execution-id))
         (input-p (and (stringp chars) (not (string-empty-p chars)))))
    (unless (or (null chars) (stringp chars))
      (signal 'mevedel-execution-input-error
              (list "Execution input must be a string")))
    (when (and input-p (mevedel-execution--record-observer record))
      (mevedel-execution--flush-observer record))
    (when (mevedel-execution--record-observer record)
      (signal 'mevedel-execution-error
              (list "An observation is already waiting")))
    (when input-p
      (cond
       ((equal chars (string 3))
        (unless (or (mevedel-execution--record-finished-p record)
                    (mevedel-execution--record-stop-p record)
                    (not (process-live-p
                          (mevedel-execution--record-process record))))
          (mevedel-execution--signal-record record 'INT)))
       ((not (mevedel-execution--record-tty-p record))
        (signal 'mevedel-execution-input-error
                (list "Pipe-mode Bash stdin is closed")))
       ((or (mevedel-execution--record-finished-p record)
            (mevedel-execution--record-stop-p record)
            (not (process-live-p
                  (mevedel-execution--record-process record))))
        (signal 'mevedel-execution-input-error
                (list "Execution is no longer running")))
       (t
        (condition-case nil
            (process-send-string
             (mevedel-execution--record-process record) chars)
          (error
           (signal 'mevedel-execution-input-error
                   (list "Execution is no longer running")))))))
    (cond
     ((mevedel-execution--record-finished-p record)
      (funcall callback (mevedel-execution--observation record t)))
     ((<= wait-ms 0)
      (funcall callback (mevedel-execution--observation record)))
     (t
      (setf (mevedel-execution--record-observer record) callback
            (mevedel-execution--record-observer-timer record)
            (run-at-time (/ wait-ms 1000.0) nil
                         #'mevedel-execution--poll-expired record))
      (when request
        (mevedel-request-push-canceller
         request
         (lambda ()
           (mevedel-execution--cancel-observer record callback))))))
    nil))

(defun mevedel-execution-list (session owner)
  "Return immutable fact snapshots for OWNER's yielded SESSION executions."
  (let ((state (mevedel-execution--state-for-session session))
        facts)
    (maphash
     (lambda (_key record)
       (when (and (mevedel-execution--record-execution-id record)
                  (mevedel-execution--record-yielded-p record)
                  (not (mevedel-execution--record-finished-p record))
                  (equal (mevedel-execution--origin-owner
                          (mevedel-execution--record-origin record))
                         (or owner "/root")))
         (push (mevedel-execution--facts record) facts)))
     (mevedel-execution--state-records state))
    (nreverse facts)))

(defun mevedel-execution--user-snapshot (record)
  "Return an immutable user-authority snapshot for live RECORD."
  (let* ((origin (mevedel-execution--record-origin record))
         (sandbox-facts (mevedel-execution--record-sandbox-facts record))
         (facts
          (plist-put
           (mevedel-execution--facts record)
           :execution-id
           (mevedel-execution--record-execution-id record))))
    (mevedel-execution--copy-event
     (append
      facts
      (list :owner (mevedel-execution--origin-owner origin)
            :yielded (and (mevedel-execution--record-yielded-p record) t)
            :started-at (mevedel-execution--record-started-at record)
            :timeout-seconds (mevedel-execution--record-timeout record)
            :output-tail
            (or (mevedel-execution--record-output-tail record) "")
            :artifact-path (mevedel-execution--record-spool-path record)
            :sandbox-state (or (plist-get sandbox-facts :sandbox) 'pending)
            :sandbox-facts sandbox-facts)))))

(defun mevedel-execution-list-user (session)
  "Return immutable snapshots for every live execution in SESSION."
  (when-let* ((state (and session
                          (mevedel-session-execution-state session))))
    (let (snapshots)
      (maphash
       (lambda (_key record)
         (when (mevedel-execution--managed-live-p record)
           (push (mevedel-execution--user-snapshot record) snapshots)))
       (mevedel-execution--state-records state))
      (sort snapshots
            (lambda (left right)
              (< (plist-get left :started-at)
                 (plist-get right :started-at)))))))

(defun mevedel-execution-count-user (session)
  "Return the number of live executions in SESSION."
  (if-let* ((state (and session
                        (mevedel-session-execution-state session))))
      (mevedel-execution--managed-count state)
    0))

(defun mevedel-execution--user-live-record (session execution-id)
  "Return live EXECUTION-ID in SESSION with user authority, or signal."
  (let* ((state (and session
                     (mevedel-session-execution-state session)))
         (record (and state
                      (gethash execution-id
                               (mevedel-execution--state-records state)))))
    (unless (and record
                 (not (mevedel-execution--record-finished-p record)))
      (signal 'mevedel-execution-not-found
              (list "No live execution with that id")))
    record))

(defun mevedel-execution-write-user (session execution-id chars)
  "Send CHARS to PTY EXECUTION-ID in SESSION with user authority."
  (unless (and (stringp chars) (not (string-empty-p chars)))
    (signal 'mevedel-execution-input-error
            (list "Execution input must be a non-empty string")))
  (let* ((record (mevedel-execution--user-live-record session execution-id))
         (process (mevedel-execution--record-process record)))
    (unless (mevedel-execution--record-tty-p record)
      (signal 'mevedel-execution-input-error
              (list "Pipe-mode Bash stdin is closed")))
    (unless (process-live-p process)
      (signal 'mevedel-execution-input-error
              (list "Execution is no longer running")))
    (process-send-string process chars)
    t))

(defun mevedel-execution-interrupt-user (session execution-id)
  "Interrupt live EXECUTION-ID in SESSION with user authority."
  (let* ((record
          (mevedel-execution--user-live-record session execution-id))
         (process (mevedel-execution--record-process record)))
    (unless process
      (signal 'mevedel-execution-input-error
              (list "Execution has not started")))
    (unless (or (process-live-p process)
                (mevedel-execution--group-live-p record))
      (signal 'mevedel-execution-not-found
              (list "Execution is no longer running")))
    (mevedel-execution--signal-record record 'INT)
    t))

(defun mevedel-execution--state-record-list (state)
  "Return a snapshot list of every private record in STATE."
  (let (records)
    (when state
      (maphash (lambda (_key record) (push record records))
               (mevedel-execution--state-records state)))
    records))

(defun mevedel-execution-owner-live-p (session owner)
  "Return non-nil when OWNER has an unsettled execution in SESSION."
  (when-let* ((state (and session
                          (mevedel-session-execution-state session))))
    (let (live-p)
      (dolist (record (mevedel-execution--state-record-list state))
         (when (and (not (mevedel-execution--record-delivery-state record))
                    (equal (mevedel-execution--origin-owner
                            (mevedel-execution--record-origin record))
                           owner))
           (setq live-p t)))
      live-p)))

(defun mevedel-execution-session-live-p (session)
  "Return non-nil when SESSION owns any unsettled child process."
  (when-let* ((state (and session
                          (mevedel-session-execution-state session))))
    (let (live-p)
      (dolist (record (mevedel-execution--state-record-list state))
         (unless (mevedel-execution--record-finished-p record)
           (setq live-p t)))
      live-p)))

(defun mevedel-execution--discard-record (record reason)
  "Immediately kill and forget RECORD because of lifecycle REASON."
  (let ((managed-live-p (mevedel-execution--managed-live-p record)))
    (unless (mevedel-execution--record-finished-p record)
      (setf (mevedel-execution--record-finished-p record) t
            (mevedel-execution--record-stop-p record) t
            (mevedel-execution--record-termination record) reason)
      (when-let* ((lease (mevedel-execution--record-scheduler-lease record)))
        (mevedel-execution-scheduler-cancel lease))
      (mevedel-execution--signal-record record 'KILL)
      (when-let* ((preparation
                  (mevedel-execution--record-sandbox-preparation record)))
        (mevedel-sandbox-cleanup preparation)))
    (when-let* ((function
                (mevedel-execution--record-teardown-function record)))
      (setf (mevedel-execution--record-teardown-function record) nil)
      (ignore-errors (funcall function)))
    (mevedel-execution--release-scheduler record)
    (mevedel-execution--cleanup-record record)
    (when managed-live-p
      (mevedel-execution--notify-state-change record))))

(defun mevedel-execution-stop-owner (session owner)
  "Discard every execution record belonging to OWNER in SESSION.
Return the number of executions selected."
  (let ((state (and session (mevedel-session-execution-state session)))
        records)
    (when state
      (dolist (record (mevedel-execution--state-record-list state))
         (when (equal owner
                      (mevedel-execution--origin-owner
                       (mevedel-execution--record-origin record)))
           (push record records)))
      (dolist (record records)
        (mevedel-execution--discard-record record 'owner-stopped)))
    (length records)))

(defun mevedel-execution-teardown-session (session)
  "Synchronously discard all child records owned by SESSION.
Return the number selected, including queued one-shot helpers."
  (let ((state (and session (mevedel-session-execution-state session)))
        records)
    (when state
      (setq records (mevedel-execution--state-record-list state))
      (dolist (record records)
        (mevedel-execution--discard-record record 'session-ended)))
    (length records)))

(defun mevedel-execution-teardown-all ()
  "Synchronously discard all session and orphan child records."
  (let ((count 0)
        sessions)
    (maphash (lambda (session _present) (push session sessions))
             mevedel-execution--sessions)
    (dolist (session sessions)
      (cl-incf count (mevedel-execution-teardown-session session)))
    (when mevedel-execution--orphan-state
      (let ((records
             (mevedel-execution--state-record-list
              mevedel-execution--orphan-state)))
        (dolist (record records)
          (mevedel-execution--discard-record record 'session-ended))
        (cl-incf count (length records))))
    count))

(defun mevedel-execution-relocate-artifacts (session old-root new-root)
  "Retarget SESSION execution artifacts moved from OLD-ROOT to NEW-ROOT.
Return the number of live or retained records updated."
  (let ((state (and session (mevedel-session-execution-state session)))
        (old-prefix (file-name-as-directory (expand-file-name old-root)))
        (new-prefix (file-name-as-directory (expand-file-name new-root)))
        (count 0))
    (when state
      (maphash
       (lambda (_key record)
         (when-let* ((path (mevedel-execution--record-spool-path record))
                     (expanded (expand-file-name path))
                     ((string-prefix-p old-prefix expanded)))
           (setf (mevedel-execution--record-spool-path record)
                 (concat new-prefix (substring expanded (length old-prefix))))
           (cl-incf count)))
       (mevedel-execution--state-records state)))
    count))

(defun mevedel-execution-stop (session owner execution-id callback)
  "Stop owner-scoped yielded EXECUTION-ID and call CALLBACK at settlement."
  (let ((record
         (mevedel-execution--owned-yielded-record
          session owner execution-id)))
    (if (mevedel-execution--record-finished-p record)
        (funcall callback (mevedel-execution--observation record t))
      (mevedel-execution--flush-observer record)
      (mevedel-execution-observe
       session owner execution-id callback :wait-ms 300000)
      (mevedel-execution--begin-stop record 'stopped))
    nil))

(defun mevedel-execution-stop-user (session execution-id)
  "Stop live EXECUTION-ID in SESSION with user delivery authority.

Unlike the owner-scoped model tool, user control may target every owner and
foreground state.  Yielded terminal output still goes to its owner mailbox."
  (let ((record
         (mevedel-execution--user-live-record session execution-id)))
    (mevedel-execution--flush-observer record)
    (if (and (mevedel-execution--record-scheduler-lease record)
             (mevedel-execution-scheduler-cancel
              (mevedel-execution--record-scheduler-lease record)))
        (progn
          (setf (mevedel-execution--record-exit-code record) -1
                (mevedel-execution--record-stop-p record) t
                (mevedel-execution--record-termination record) 'stopped)
          (mevedel-execution--finish-managed record))
      (mevedel-execution--begin-stop record 'stopped))
    t))


;;
;;; Confined one-shot interface

(cl-defun mevedel-execution-start-one-shot
    (callback &key name command workdir writable-roots timeout
              additional-permissions sandbox-permissions session owner
              teardown-function)
  "Start one confined COMMAND and call CALLBACK with terminal facts.

NAME identifies the operating-system process.  WORKDIR and WRITABLE-ROOTS
describe its filesystem boundary.  TIMEOUT is nil or a positive number of
seconds.  ADDITIONAL-PERMISSIONS and SANDBOX-PERMISSIONS are already-authorized
confinement inputs.  SESSION and OWNER fix the transient ownership boundary.
TEARDOWN-FUNCTION releases caller-owned resources when lifecycle destruction
discards the process without invoking CALLBACK."
  (require 'mevedel-sandbox)
  (let* ((telemetry-span
          (and session
               (fboundp 'mevedel-telemetry-start)
               (mevedel-telemetry-start
                session 'child-process
                :name name :owner owner
                :command-hash (secure-hash 'sha256 (format "%S" command)))))
         (active-token (and session (gensym "sandbox-child-")))
         (preparation
          (mevedel-sandbox-prepare
           command workdir writable-roots additional-permissions
           sandbox-permissions)))
    (cl-labels
        ((show-active (facts)
           (when active-token
             (mevedel-sandbox-track-active session active-token facts)))
         (finish (child-result facts)
           (when active-token
             (mevedel-sandbox-track-active session active-token nil))
           (when telemetry-span
             (apply #'mevedel-telemetry-finish
                    telemetry-span
                    :outcome (if (zerop (or (plist-get child-result :exit-code)
                                            -1))
                                 'success
                               'error)
                    :exit-code (plist-get child-result :exit-code)
                    :output-bytes (plist-get child-result :output-bytes)
                    :timed-out (and (plist-get child-result :timed-out-p) t)
                    (mevedel-execution--telemetry-facts facts)))
           (funcall callback
                    (plist-put (copy-sequence child-result)
                               :sandbox-facts facts)))
         (teardown ()
           (when active-token
             (mevedel-sandbox-track-active session active-token nil))
           (mevedel-sandbox-cleanup preparation)
           (when teardown-function
             (funcall teardown-function))))
      (pcase (plist-get preparation :state)
        ('refused
         (when telemetry-span
           (apply #'mevedel-telemetry-finish
                  telemetry-span :outcome 'refused
                  (mevedel-execution--telemetry-facts
                   (plist-get preparation :facts))))
         (funcall
          callback
          (list :exit-code -1
                :output ""
                :output-bytes 0
                :timed-out-p nil
                :output-limit-p nil
                :wall-time-seconds 0.0
                :error (list 'error (plist-get preparation :error))
                :sandbox-facts (plist-get preparation :facts)))
         nil)
        ('unrestricted
         (when (and session (fboundp 'mevedel-telemetry-record))
           (apply #'mevedel-telemetry-record
                  session 'execution-unrestricted
                  :name name :owner owner
                  :reason-class
                  (plist-get (plist-get preparation :facts) :sandbox)
                  :after-confined-launch-failure nil
                  (mevedel-execution--telemetry-facts
                   (plist-get preparation :facts))))
         (show-active (plist-get preparation :facts))
         (mevedel-execution--start-process
          (lambda (child-result)
            (finish child-result (plist-get preparation :facts)))
          name (plist-get preparation :command) workdir timeout session owner
          #'teardown))
        ('confined
         (show-active (plist-get preparation :facts))
         (mevedel-execution--start-process
          (lambda (child-result)
            (if (and (plist-get preparation :fallback-p)
                     (mevedel-sandbox-launch-failed-p
                      preparation child-result))
                (let ((facts
                       (mevedel-sandbox--record-launch-failure child-result)))
                  (when (and session (fboundp 'mevedel-telemetry-record))
                    (apply #'mevedel-telemetry-record
                           session 'sandbox-fallback
                           :name name :owner owner
                           :launch-failure-stage 'before-command-start
                           :launch-failure-reason-class
                           'sandbox-launch-failure
                           :fallback-offered t
                           :full-execution-approval-offered nil
                           (mevedel-execution--telemetry-facts facts)))
                  (when (and session (fboundp 'mevedel-telemetry-record))
                    (apply #'mevedel-telemetry-record
                           session 'execution-unrestricted
                           :name name :owner owner
                           :reason-class 'sandbox-launch-failure
                           :after-confined-launch-failure t
                           (mevedel-execution--telemetry-facts facts)))
                  (show-active facts)
                  (mevedel-sandbox-cleanup preparation)
                  (mevedel-execution--start-process
                   (lambda (fallback-result)
                     (finish fallback-result facts))
                   name (plist-get preparation :original-command)
                   workdir timeout session owner #'teardown))
              (let ((facts
                     (if (mevedel-sandbox-launch-failed-p
                          preparation child-result)
                         (mevedel-sandbox--record-launch-failure child-result)
                       (plist-get preparation :facts)))
                    (clean-result
                     (mevedel-sandbox-strip-marker preparation child-result)))
                (mevedel-sandbox-cleanup preparation)
                (finish clean-result facts))))
          name (plist-get preparation :command) workdir timeout session owner
          #'teardown))
        (_ (error "Unknown sandbox preparation state: %s"
                  (plist-get preparation :state))))
      nil)))

(defun mevedel-execution--owner-teardown-result ()
  "Return structured settlement for a synchronously discarded child."
  '(:exit-code -1 :output "" :output-bytes 0
    :timed-out-p nil :output-limit-p nil :wall-time-seconds 0.0
    :error (error "Execution owner was torn down")))

(cl-defun mevedel-execution-run-one-shot
    (&key name command workdir writable-roots timeout
          additional-permissions sandbox-permissions session owner)
  "Run one confined COMMAND synchronously and return terminal facts.

All keyword arguments follow `mevedel-execution-start-one-shot'."
  (let (done result)
    (mevedel-execution-start-one-shot
     (lambda (child-result)
       (setq result child-result
             done t))
     :name name :command command :workdir workdir
     :writable-roots writable-roots :timeout timeout
     :additional-permissions additional-permissions
     :sandbox-permissions sandbox-permissions :session session :owner owner
     :teardown-function
     (lambda ()
       (setq result (mevedel-execution--owner-teardown-result)
             done t)))
    (while (not done)
      (accept-process-output nil 0.05))
    result))


;;
;;; External helper interface

(cl-defun mevedel-execution-start-helper
    (callback name command read-paths writable-roots
              &key timeout session owner teardown-callback)
  "Start external helper COMMAND and call CALLBACK when it settles.

READ-PATHS are mounted read-only.  WRITABLE-ROOTS are explicit artifact
directories.  A private writable scratch directory is the helper's working
directory and is removed before CALLBACK runs."
  (let* ((scratch (make-temp-file "mevedel-helper-" t))
         (roots
          (delete-dups
           (mapcar (lambda (path)
                     (file-name-as-directory (expand-file-name path)))
                   (cons scratch writable-roots))))
         (permissions
          (and read-paths
               (list :file-system
                     (mapcar (lambda (path)
                               (list :path (expand-file-name path)
                                     :access 'read))
                             (delete-dups read-paths)))))
         finished
         (cleanup
          (lambda ()
            (unless finished
              (setq finished t)
              (ignore-errors (delete-directory scratch t)))))
         (teardown
          (lambda ()
            (funcall cleanup)
            (when teardown-callback
              (funcall teardown-callback)))))
    (condition-case err
        (mevedel-execution-start-one-shot
         (lambda (child-result)
           (funcall cleanup)
           (funcall callback child-result))
         :name name :command command :workdir scratch
         :writable-roots roots :timeout timeout
         :additional-permissions permissions :session session :owner owner
         :teardown-function teardown)
      (error
       (funcall cleanup)
       (signal (car err) (cdr err))))))

(cl-defun mevedel-execution-run-helper
    (name command read-paths writable-roots &key timeout session owner)
  "Run external helper COMMAND synchronously and return terminal facts.

NAME, READ-PATHS, WRITABLE-ROOTS, TIMEOUT, and SESSION follow
`mevedel-execution-start-helper'."
  (let (done result)
    (mevedel-execution-start-helper
     (lambda (child-result)
       (setq result child-result
             done t))
     name command read-paths writable-roots
     :timeout timeout :session session :owner owner
     :teardown-callback
     (lambda ()
       (setq result (mevedel-execution--owner-teardown-result)
             done t)))
    (while (not done)
      (accept-process-output nil 0.05))
    result))

(provide 'mevedel-execution)
;;; mevedel-execution.el ends here
