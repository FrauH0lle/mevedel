;;; mevedel-execution.el --- Bounded child-process lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Owns model-triggered operating-system processes: stable environments,
;; process groups, confinement, timeouts, bounded disk spooling, and private
;; process records.  Callers receive result plists and never process objects.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

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
(declare-function mevedel-session--set-execution-state
                  "mevedel-structs" (session state))
(declare-function mevedel-session-execution-state "mevedel-structs" (cl-x) t)


;;
;;; Configuration and private state

(defcustom mevedel-execution-output-limit (* 64 1024 1024)
  "Maximum bytes retained from one child process.

The execution module terminates a child when its merged stdout and stderr
reach this limit.  Bytes already written to its spool remain available in the
terminal result."
  :type 'integer
  :group 'mevedel)

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

(cl-defstruct (mevedel-execution--state
               (:constructor mevedel-execution--state-create))
  "Opaque per-session execution state."
  records)

(cl-defstruct (mevedel-execution--record
               (:constructor mevedel-execution--record-create))
  "Private state for one operating-system process."
  callback
  exit-code
  finished-p
  force-timer
  group-file
  output-limit-p
  process
  session
  settle-timer
  spool-path
  started-at
  stop-p
  timed-out-p
  timeout-timer
  token
  watch-timer)

(defvar mevedel-execution--orphan-state nil
  "Private state for children that have no chat session owner.")


;;
;;; State and environment

(defun mevedel-execution--new-state ()
  "Return an empty opaque execution state."
  (mevedel-execution--state-create
   :records (make-hash-table :test #'eq)))

(defun mevedel-execution--state-for-session (session)
  "Return the private execution state for SESSION.

When SESSION is nil, use the module-owned state for direct non-session calls."
  (if session
      (or (mevedel-session-execution-state session)
          (let ((state (mevedel-execution--new-state)))
            (mevedel-session--set-execution-state session state)
            state))
    (or mevedel-execution--orphan-state
        (setq mevedel-execution--orphan-state
              (mevedel-execution--new-state)))))

(defun mevedel-execution--process-environment ()
  "Return a child environment with stable execution defaults."
  (let ((process-environment (copy-sequence process-environment)))
    (dolist (entry mevedel-execution--environment)
      (setenv (car entry) (cdr entry)))
    process-environment))


;;
;;; Process groups and output spooling

(defun mevedel-execution--spawn-command (command)
  "Return process-group spawn metadata for COMMAND."
  (let ((setsid (and (not (eq system-type 'windows-nt))
                     (executable-find "setsid"))))
    (if (not setsid)
        (list :command command)
      (let ((group-file (make-temp-file "mevedel-child-group-")))
        (list
         :command
         (append
          (list setsid "-f" "-w"
                "sh" "-c"
                "printf '%s' \"$$\" > \"$1\"; shift; exec \"$@\""
                "mevedel-child" group-file)
          command)
         :group-file group-file)))))

(defun mevedel-execution--group-id (group-file)
  "Return the positive process group id recorded in GROUP-FILE."
  (when (and group-file (file-readable-p group-file))
    (let ((id
           (string-to-number
            (string-trim
             (with-temp-buffer
               (insert-file-contents group-file)
               (buffer-string))))))
      (and (> id 0) id))))

(defun mevedel-execution--signal-record (record signal)
  "Send SIGNAL to RECORD's process group when available."
  (let* ((process (mevedel-execution--record-process record))
         (group-id
          (mevedel-execution--group-id
           (mevedel-execution--record-group-file record)))
         (target (or group-id
                     (and (processp process) (process-id process)))))
    (condition-case nil
        (if (and group-id (> group-id 0))
            (signal-process (- group-id) signal)
          (when (and (integerp target) (> target 0))
            (signal-process target signal)))
      (error
       (when (process-live-p process)
         (ignore-errors (signal-process process signal)))))))

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
        (mevedel-execution--begin-stop record)))))

(defun mevedel-execution--read-output (record)
  "Return RECORD's complete decoded spooled output."
  (let ((path (mevedel-execution--record-spool-path record)))
    (if (not (file-readable-p path))
        ""
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (decode-coding-string (buffer-string) 'utf-8-unix t)))))

(defun mevedel-execution--cleanup-record (record)
  "Release process, timer, group, spool, and registry state for RECORD."
  (dolist (timer (list (mevedel-execution--record-timeout-timer record)
                       (mevedel-execution--record-force-timer record)
                       (mevedel-execution--record-settle-timer record)
                       (mevedel-execution--record-watch-timer record)))
    (when (timerp timer)
      (cancel-timer timer)))
  (when-let* ((process (mevedel-execution--record-process record)))
    (set-process-query-on-exit-flag process nil)
    (when (process-live-p process)
      (ignore-errors (delete-process process))))
  (when-let* ((group-file (mevedel-execution--record-group-file record)))
    (ignore-errors (delete-file group-file)))
  (when-let* ((path (mevedel-execution--record-spool-path record)))
    (ignore-errors (delete-file path)))
  (let ((state
         (mevedel-execution--state-for-session
          (mevedel-execution--record-session record))))
    (remhash (mevedel-execution--record-token record)
             (mevedel-execution--state-records state))))

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
    (mevedel-execution--finish-record
     record (or (mevedel-execution--record-exit-code record) -1))))

(defun mevedel-execution--force-kill (record)
  "Force-kill RECORD's process group and schedule bounded settlement."
  (unless (mevedel-execution--record-finished-p record)
    (setf (mevedel-execution--record-force-timer record) nil)
    (mevedel-execution--signal-record record 'KILL)
    (setf (mevedel-execution--record-settle-timer record)
          (run-at-time mevedel-execution--child-kill-delay nil
                       #'mevedel-execution--settle-after-kill record))))

(defun mevedel-execution--begin-stop (record)
  "Terminate RECORD with TERM followed by a bounded KILL grace."
  (unless (or (mevedel-execution--record-finished-p record)
              (mevedel-execution--record-stop-p record))
    (setf (mevedel-execution--record-stop-p record) t)
    (mevedel-execution--signal-record record 'TERM)
    (setf (mevedel-execution--record-force-timer record)
          (run-at-time mevedel-execution--child-kill-delay nil
                       #'mevedel-execution--force-kill record))))

(defun mevedel-execution--time-out (record)
  "Mark RECORD timed out and terminate its process group."
  (unless (mevedel-execution--record-finished-p record)
    (setf (mevedel-execution--record-timed-out-p record) t)
    (mevedel-execution--begin-stop record)))

(defun mevedel-execution--process-ended (record process)
  "Settle RECORD when PROCESS reaches a terminal state."
  (when (memq (process-status process) '(exit signal))
    (setf (mevedel-execution--record-exit-code record)
          (process-exit-status process))
    (unless (and (mevedel-execution--record-stop-p record)
                 (timerp (mevedel-execution--record-force-timer record)))
      (mevedel-execution--finish-record
       record (mevedel-execution--record-exit-code record)))))

(defun mevedel-execution--start-process
    (callback name command workdir timeout session)
  "Start raw COMMAND and call CALLBACK with its bounded terminal result."
  (let* ((record
          (mevedel-execution--record-create
           :callback callback
           :session session
           :spool-path (make-temp-file "mevedel-execution-output-")
           :started-at (float-time)
           :token (gensym "execution-process-")))
         (state (mevedel-execution--state-for-session session))
         spawn)
    (puthash (mevedel-execution--record-token record) record
             (mevedel-execution--state-records state))
    (condition-case err
        (let* ((executable (car-safe command))
               (_resolved
                (or (and (stringp executable)
                         (executable-find executable))
                    (signal 'file-missing
                            (list "Executable not found" executable))))
               (process-environment (mevedel-execution--process-environment))
               (default-directory workdir))
          (setq spawn (mevedel-execution--spawn-command command))
          (setf (mevedel-execution--record-group-file record)
                (plist-get spawn :group-file))
          (setf
           (mevedel-execution--record-process record)
           (make-process
            :name name
            :buffer nil
            :command (plist-get spawn :command)
            :coding 'no-conversion
            :connection-type 'pipe
            :filter (lambda (_process chunk)
                      (mevedel-execution--append-output record chunk))
            :noquery t
            :sentinel (lambda (process _event)
                        (mevedel-execution--process-ended record process))))
          (process-send-eof (mevedel-execution--record-process record))
          ;; Emacs can observe a terminal process state without delivering its
          ;; sentinel when callers run inside code that temporarily inhibits
          ;; sentinels.  Keep settlement bounded in that case as well.
          (setf (mevedel-execution--record-watch-timer record)
                (run-at-time
                 0.1 0.1
                 (lambda ()
                   (unless (mevedel-execution--record-finished-p record)
                     (let ((process
                            (mevedel-execution--record-process record)))
                       (when (and (processp process)
                                  (memq (process-status process)
                                        '(exit signal)))
                         (mevedel-execution--process-ended
                          record process)))))))
          (when timeout
            (setf (mevedel-execution--record-timeout-timer record)
                  (run-at-time timeout nil
                               #'mevedel-execution--time-out record)))
          (mevedel-execution--record-process record))
      (error
       (mevedel-execution--finish-record record -1 err)
       nil))))


;;
;;; Confined one-shot interface

(cl-defun mevedel-execution-start-one-shot
    (callback &key name command workdir writable-roots timeout
              additional-permissions sandbox-permissions session)
  "Start one confined COMMAND and call CALLBACK with terminal facts.

NAME identifies the operating-system process.  WORKDIR and WRITABLE-ROOTS
describe its filesystem boundary.  TIMEOUT is nil or a positive number of
seconds.  ADDITIONAL-PERMISSIONS and SANDBOX-PERMISSIONS are already-authorized
confinement inputs.  SESSION owns the transient process state and visible
sandbox boundary."
  (require 'mevedel-sandbox)
  (let* ((active-token (and session (gensym "sandbox-child-")))
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
           (funcall callback
                    (plist-put (copy-sequence child-result)
                               :sandbox-facts facts))))
      (pcase (plist-get preparation :state)
        ('refused
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
         (show-active (plist-get preparation :facts))
         (mevedel-execution--start-process
          (lambda (child-result)
            (finish child-result (plist-get preparation :facts)))
          name (plist-get preparation :command) workdir timeout session))
        ('confined
         (show-active (plist-get preparation :facts))
         (mevedel-execution--start-process
          (lambda (child-result)
            (if (and (plist-get preparation :fallback-p)
                     (mevedel-sandbox-launch-failed-p
                      preparation child-result))
                (let ((facts
                       (mevedel-sandbox--record-launch-failure child-result)))
                  (show-active facts)
                  (mevedel-sandbox-cleanup preparation)
                  (mevedel-execution--start-process
                   (lambda (fallback-result)
                     (finish fallback-result facts))
                   name (plist-get preparation :original-command)
                   workdir timeout session))
              (let ((facts
                     (if (mevedel-sandbox-launch-failed-p
                          preparation child-result)
                         (mevedel-sandbox--record-launch-failure child-result)
                       (plist-get preparation :facts)))
                    (clean-result
                     (mevedel-sandbox-strip-marker preparation child-result)))
                (mevedel-sandbox-cleanup preparation)
                (finish clean-result facts))))
          name (plist-get preparation :command) workdir timeout session))
        (_ (error "Unknown sandbox preparation state: %s"
                  (plist-get preparation :state))))
      nil)))

(cl-defun mevedel-execution-run-one-shot
    (&key name command workdir writable-roots timeout
          additional-permissions sandbox-permissions session)
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
     :sandbox-permissions sandbox-permissions :session session)
    (while (not done)
      (accept-process-output nil 0.05))
    result))


;;
;;; External helper interface

(defun mevedel-execution-start-helper
    (callback name command read-paths writable-roots &optional timeout session)
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
         finished)
    (condition-case err
        (mevedel-execution-start-one-shot
         (lambda (child-result)
           (unless finished
             (setq finished t)
             (ignore-errors (delete-directory scratch t))
             (funcall callback child-result)))
         :name name :command command :workdir scratch
         :writable-roots roots :timeout timeout
         :additional-permissions permissions :session session)
      (error
       (unless finished
         (setq finished t)
         (ignore-errors (delete-directory scratch t)))
       (signal (car err) (cdr err))))))

(defun mevedel-execution-run-helper
    (name command read-paths writable-roots &optional timeout session)
  "Run external helper COMMAND synchronously and return terminal facts.

NAME, READ-PATHS, WRITABLE-ROOTS, TIMEOUT, and SESSION follow
`mevedel-execution-start-helper'."
  (let (done result)
    (mevedel-execution-start-helper
     (lambda (child-result)
       (setq result child-result
             done t))
     name command read-paths writable-roots timeout session)
    (while (not done)
      (accept-process-output nil 0.05))
    result))

(provide 'mevedel-execution)
;;; mevedel-execution.el ends here
