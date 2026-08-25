;;; mevedel-execution-process.el -- Opaque child process lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Owns one operating-system child, its process group, timeout/termination
;; timers, and bounded raw output spool.  Callers receive output chunks and one
;; immutable terminal result; managed session state never crosses this module.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'tramp-cache))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create
                  "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-direct-async-capable-p
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))
(autoload 'mevedel-execution-target-create "mevedel-execution-target")
(autoload 'mevedel-execution-target-direct-async-capable-p
  "mevedel-execution-target")
(autoload 'mevedel-execution-target-native-path "mevedel-execution-target")

;; `tramp'
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))

;; `tramp-cache'
(declare-function tramp-get-hash-table "tramp-cache" (key))
(declare-function tramp-set-connection-property
                  "tramp-cache" (key property value))

(defcustom mevedel-execution-process-output-limit (* 64 1024 1024)
  "Maximum bytes retained from one child process."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-execution-process-remote-direct-async t
  "When non-nil, eligible remote children use a private TRAMP channel."
  :type 'boolean
  :group 'mevedel)

(defconst mevedel-execution-process--direct-async-command-limit 3584
  "Largest quoted remote command, in bytes, spawned direct-async.")

(defconst mevedel-execution-process--child-kill-delay 2
  "Seconds to wait before force-killing a stopped child process group.")

(defconst mevedel-execution-process--remote-control-timeout 5
  "Seconds allowed for one target process-control operation.")

(defconst mevedel-execution-process--environment
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

(defconst mevedel-execution-process--remote-group-script
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

(defconst mevedel-execution-process--remote-group-status-script
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

(cl-defstruct (mevedel-execution-process--child
               (:constructor mevedel-execution-process--child-create))
  "Private state for one operating-system child."
  coding
  confined-p
  direct-async-p
  error
  exit-code
  finished-p
  force-timer
  filter-function
  group-id
  group-marker
  group-marker-buffer
  group-start-time
  launch-attempted-p
  output-function
  output-limit-p
  process
  settle-timer
  spool-path
  started-at
  stop-p
  terminal-function
  termination
  timed-out-p
  timeout-timer
  tty-p
  watch-timer
  workdir)

(cl-defun mevedel-execution-process-create
    (&key workdir tty spool-path filter-function output-function
          terminal-function)
  "Create an opaque child for WORKDIR without launching it."
  (mevedel-execution-process--child-create
   :filter-function filter-function
   :output-function output-function
   :spool-path (or spool-path (make-temp-file "mevedel-execution-output-"))
   :started-at (float-time)
   :terminal-function terminal-function
   :tty-p (and tty t)
   :workdir workdir))

(defun mevedel-execution-process-spool-path (child)
  "Return CHILD's private raw output spool path."
  (mevedel-execution-process--child-spool-path child))

(defun mevedel-execution-process-launch-attempted-p (child)
  "Return non-nil when CHILD reached the operating-system spawn call."
  (mevedel-execution-process--child-launch-attempted-p child))

(defun mevedel-execution-process--refresh-terminal (child)
  "Reconcile CHILD when its exact process is already terminal."
  (when-let* (((not (mevedel-execution-process--child-finished-p child)))
              (process (mevedel-execution-process--child-process child))
              ((memq (process-status process) '(exit signal))))
    (mevedel-execution-process--ended child process)))

(defun mevedel-execution-process-live-p (child)
  "Return non-nil when CHILD still owns a live process."
  (when child (mevedel-execution-process--refresh-terminal child))
  (and child
       (not (mevedel-execution-process--child-finished-p child))
       (process-live-p (mevedel-execution-process--child-process child))))

(defun mevedel-execution-process-terminal-p (child)
  "Return non-nil when CHILD has emitted its terminal result."
  (when child (mevedel-execution-process--refresh-terminal child))
  (and child (mevedel-execution-process--child-finished-p child)))

(defun mevedel-execution-process-status (child)
  "Return immutable current process facts for CHILD."
  (list :error (mevedel-execution-process--child-error child)
        :direct-async-p
        (mevedel-execution-process--child-direct-async-p child)
        :exit-code (mevedel-execution-process--child-exit-code child)
        :group-id (mevedel-execution-process--child-group-id child)
        :group-start-time
        (mevedel-execution-process--child-group-start-time child)
        :launch-attempted-p
        (mevedel-execution-process--child-launch-attempted-p child)
        :output-limit-p
        (mevedel-execution-process--child-output-limit-p child)
        :stop-p (mevedel-execution-process--child-stop-p child)
        :termination (mevedel-execution-process--child-termination child)
        :timed-out-p (mevedel-execution-process--child-timed-out-p child)
        :tty-p (mevedel-execution-process--child-tty-p child)
        :workdir (mevedel-execution-process--child-workdir child)))

(defun mevedel-execution-process--process-environment (&optional remote)
  "Return stable child environment, preserving the target when REMOTE."
  (let ((process-environment
         (and (not remote) (copy-sequence process-environment))))
    (dolist (entry mevedel-execution-process--environment)
      (setenv (car entry) (cdr entry)))
    process-environment))

(defun mevedel-execution-process--localize-command (command workdir target)
  "Return COMMAND in TARGET's native path domain for WORKDIR."
  (let ((target (or target (mevedel-execution-target-create workdir))))
    (mapcar (lambda (argument)
              (if (stringp argument)
                  (mevedel-execution-target-native-path target argument)
                argument))
            command)))

(defun mevedel-execution-process--remote-command (child command)
  "Wrap COMMAND with target-side process-group ownership for CHILD."
  (let ((marker (make-temp-name "MEVEDEL_PROCESS_GROUP_")))
    (setf (mevedel-execution-process--child-group-marker child) marker
          (mevedel-execution-process--child-group-marker-buffer child) "")
    (append
     (when (mevedel-execution-process--child-direct-async-p child)
       (cons "env"
             (mapcar (lambda (entry)
                       (format "%s=%s" (car entry) (cdr entry)))
                     mevedel-execution-process--environment)))
     (list "setsid" "-f" "-w" "bash" "-c"
           mevedel-execution-process--remote-group-script marker)
     command)))

(defun mevedel-execution-process--with-spawn-channel
    (remote direct-async thunk)
  "Call THUNK through the selected REMOTE spawn channel."
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
              (progn
                (require 'tramp-cache)
                (let ((vec (tramp-dissect-file-name remote)))
                  (with-tramp-saved-connection-property vec "direct-async"
                    (tramp-set-connection-property vec "direct-async" t)
                    (funcall thunk))))
            (funcall thunk))))
    (funcall thunk)))

(defun mevedel-execution-process--direct-async-p (child command workdir)
  "Return non-nil when CHILD may spawn COMMAND direct-async."
  (and mevedel-execution-process-remote-direct-async
       (not (mevedel-execution-process--child-tty-p child))
       (fboundp 'tramp-direct-async-process-p)
       (mevedel-execution-target-direct-async-capable-p
        (mevedel-execution-target-create workdir))
       (< (+ (string-bytes
              (mapconcat #'shell-quote-argument command " "))
             (string-bytes mevedel-execution-process--remote-group-script)
             256)
          mevedel-execution-process--direct-async-command-limit)))

(defun mevedel-execution-process--filter-group-marker (child chunk)
  "Strip CHILD's remote group marker and consume output CHUNK."
  (let ((marker (mevedel-execution-process--child-group-marker child)))
    (if (or (null marker)
            (eq :done
                (mevedel-execution-process--child-group-marker-buffer child)))
        (mevedel-execution-process--consume-output child chunk)
      (setq chunk
            (concat
             (mevedel-execution-process--child-group-marker-buffer child)
             chunk))
      (if-let* ((newline (string-search "\n" chunk)))
          (let ((line (string-trim-right (substring chunk 0 newline) "\r")))
            (setf (mevedel-execution-process--child-group-marker-buffer child)
                  :done)
            (if (string-match
                 (concat "\\`" (regexp-quote marker)
                         "\\([0-9]+\\):\\([0-9]+\\)\\'")
                 line)
                (setf (mevedel-execution-process--child-group-id child)
                      (string-to-number (match-string 1 line))
                      (mevedel-execution-process--child-group-start-time child)
                      (match-string 2 line))
              (mevedel-execution-process--consume-output
               child (substring chunk 0 (1+ newline))))
            (let ((rest (substring chunk (1+ newline))))
              (unless (string-empty-p rest)
                (mevedel-execution-process--consume-output child rest))))
        (setf (mevedel-execution-process--child-group-marker-buffer child)
              chunk)))))

(defun mevedel-execution-process--remote-group-status (child)
  "Return CHILD's remote group status as `live', `dead', or `ambiguous'."
  (let ((group-id (mevedel-execution-process--child-group-id child))
        (start-time
         (mevedel-execution-process--child-group-start-time child))
        (workdir (mevedel-execution-process--child-workdir child))
        (process-environment nil))
    (if (not (and (integerp group-id) (> group-id 0)
                  (stringp start-time) (stringp workdir)
                  (file-remote-p workdir)))
        'ambiguous
      (with-timeout
          (mevedel-execution-process--remote-control-timeout
           (error "Target process status probe timed out"))
        (with-temp-buffer
          (setq default-directory workdir)
          (pcase
              (process-file
               "bash" nil nil nil "-c"
               mevedel-execution-process--remote-group-status-script
               "mevedel-process-group" (number-to-string group-id)
               start-time)
            (0 'live)
            (1 'dead)
            (_ 'ambiguous)))))))

(defun mevedel-execution-process--mark-unknown (child error-data)
  "Mark CHILD's target outcome unprovable with ERROR-DATA."
  (setf (mevedel-execution-process--child-termination child) 'unknown
        (mevedel-execution-process--child-error child) error-data)
  (unless (mevedel-execution-process--timer-pending-p
           (mevedel-execution-process--child-settle-timer child))
    (setf (mevedel-execution-process--child-settle-timer child)
          (run-at-time mevedel-execution-process--child-kill-delay nil
                       #'mevedel-execution-process--settle-after-kill child)))
  (when-let* ((process (mevedel-execution-process--child-process child))
              ((process-live-p process)))
    (ignore-errors (delete-process process))))

(defun mevedel-execution-process--signal-confined-group (child signal)
  "Send SIGNAL to CHILD's foreground process group inside Bubblewrap."
  (let ((outer-group-id (mevedel-execution-process--child-group-id child))
        (children (make-hash-table :test #'eql))
        (attributes (make-hash-table :test #'eql))
        (pending (list (mevedel-execution-process--child-group-id child)))
        group-id)
    (dolist (pid (list-system-processes))
      (when-let* ((attrs (process-attributes pid))
                  (parent (alist-get 'ppid attrs)))
        (puthash pid attrs attributes)
        (push pid (gethash parent children))))
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

(defun mevedel-execution-process--signal (child signal)
  "Send SIGNAL to CHILD's process group when available."
  (let* ((process (mevedel-execution-process--child-process child))
         (group-id (mevedel-execution-process--child-group-id child))
         (workdir (mevedel-execution-process--child-workdir child))
         (remote (and workdir (file-remote-p workdir)))
         (confined-p (mevedel-execution-process--child-confined-p child)))
    (condition-case err
        (cond
         ((and (not remote) confined-p (eq signal 'INT)
               (mevedel-execution-process--signal-confined-group
                child signal)))
         ((and (not remote) (eq system-type 'windows-nt)
               (process-live-p process))
          (kill-process process t))
         ((and (eq signal 'INT)
               (mevedel-execution-process--child-tty-p child)
               (process-live-p process))
          (process-send-string process (string 3)))
         ((and (or remote (not (eq system-type 'windows-nt)))
               (integerp group-id) (> group-id 0))
          (if remote
              (pcase (mevedel-execution-process--remote-group-status child)
                ('live
                 (with-timeout
                     (mevedel-execution-process--remote-control-timeout
                      (error "Target process signal timed out"))
                   (signal-process (- group-id) signal workdir)))
                ('dead -1)
                (status
                 (error "Remote process-group status is %s" status)))
            (signal-process (- group-id) signal)))
         (remote (error "Remote process-group identity is unavailable"))
         ((process-live-p process) (signal-process process signal)))
      (error
       (if remote
           (mevedel-execution-process--mark-unknown child err)
         (when (process-live-p process)
           (ignore-errors (signal-process process signal))))))))

(defun mevedel-execution-process--group-live-p (child)
  "Return non-nil when CHILD's process group still has a live member."
  (let* ((workdir (mevedel-execution-process--child-workdir child))
         (remote (and workdir (file-remote-p workdir)))
         (group-id (mevedel-execution-process--child-group-id child)))
    (cond
     ((and remote (not group-id))
      (mevedel-execution-process--mark-unknown
       child '(error "Remote process-group identity was not received"))
      nil)
     (remote
      (condition-case err
          (not (eq 'dead
                   (mevedel-execution-process--remote-group-status child)))
        (error
         (mevedel-execution-process--mark-unknown child err)
         nil)))
     ((not (eq system-type 'windows-nt))
      (when group-id
        (condition-case nil
            (zerop (signal-process (- group-id) 0))
          (error nil)))))))

(defun mevedel-execution-process--consume-output (child chunk)
  "Append bounded raw output CHUNK to CHILD and notify its consumer."
  (unless (or (mevedel-execution-process--child-finished-p child)
              (mevedel-execution-process--child-output-limit-p child))
    (condition-case err
        (let* ((filter
                (mevedel-execution-process--child-filter-function child))
               (chunk (or (if filter (funcall filter child chunk) chunk) ""))
               (path (mevedel-execution-process--child-spool-path child))
               (current (file-attribute-size (file-attributes path)))
               (remaining
                (- mevedel-execution-process-output-limit current))
               (coding (mevedel-execution-process--child-coding child))
               (encoded (encode-coding-string chunk coding t))
               (length (string-bytes encoded))
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
                    (substring chunk 0 low))))
               (retained-bytes (encode-coding-string retained coding t)))
          (when (and (> remaining 0) (not (string-empty-p retained)))
            (let ((coding-system-for-write 'no-conversion))
              (write-region retained-bytes nil path t 'silent)))
          (when-let* ((function
                       (mevedel-execution-process--child-output-function child)))
            (unless (string-empty-p retained)
              (funcall function child retained)))
          (when (> length remaining)
            (setf (mevedel-execution-process--child-output-limit-p child) t)
            (mevedel-execution-process-stop child 'output-limit)))
      (error
       (setf (mevedel-execution-process--child-error child) err)
       (mevedel-execution-process-stop child 'output-write-failed)))))

(defun mevedel-execution-process-read (child)
  "Return CHILD's complete decoded spooled output."
  (let ((path (mevedel-execution-process--child-spool-path child)))
    (if (not (file-readable-p path))
        ""
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (decode-coding-string (buffer-string) 'utf-8-unix t)))))

(defun mevedel-execution-process-release (child &optional preserve-spool)
  "Release CHILD's runtime and delete its spool unless PRESERVE-SPOOL."
  (let ((process (mevedel-execution-process--child-process child)))
    (dolist (timer
             (list (mevedel-execution-process--child-timeout-timer child)
                   (mevedel-execution-process--child-force-timer child)
                   (mevedel-execution-process--child-settle-timer child)
                   (mevedel-execution-process--child-watch-timer child)))
      (when (timerp timer) (cancel-timer timer)))
    (setf (mevedel-execution-process--child-process child) nil
          (mevedel-execution-process--child-timeout-timer child) nil
          (mevedel-execution-process--child-force-timer child) nil
          (mevedel-execution-process--child-settle-timer child) nil
          (mevedel-execution-process--child-watch-timer child) nil)
    (when process
      (set-process-query-on-exit-flag process nil)
      (ignore-errors (delete-process process)))
    (unless preserve-spool
      (when-let* ((path (mevedel-execution-process--child-spool-path child)))
        (ignore-errors (delete-file path))))))

(defun mevedel-execution-process-relocate-spool (child destination)
  "Move CHILD's spool to DESTINATION and return the new path."
  (let ((source (mevedel-execution-process--child-spool-path child)))
    (unless (equal source destination)
      (make-directory (file-name-directory destination) t)
      (rename-file source destination t)
      (setf (mevedel-execution-process--child-spool-path child) destination))
    destination))

(defun mevedel-execution-process-rebind-spool (child path)
  "Record PATH as CHILD's spool after an external tree relocation."
  (setf (mevedel-execution-process--child-spool-path child) path))

(defun mevedel-execution-process--timer-pending-p (timer)
  "Return non-nil when TIMER is armed and still due to fire."
  (and (timerp timer) (memq timer timer-list) t))

(defun mevedel-execution-process--finish
    (child status &optional error-data)
  "Emit CHILD's immutable terminal result once."
  (unless (mevedel-execution-process--child-finished-p child)
    (when-let* ((process (mevedel-execution-process--child-process child))
                ((memq (process-status process) '(exit signal))))
      (while (accept-process-output process 0.01 nil 1)))
    (setf (mevedel-execution-process--child-finished-p child) t
          (mevedel-execution-process--child-exit-code child) status)
    (let* ((path (mevedel-execution-process--child-spool-path child))
           (bytes (or (and (file-readable-p path)
                           (file-attribute-size (file-attributes path)))
                      0))
           (result
            (list :error (or error-data
                             (mevedel-execution-process--child-error child))
                  :exit-code status
                  :group-id (mevedel-execution-process--child-group-id child)
                  :group-start-time
                  (mevedel-execution-process--child-group-start-time child)
                  :output (mevedel-execution-process-read child)
                  :output-bytes bytes
                  :output-limit-p
                  (mevedel-execution-process--child-output-limit-p child)
                  :termination
                  (mevedel-execution-process--child-termination child)
                  :timed-out-p
                  (mevedel-execution-process--child-timed-out-p child)
                  :wall-time-seconds
                  (- (float-time)
                     (mevedel-execution-process--child-started-at child))
                  :workdir
                  (mevedel-execution-process--child-workdir child)))
           (terminal
            (mevedel-execution-process--child-terminal-function child)))
      (mevedel-execution-process--release-runtime child)
      (when terminal (funcall terminal child result)))))

(defun mevedel-execution-process--release-runtime (child)
  "Release CHILD's process and timers without deleting its spool."
  (let ((path (mevedel-execution-process--child-spool-path child)))
    (mevedel-execution-process-release child t)
    (setf (mevedel-execution-process--child-spool-path child) path)))

(defun mevedel-execution-process--settle-after-kill (child)
  "Settle CHILD after its final process-group signal."
  (unless (mevedel-execution-process--child-finished-p child)
    (when (and (file-remote-p
                (or (mevedel-execution-process--child-workdir child) ""))
               (not (eq 'unknown
                        (mevedel-execution-process--child-termination child)))
               (mevedel-execution-process--group-live-p child))
      (mevedel-execution-process--mark-unknown
       child '(error "Remote process group survived the final KILL signal")))
    (mevedel-execution-process--finish
     child (or (mevedel-execution-process--child-exit-code child) -1))))

(defun mevedel-execution-process--force-kill (child)
  "Force-kill CHILD and schedule bounded settlement."
  (unless (mevedel-execution-process--child-finished-p child)
    (setf (mevedel-execution-process--child-force-timer child) nil)
    (mevedel-execution-process--signal child 'KILL)
    (unless (mevedel-execution-process--timer-pending-p
             (mevedel-execution-process--child-settle-timer child))
      (setf (mevedel-execution-process--child-settle-timer child)
            (run-at-time
             mevedel-execution-process--child-kill-delay nil
             #'mevedel-execution-process--settle-after-kill child)))))

(defun mevedel-execution-process--start-stop (child)
  "Send CHILD's first stop signal outside its process filter."
  (setf (mevedel-execution-process--child-force-timer child) nil)
  (unless (mevedel-execution-process--child-finished-p child)
    (mevedel-execution-process--signal child 'TERM)
    (unless (eq 'unknown
                (mevedel-execution-process--child-termination child))
      (setf (mevedel-execution-process--child-force-timer child)
            (run-at-time
             mevedel-execution-process--child-kill-delay nil
             #'mevedel-execution-process--force-kill child)))))

(defun mevedel-execution-process-stop (child reason)
  "Latch REASON and terminate CHILD with TERM then bounded KILL grace."
  (unless (or (mevedel-execution-process--child-finished-p child)
              (mevedel-execution-process--child-stop-p child))
    (setf (mevedel-execution-process--child-stop-p child) t
          (mevedel-execution-process--child-termination child) reason)
    (let* ((process (mevedel-execution-process--child-process child))
           (remote
            (file-remote-p
             (or (mevedel-execution-process--child-workdir child) ""))))
      (cond
       ((and (processp process)
             (memq (process-status process) '(exit signal))
             (not remote)
             (mevedel-execution-process--group-live-p child))
        (mevedel-execution-process--signal child 'TERM)
        (setf (mevedel-execution-process--child-force-timer child)
              (run-at-time
               mevedel-execution-process--child-kill-delay nil
               #'mevedel-execution-process--force-kill child)))
       ((and (processp process)
             (memq (process-status process) '(exit signal)))
        (mevedel-execution-process--ended child process))
       ((not (process-live-p process))
        (mevedel-execution-process--finish
         child (or (mevedel-execution-process--child-exit-code child) -1)))
       (remote
        (setf (mevedel-execution-process--child-force-timer child)
              (run-at-time
               0 nil #'mevedel-execution-process--start-stop child)))
       (t
        (mevedel-execution-process--signal child 'TERM)
        (setf (mevedel-execution-process--child-force-timer child)
              (run-at-time
               mevedel-execution-process--child-kill-delay nil
               #'mevedel-execution-process--force-kill child)))))
    t))

(defun mevedel-execution-process--settle-timed-out (child)
  "Forcibly settle CHILD when its timeout escalation did not finish."
  (unless (mevedel-execution-process--child-finished-p child)
    (when (and (file-remote-p
                (or (mevedel-execution-process--child-workdir child) ""))
               (not (eq 'unknown
                        (mevedel-execution-process--child-termination child))))
      (mevedel-execution-process--mark-unknown
       child '(error "Timed-out child did not settle")))
    (mevedel-execution-process--finish
     child (or (mevedel-execution-process--child-exit-code child) -1))))

(defun mevedel-execution-process--time-out (child)
  "Mark CHILD timed out and terminate its process group."
  (when (mevedel-execution-process-stop child 'timed-out)
    (setf (mevedel-execution-process--child-timed-out-p child) t)
    (run-at-time
     (+ (* 2 mevedel-execution-process--child-kill-delay)
        (* 4 mevedel-execution-process--remote-control-timeout))
     nil #'mevedel-execution-process--settle-timed-out child)))

(defun mevedel-execution-process--settle-main-exit (child)
  "Drain descendants or settle CHILD after its main process exits."
  (let ((workdir (mevedel-execution-process--child-workdir child)))
    (setf (mevedel-execution-process--child-settle-timer child) nil)
    (if (and (not (eq system-type 'windows-nt))
             (not (file-remote-p (or workdir "")))
             (mevedel-execution-process--group-live-p child))
        (mevedel-execution-process-stop
         child
         (or (mevedel-execution-process--child-termination child) 'exited))
      (mevedel-execution-process--finish
       child (or (mevedel-execution-process--child-exit-code child) -1)))))

(defun mevedel-execution-process--settle-stop-main-exit (child)
  "Settle stopped remote CHILD early when its group already drained."
  (setf (mevedel-execution-process--child-settle-timer child) nil)
  (unless (mevedel-execution-process--child-finished-p child)
    (when (eq 'dead
              (condition-case nil
                  (mevedel-execution-process--remote-group-status child)
                (error nil)))
      (when-let* ((timer
                   (mevedel-execution-process--child-force-timer child)))
        (when (timerp timer) (cancel-timer timer)))
      (setf (mevedel-execution-process--child-force-timer child) nil)
      (mevedel-execution-process--finish
       child (or (mevedel-execution-process--child-exit-code child) -1)))))

(defun mevedel-execution-process--ended (child process)
  "Settle CHILD when its exact PROCESS reaches a terminal state."
  (when (eq process (mevedel-execution-process--child-process child))
    (let ((status (process-status process)))
      (when (memq status '(exit signal))
        (setf (mevedel-execution-process--child-exit-code child)
              (process-exit-status process))
        (when (and
               (not (mevedel-execution-process--child-termination child))
               (eq status 'signal))
          (setf (mevedel-execution-process--child-termination child)
                'signaled))
        (cond
         ((mevedel-execution-process--timer-pending-p
           (mevedel-execution-process--child-settle-timer child)))
         ((and (mevedel-execution-process--child-stop-p child)
               (mevedel-execution-process--timer-pending-p
                (mevedel-execution-process--child-force-timer child)))
          (setf (mevedel-execution-process--child-settle-timer child)
                (run-at-time
                 0.02 nil
                 (if (file-remote-p
                      (or (mevedel-execution-process--child-workdir child) ""))
                     #'mevedel-execution-process--settle-stop-main-exit
                   #'mevedel-execution-process--settle-main-exit)
                 child)))
         (t
          (setf (mevedel-execution-process--child-settle-timer child)
                (run-at-time
                 0.02 nil #'mevedel-execution-process--settle-main-exit
                 child))))))))

(cl-defun mevedel-execution-process-start
    (child &key name command target coding timeout confined)
  "Launch COMMAND in CHILD and return CHILD, or nil on failure."
  (let* ((workdir (mevedel-execution-process--child-workdir child))
         (remote (file-remote-p workdir))
         (command
          (mevedel-execution-process--localize-command
           command workdir target))
         (direct-async
          (and remote
               (mevedel-execution-process--direct-async-p
                child command workdir)))
         (_ (setf (mevedel-execution-process--child-direct-async-p child)
                  direct-async))
         (command
          (if remote
              (mevedel-execution-process--remote-command child command)
            command))
         (executable (car-safe command))
         (process-environment
          (mevedel-execution-process--process-environment remote)))
    (setf (mevedel-execution-process--child-confined-p child)
          (and confined t)
          (mevedel-execution-process--child-coding child) coding)
    (condition-case err
        (progn
          (with-temp-buffer
            (setq default-directory workdir)
            (unless (and (stringp executable)
                         (if remote
                             (executable-find executable remote)
                           (executable-find executable)))
              (signal 'file-missing (list "Executable not found" executable)))
            (setf (mevedel-execution-process--child-launch-attempted-p child)
                  t
                  (mevedel-execution-process--child-process child)
                  (mevedel-execution-process--with-spawn-channel
                   remote direct-async
                   (lambda ()
                     (make-process
                      :name name :buffer nil :command command :coding coding
                      :connection-type
                      (if (mevedel-execution-process--child-tty-p child)
                          'pty
                        'pipe)
                      :file-handler t
                      :filter
                      (lambda (_process chunk)
                        (mevedel-execution-process--filter-group-marker
                         child chunk))
                      :noquery t
                      :sentinel
                      (lambda (process _event)
                        (mevedel-execution-process--ended child process)))))))
          (unless remote
            (setf (mevedel-execution-process--child-group-id child)
                  (process-id
                   (mevedel-execution-process--child-process child))))
          (when (and
                 (not (mevedel-execution-process--child-tty-p child))
                 (process-live-p
                  (mevedel-execution-process--child-process child)))
            (process-send-eof
             (mevedel-execution-process--child-process child)))
          (setf (mevedel-execution-process--child-watch-timer child)
                (run-at-time
                 0.1 0.1
                 (lambda ()
                   (unless
                       (mevedel-execution-process--child-finished-p child)
                     (let ((process
                            (mevedel-execution-process--child-process child)))
                       (when (and
                              (processp process)
                              (memq (process-status process) '(exit signal)))
                         (mevedel-execution-process--ended child process)))))))
          (when timeout
            (setf (mevedel-execution-process--child-timeout-timer child)
                  (run-at-time
                   timeout nil #'mevedel-execution-process--time-out child)))
          child)
      (error
       (mevedel-execution-process--finish child -1 err)
       nil))))

(defun mevedel-execution-process-write (child input)
  "Write INPUT to live TTY CHILD."
  (let ((process (mevedel-execution-process--child-process child)))
    (when (process-live-p process)
      (process-send-string process input)
      t)))

(defun mevedel-execution-process-interrupt (child)
  "Interrupt CHILD through its supported process-group boundary."
  (unless (mevedel-execution-process--child-finished-p child)
    (mevedel-execution-process--signal child 'INT)
    t))

(provide 'mevedel-execution-process)
;;; mevedel-execution-process.el ends here
