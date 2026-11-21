;;; niwa-journal-worker.el --- Isolated journal maintenance -*- lexical-binding: t -*-

;;; Commentary:

;; Journal maintenance, consolidation storage and cold publication observations
;; run in a short-lived Emacs child. Frozen storage inputs cross the boundary;
;; the child uses the normal fenced APIs and never starts a model or loads init.
;; Provider configuration and live-buffer artifact retention stay in the editor.
;; Editor-side alternatives belong to callers: yielding cleanup, digest
;; admission and collection slices deliberately do not share a placement adapter.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-structs)
(require 'niwa-transport)

;; `niwa-journal-capture'
(defvar niwa-journal-enabled)

;; `niwa-journal-cleanup'
(defvar niwa-journal-cleanup--inhibit-scheduling)

;; `niwa-journal-process'
(defvar niwa-journal-process--inhibit-scheduling)

;; `niwa-journal-store'
(defvar niwa-journal-max-age-days)
(defvar niwa-memory-history-max-age-days)

;; `niwa-memory-pass'
(defvar niwa-memory-pass--inhibit-scheduling)

;; `niwa-utilities'
(declare-function niwa--emacs-executable "niwa-utilities" ())
(declare-function niwa-diagnostic-warning "niwa-utilities" (scope type message &optional level buffer-name))
(autoload 'niwa--emacs-executable "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")

;; `subr'
(defvar read-eval)

(defvar niwa-journal-worker--child-p nil
  "Non-nil inside an isolated maintenance child.")

(defvar niwa-journal-worker--processes nil
  "Maintenance children owned by this editor.")

(defvar niwa-journal-worker--stopping nil
  "Non-nil while all maintenance children are being stopped on teardown.")

(defconst niwa-journal-worker--operations
  '((capture-prepare niwa-journal-capture niwa-journal-capture-prepare-child 8388608)
    (digest-prepare niwa-journal-process niwa-journal-process-prepare-child 4096)
    (recovery niwa-journal-recovery niwa-journal-recovery-child 4096)
    (cleanup niwa-journal-cleanup niwa-journal-cleanup-child 4096)
    (memory-recover niwa-memory-decision niwa-memory-decision-recover-child selection)
    (memory-decision-publish niwa-memory-decision niwa-memory-decision-publish-child 4096)
    (memory-prepare niwa-memory-pass niwa-memory-pass-prepare-child 8388608)
    (memory-publish niwa-memory-pass niwa-memory-pass-publish-child 8388608)
    (generation-observations niwa-session-publication niwa-session-publication-observe-child 8388608)
    (memory-display niwa-memory-list niwa-memory-list-scan-child 8388608))
  "Closed operation table: name, owner feature, public entry and reply bound.")

(defun niwa-journal-worker--reply-limit (entry payload)
  "Return ENTRY's reply byte bound for frozen PAYLOAD."
  (let ((policy (nth 3 entry)))
    (if (eq policy 'selection)
        (if (or (plist-get payload :application) (plist-get payload :pending-selection)
                (plist-get payload :write-selection)) 8388608 4096)
      policy)))

(defun niwa-journal-worker--write (file value)
  "Print VALUE to FILE for the other side of a worker exchange.
The coding is explicit and lossless: detection would read a file holding a NUL
byte, such as tool output from a binary file, as raw bytes and turn every
non-ASCII string into unibyte bytes."
  (let ((coding-system-for-write 'utf-8-emacs-unix))
    (with-temp-file file
      (let ((print-length nil) (print-level nil))
        (prin1 value (current-buffer))))))

(defun niwa-journal-worker--read (file)
  "Read the value `niwa-journal-worker--write' printed to FILE."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-emacs-unix))
      (insert-file-contents file))
    (let ((read-eval nil)) (read (current-buffer)))))

(defun niwa-journal-worker-local-p (workspace &optional roots)
  "Return non-nil when WORKSPACE and path strings ROOTS allow a local child."
  (and (not niwa-journal-worker--child-p)
       (eq system-type 'gnu/linux)
       (not (file-remote-p (niwa-workspace-root workspace)))
       (not (cl-some #'file-remote-p roots))))

(cl-defun niwa-journal-worker-start (workspace operation payload callback &key owner)
  "Run WORKSPACE OPERATION in a child and call CALLBACK with its result.
OPERATION is `capture-prepare', `digest-prepare', `recovery', `cleanup',
`memory-recover', `memory-decision-publish', `memory-prepare',
`memory-publish', `generation-observations', or `memory-display'.
PAYLOAD freezes owner inputs; cleanup's :force bypasses its throttle.
OWNER, when non-nil, is the parent lifecycle's mutable job plist.  Record its
:worker before pending quit delivery; that owner fences and retires the child
even if setup subsequently signals.  This handoff executes no callbacks.
Return the child process.  CALLBACK receives (:ok t :count N :more BOOL
:cleanup-at TIME), operation-specific :prepared/:accepted/:entry/:observations,
or (:error MESSAGE). Capture preparation returns bounded frozen evidence.
Launch failure signals without calling CALLBACK.  Completion is delivered once
through idle transport, never in the sentinel or before this function returns.
Explicit cancellation through `niwa-journal-worker-cancel' drops delivery;
owners must first fence mutation claims.  Unsolicited signals report errors."
  (unless (and (niwa-journal-worker-local-p workspace)
               (assq operation niwa-journal-worker--operations))
    (error "Unsupported journal worker operation"))
  (require 'niwa-journal-store)
  (let* ((directory (make-temp-file "niwa-journal-worker-" t))
         (request (file-name-concat directory "request.el"))
         (reply (file-name-concat directory "reply.el"))
         (root (niwa-workspace-root workspace))
         (default-directory temporary-file-directory)
         (limit (niwa-journal-worker--reply-limit
                 (assq operation niwa-journal-worker--operations) payload))
         process returned queued done aborted)
    (condition-case err
        (progn
          (niwa-journal-worker--write
           request
           (list :root root :operation operation
                 :cleanup-at (niwa-workspace-journal-cleanup-at workspace)
                 :age niwa-journal-max-age-days
                 :history-age niwa-memory-history-max-age-days
                 :journal-enabled (bound-and-true-p niwa-journal-enabled)
                 :payload payload))
          (add-hook 'kill-emacs-hook #'niwa-journal-worker-stop-all)
          ;; Request IO above remains interruptible.  Only local launch and
          ;; ownership handoff are indivisible with respect to pending C-g.
          (let ((inhibit-quit t))
            (setq process
                  (make-process
                   :name "niwa-journal-worker" :noquery t :connection-type 'pipe
                   :buffer nil
                   :command
                   (list (niwa--emacs-executable)
			 "--batch" "-Q" "--eval"
			 (let ((print-length nil) (print-level nil))
                           (prin1-to-string `(setq load-path ',load-path)))
			 "-l" (locate-library "niwa-journal-worker")
			 "--eval" (prin1-to-string `(niwa-journal-worker--run ,request ,reply)))
                   :sentinel
                   (lambda (child _event)
                     (when (and (not queued) (memq (process-status child) '(exit signal)))
                       (setq queued t)
                       (if (process-get child 'niwa-journal-worker-cancelled)
                           (progn
                             (setq done t niwa-journal-worker--processes
                                   (delq child niwa-journal-worker--processes))
                             (when (file-directory-p directory) (delete-directory directory t)))
                         (let ((result
                                (condition-case failure
                                    (progn
                                      (unless (and (zerop (process-exit-status child))
                                                   (file-exists-p reply)
                                                   (<= (file-attribute-size (file-attributes reply)) limit))
                                        (error "Journal worker exited with status %s" (process-exit-status child)))
                                      (niwa-journal-worker--read reply))
                                  (error (list :error (error-message-string failure))))))
                           (cl-labels
                               ((deliver ()
                                  (unless done
                                    (if (not returned)
                                        (niwa-transport-run-at-time 0 #'deliver)
                                      (setq done t
                                            niwa-journal-worker--processes
                                            (delq child niwa-journal-worker--processes))
                                      (unwind-protect
                                          (when (file-directory-p directory) (delete-directory directory t))
                                        (when (and (not aborted)
                                                   (not (process-get child 'niwa-journal-worker-cancelled)))
                                          (funcall callback result))))))
                                (fallback () (niwa-transport-run-at-time 0 #'deliver)))
                             (unless (niwa-transport-run-when-idle
                                      child root #'deliver #'fallback 0)
                               (fallback)))))))))
            (push process niwa-journal-worker--processes)
            (process-put process 'niwa-journal-worker-directory directory)
            (process-put process 'niwa-journal-worker-root root)
            (when owner (plist-put owner :worker process)))
          ;; Interpreted code need not deliver a pending quit at this binding's
          ;; exit.  Deliver it inside the setup handler, after ownership handoff,
          ;; rather than letting it escape at a later caller/event-loop check.
          (when (and quit-flag (not inhibit-quit))
            (setq quit-flag nil)
            (signal 'quit nil))
          (setq returned t)
          process)
      ((error quit)
       ;; An owned lifecycle now has the handle and must prove its exact-token
       ;; fence before deletion.  Never preempt that authority in this helper.
       (setq aborted t returned t)
       (unless (and process owner)
         (when process (niwa-journal-worker-cancel process))
         (when (file-directory-p directory) (delete-directory directory t)))
       (signal (car err) (cdr err))))))

(defun niwa-journal-worker--stop (process)
  "Stop PROCESS and its synchronous subprocess groups before returning.
Local `process-file' children lead separate groups, so deleting only the
worker (or signalling only its group) leaves their filesystem work alive."
  (when (process-live-p process)
    (let ((inhibit-quit t)
          (pid (process-id process))
          (until (+ (float-time) 2))
          groups)
      (unwind-protect
          (progn
            ;; Freeze launch/reaping before observing direct children.  Their
            ;; non-job-control shells keep all shell descendants in one group.
            (signal-process pid 'SIGSTOP)
            (while (and (process-live-p process)
                        (not (member (alist-get 'state (process-attributes pid)) '("T" "Z")))
                        (< (float-time) until))
              (accept-process-output process .01 nil 1))
            (when (and (process-live-p process)
                       (not (member (alist-get 'state (process-attributes pid)) '("T" "Z"))))
              (error "Journal worker did not stop"))
            (dolist (child (list-system-processes))
              (let ((attributes (process-attributes child)))
                (when (eql pid (alist-get 'ppid attributes))
                  ;; Both synchronous and pipe children are group leaders.
                  ;; Never signal a group we cannot prove is this child's.
                  (unless (eql child (alist-get 'pgrp attributes))
                    (error "Journal subprocess is not a group leader"))
                  (push child groups)
                  (signal-process (- child) 'SIGKILL))))
            ;; SIGKILL can still be waiting on a syscall.  Establish quiescence,
            ;; not merely signal delivery; zombies cannot publish anything.
            (cl-labels ((running-p ()
                          (cl-some
                           (lambda (child)
                             (let ((attributes (process-attributes child)))
                               (and (memq (alist-get 'pgrp attributes) groups)
                                    (not (equal "Z" (alist-get 'state attributes))))))
                           (list-system-processes))))
              (while (and (running-p) (< (float-time) until))
                (accept-process-output process .01 nil 1))
              (when (running-p) (error "Journal subprocesses did not stop"))))
        (when (process-live-p process) (delete-process process))))))

(defun niwa-journal-worker-cancel (process &optional signal-errors)
  "Cancel PROCESS and queued delivery after stopping its filesystem children.
Return t only when its synchronous subprocess groups cannot publish late writes.
A two-second shutdown deadline reports a scoped warning and returns nil instead
of interrupting owner retirement.  Internal bulk callers use SIGNAL-ERRORS to
collect errors and re-signal the first after attempting every worker.
Retire the private exchange and launcher registration once the worker is dead.
Failure to delete a still-live worker signals and retains its state for retry.
Owners must fence mutation claims before calling this function.  Cancellation
is explicit: an unsolicited process signal remains a deferred error reply."
  (process-put process 'niwa-journal-worker-cancelled t)
  (let (failure)
    (condition-case err (niwa-journal-worker--stop process)
      (error (setq failure err)))
    ;; A live worker still needs its retry handle.  Do not turn a failed
    ;; parent deletion into a quiescence warning or discard its exchange.
    (when (and failure (process-live-p process))
      (signal (car failure) (cdr failure)))
    (when-let* ((directory (process-get process 'niwa-journal-worker-directory))
                ((file-directory-p directory)))
      (delete-directory directory t))
    (setq niwa-journal-worker--processes (delq process niwa-journal-worker--processes))
    (cond
     ((not failure) t)
     (signal-errors (signal (car failure) (cdr failure)))
     (t
       (niwa-diagnostic-warning
        (or (process-get process 'niwa-journal-worker-root) 'unavailable)
        'niwa (error-message-string failure) :warning)
       nil))))

(defun niwa-journal-worker-stop-all ()
  "Stop this editor's maintenance children and their filesystem subprocesses.
Do not finish queued work or wait for a model; use bounded forced shutdown.
Attempt every worker before re-signalling the first shutdown error."
  (let ((niwa-journal-process--inhibit-scheduling t)
        (niwa-journal-cleanup--inhibit-scheduling t)
        (niwa-memory-pass--inhibit-scheduling t)
        (niwa-journal-worker--stopping t)
        failure)
    (dolist (process (copy-sequence niwa-journal-worker--processes))
      (condition-case err
          (niwa-journal-worker-cancel process t)
        (error (unless failure (setq failure err)))))
    (when failure (signal (car failure) (cdr failure)))))

(defun niwa-journal-worker--run (request reply)
  "Execute private REQUEST in a batch child, writing a bounded REPLY."
  (unless noninteractive (error "Journal worker requires batch Emacs"))
  (let* ((options (niwa-journal-worker--read request))
         (root (plist-get options :root))
         (entry (assq (plist-get options :operation) niwa-journal-worker--operations))
         (workspace (niwa-workspace--create :root root :id root :type 'project))
         (niwa-journal-worker--child-p t)
         (niwa-journal-enabled (plist-get options :journal-enabled))
         (niwa-journal-max-age-days (plist-get options :age))
         (niwa-memory-history-max-age-days (plist-get options :history-age))
         (niwa-journal-process--inhibit-scheduling t)
         (niwa-memory-pass--inhibit-scheduling t)
         (niwa-journal-cleanup--inhibit-scheduling t)
         (result
          (condition-case err
              (progn
                (unless (and (stringp root) (file-name-absolute-p root) (not (file-remote-p root)))
                  (error "Journal worker needs a local absolute workspace"))
                (unless entry (error "Unknown journal worker operation"))
                (unless (and (proper-list-p options) (zerop (% (length options) 2)))
                  (error "Malformed journal worker request"))
                (let ((fields options) seen)
                  (while fields
                    (let ((key (pop fields)))
                      (unless (and (memq key '(:root :operation :cleanup-at :age :history-age :journal-enabled :payload))
                                   (not (memq key seen)))
                        (error "Unsupported journal worker request field: %S" key))
                      (push key seen)
                      (pop fields))))
                (setf (niwa-workspace-journal-cleanup-at workspace) (plist-get options :cleanup-at))
                (require (nth 1 entry))
                (funcall (nth 2 entry) workspace (plist-get options :payload)))
            (error (list :error (truncate-string-to-width (error-message-string err) 1000))))))
    (niwa-journal-worker--write reply result)))

(provide 'niwa-journal-worker)
;;; niwa-journal-worker.el ends here
