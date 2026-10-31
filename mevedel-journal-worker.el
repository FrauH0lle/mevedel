;;; mevedel-journal-worker.el --- Isolated journal maintenance -*- lexical-binding: t -*-

;;; Commentary:

;; Journal maintenance, consolidation storage and cold publication observations
;; run in a short-lived Emacs child. Frozen storage inputs cross the boundary;
;; the child uses the normal fenced APIs and never starts a model or loads init.
;; Provider configuration and live-buffer artifact retention stay in the editor.
;; Editor-side alternatives belong to callers: yielding cleanup, digest
;; admission and collection slices deliberately do not share a placement adapter.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-structs)
(require 'mevedel-transport)

;; `mevedel-journal-capture'
(defvar mevedel-journal-enabled)

;; `mevedel-journal-cleanup'
(defvar mevedel-journal-cleanup--inhibit-scheduling)

;; `mevedel-journal-process'
(defvar mevedel-journal-process--inhibit-scheduling)

;; `mevedel-journal-store'
(defvar mevedel-journal-max-age-days)
(defvar mevedel-memory-history-max-age-days)

;; `mevedel-memory-pass'
(defvar mevedel-memory-pass--inhibit-scheduling)

;; `subr'
(defvar read-eval)

(defvar mevedel-journal-worker--child-p nil
  "Non-nil inside an isolated maintenance child.")

(defvar mevedel-journal-worker--processes nil
  "Maintenance children owned by this editor.")

(defvar mevedel-journal-worker--stopping nil
  "Non-nil while all maintenance children are being stopped on teardown.")

(defconst mevedel-journal-worker--operations
  '((capture-prepare mevedel-journal-capture mevedel-journal-capture-prepare-child 8388608)
    (digest-prepare mevedel-journal-process mevedel-journal-process-prepare-child 4096)
    (recovery mevedel-journal-recovery mevedel-journal-recovery-child 4096)
    (cleanup mevedel-journal-cleanup mevedel-journal-cleanup-child 4096)
    (memory-recover mevedel-memory-decision mevedel-memory-decision-recover-child selection)
    (memory-decision-publish mevedel-memory-decision mevedel-memory-decision-publish-child 4096)
    (memory-prepare mevedel-memory-pass mevedel-memory-pass-prepare-child 8388608)
    (memory-publish mevedel-memory-pass mevedel-memory-pass-publish-child 8388608)
    (generation-observations mevedel-session-publication mevedel-session-publication-observe-child 8388608)
    (memory-display mevedel-memory-list mevedel-memory-list-scan-child 8388608))
  "Closed operation table: name, owner feature, public entry and reply bound.")

(defun mevedel-journal-worker--reply-limit (entry payload)
  "Return ENTRY's reply byte bound for frozen PAYLOAD."
  (let ((policy (nth 3 entry)))
    (if (eq policy 'selection)
        (if (or (plist-get payload :application) (plist-get payload :pending-selection)
                (plist-get payload :write-selection)) 8388608 4096)
      policy)))

(defun mevedel-journal-worker--write (file value)
  "Print VALUE to FILE for the other side of a worker exchange.
The coding is explicit and lossless: detection would read a file holding a NUL
byte, such as tool output from a binary file, as raw bytes and turn every
non-ASCII string into unibyte bytes."
  (let ((coding-system-for-write 'utf-8-emacs-unix))
    (with-temp-file file
      (let ((print-length nil) (print-level nil))
        (prin1 value (current-buffer))))))

(defun mevedel-journal-worker--read (file)
  "Read the value `mevedel-journal-worker--write' printed to FILE."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-emacs-unix))
      (insert-file-contents file))
    (let ((read-eval nil)) (read (current-buffer)))))

(defun mevedel-journal-worker-local-p (workspace &optional roots)
  "Return non-nil when WORKSPACE and path strings ROOTS allow a local child."
  (and (not mevedel-journal-worker--child-p)
       (eq system-type 'gnu/linux)
       (not (file-remote-p (mevedel-workspace-root workspace)))
       (not (cl-some #'file-remote-p roots))))

(cl-defun mevedel-journal-worker-start (workspace operation payload callback &key owner)
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
Deleting a live process cancels delivery; owners must first fence mutation
claims."
  (unless (and (mevedel-journal-worker-local-p workspace)
               (assq operation mevedel-journal-worker--operations))
    (error "Unsupported journal worker operation"))
  (require 'mevedel-journal-store)
  (let* ((directory (make-temp-file "mevedel-journal-worker-" t))
         (request (file-name-concat directory "request.el"))
         (reply (file-name-concat directory "reply.el"))
         (root (mevedel-workspace-root workspace))
         (default-directory temporary-file-directory)
         (limit (mevedel-journal-worker--reply-limit
                 (assq operation mevedel-journal-worker--operations) payload))
         process returned queued done aborted)
    (condition-case err
        (progn
          (mevedel-journal-worker--write
           request
           (list :root root :operation operation
                 :cleanup-at (mevedel-workspace-journal-cleanup-at workspace)
                 :age mevedel-journal-max-age-days
                 :history-age mevedel-memory-history-max-age-days
                 :journal-enabled (bound-and-true-p mevedel-journal-enabled)
                 :payload payload))
          (add-hook 'kill-emacs-hook #'mevedel-journal-worker-stop-all)
          ;; Request IO above remains interruptible.  Only local launch and
          ;; ownership handoff are indivisible with respect to pending C-g.
          (let ((inhibit-quit t))
            (setq process
                  (make-process
                   :name "mevedel-journal-worker" :noquery t :connection-type 'pipe
                   :buffer nil
                   :command
                   (list (expand-file-name invocation-name invocation-directory)
			 "--batch" "-Q" "--eval"
			 (let ((print-length nil) (print-level nil))
                           (prin1-to-string `(setq load-path ',load-path)))
			 "-l" (locate-library "mevedel-journal-worker")
			 "--eval" (prin1-to-string `(mevedel-journal-worker--run ,request ,reply)))
                   :sentinel
                   (lambda (child _event)
                     (when (and (not queued) (memq (process-status child) '(exit signal)))
                       (setq queued t)
                       (if (eq (process-status child) 'signal)
                           (progn
                             (setq done t mevedel-journal-worker--processes
                                   (delq child mevedel-journal-worker--processes))
                             (when (file-directory-p directory) (delete-directory directory t)))
                         (let ((result
                                (condition-case failure
                                    (progn
                                      (unless (and (zerop (process-exit-status child))
                                                   (file-exists-p reply)
                                                   (<= (file-attribute-size (file-attributes reply)) limit))
                                        (error "Journal worker exited with status %s" (process-exit-status child)))
                                      (mevedel-journal-worker--read reply))
                                  (error (list :error (error-message-string failure))))))
                           (cl-labels
                               ((deliver ()
                                  (unless done
                                    (if (not returned)
                                        (mevedel-transport-run-at-time 0 #'deliver)
                                      (setq done t
                                            mevedel-journal-worker--processes
                                            (delq child mevedel-journal-worker--processes))
                                      (unwind-protect
                                          (when (file-directory-p directory) (delete-directory directory t))
                                        (when (and (not aborted) (eq (process-status child) 'exit)
                                                   (not (process-get child 'mevedel-journal-worker-cancelled)))
                                          (funcall callback result))))))
                                (fallback () (mevedel-transport-run-at-time 0 #'deliver)))
                             (unless (mevedel-transport-run-when-idle
                                      child root #'deliver #'fallback 0)
                               (fallback)))))))))
            (push process mevedel-journal-worker--processes)
            (process-put process 'mevedel-journal-worker-directory directory)
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
         (when (and process (process-live-p process)) (delete-process process))
         (when (file-directory-p directory) (delete-directory directory t)))
       (signal (car err) (cdr err))))))

(defun mevedel-journal-worker-stop-all ()
  "Stop this editor's maintenance children without waiting for storage work."
  (let ((mevedel-journal-process--inhibit-scheduling t)
        (mevedel-journal-cleanup--inhibit-scheduling t)
        (mevedel-memory-pass--inhibit-scheduling t)
        (mevedel-journal-worker--stopping t))
    (dolist (process (copy-sequence mevedel-journal-worker--processes))
      (process-put process 'mevedel-journal-worker-cancelled t)
      (delete-process process)
      (when-let* ((directory (process-get process 'mevedel-journal-worker-directory))
                  ((file-directory-p directory)))
        (delete-directory directory t))
      (setq mevedel-journal-worker--processes (delq process mevedel-journal-worker--processes)))))

(defun mevedel-journal-worker--run (request reply)
  "Execute private REQUEST in a batch child, writing a bounded REPLY."
  (unless noninteractive (error "Journal worker requires batch Emacs"))
  (let* ((options (mevedel-journal-worker--read request))
         (root (plist-get options :root))
         (entry (assq (plist-get options :operation) mevedel-journal-worker--operations))
         (workspace (mevedel-workspace--create :root root :id root :type 'project))
         (mevedel-journal-worker--child-p t)
         (mevedel-journal-enabled (plist-get options :journal-enabled))
         (mevedel-journal-max-age-days (plist-get options :age))
         (mevedel-memory-history-max-age-days (plist-get options :history-age))
         (mevedel-journal-process--inhibit-scheduling t)
         (mevedel-memory-pass--inhibit-scheduling t)
         (mevedel-journal-cleanup--inhibit-scheduling t)
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
                (setf (mevedel-workspace-journal-cleanup-at workspace) (plist-get options :cleanup-at))
                (require (nth 1 entry))
                (funcall (nth 2 entry) workspace (plist-get options :payload)))
            (error (list :error (truncate-string-to-width (error-message-string err) 1000))))))
    (mevedel-journal-worker--write reply result)))

(provide 'mevedel-journal-worker)
;;; mevedel-journal-worker.el ends here
