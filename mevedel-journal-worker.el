;;; mevedel-journal-worker.el --- Isolated journal maintenance -*- lexical-binding: t -*-

;;; Commentary:

;; Journal maintenance, consolidation storage and cold publication observations
;; run in a short-lived Emacs child. Frozen storage inputs cross the boundary;
;; the child uses the normal fenced APIs and never starts a model or loads init.
;; Provider configuration and live-buffer artifact retention stay in the editor.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-structs)

;; `mevedel-journal-capture'
(defvar mevedel-journal-enabled)

;; `mevedel-journal-cleanup'
(declare-function mevedel-journal-cleanup-expired "mevedel-journal-cleanup" (workspace &optional force))
(defvar mevedel-journal-cleanup--inhibit-scheduling)
(defvar mevedel-journal-cleanup--journal-only)
(defvar mevedel-journal-cleanup--more)

;; `mevedel-journal-process'
(defvar mevedel-journal-process--inhibit-scheduling)

;; `mevedel-journal-recovery'
(declare-function mevedel-journal-recovery-run "mevedel-journal-recovery" (workspace))

;; `mevedel-journal-store'
(defvar mevedel-journal-max-age-days)
(defvar mevedel-memory-history-max-age-days)

;; `mevedel-memory-pass'
(declare-function mevedel-memory-pass--prepare "mevedel-memory-pass" (workspace payload))
(declare-function mevedel-memory-pass--publish "mevedel-memory-pass" (workspace payload))
(defvar mevedel-memory-pass--inhibit-scheduling)

;; `mevedel-session-publication'
(declare-function mevedel-session-publication--cached-generation "mevedel-session-publication" (session-dir head))
(declare-function mevedel-session-publication--cached-sidecar-facts "mevedel-session-publication" (session-dir head generation))

;; `subr'
(defvar read-eval)

(defvar mevedel-journal-worker--child-p nil
  "Non-nil inside an isolated maintenance child.")

(defvar mevedel-journal-worker--processes nil
  "Maintenance children owned by this editor.")

(defvar mevedel-journal-worker--stopping nil
  "Non-nil while all maintenance children are being stopped on teardown.")

(defun mevedel-journal-worker-supported-p (workspace)
  "Return non-nil when WORKSPACE can use a local maintenance child."
  (and (not mevedel-journal-worker--child-p)
       (eq system-type 'gnu/linux)
       (not (file-remote-p (mevedel-workspace-root workspace)))))

(defun mevedel-journal-worker-start (workspace operation callback &optional force payload)
  "Run WORKSPACE OPERATION in a child and call CALLBACK with its result.
OPERATION is `recovery', `cleanup', `memory-prepare', `memory-publish', or
`generation-observations'.
FORCE bypasses cleanup throttling.  PAYLOAD freezes consolidation inputs.
Return the child process.  CALLBACK receives (:ok t :count N :more BOOL
:cleanup-at TIME), operation-specific :prepared/:accepted/:entry/:observations,
or (:error MESSAGE). It receives no transcript bodies.
Deleting the process cancels work; durable claims fence interrupted attempts."
  (unless (and (mevedel-journal-worker-supported-p workspace)
               (memq operation '(recovery cleanup memory-prepare memory-publish generation-observations)))
    (error "Unsupported journal worker operation"))
  (let* ((directory (make-temp-file "mevedel-journal-worker-" t))
         (request (file-name-concat directory "request.el"))
         (reply (file-name-concat directory "reply.el"))
         (root (mevedel-workspace-root workspace))
         (default-directory temporary-file-directory)
         process)
    (condition-case err
        (progn
          (with-temp-file request
            (let ((print-length nil) (print-level nil))
              (prin1 (list :root root :operation operation :force force
                           :cleanup-at (mevedel-workspace-journal-cleanup-at workspace)
                           :age mevedel-journal-max-age-days
                           :history-age mevedel-memory-history-max-age-days
                           :journal-enabled (bound-and-true-p mevedel-journal-enabled)
                           :payload payload)
                     (current-buffer))))
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
                   (when (memq (process-status child) '(exit signal))
                     (setq mevedel-journal-worker--processes
                           (delq child mevedel-journal-worker--processes))
                     (let ((result
                            (condition-case failure
                                (progn
                                  (unless (and (zerop (process-exit-status child))
                                               (file-exists-p reply)
                                               (<= (file-attribute-size (file-attributes reply))
                                                   (if (memq operation '(memory-prepare memory-publish generation-observations)) (* 8 1024 1024) 4096)))
                                    (error "Journal worker exited with status %s" (process-exit-status child)))
                                  (with-temp-buffer
                                    (insert-file-contents reply)
                                    (let ((read-eval nil)) (read (current-buffer)))))
                              (error (list :error (error-message-string failure))))))
                       (unwind-protect (funcall callback result)
                         (delete-directory directory t)))))))
          (push process mevedel-journal-worker--processes)
          (add-hook 'kill-emacs-hook #'mevedel-journal-worker-stop-all)
          process)
      (error
       (when (and process (process-live-p process)) (delete-process process))
       (when (file-directory-p directory) (delete-directory directory t))
       (signal (car err) (cdr err))))))

(defun mevedel-journal-worker-stop-all ()
  "Stop this editor's maintenance children without waiting for storage work."
  (let ((mevedel-journal-process--inhibit-scheduling t)
        (mevedel-journal-cleanup--inhibit-scheduling t)
        (mevedel-memory-pass--inhibit-scheduling t)
        (mevedel-journal-worker--stopping t))
    (dolist (process (copy-sequence mevedel-journal-worker--processes))
      (when (process-live-p process) (delete-process process)))))

(defun mevedel-journal-worker--run (request reply)
  "Execute private REQUEST in a batch child, writing a bounded REPLY."
  (unless noninteractive (error "Journal worker requires batch Emacs"))
  (require 'mevedel-journal-recovery)
  (require 'mevedel-journal-cleanup)
  (let* ((options (with-temp-buffer
                    (insert-file-contents request)
                    (let ((read-eval nil)) (read (current-buffer)))))
         (root (plist-get options :root))
         (workspace (mevedel-workspace--create :root root :id root :type 'project))
         (mevedel-journal-worker--child-p t)
         (mevedel-journal-enabled (plist-get options :journal-enabled))
         (mevedel-journal-max-age-days (plist-get options :age))
         (mevedel-memory-history-max-age-days (plist-get options :history-age))
         (mevedel-journal-process--inhibit-scheduling t)
         (mevedel-memory-pass--inhibit-scheduling t)
         (mevedel-journal-cleanup--inhibit-scheduling t)
         (mevedel-journal-cleanup--journal-only t)
         (mevedel-journal-cleanup--more nil)
         (result
          (condition-case err
              (progn
                (unless (and (stringp root) (file-name-absolute-p root) (not (file-remote-p root)))
                  (error "Journal worker needs a local absolute workspace"))
                (setf (mevedel-workspace-journal-cleanup-at workspace) (plist-get options :cleanup-at))
                (pcase (plist-get options :operation)
                  ((or 'memory-prepare 'memory-publish)
                   (require 'mevedel-memory-pass)
                   (if (eq (plist-get options :operation) 'memory-prepare)
                       (list :ok t :prepared
                             (mevedel-memory-pass--prepare workspace (plist-get options :payload)))
                     (mevedel-memory-pass--publish workspace (plist-get options :payload))))
                  ('generation-observations
                   (require 'mevedel-session-publication)
                   (let* ((payload (plist-get options :payload))
                          (directory (plist-get payload :directory)))
                     (unless (and (stringp directory) (file-name-absolute-p directory)
                                  (not (file-remote-p directory)))
                       (error "Publication observations require a local absolute directory"))
                     (list :ok t :observations
                           (mapcar
                            (lambda (generation)
                              (let* ((head (plist-get generation :head))
                                     (observed (mevedel-session-publication--cached-generation directory head)))
                                (list head observed
                                      (and observed (mevedel-session-publication--cached-sidecar-facts
                                                     directory head observed)))))
                            (plist-get payload :generations)))))
                  (_
                   (let ((count
                          (pcase (plist-get options :operation)
                            ('recovery (length (mevedel-journal-recovery-run workspace)))
                            ('cleanup (mevedel-journal-cleanup-expired workspace (plist-get options :force)))
                            (_ (error "Unknown journal worker operation")))))
                     (list :ok t :count count :more mevedel-journal-cleanup--more
                           :cleanup-at (mevedel-workspace-journal-cleanup-at workspace))))))
            (error (list :error (truncate-string-to-width (error-message-string err) 1000))))))
    (with-temp-file reply
      (let ((print-length nil) (print-level nil)) (prin1 result (current-buffer))))))

(provide 'mevedel-journal-worker)
;;; mevedel-journal-worker.el ends here
