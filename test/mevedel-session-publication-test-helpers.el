;;; mevedel-session-publication-test-helpers.el --- Warm publication fixtures -*- lexical-binding: t -*-

;;; Commentary:

;; Shared real-filesystem fixtures for asynchronous session publication and
;; execution acknowledgement tests.  Loading this file registers no tests.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "mevedel-session-test-support"))
(require 'mevedel-agent-control)
(require 'mevedel-agent-persistence)

(defun test-mevedel-publication-async--wait (predicate)
  "Wait at most ten seconds for PREDICATE, failing instead of hanging ERT."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(defun test-mevedel-publication-async--fixture (body)
  "Call BODY with a warm local portable SESSION and its root BUFFER."
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-async-review-" t)))
         (temporary-file-directory root)
         (timers (append timer-list timer-idle-list))
         (processes (process-list))
         (buffers (buffer-list))
         (mevedel-transport--pending (make-hash-table :test #'equal))
         (mevedel-session-publication--generation-cache (make-hash-table :test #'equal))
         (mevedel-session-publication--facts-cache (make-hash-table :test #'equal))
         (mevedel-session-persistence--deferred-agent-saves (make-hash-table :test #'eq))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "async-review" workspace))
         (buffer (generate-new-buffer " *mevedel-async-review*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (setq-local mevedel--session session)
          (insert "*** Original prompt\nOriginal response\n")
          (mevedel-session-set-root-buffer session buffer)
          (mevedel-session-artifacts-save session buffer nil t)
          (should (mevedel-session-publication-async-eligible-p session))
          (with-timeout (20 (ert-fail "Async publication fixture exceeded its deadline"))
            (funcall body session buffer)))
      ;; A deliberately failed test may leave a paused or unresolved job.
      ;; Detach callbacks before killing only this fixture's processes.
      (setf (mevedel-session-publication-job session) nil
            (mevedel-session-publication-preparations session) nil
            (mevedel-session-publication-active-p session) nil)
      (dolist (process (seq-difference (process-list) processes))
        (set-process-sentinel process #'ignore)
        (when (process-live-p process) (delete-process process)))
      (mevedel-transport-cancel-pending)
      (mevedel-session-persistence-cancel-deferred-agent-save session)
      (mevedel-session-durability--cancel-renewal session)
      (ignore-errors (mevedel-session-publication-clear-transient session))
      (dolist (timer (seq-difference (append timer-list timer-idle-list) timers))
        (cancel-timer timer))
      (dolist (created (seq-difference (buffer-list) buffers))
        (when (buffer-live-p created)
          (with-current-buffer created
            (setq-local kill-buffer-hook nil kill-buffer-query-functions nil)
            (set-buffer-modified-p nil))
          (kill-buffer created)))
      (mevedel-workspace-clear-registry)
      (delete-directory root t))))

(defun test-mevedel-publication-async--mail (session)
  "Return SESSION's live root mailbox in FIFO order."
  (reverse (mevedel-agent-control--mailbox-queue session "/root")))

(defun test-mevedel-publication-async--restored-mail (session)
  "Deserialize SESSION's authoritative on-disk sidecar and return root mail.
Never use the fixed discovery sidecar or this session's captured publication."
  (let* ((mevedel-session-publication--generation-cache (make-hash-table :test #'equal))
         (mevedel-session-publication--facts-cache (make-hash-table :test #'equal))
         (publication (mevedel-session-publication-read (mevedel-session-save-path session)))
         (raw (mevedel-session-persistence-load-sidecar (plist-get publication :sidecar)))
         (restored (mevedel-session-codec-deserialize raw (mevedel-session-workspace session))))
    (test-mevedel-publication-async--mail (plist-get restored :session))))

(defun test-mevedel-publication-async--ids (records)
  "Return the durable IDs of RECORDS in their current order."
  (mapcar (lambda (record) (plist-get record :id)) records))

(defun test-mevedel-publication-async--send (session id ack)
  "Submit execution ID through SESSION's production delivery entry point."
  (mevedel-agent-control-enqueue-execution-result
   session "/root" (concat "completed " id) id nil
   (list :execution_id id :outcome "success") ack))

(provide 'mevedel-session-publication-test-helpers)
;;; mevedel-session-publication-test-helpers.el ends here
