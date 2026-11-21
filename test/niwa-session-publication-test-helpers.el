;;; niwa-session-publication-test-helpers.el --- Warm publication fixtures -*- lexical-binding: t -*-

;;; Commentary:

;; Shared real-filesystem fixtures for asynchronous session publication and
;; execution acknowledgement tests.  Loading this file registers no tests.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "niwa-session-test-support"))
(require 'niwa-agent-control)
(require 'niwa-agent-persistence)

(defun test-niwa-publication-async--wait (predicate)
  "Wait at most ten seconds for PREDICATE, failing instead of hanging ERT."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(defun test-niwa-publication-async--fixture (body)
  "Call BODY with a warm local portable SESSION and its root BUFFER."
  (let* ((root (file-name-as-directory (make-temp-file "niwa-async-review-" t)))
         (temporary-file-directory root)
         (timers (append timer-list timer-idle-list))
         (processes (process-list))
         (buffers (buffer-list))
         (niwa-transport--pending (make-hash-table :test #'equal))
         (niwa-session-publication--generation-cache (make-hash-table :test #'equal))
         (niwa-session-publication--facts-cache (make-hash-table :test #'equal))
         (niwa-session-persistence--deferred-agent-saves (make-hash-table :test #'eq))
         (workspace (test-niwa-session-persistence--make-workspace root))
         (session (niwa-session-create "async-review" workspace))
         (buffer (generate-new-buffer " *niwa-async-review*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (setq-local niwa--session session)
          (insert "*** Original prompt\nOriginal response\n")
          (niwa-session-set-root-buffer session buffer)
          (niwa-session-artifacts-save session buffer nil t)
          (should (niwa-session-publication-async-eligible-p session))
          (with-timeout (20 (ert-fail "Async publication fixture exceeded its deadline"))
            (funcall body session buffer)))
      ;; A deliberately failed test may leave a paused or unresolved job.
      ;; Detach callbacks before killing only this fixture's processes.
      (setf (niwa-session-publication-job session) nil
            (niwa-session-publication-preparations session) nil
            (niwa-session-publication-active-p session) nil)
      (dolist (process (seq-difference (process-list) processes))
        (set-process-sentinel process #'ignore)
        (when (process-live-p process) (delete-process process)))
      (niwa-transport-cancel-pending)
      (niwa-session-persistence-cancel-deferred-agent-save session)
      (niwa-session-durability--cancel-renewal session)
      (ignore-errors (niwa-session-publication-clear-transient session))
      (dolist (timer (seq-difference (append timer-list timer-idle-list) timers))
        (cancel-timer timer))
      (dolist (created (seq-difference (buffer-list) buffers))
        (when (buffer-live-p created)
          (with-current-buffer created
            (setq-local kill-buffer-hook nil kill-buffer-query-functions nil)
            (set-buffer-modified-p nil))
          (kill-buffer created)))
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(defun test-niwa-publication-async--mail (session)
  "Return SESSION's live root mailbox in FIFO order."
  (reverse (niwa-agent-control--mailbox-queue session "/root")))

(defun test-niwa-publication-async--restored-mail (session)
  "Deserialize SESSION's authoritative on-disk sidecar and return root mail.
Never use the fixed discovery sidecar or this session's captured publication."
  (let* ((niwa-session-publication--generation-cache (make-hash-table :test #'equal))
         (niwa-session-publication--facts-cache (make-hash-table :test #'equal))
         (publication (niwa-session-publication-read (niwa-session-save-path session)))
         (raw (niwa-session-persistence-load-sidecar (plist-get publication :sidecar)))
         (restored (niwa-session-codec-deserialize raw (niwa-session-workspace session))))
    (test-niwa-publication-async--mail (plist-get restored :session))))

(defun test-niwa-publication-async--ids (records)
  "Return the durable IDs of RECORDS in their current order."
  (mapcar (lambda (record) (plist-get record :id)) records))

(defun test-niwa-publication-async--send (session id ack)
  "Submit execution ID through SESSION's production delivery entry point."
  (niwa-agent-control-enqueue-execution-result
   session "/root" (concat "completed " id) id nil
   (list :execution_id id :outcome "success") ack))

(provide 'niwa-session-publication-test-helpers)
;;; niwa-session-publication-test-helpers.el ends here
