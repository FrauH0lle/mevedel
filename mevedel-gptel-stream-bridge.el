;;; mevedel-gptel-stream-bridge.el -- gptel stream compatibility -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the version-sensitive advice between gptel's private streaming
;; lifecycle and mevedel data buffers.  View progress and redraw scheduling
;; remain in mevedel-view-stream.el.

;;; Code:

;; `gptel'
(declare-function gptel-curl--stream-filter
                  "ext:gptel-request" (process output))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel--request-alist)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-path "mevedel-agents" (cl-x))

;; `mevedel-structs'
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-current-session
                  "mevedel-telemetry" (&optional buffer))
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-utilities'
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))

(defcustom mevedel-gptel-stream-bridge-insert-batch-delay 0.04
  "Seconds to batch consecutive string stream inserts in data buffers.

When positive, mevedel coalesces adjacent plain text stream chunks before
letting gptel insert them into the authoritative transcript buffer.  nil
or zero disables batching and preserves immediate insertion."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'mevedel)
(defvar mevedel-gptel-stream-bridge--gptel-stream-advice-installed nil
  "Non-nil when mevedel's gptel stream repair advice should be active.")

(defconst mevedel-gptel-stream-bridge--gptel-stream-filter-max-retries 100
  "Maximum deferred flush attempts for early gptel stream chunks.")

(defvar mevedel-gptel-stream-bridge--insert-batching-suspended nil
  "Non-nil means nested gptel stream insert calls should not batch.")

(defun mevedel-gptel-stream-bridge--gptel-data-buffer (buffer)
  "Return BUFFER's mevedel data buffer, or nil when BUFFER is unrelated.
If BUFFER is a view buffer, return its backing data buffer.  If BUFFER
already is a mevedel data buffer, return BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((and (derived-mode-p 'mevedel-view-mode)
             (boundp 'mevedel--data-buffer)
             mevedel--data-buffer
             (buffer-live-p mevedel--data-buffer))
        mevedel--data-buffer)
       ((and (boundp 'mevedel--view-buffer)
             mevedel--view-buffer
             (buffer-live-p mevedel--view-buffer)
             (with-current-buffer mevedel--view-buffer
               (and (derived-mode-p 'mevedel-view-mode)
                    (eq mevedel--data-buffer buffer))))
        buffer)))))

(defun mevedel-gptel-stream-bridge--live-marker-p (marker)
  "Return non-nil when MARKER points into a live buffer."
  (and (markerp marker)
       (marker-position marker)
       (buffer-live-p (marker-buffer marker))))

(defun mevedel-gptel-stream-bridge--gptel-stream-info-p (info)
  "Return non-nil when INFO belongs to a mevedel gptel stream."
  (when-let* ((buffer (and (consp info) (plist-get info :buffer)))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (or (bound-and-true-p mevedel--session)
          (mevedel-gptel-stream-bridge--gptel-data-buffer buffer)))))

(defun mevedel-gptel-stream-bridge--repair-gptel-stream-info (info)
  "Repair detached stream markers in gptel INFO when it belongs to mevedel.

gptel's streaming insertion path expects `:position' to point somewhere
and calls `goto-char' on it before mevedel gets control back.  A detached
marker can happen after request teardown or buffer reconstruction races.
For mevedel streams, recover by appending future chunks to the data buffer
and clear stale tracking markers so gptel reinitializes them."
  (when (mevedel-gptel-stream-bridge--gptel-stream-info-p info)
    (let ((buffer (plist-get info :buffer)))
      (mevedel-gptel-stream-bridge--wrap-gptel-stream-transformer info)
      (unless (mevedel-gptel-stream-bridge--live-marker-p (plist-get info :position))
        (with-current-buffer buffer
          (plist-put info :position (copy-marker (point-max) nil))))
      (dolist (key '(:tracking-marker :reasoning-marker))
        (let ((marker (plist-get info key)))
          (when (and marker
                     (not (mevedel-gptel-stream-bridge--live-marker-p marker)))
            (plist-put info key nil))))))
  info)

(defun mevedel-gptel-stream-bridge--wrap-gptel-stream-transformer (info)
  "Wrap INFO's stream transformer so stale cleanup does not signal.

gptel's streaming Org converter owns an internal temporary buffer.  In
some teardown orders that buffer is killed before the final callback
reuses the transformer.  Let the response finish by returning the raw
chunk when that stale transformer fails."
  (when-let* ((transformer (and (consp info)
                                (plist-get info :transformer)))
              ((functionp transformer))
              ((not (plist-get info :mevedel-transformer-wrapped))))
    (plist-put info :mevedel-transformer-wrapped t)
    (plist-put
     info :transformer
     (lambda (str)
       (condition-case err
           (funcall transformer str)
         (error
          (mevedel--warn-once
           'stream-bridge-transformer
           "Ignoring stale gptel stream transformer: %s"
           (error-message-string err))
          str))))))

(defun mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
    (orig-fn response info &optional raw)
  "Repair mevedel stream INFO before invoking ORIG-FN with RESPONSE."
  (mevedel-gptel-stream-bridge--repair-gptel-stream-info info)
  (cond
   ((and (stringp response)
         (not raw)
         (mevedel-gptel-stream-bridge--gptel-stream-info-p info)
         (not mevedel-gptel-stream-bridge--insert-batching-suspended)
         (numberp mevedel-gptel-stream-bridge-insert-batch-delay)
         (> mevedel-gptel-stream-bridge-insert-batch-delay 0))
    (mevedel-gptel-stream-bridge--queue-gptel-stream-insert-batch
     orig-fn response info raw))
   (t
    (when (mevedel-gptel-stream-bridge--gptel-stream-info-p info)
      (mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch info))
    (let ((inhibit-modification-hooks
           (or inhibit-modification-hooks
               (mevedel-gptel-stream-bridge--gptel-stream-info-p info)))
          (mevedel-gptel-stream-bridge--insert-batching-suspended
           (or mevedel-gptel-stream-bridge--insert-batching-suspended
               (not (stringp response)))))
      (funcall orig-fn response info raw)))))

(defun mevedel-gptel-stream-bridge--gptel-handle-wait-advice (orig-fn fsm)
  "Preserve an open mevedel reasoning fence across ORIG-FN for FSM."
  (let* ((info (gptel-fsm-info fsm))
         (reasoning-open
          (and (mevedel-gptel-stream-bridge--gptel-stream-info-p info)
               (plist-get info :reasoning-open))))
    (prog1 (funcall orig-fn fsm)
      (when reasoning-open
        (plist-put info :reasoning-open t)))))

(defun mevedel-gptel-stream-bridge--queue-gptel-stream-insert-batch
    (orig-fn response info raw)
  "Queue string RESPONSE for ORIG-FN as a batched gptel stream insert."
  (when (and (plist-get info :mevedel-stream-insert-parts)
             (not (equal raw (plist-get info :mevedel-stream-insert-raw))))
    (mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch info))
  (plist-put info :mevedel-stream-insert-orig orig-fn)
  (plist-put info :mevedel-stream-insert-raw raw)
  (plist-put info :mevedel-stream-insert-parts
             (cons response
                   (plist-get info :mevedel-stream-insert-parts)))
  (unless (timerp (plist-get info :mevedel-stream-insert-timer))
    (plist-put
     info :mevedel-stream-insert-timer
     (run-at-time mevedel-gptel-stream-bridge-insert-batch-delay nil
                  #'mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch
                  info))))

(defun mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch (info)
  "Flush any pending batched string stream insert on INFO."
  (when-let* ((timer (plist-get info :mevedel-stream-insert-timer))
              ((timerp timer)))
    (cancel-timer timer))
  (plist-put info :mevedel-stream-insert-timer nil)
  (when-let* ((parts (plist-get info :mevedel-stream-insert-parts))
              (orig-fn (plist-get info :mevedel-stream-insert-orig))
              ((functionp orig-fn)))
    (plist-put info :mevedel-stream-insert-parts nil)
    (let ((raw (plist-get info :mevedel-stream-insert-raw))
          (inhibit-modification-hooks t)
          (mevedel-gptel-stream-bridge--insert-batching-suspended t))
      (mevedel-gptel-stream-bridge--repair-gptel-stream-info info)
      (when (mevedel-gptel-stream-bridge--gptel-stream-info-p info)
        (funcall orig-fn
                 (apply #'concat (nreverse parts))
                 info raw)))))

(defun mevedel-gptel-stream-bridge--gptel-stream-cleanup-advice (orig-fn process status)
  "Call ORIG-FN after wrapping stream transformers for PROCESS.
STATUS is passed through unchanged."
  (require 'mevedel-telemetry)
  (let* ((entry (alist-get process gptel--request-alist))
         (fsm (car-safe entry))
         (info (and fsm (fboundp 'gptel-fsm-info)
                    (ignore-errors (gptel-fsm-info fsm))))
         (chat-buffer (plist-get info :buffer))
         (session (and (buffer-live-p chat-buffer)
                       (mevedel-telemetry-current-session chat-buffer))))
    (when (mevedel-gptel-stream-bridge--gptel-stream-info-p info)
      (mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch info)
      (mevedel-gptel-stream-bridge--wrap-gptel-stream-transformer info))
    (prog1 (funcall orig-fn process status)
      (when (and session (fboundp 'mevedel-telemetry-record))
        (mevedel-telemetry-record
         session 'provider-stream-ended
         :request-id (plist-get info :mevedel-request-id)
         :agent-path
         (when-let* ((invocation (plist-get info :mevedel-agent-invocation)))
           (mevedel-agent-invocation-path invocation))
         :provider-status status
         :first-byte-seen
         (and (process-get process 'mevedel-telemetry-first-byte) t))))))

(defun mevedel-gptel-stream-bridge--gptel-stream-filter-registered-p (process)
  "Return non-nil when PROCESS has a registered gptel FSM."
  (and (boundp 'gptel--request-alist)
       (car-safe (alist-get process gptel--request-alist))))

(defun mevedel-gptel-stream-bridge--schedule-gptel-stream-filter-flush (process)
  "Schedule a deferred gptel stream filter flush for PROCESS."
  (unless (process-get process 'mevedel-gptel-stream-bridge--filter-timer)
    (process-put
     process 'mevedel-gptel-stream-bridge--filter-timer
     (run-at-time 0 nil
                  #'mevedel-gptel-stream-bridge--flush-gptel-stream-filter process))))

(defun mevedel-gptel-stream-bridge--flush-gptel-stream-filter (process)
  "Flush buffered early stream chunks for PROCESS once gptel is ready."
  (process-put process 'mevedel-gptel-stream-bridge--filter-timer nil)
  (when (process-get process 'mevedel-gptel-stream-bridge--pending-output)
    (cond
     ((not (process-live-p process))
      (process-put process 'mevedel-gptel-stream-bridge--pending-output nil)
      (process-put process 'mevedel-gptel-stream-bridge--filter-retries nil))
     ((mevedel-gptel-stream-bridge--gptel-stream-filter-registered-p process)
      (process-put process 'mevedel-gptel-stream-bridge--filter-retries nil)
      (gptel-curl--stream-filter process ""))
     (t
      (let ((retries
             (1+ (or (process-get process
                                  'mevedel-gptel-stream-bridge--filter-retries)
                     0))))
        (if (> retries mevedel-gptel-stream-bridge--gptel-stream-filter-max-retries)
            (progn
              (process-put process 'mevedel-gptel-stream-bridge--pending-output nil)
              (process-put process 'mevedel-gptel-stream-bridge--filter-retries nil)
              (mevedel--warn-once
               'stream-bridge-orphan-chunk
               "Dropping gptel stream chunk without registered request FSM"))
          (process-put process 'mevedel-gptel-stream-bridge--filter-retries retries)
          (process-put
           process 'mevedel-gptel-stream-bridge--filter-timer
           (run-at-time 0.01 nil
                        #'mevedel-gptel-stream-bridge--flush-gptel-stream-filter
                        process))))))))

(defun mevedel-gptel-stream-bridge--gptel-stream-filter-advice (orig-fn process output)
  "Delay ORIG-FN until gptel has registered PROCESS's FSM.
OUTPUT is the stream chunk passed to gptel's process filter.

`gptel-curl-get-response' installs the streaming process filter before
it records PROCESS in `gptel--request-alist'.  If curl produces an
early chunk in that gap, gptel's filter sees a nil FSM.  Preserve the
chunk and replay it once the request entry exists."
  (when (and (> (length output) 0)
             (not (process-get process 'mevedel-telemetry-first-byte)))
    (require 'mevedel-telemetry)
    (when-let* ((entry (and (boundp 'gptel--request-alist)
                            (alist-get process gptel--request-alist)))
                (fsm (car-safe entry))
                (info (and (fboundp 'gptel-fsm-info)
                           (ignore-errors (gptel-fsm-info fsm))))
                (chat-buffer (plist-get info :buffer))
                ((buffer-live-p chat-buffer)))
      (process-put process 'mevedel-telemetry-first-byte t)
      (with-current-buffer chat-buffer
        (when (and (mevedel-telemetry-current-session chat-buffer)
                   (fboundp 'mevedel-telemetry-record))
          (mevedel-telemetry-record
           (mevedel-telemetry-current-session chat-buffer)
           'provider-first-byte
           :request-id (plist-get info :mevedel-request-id)
           :agent-path
           (when-let* ((invocation
                        (plist-get info :mevedel-agent-invocation)))
             (mevedel-agent-invocation-path invocation))
           :chunk-bytes (string-bytes output))))))
  (let ((pending (process-get process
                              'mevedel-gptel-stream-bridge--pending-output)))
    (if (mevedel-gptel-stream-bridge--gptel-stream-filter-registered-p process)
        (progn
          (when pending
            (setq output (concat pending output))
            (process-put process 'mevedel-gptel-stream-bridge--pending-output nil))
          (process-put process 'mevedel-gptel-stream-bridge--filter-retries nil)
          (funcall orig-fn process output))
      (process-put process 'mevedel-gptel-stream-bridge--pending-output
                   (concat pending output))
      (mevedel-gptel-stream-bridge--schedule-gptel-stream-filter-flush process))))

(defun mevedel-gptel-stream-bridge--advice-add-if-bound (symbol where function)
  "Add advice FUNCTION to SYMBOL at WHERE when SYMBOL is fbound."
  (when (and (fboundp symbol)
             (not (advice-member-p function symbol)))
    (advice-add symbol where function)))

(defun mevedel-gptel-stream-bridge--advice-remove-if-bound (symbol function)
  "Remove advice FUNCTION from SYMBOL when SYMBOL is fbound."
  (when (fboundp symbol)
    (advice-remove symbol function)))

(defun mevedel-gptel-stream-bridge--install-advice ()
  "Install gptel stream marker repair advice."
  (mevedel-gptel-stream-bridge--advice-add-if-bound
   'gptel--handle-wait
   :around #'mevedel-gptel-stream-bridge--gptel-handle-wait-advice)
  (mevedel-gptel-stream-bridge--advice-add-if-bound
   'gptel-curl--stream-insert-response
   :around #'mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice)
  (mevedel-gptel-stream-bridge--advice-add-if-bound
   'gptel-curl--stream-cleanup
   :around #'mevedel-gptel-stream-bridge--gptel-stream-cleanup-advice)
  (mevedel-gptel-stream-bridge--advice-add-if-bound
   'gptel-curl--stream-filter
   :around #'mevedel-gptel-stream-bridge--gptel-stream-filter-advice))

(defun mevedel-gptel-stream-bridge--install-if-enabled ()
  "Install gptel stream marker repair advice when enabled."
  (when mevedel-gptel-stream-bridge--gptel-stream-advice-installed
    (mevedel-gptel-stream-bridge--install-advice)))

(defun mevedel-gptel-stream-bridge--uninstall-advice ()
  "Remove gptel stream marker repair advice."
  (mevedel-gptel-stream-bridge--advice-remove-if-bound
   'gptel--handle-wait
   #'mevedel-gptel-stream-bridge--gptel-handle-wait-advice)
  (mevedel-gptel-stream-bridge--advice-remove-if-bound
   'gptel-curl--stream-insert-response
   #'mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice)
  (mevedel-gptel-stream-bridge--advice-remove-if-bound
   'gptel-curl--stream-cleanup
   #'mevedel-gptel-stream-bridge--gptel-stream-cleanup-advice)
  (mevedel-gptel-stream-bridge--advice-remove-if-bound
   'gptel-curl--stream-filter
   #'mevedel-gptel-stream-bridge--gptel-stream-filter-advice))

(defun mevedel-gptel-stream-bridge-install ()
  "Install gptel stream compatibility advice."
  (setq mevedel-gptel-stream-bridge--gptel-stream-advice-installed t)
  (mevedel-gptel-stream-bridge--install-if-enabled)
  (with-eval-after-load 'gptel
    (mevedel-gptel-stream-bridge--install-if-enabled))
  (with-eval-after-load 'gptel-request
    (mevedel-gptel-stream-bridge--install-if-enabled)))

(defun mevedel-gptel-stream-bridge-uninstall ()
  "Remove gptel stream compatibility advice."
  (setq mevedel-gptel-stream-bridge--gptel-stream-advice-installed nil)
  (mevedel-gptel-stream-bridge--uninstall-advice))

(provide 'mevedel-gptel-stream-bridge)

;;; mevedel-gptel-stream-bridge.el ends here
