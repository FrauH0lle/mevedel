;;; mevedel-view-stream.el --- Streaming view lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Owns gptel streaming interception, active-turn progress state, and
;; coalesced streaming/tool-boundary redraws.  Transcript interpretation and
;; composer editing remain behind the view module's rendering interface.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `cl-extra'
(declare-function cl-subseq "cl-extra" (sequence start &optional end))

;; `gptel'
(declare-function gptel-curl--stream-filter "ext:gptel-request" (process output))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel--request-alist)

;; `mevedel-compact'
(defvar mevedel--compaction-in-flight)

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-update-tool-render-data
                  "mevedel-pipeline" (buffer tool-use-id updates))

;; `mevedel-structs'
(declare-function mevedel-request-started-at "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-tool-exec'
(declare-function mevedel-tool-exec-format-execution-metadata
                  "mevedel-tool-exec" (facts timeout-seconds))

;; `mevedel-view'
(declare-function mevedel-view--tool-status-string "mevedel-view" (tool-name args))
(declare-function mevedel-view-rerender "mevedel-view" (&optional buffer))
(defvar mevedel-view--display-map)
(defvar mevedel-view-pending-tools-visible-max)
(defvar mevedel-view-spinner-animate)
(defvar mevedel-view-spinner-frames)
(defvar mevedel-view-spinner-interval)

;; `mevedel-view-agent'
(declare-function mevedel-view--agent-status-counts "mevedel-view-agent" ())
(defvar mevedel-view--agent-id)
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-composer'
(declare-function mevedel-view--agent-fsm-p
                  "mevedel-view-composer" (info data-buffer))
(declare-function mevedel-view--call-preserving-input-point
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--call-preserving-user-view-state
                  "mevedel-view-composer" (thunk))

;; `mevedel-view-render'
(declare-function mevedel-view--append-request-summary
                  "mevedel-view-render" (data-buf start))
(declare-function mevedel-view--cache-put
                  "mevedel-view-render" (table key value counter-symbol))
(declare-function mevedel-view--debug-log
                  "mevedel-view-render" (event &rest data))
(declare-function mevedel-view--debug-spinner-state
                  "mevedel-view-render" ())
(declare-function mevedel-view--debug-state
                  "mevedel-view-render" (&optional data-buf start end))
(declare-function mevedel-view--history-insertion-marker
                  "mevedel-view-render" ())
(declare-function mevedel-view--pending-tool-fragments
                  "mevedel-view-render" (entries))
(declare-function mevedel-view--pending-tool-insertion-target
                  "mevedel-view-render" ())
(declare-function mevedel-view--render-incremental
                  "mevedel-view-render" (data-buf &optional start end))
(declare-function mevedel-view--request-progress-anchor
                  "mevedel-view-render" ())
(defvar mevedel-view--tool-rendering-cache)

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-clear "mevedel-view-zone" (namespace))
(declare-function mevedel-view-zone-forget "mevedel-view-zone" (&optional namespace))
(declare-function mevedel-view-zone-reconcile "mevedel-view-zone" (namespace start end fragments))
(declare-function mevedel-view-zone-region "mevedel-view-zone" (namespace))
(declare-function mevedel-view-zone-start "mevedel-view-zone" (namespace))

(defvar-local mevedel-view--spinner-status nil
  "Current base status text shown by the request-progress row.")

(defvar-local mevedel-view--spinner-start-time nil
  "Fallback wall-clock start time for spinner elapsed display.
Used before a `mevedel-request' exists, or in tests that exercise the
spinner without a data-buffer request.")

(defvar-local mevedel-view--spinner-timer nil
  "Buffer-local timer animating visible spinner frames.")

(defvar-local mevedel-view--spinner-frame-index 0
  "Current frame index for animated view buffer spinners.")

(defvar-local mevedel-view--request-progress-suppressed nil
  "Non-nil means the request progress row must not be recreated.
Set during terminal cleanup; cleared when a new progress row is
explicitly started.")

(defvar-local mevedel-view--in-flight-turn-start nil
  "View-buffer marker at which the current assistant turn's render begins.

Set by the send path right after the user turn is echoed, consumed by
`mevedel-view--render-incremental' to bound the delete-and-re-render
region for each progress update, and cleared when the final
`gptel-post-response-functions' render completes.  Nil outside an
active exchange.")

(defvar-local mevedel-view--data-turn-start nil
  "Data-buffer marker at which the current assistant turn starts.

Anchored just after the user prompt was forwarded to the data
buffer, so `mevedel-view--render-incremental' can extract only the
in-flight assistant portion (not the whole conversation) when
rebuilding the view.  Nil outside an active exchange.")

(defvar-local mevedel-view--pending-tool-calls nil
  "Alist of in-flight tool calls.
Each entry is `(KEY . TOOL-NAME)' where KEY identifies the dispatch
and TOOL-NAME is the displayed tool name.

KEY is the backend call id when gptel exposes one, falling back to a
fingerprint built from `(NAME . ARGS-PRINT)' on older gptel builds.

Pre-tool hook adds an entry; post-tool hook removes by KEY.  The
render path walks this alist and emits one `Calling X…' line per
entry in arrival order, respecting
`mevedel-view-pending-tools-visible-max' for truncation when many
tools are in flight in parallel.")

(defvar-local mevedel-view--execution-event-entries 0
  "Number of entries retained in `mevedel-view--execution-events'.")

(defvar-local mevedel-view--execution-events nil
  "Latest transient Bash event keyed by durable tool-use id.")

(defvar-local mevedel-view-stream--pending-execution-terminals nil
  "Terminal Bash render data waiting for its transcript row.")

(defconst mevedel-view-stream--pending-execution-terminal-limit 64
  "Maximum terminal Bash updates retained while rows are unavailable.")

(defvar-local mevedel-view--stream-render-timer nil
  "Idle timer scheduling a `gptel-post-stream-hook'-driven render.

`mevedel-view-stream-schedule' sets this on each stream chunk
to batch the burst of per-chunk hook fires into one incremental render
after a short quiescence window.")

(defvar-local mevedel-view--tool-boundary-render-timer nil
  "Timer scheduling a tool-boundary incremental render.")

(defcustom mevedel-view-stream-render-delay 0.4
  "Seconds to wait after the last stream chunk before re-rendering.

The `gptel-post-stream-hook' path fires once per streamed chunk (up to
dozens per second).  `mevedel-view-stream-schedule' debounces
those fires by waiting this long for no new chunks before calling
`mevedel-view--render-incremental'.  Tune higher if the render cost
is visible in your environment; lower for snappier updates.

Tool-boundary hooks have their own shorter debounce."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-view-stream-insert-batch-delay 0.04
  "Seconds to batch consecutive string stream inserts in data buffers.

When positive, mevedel coalesces adjacent plain text stream chunks before
letting gptel insert them into the authoritative transcript buffer.  nil
or zero disables batching and preserves immediate insertion."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'mevedel)

(defcustom mevedel-view-tool-boundary-render-delay 0.05
  "Seconds to coalesce incremental renders around tool boundaries.
Pre/post tool hooks update their lightweight pending-tool status lines
immediately, then use this delay for the heavier transcript render."
  :type 'number
  :group 'mevedel)
(defvar mevedel-view-stream--gptel-stream-advice-installed nil
  "Non-nil when mevedel's gptel stream repair advice should be active.")

(defconst mevedel-view-stream--gptel-stream-filter-max-retries 100
  "Maximum deferred flush attempts for early gptel stream chunks.")

(defvar mevedel-view--stream-insert-batching-suspended nil
  "Non-nil means nested gptel stream insert calls should not batch.")

(defun mevedel-view-stream--gptel-data-buffer (buffer)
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

(defun mevedel-view--live-marker-p (marker)
  "Return non-nil when MARKER points into a live buffer."
  (and (markerp marker)
       (marker-position marker)
       (buffer-live-p (marker-buffer marker))))

(defun mevedel-view-stream--gptel-stream-info-p (info)
  "Return non-nil when INFO belongs to a mevedel gptel stream."
  (when-let* ((buffer (and (consp info) (plist-get info :buffer)))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (or (bound-and-true-p mevedel--session)
          (mevedel-view-stream--gptel-data-buffer buffer)))))

(defun mevedel-view-stream--repair-gptel-stream-info (info)
  "Repair detached stream markers in gptel INFO when it belongs to mevedel.

gptel's streaming insertion path expects `:position' to point somewhere
and calls `goto-char' on it before mevedel gets control back.  A detached
marker can happen after request teardown or buffer reconstruction races.
For mevedel streams, recover by appending future chunks to the data buffer
and clear stale tracking markers so gptel reinitializes them."
  (when (mevedel-view-stream--gptel-stream-info-p info)
    (let ((buffer (plist-get info :buffer)))
      (mevedel-view-stream--wrap-gptel-stream-transformer info)
      (unless (mevedel-view--live-marker-p (plist-get info :position))
        (with-current-buffer buffer
          (plist-put info :position (copy-marker (point-max) nil))))
      (dolist (key '(:tracking-marker :reasoning-marker))
        (let ((marker (plist-get info key)))
          (when (and marker
                     (not (mevedel-view--live-marker-p marker)))
            (plist-put info key nil))))))
  info)

(defun mevedel-view-stream--wrap-gptel-stream-transformer (info)
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
          (display-warning
           'mevedel
           (format "Ignoring stale gptel stream transformer: %s"
                   (error-message-string err))
           :warning)
          str))))))

(defun mevedel-view-stream--gptel-stream-insert-response-advice
    (orig-fn response info &optional raw)
  "Repair mevedel stream INFO before invoking ORIG-FN with RESPONSE."
  (mevedel-view-stream--repair-gptel-stream-info info)
  (cond
   ((and (stringp response)
         (mevedel-view-stream--gptel-stream-info-p info)
         (not mevedel-view--stream-insert-batching-suspended)
         (numberp mevedel-view-stream-insert-batch-delay)
         (> mevedel-view-stream-insert-batch-delay 0))
    (mevedel-view-stream--queue-gptel-stream-insert-batch
     orig-fn response info raw))
   (t
    (when (mevedel-view-stream--gptel-stream-info-p info)
      (mevedel-view-stream--flush-gptel-stream-insert-batch info))
    (let ((inhibit-modification-hooks
           (or inhibit-modification-hooks
               (mevedel-view-stream--gptel-stream-info-p info)))
          (mevedel-view--stream-insert-batching-suspended
           (or mevedel-view--stream-insert-batching-suspended
               (not (stringp response)))))
      (funcall orig-fn response info raw)))))

(defun mevedel-view-stream--queue-gptel-stream-insert-batch
    (orig-fn response info raw)
  "Queue string RESPONSE for ORIG-FN as a batched gptel stream insert."
  (when (and (plist-get info :mevedel-stream-insert-parts)
             (not (equal raw (plist-get info :mevedel-stream-insert-raw))))
    (mevedel-view-stream--flush-gptel-stream-insert-batch info))
  (plist-put info :mevedel-stream-insert-orig orig-fn)
  (plist-put info :mevedel-stream-insert-raw raw)
  (plist-put info :mevedel-stream-insert-parts
             (cons response
                   (plist-get info :mevedel-stream-insert-parts)))
  (unless (timerp (plist-get info :mevedel-stream-insert-timer))
    (plist-put
     info :mevedel-stream-insert-timer
     (run-at-time mevedel-view-stream-insert-batch-delay nil
                  #'mevedel-view-stream--flush-gptel-stream-insert-batch
                  info))))

(defun mevedel-view-stream--flush-gptel-stream-insert-batch (info)
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
          (mevedel-view--stream-insert-batching-suspended t))
      (mevedel-view-stream--repair-gptel-stream-info info)
      (when (mevedel-view-stream--gptel-stream-info-p info)
        (funcall orig-fn
                 (apply #'concat (nreverse parts))
                 info raw)))))

(defun mevedel-view-stream--gptel-stream-cleanup-advice (orig-fn process status)
  "Call ORIG-FN after wrapping stream transformers for PROCESS.
STATUS is passed through unchanged."
  (when-let* ((entry (alist-get process gptel--request-alist))
              (fsm (car entry))
              (info (and (fboundp 'gptel-fsm-info)
                         (gptel-fsm-info fsm))))
    (when (mevedel-view-stream--gptel-stream-info-p info)
      (mevedel-view-stream--flush-gptel-stream-insert-batch info)
      (mevedel-view-stream--wrap-gptel-stream-transformer info)))
  (funcall orig-fn process status))

(defun mevedel-view-stream--gptel-stream-filter-registered-p (process)
  "Return non-nil when PROCESS has a registered gptel FSM."
  (and (boundp 'gptel--request-alist)
       (car-safe (alist-get process gptel--request-alist))))

(defun mevedel-view-stream--schedule-gptel-stream-filter-flush (process)
  "Schedule a deferred gptel stream filter flush for PROCESS."
  (unless (process-get process 'mevedel-view--stream-filter-timer)
    (process-put
     process 'mevedel-view--stream-filter-timer
     (run-at-time 0 nil
                  #'mevedel-view-stream--flush-gptel-stream-filter process))))

(defun mevedel-view-stream--flush-gptel-stream-filter (process)
  "Flush buffered early stream chunks for PROCESS once gptel is ready."
  (process-put process 'mevedel-view--stream-filter-timer nil)
  (when (process-get process 'mevedel-view--pending-stream-output)
    (cond
     ((not (process-live-p process))
      (process-put process 'mevedel-view--pending-stream-output nil)
      (process-put process 'mevedel-view--stream-filter-retries nil))
     ((mevedel-view-stream--gptel-stream-filter-registered-p process)
      (process-put process 'mevedel-view--stream-filter-retries nil)
      (gptel-curl--stream-filter process ""))
     (t
      (let ((retries
             (1+ (or (process-get process
                                   'mevedel-view--stream-filter-retries)
                     0))))
        (if (> retries mevedel-view-stream--gptel-stream-filter-max-retries)
            (progn
              (process-put process 'mevedel-view--pending-stream-output nil)
              (process-put process 'mevedel-view--stream-filter-retries nil)
              (display-warning
               'mevedel
               "Dropping gptel stream chunk without registered request FSM"
               :warning))
          (process-put process 'mevedel-view--stream-filter-retries retries)
          (process-put
           process 'mevedel-view--stream-filter-timer
           (run-at-time 0.01 nil
                        #'mevedel-view-stream--flush-gptel-stream-filter
                        process))))))))

(defun mevedel-view-stream--gptel-stream-filter-advice (orig-fn process output)
  "Delay ORIG-FN until gptel has registered PROCESS's FSM.
OUTPUT is the stream chunk passed to gptel's process filter.

`gptel-curl-get-response' installs the streaming process filter before
it records PROCESS in `gptel--request-alist'.  If curl produces an
early chunk in that gap, gptel's filter sees a nil FSM.  Preserve the
chunk and replay it once the request entry exists."
  (let ((pending (process-get process
                              'mevedel-view--pending-stream-output)))
    (if (mevedel-view-stream--gptel-stream-filter-registered-p process)
        (progn
          (when pending
            (setq output (concat pending output))
            (process-put process 'mevedel-view--pending-stream-output nil))
          (process-put process 'mevedel-view--stream-filter-retries nil)
          (funcall orig-fn process output))
      (process-put process 'mevedel-view--pending-stream-output
                   (concat pending output))
      (mevedel-view-stream--schedule-gptel-stream-filter-flush process))))

(defun mevedel-view-stream--advice-add-if-bound (symbol where function)
  "Add advice FUNCTION to SYMBOL at WHERE when SYMBOL is fbound."
  (when (and (fboundp symbol)
             (not (advice-member-p function symbol)))
    (advice-add symbol where function)))

(defun mevedel-view-stream--advice-remove-if-bound (symbol function)
  "Remove advice FUNCTION from SYMBOL when SYMBOL is fbound."
  (when (fboundp symbol)
    (advice-remove symbol function)))

(defun mevedel-view-stream--install-advice ()
  "Install gptel stream marker repair advice."
  (mevedel-view-stream--advice-add-if-bound
   'gptel-curl--stream-insert-response
   :around #'mevedel-view-stream--gptel-stream-insert-response-advice)
  (mevedel-view-stream--advice-add-if-bound
   'gptel-curl--stream-cleanup
   :around #'mevedel-view-stream--gptel-stream-cleanup-advice)
  (mevedel-view-stream--advice-add-if-bound
   'gptel-curl--stream-filter
   :around #'mevedel-view-stream--gptel-stream-filter-advice))

(defun mevedel-view-stream--install-if-enabled ()
  "Install gptel stream marker repair advice when enabled."
  (when mevedel-view-stream--gptel-stream-advice-installed
    (mevedel-view-stream--install-advice)))

(defun mevedel-view-stream--uninstall-advice ()
  "Remove gptel stream marker repair advice."
  (mevedel-view-stream--advice-remove-if-bound
   'gptel-curl--stream-insert-response
   #'mevedel-view-stream--gptel-stream-insert-response-advice)
  (mevedel-view-stream--advice-remove-if-bound
   'gptel-curl--stream-cleanup
   #'mevedel-view-stream--gptel-stream-cleanup-advice)
  (mevedel-view-stream--advice-remove-if-bound
   'gptel-curl--stream-filter
   #'mevedel-view-stream--gptel-stream-filter-advice))

(defun mevedel-view-stream-install ()
  "Install gptel view stream repair advice."
  (setq mevedel-view-stream--gptel-stream-advice-installed t)
  (mevedel-view-stream--install-if-enabled)
  (with-eval-after-load 'gptel
    (mevedel-view-stream--install-if-enabled))
  (with-eval-after-load 'gptel-request
    (mevedel-view-stream--install-if-enabled)))

(defun mevedel-view-stream-uninstall ()
  "Remove gptel view stream repair advice."
  (setq mevedel-view-stream--gptel-stream-advice-installed nil)
  (mevedel-view-stream--uninstall-advice))

(defun mevedel-view--spinner-frame ()
  "Return the current spinner frame string."
  (or (nth (mod mevedel-view--spinner-frame-index
                (max 1 (length mevedel-view-spinner-frames)))
           mevedel-view-spinner-frames)
      ""))

(defun mevedel-view--duration-label (seconds)
  "Return a compact elapsed-time label for SECONDS."
  (let ((total (max 0 (floor (or seconds 0)))))
    (cond
     ((< total 60)
      (format "%ds" total))
     ((< total 3600)
      (format "%dm %02ds" (/ total 60) (% total 60)))
     (t
      (format "%dh %02dm" (/ total 3600) (% (/ total 60) 60))))))

(defun mevedel-view--spinner-started-at ()
  "Return the wall-clock start time for the current visible spinner."
  (or (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                                 mevedel--data-buffer))
                  ((buffer-live-p data-buf))
                  (request (buffer-local-value 'mevedel--current-request
                                               data-buf)))
        (mevedel-request-started-at request))
      mevedel-view--spinner-start-time))

(defun mevedel-view--spinner-elapsed-label ()
  "Return the elapsed-time label for the current spinner, or nil."
  (when-let* ((started-at (mevedel-view--spinner-started-at)))
    (mevedel-view--duration-label
     (float-time (time-subtract (current-time) started-at)))))

(defun mevedel-view--request-progress-active-p (&optional data-buf)
  "Return non-nil when the current view should show request progress.
DATA-BUF defaults to this view's data buffer.  The predicate accepts
both fully materialized requests and the short pre-WAIT interval where
the view has already inserted the in-flight markers."
  (and (not mevedel-view--agent-transcript-p)
       (not mevedel-view--request-progress-suppressed)
       (or mevedel-view--spinner-start-time
           (mevedel-view--in-flight-turn-start-position)
           (let ((buf (or data-buf
                          (and (boundp 'mevedel--data-buffer)
                               mevedel--data-buffer))))
             (and buf
                  (buffer-live-p buf)
                  (buffer-local-value 'mevedel--current-request buf))))))

(defun mevedel-view--request-progress-visible-p ()
  "Return non-nil when a request-progress fragment is visible."
  (require 'mevedel-view-zone)
  (let ((ov (mevedel-view-zone-region 'progress)))
    (and (overlayp ov)
         (eq (overlay-buffer ov) (current-buffer))
         (overlay-start ov)
         (overlay-end ov)
         (text-property-any (overlay-start ov) (overlay-end ov)
                            'mevedel-view-zone-namespace
                            'progress))))

(defun mevedel-view--request-progress-region-start ()
  "Return the start of the visible request-progress region, or nil."
  (require 'mevedel-view-zone)
  (and (mevedel-view--request-progress-visible-p)
       (mevedel-view-zone-start 'progress)))

(defun mevedel-view--request-progress-fragments (status)
  "Return the fragment list for request-progress STATUS."
  (list (list :namespace 'progress
              :id 'request
              :priority 0
              :body (mevedel-view--format-spinner-block status)
              :keymap mevedel-view--display-map
              :navigatable nil)))

(defun mevedel-view--render-request-progress ()
  "Render the current request-progress row from buffer-local state."
  (when mevedel-view--spinner-status
    (require 'mevedel-view-zone)
    (let ((anchor (mevedel-view--request-progress-anchor)))
      (mevedel-view-zone-reconcile
       'progress anchor anchor
       (mevedel-view--request-progress-fragments
        mevedel-view--spinner-status)))))

(defun mevedel-view--clear-request-progress ()
  "Remove the fragment-managed request-progress row."
  (require 'mevedel-view-zone)
  (mevedel-view-zone-clear 'progress))

(defun mevedel-view--forget-request-progress-region ()
  "Forget the request-progress region after a larger redraw deleted it."
  (require 'mevedel-view-zone)
  (mevedel-view-zone-forget 'progress))

(defun mevedel-view--ensure-request-progress (&optional data-buf status)
  "Ensure the foreground request progress row is visible.
DATA-BUF is the authoritative data buffer for elapsed-time lookup.
STATUS is the base label to show; nil preserves the current label or
falls back to \"Working...\"."
  (when (mevedel-view--request-progress-active-p data-buf)
    (let ((mevedel--data-buffer (or data-buf
                                    (and (boundp 'mevedel--data-buffer)
                                         mevedel--data-buffer)))
          (label (or status mevedel-view--spinner-status "Working...")))
      (setq mevedel-view--spinner-status label)
      (mevedel-view--render-request-progress)
      (mevedel-view--start-spinner-timer))))

(defun mevedel-view--spinner-agent-count-label ()
  "Return a compact active-agent count for the spinner, or nil."
  (when-let* ((counts (mevedel-view--agent-status-counts)))
    (let ((blocked (plist-get counts :blocked))
          (running (plist-get counts :running))
          label)
      (setq label
            (string-join
             (delq nil
                   (list
                    (when (and blocked (> blocked 0))
                      (format "%d %s blocked"
                              blocked
                              (if (= blocked 1) "agent" "agents")))
                    (when (and running (> running 0))
                      (format "%d %s running"
                              running
                              (if (= running 1) "agent" "agents")))))
             " · "))
      (unless (string-empty-p label)
        label))))

(defun mevedel-view--spinner-dynamic-label-p (text)
  "Return non-nil when TEXT is a generated spinner metadata label."
  (or (string-match-p "\\`[0-9]+s\\'" text)
      (string-match-p "\\`[0-9]+m [0-9][0-9]s\\'" text)
      (string-match-p "\\`[0-9]+h [0-9][0-9]m\\'" text)
      (string-match-p "\\`[0-9]+ agents? \\(?:blocked\\|running\\)\\'"
                      text)))

(defun mevedel-view--spinner-base-status (status)
  "Return STATUS without generated elapsed-time and agent-count labels."
  (let* ((status (if (or (null status) (string-empty-p status))
                     "Thinking..."
                   status))
         (parts (string-split status " · " t "[ \t\n]+")))
    (while (and (cdr parts)
                (mevedel-view--spinner-dynamic-label-p (car (last parts))))
      (setq parts (butlast parts)))
    (let ((base (string-join parts " · ")))
      (if (or (string-empty-p base)
              (string= base "Thinking..."))
          "Working..."
        base))))

(defun mevedel-view--spinner-display-status (status)
  "Return STATUS decorated with elapsed time and active-agent counts."
  (let* ((base (mevedel-view--spinner-base-status status))
         (elapsed (mevedel-view--spinner-elapsed-label))
         (agents (mevedel-view--spinner-agent-count-label)))
    (string-join (delq nil (list base elapsed agents)) " · ")))

(defun mevedel-view--format-spinner-line (status &optional face)
  "Return propertized spinner line for STATUS.
FACE defaults to `mevedel-view-spinner'."
  (let* ((frame (mevedel-view--spinner-frame))
         (face (or face 'mevedel-view-spinner))
         (display-status (mevedel-view--spinner-display-status status)))
    (concat
     (unless (string-empty-p frame)
       (concat
        (propertize frame
                    'font-lock-face face
                    'mevedel-view-spinner-frame t
                    'display frame
                    'read-only t
                    'keymap mevedel-view--display-map
                    'front-sticky '(read-only keymap)
                    'rear-nonsticky '(read-only keymap))
        (propertize " "
                    'font-lock-face face
                    'read-only t
                    'keymap mevedel-view--display-map
                    'front-sticky '(read-only keymap)
                    'rear-nonsticky '(read-only keymap))))
     (propertize (concat display-status "\n")
                 'font-lock-face face
                 'mevedel-view-spinner-status
                 (mevedel-view--spinner-base-status status)
                 'read-only t
                 'keymap mevedel-view--display-map
                 'front-sticky '(read-only keymap)
                 'rear-nonsticky '(read-only keymap)))))

(defun mevedel-view--request-progress-prefix ()
  "Return separator text before the request progress row."
  (require 'mevedel-view-zone)
  (let* ((pos (or (mevedel-view-zone-start 'progress)
                  (mevedel-view--request-progress-anchor)))
         (prefix
          (cond
           ((or (null pos) (<= pos (point-min))) nil)
           ((eq (char-before pos) ?\n)
            (unless (and (> pos (1+ (point-min)))
                         (eq (char-before (1- pos)) ?\n))
              "\n"))
           (t "\n\n"))))
    (when prefix
      (propertize prefix
                  'font-lock-face 'mevedel-view-spinner
                  'mevedel-view-spinner-separator t
                  'read-only t
                  'keymap mevedel-view--display-map
                  'front-sticky '(read-only keymap)
                  'rear-nonsticky '(read-only keymap)))))

(defun mevedel-view--format-spinner-block (status)
  "Return request-progress spinner text for STATUS at point."
  (concat (mevedel-view--request-progress-prefix)
          (mevedel-view--format-spinner-line status)))

(defun mevedel-view--spinner-active-p ()
  "Return non-nil when this view buffer has visible spinner work."
  (or mevedel-view--pending-tool-calls
      (mevedel-view--request-progress-visible-p)
      (mevedel-view--request-progress-active-p)))

(defun mevedel-view--stop-spinner-timer ()
  "Stop the buffer-local spinner animation timer."
  (when (timerp mevedel-view--spinner-timer)
    (cancel-timer mevedel-view--spinner-timer))
  (setq mevedel-view--spinner-timer nil))

(defun mevedel-view--start-spinner-timer ()
  "Start the buffer-local spinner animation timer when needed."
  (when (and mevedel-view-spinner-animate
             (cdr mevedel-view-spinner-frames)
             (not (timerp mevedel-view--spinner-timer)))
    (let ((buffer (current-buffer))
          timer)
      (setq timer
            (run-at-time
             mevedel-view-spinner-interval
             mevedel-view-spinner-interval
             (lambda ()
               (if (not (buffer-live-p buffer))
                   (cancel-timer timer)
                 (with-current-buffer buffer
                   (if (mevedel-view--spinner-active-p)
                       (mevedel-view--spinner-tick)
                     (mevedel-view--stop-spinner-timer)))))))
      (setq mevedel-view--spinner-timer timer))))

(defun mevedel-view--refresh-spinner-frame-spans (property start end face)
  "Refresh spinner frame spans with PROPERTY between START and END.
FACE is kept on the span while its `display' property changes to
the current frame.  This avoids rewriting buffer text during
animation ticks, so point does not jump when it sits on a spinner
line."
  (let ((frame (mevedel-view--spinner-frame))
        (pos start))
    (while (and pos (< pos end))
      (setq pos (text-property-any pos end property t))
      (when pos
        (let ((span-end (or (next-single-property-change pos property nil end)
                            end)))
          (put-text-property pos span-end 'display frame)
          (put-text-property pos span-end 'font-lock-face face)
          (setq pos span-end))))))

(defun mevedel-view--refresh-inline-spinner-frames ()
  "Refresh all inline pending-tool spinner frame spans."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (mevedel-view--refresh-spinner-frame-spans
     'mevedel-view-inline-spinner-frame
     (point-min)
     (point-max)
     'mevedel-view-ephemeral)))

(defun mevedel-view--spinner-tick ()
  "Advance visible spinner frames in the current view buffer."
  (mevedel-view--call-preserving-user-view-state
   (lambda ()
     (setq mevedel-view--spinner-frame-index
           (mod (1+ mevedel-view--spinner-frame-index)
                (max 1 (length mevedel-view-spinner-frames))))
     (mevedel-view--ensure-request-progress)
     (mevedel-view--refresh-inline-spinner-frames))))

(defun mevedel-view--start-spinner (&optional status)
  "Show request progress with STATUS text in the view buffer.
STATUS defaults to \"Thinking...\"."
  (mevedel-view--call-preserving-input-point
   (lambda ()
     (mevedel-view--debug-log
      'spinner-start
      :status status
      :state (mevedel-view--debug-state mevedel--data-buffer))
     (when mevedel-view--request-progress-suppressed
       (setq mevedel-view--spinner-start-time nil))
     (setq mevedel-view--request-progress-suppressed nil)
     (unless mevedel-view--spinner-start-time
       (setq mevedel-view--spinner-start-time (current-time)))
     (setq mevedel-view--spinner-status (or status "Thinking..."))
     (save-excursion
       (mevedel-view--render-request-progress))
     (mevedel-view--start-spinner-timer))))

(defun mevedel-view--spinner-region-p (start end)
  "Return non-nil when START..END still contain spinner text."
  (and start
       end
       (< start end)
       (text-property-any start end
                          'font-lock-face
                          'mevedel-view-spinner)))

(defun mevedel-view--update-spinner (status)
  "Update request progress to show STATUS text."
  (mevedel-view--call-preserving-input-point
   (lambda ()
     (mevedel-view--debug-log
      'spinner-update
      :status status
      :state (mevedel-view--debug-state mevedel--data-buffer))
     (setq mevedel-view--request-progress-suppressed nil)
     (unless mevedel-view--spinner-start-time
       (setq mevedel-view--spinner-start-time (current-time)))
     (setq mevedel-view--spinner-status status)
     (save-excursion
       (mevedel-view--render-request-progress))
     (mevedel-view--start-spinner-timer))))

(defun mevedel-view--stop-spinner ()
  "Remove request progress if present."
  (mevedel-view--call-preserving-input-point
   (lambda ()
     (mevedel-view--debug-log
      'spinner-stop-delete
      :spinner (mevedel-view--debug-spinner-state)
      :state (mevedel-view--debug-state mevedel--data-buffer))
     (mevedel-view--clear-request-progress)
     (setq mevedel-view--spinner-status nil)
     (unless mevedel-view--pending-tool-calls
        (unless (and (boundp 'mevedel--data-buffer)
                     mevedel--data-buffer
                     (buffer-live-p mevedel--data-buffer)
                     (buffer-local-value 'mevedel--current-request
                                         mevedel--data-buffer))
          (setq mevedel-view--spinner-start-time nil))
        (mevedel-view--stop-spinner-timer)))))

(defun mevedel-view--stop-request-progress ()
  "Stop and suppress the current request progress row."
  (setq mevedel-view--request-progress-suppressed t)
  (mevedel-view--stop-spinner)
  (setq mevedel-view--spinner-start-time nil))

(defun mevedel-view-stream-spinner-hook (info)
  "Update spinner from `gptel-pre-tool-call-functions'.
INFO is a plist with at least :name and :args."
  (when-let* ((view-buf (buffer-local-value 'mevedel--view-buffer
                                            (current-buffer)))
              (_ (buffer-live-p view-buf))
              (tool-name (plist-get info :name))
              (args (plist-get info :args)))
    (with-current-buffer view-buf
      ;; `mevedel-view-stream-pre-tool' owns in-flight tool status lines.
      ;; Avoid creating a second "Calling ..." line before that hook renders
      ;; the animated pending-tool live tail.
      (unless (and (mevedel-view--in-flight-turn-start-position)
                   (markerp mevedel-view--data-turn-start)
                   (marker-position mevedel-view--data-turn-start))
        (let ((summary (mevedel-view--tool-status-string tool-name args)))
          (mevedel-view--update-spinner summary)))))
  ;; Return nil so the hook does not interfere with tool execution.
  nil)

(defun mevedel-view--in-flight-turn-start-position ()
  "Return the current in-flight turn start position, or nil."
  (when (markerp mevedel-view--in-flight-turn-start)
    (marker-position mevedel-view--in-flight-turn-start)))

(defun mevedel-view--set-in-flight-turn-start (position)
  "Set `mevedel-view--in-flight-turn-start' to POSITION as a marker.
POSITION may be an integer or marker."
  (setq mevedel-view--in-flight-turn-start
        (copy-marker position nil)))

(defun mevedel-view--cancel-stream-render ()
  "Cancel any pending debounced stream render on the view buffer."
  (when (and (boundp 'mevedel-view--stream-render-timer)
             mevedel-view--stream-render-timer)
    (cancel-timer mevedel-view--stream-render-timer)
    (setq mevedel-view--stream-render-timer nil)))

(defun mevedel-view--cancel-tool-boundary-render ()
  "Cancel any pending debounced tool-boundary render."
  (when (and (boundp 'mevedel-view--tool-boundary-render-timer)
             mevedel-view--tool-boundary-render-timer)
    (cancel-timer mevedel-view--tool-boundary-render-timer)
    (setq mevedel-view--tool-boundary-render-timer nil)))

(defun mevedel-view--refresh-pending-tool-lines ()
  "Refresh lightweight pending-tool live-tail lines."
  (mevedel-view--delete-pending-tool-live-lines)
  (when mevedel-view--pending-tool-calls
    (let* ((cap mevedel-view-pending-tools-visible-max)
           (visible (cl-subseq mevedel-view--pending-tool-calls
                               0
                               (min cap
                                    (length
                                     mevedel-view--pending-tool-calls)))))
      (mevedel-view--insert-pending-tool-lines visible))))

(defun mevedel-view-stream--execution-view-buffer (data-buffer)
  "Return the visible view backed by DATA-BUFFER, or nil."
  (and (buffer-live-p data-buffer)
       (cl-find-if
        (lambda (buffer)
          (and (buffer-live-p buffer)
               (eq data-buffer
                   (buffer-local-value 'mevedel--data-buffer buffer))))
        (buffer-list))))

(defun mevedel-view-stream--execution-status (facts)
  "Return visual status for terminal execution FACTS."
  (if (memq (plist-get facts :outcome)
            '(success no-match different false))
      'success
    'error))

(defun mevedel-view-stream--terminal-render-data (event)
  "Return durable terminal render data projected from EVENT."
  (let ((facts (copy-tree (plist-get event :facts))))
    (append
     facts
     (list :status (mevedel-view-stream--execution-status facts)
           :live-execution-p nil
           :timeout-seconds (plist-get event :timeout-seconds)
           :sandbox-facts
           (copy-tree
            (plist-get (plist-get event :observation) :sandbox-facts))
           :execution-output
           (copy-sequence (or (plist-get event :whole-output) ""))))))

(defun mevedel-view-stream--pending-execution-table (data-buffer)
  "Return DATA-BUFFER's pending terminal table, creating it if needed."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (unless (hash-table-p mevedel-view-stream--pending-execution-terminals)
        (setq-local mevedel-view-stream--pending-execution-terminals
                    (make-hash-table :test #'equal)))
      mevedel-view-stream--pending-execution-terminals)))

(defun mevedel-view-stream--store-pending-execution-terminal
    (data-buffer tool-use-id render-data)
  "Retain RENDER-DATA until TOOL-USE-ID appears in DATA-BUFFER."
  (when-let* ((table
               (mevedel-view-stream--pending-execution-table data-buffer)))
    (when (and (not (gethash tool-use-id table))
               (>= (hash-table-count table)
                   mevedel-view-stream--pending-execution-terminal-limit))
      (let (evicted)
        (maphash (lambda (key _value)
                   (unless evicted (setq evicted key)))
                 table)
        (when evicted
          (remhash evicted table)
          (display-warning
           'mevedel
           (format "Discarding stale terminal update for tool %s" evicted)
           :warning))))
    (puthash tool-use-id render-data table)))

(defun mevedel-view-stream--retry-pending-execution-terminals (data-buffer)
  "Persist pending terminal Bash updates whose rows exist in DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (when (hash-table-p mevedel-view-stream--pending-execution-terminals)
        (require 'mevedel-pipeline)
        (let (settled)
          (maphash
           (lambda (tool-use-id render-data)
             (when (mevedel-pipeline-update-tool-render-data
                    data-buffer tool-use-id render-data)
               (push tool-use-id settled)))
           mevedel-view-stream--pending-execution-terminals)
          (dolist (tool-use-id settled)
            (remhash tool-use-id
                     mevedel-view-stream--pending-execution-terminals)))))))

(defun mevedel-view-stream-retry-execution-terminals (&rest _args)
  "Persist pending terminal Bash updates in the current data buffer."
  (mevedel-view-stream--retry-pending-execution-terminals (current-buffer))
  nil)

(defun mevedel-view-stream--cache-execution-progress (event)
  "Cache bounded transient progress from EVENT in the current view."
  (when-let* ((tool-use-id (plist-get event :tool-use-id)))
    (unless (hash-table-p mevedel-view--execution-events)
      (setq mevedel-view--execution-events (make-hash-table :test #'equal)))
    (mevedel-view--cache-put
     mevedel-view--execution-events tool-use-id
     (list :type 'progress
           :facts (copy-tree (plist-get event :facts))
           :timeout-seconds (plist-get event :timeout-seconds)
           :output-tail (plist-get event :output-tail))
     'mevedel-view--execution-event-entries)))

(defun mevedel-view-stream--remove-execution-progress (tool-use-id)
  "Remove TOOL-USE-ID's transient progress from the current view."
  (when (and tool-use-id
             (hash-table-p mevedel-view--execution-events)
             (gethash tool-use-id mevedel-view--execution-events))
    (remhash tool-use-id mevedel-view--execution-events)
    (setq mevedel-view--execution-event-entries
          (max 0 (1- mevedel-view--execution-event-entries)))))

(defun mevedel-view-stream--update-execution-pending-row (event)
  "Update the current view's pending row from progress EVENT."
  (let* ((tool-use-id (plist-get event :tool-use-id))
         (facts (plist-get event :facts))
         (args (plist-get event :tool-args))
         (tail (plist-get event :output-tail))
         (entry (and tool-use-id
                     (assoc tool-use-id mevedel-view--pending-tool-calls))))
    (when entry
      (setcdr entry
              (concat
               (mevedel-view--tool-status-string "Bash" args)
               " — "
               (mevedel-tool-exec-format-execution-metadata
                facts (plist-get event :timeout-seconds))
               (and (stringp tail)
                    (not (string-empty-p tail))
                    (concat "\n" tail))))
      (mevedel-view--refresh-pending-tool-lines))))

(defun mevedel-view-stream-handle-execution-event (event)
  "Apply Bash EVENT to its authoritative row and visible view.
Always return nil; only the mailbox sink may acknowledge durable delivery."
  (let* ((type (plist-get event :type))
         (tool-use-id (plist-get event :tool-use-id))
         (data-buffer (plist-get event :data-buffer))
         (view-buffer
          (mevedel-view-stream--execution-view-buffer data-buffer)))
    (when (and (eq type 'terminal) tool-use-id)
      (require 'mevedel-pipeline)
      (let ((render-data
             (mevedel-view-stream--terminal-render-data event)))
        (if (mevedel-pipeline-update-tool-render-data
             data-buffer tool-use-id render-data)
            (when-let* ((table
                         (mevedel-view-stream--pending-execution-table
                          data-buffer)))
              (remhash tool-use-id table))
          (mevedel-view-stream--store-pending-execution-terminal
           data-buffer tool-use-id render-data))))
    (when view-buffer
      (with-current-buffer view-buffer
        (pcase type
          ('progress
           (mevedel-view-stream--cache-execution-progress event)
           (mevedel-view-stream--update-execution-pending-row event))
          ('terminal
           (mevedel-view-stream--remove-execution-progress tool-use-id)))
        (when (hash-table-p mevedel-view--tool-rendering-cache)
          (clrhash mevedel-view--tool-rendering-cache))
        (mevedel-view-rerender view-buffer))))
  nil)

(defun mevedel-view--render-stream-update (data-buf)
  "Incrementally render DATA-BUF, isolating observer-view failures."
  (mevedel-view-stream--retry-pending-execution-terminals data-buf)
  (if (not mevedel-view--agent-transcript-p)
      (mevedel-view--render-incremental data-buf)
    (condition-case err
        (atomic-change-group
          (mevedel-view--render-incremental data-buf))
      (error
       (display-warning
        'mevedel
        (format "Live agent transcript render failed: %s"
                (error-message-string err))
        :warning)))))

(defun mevedel-view--schedule-tool-boundary-render (data-buf)
  "Schedule a coalesced incremental render for DATA-BUF."
  (when (and (buffer-live-p data-buf)
             (mevedel-view--in-flight-turn-start-position)
             (markerp mevedel-view--data-turn-start))
    (mevedel-view--cancel-tool-boundary-render)
    (if (and (numberp mevedel-view-tool-boundary-render-delay)
             (> mevedel-view-tool-boundary-render-delay 0))
        (let ((view-buf (current-buffer)))
          (setq mevedel-view--tool-boundary-render-timer
                (run-at-time
                 mevedel-view-tool-boundary-render-delay nil
                 (lambda ()
                   (when (and (buffer-live-p view-buf)
                              (buffer-live-p data-buf))
                     (with-current-buffer view-buf
                       (setq mevedel-view--tool-boundary-render-timer nil)
                       (mevedel-view--render-stream-update data-buf)))))))
      (mevedel-view--render-stream-update data-buf))))

(defun mevedel-view-stream-schedule ()
  "Schedule a debounced incremental render driven by the stream hook.

Intended for `gptel-post-stream-hook', which fires once per streamed
chunk in the data buffer.  Defers the incremental render by
`mevedel-view-stream-render-delay' seconds of quiescence so the view
rebuilds at most a few times per second rather than per token."
  (when-let* ((view-buf (and (boundp 'mevedel--view-buffer)
                             mevedel--view-buffer))
              ((buffer-live-p view-buf))
              (data-buf (current-buffer)))
    (with-current-buffer view-buf
      (mevedel-view--debug-log
       'stream-render-schedule
       :state (mevedel-view--debug-state data-buf))
      ;; Only schedule when a turn is in-flight.  Before the first
      ;; user send -- or after the final post-response cleanup -- the
      ;; incremental markers are nil and rendering would no-op.
      (when (and (mevedel-view--in-flight-turn-start-position)
                 (markerp mevedel-view--data-turn-start))
        (unless mevedel-view--stream-render-timer
          (setq mevedel-view--stream-render-timer
                (run-at-time
                 mevedel-view-stream-render-delay nil
                 (lambda ()
                   (when (buffer-live-p view-buf)
                     (with-current-buffer view-buf
                       (setq mevedel-view--stream-render-timer nil)
                       (when (buffer-live-p data-buf)
                         (mevedel-view--debug-log
                          'stream-render-fire
                          :state (mevedel-view--debug-state data-buf))
                         (mevedel-view--render-stream-update data-buf)))))))))))
  nil)

(defun mevedel-view--pending-tool-key (info)
  "Return the call-id key for pending tool INFO.

Prefer gptel's per-call id when present so identical parallel calls
remain distinct in the live tail.  Fall back to the old name/args
fingerprint only for older gptel builds that do not expose an id."
  (or (plist-get info :id)
      (plist-get info :call-id)
      (plist-get info :tool-call-id)
      (plist-get info :tool_call_id)
      (cons (plist-get info :name)
            (let ((print-level 4)
                  (print-length 32)
                  (print-circle t))
              (prin1-to-string (plist-get info :args))))))

(defun mevedel-view-stream-pre-tool (args)
  "Mark an in-flight tool call from ARGS and schedule a view render.

Runs as a `gptel-pre-tool-call-functions' hook in the data buffer.
Adds an entry to `mevedel-view--pending-tool-calls' on the
associated view buffer.  The lightweight pending-tool live line is
refreshed immediately, while the heavier incremental render is
debounced so bursts of tool boundary hooks coalesce."
  (when-let* ((view-buf (and (boundp 'mevedel--view-buffer)
                             mevedel--view-buffer))
              ((buffer-live-p view-buf))
              (name (plist-get args :name))
              (data-buf (current-buffer)))
    (with-current-buffer view-buf
      (mevedel-view--debug-log
       'pre-tool-hook
       :args (list :id (plist-get args :id)
                   :call-id (plist-get args :call-id)
                   :name name)
       :state (mevedel-view--debug-state data-buf))
      (mevedel-view--cancel-stream-render)
      (unless (equal name "Agent")
        (let ((key (mevedel-view--pending-tool-key args))
              (label (mevedel-view--tool-status-string
                      name (plist-get args :args))))
          (unless (assoc key mevedel-view--pending-tool-calls)
            (setq mevedel-view--pending-tool-calls
                  (append mevedel-view--pending-tool-calls
                          (list (cons key label)))))))
      ;; Keep the request-level progress row visible; pending-tool lines
      ;; are detail rows below it, not a replacement for elapsed request
      ;; progress.
      (mevedel-view--ensure-request-progress data-buf)
      (mevedel-view--start-spinner-timer)
      (when (and (mevedel-view--in-flight-turn-start-position)
                 (markerp mevedel-view--data-turn-start))
        (mevedel-view--refresh-pending-tool-lines)
        (mevedel-view--schedule-tool-boundary-render data-buf)
        (mevedel-view--debug-log
         'pre-tool-hook-after-schedule
         :state (mevedel-view--debug-state data-buf)))))
  ;; gptel pre-tool hooks must return nil unless they intentionally
  ;; provide a control plist.
  nil)

(defun mevedel-view-stream-post-tool (args)
  "Clear the in-flight tool marker and schedule a view render.

Runs as a `gptel-post-tool-call-functions' hook in the data buffer.
ARGS is the tool-call plist.  The lightweight pending-tool live line is
refreshed immediately, while the heavier incremental render is
debounced so bursts of completed tool calls coalesce."
  (mevedel-view-stream--retry-pending-execution-terminals (current-buffer))
  (when-let* ((view-buf (and (boundp 'mevedel--view-buffer)
                             mevedel--view-buffer))
              ((buffer-live-p view-buf))
              (name (plist-get args :name))
              (data-buf (current-buffer)))
    (with-current-buffer view-buf
      (mevedel-view--debug-log
       'post-tool-hook
       :args (list :id (plist-get args :id)
                   :call-id (plist-get args :call-id)
                   :name name)
       :state (mevedel-view--debug-state data-buf))
      (mevedel-view--cancel-stream-render)
      (let ((key (mevedel-view--pending-tool-key args)))
        (setq mevedel-view--pending-tool-calls
              (assoc-delete-all key mevedel-view--pending-tool-calls)))
      (unless mevedel-view--pending-tool-calls
        (mevedel-view--delete-pending-tool-live-lines))
      (unless (or mevedel-view--pending-tool-calls
                  (mevedel-view--request-progress-visible-p))
        (mevedel-view--stop-spinner-timer))
      (when (and (mevedel-view--in-flight-turn-start-position)
                 (markerp mevedel-view--data-turn-start))
        (mevedel-view--refresh-pending-tool-lines)
        (mevedel-view--schedule-tool-boundary-render data-buf)
        (mevedel-view--debug-log
         'post-tool-hook-after-schedule
         :state (mevedel-view--debug-state data-buf)))))
  ;; gptel post-tool hooks must return nil unless they intentionally
  ;; provide a control plist.
  nil)

(defun mevedel-view--delete-pending-tool-live-lines ()
  "Delete fragment-backed pending-tool live-tail rows from the view buffer."
  (require 'mevedel-view-zone)
  (mevedel-view-zone-clear 'history-live))

(defun mevedel-view--insert-pending-tool-lines (entries)
  "Render fragment-backed pending tool live-tail rows for ENTRIES.
ENTRIES is a subset of `mevedel-view--pending-tool-calls' (head N).
When the full list exceeds `mevedel-view-pending-tools-visible-max',
the caller passes only the visible head and a tail-summary row is
appended.

Pending-tool rows are part of the in-flight transcript live tail, so
they fall back to the history/status boundary rather than the input
  marker when no render insertion marker is dynamically bound."
  (require 'mevedel-view-zone)
  (let ((anchor (mevedel-view--pending-tool-insertion-target)))
    (mevedel-view-zone-reconcile
     'history-live anchor anchor
     (mevedel-view--pending-tool-fragments entries))))

(defun mevedel-view-stream-active-response-marker (info data-buffer)
  "Return INFO's active response insertion marker for DATA-BUFFER."
  (let ((tracking (plist-get info :tracking-marker))
        (position (plist-get info :position)))
    (cond
     ((and (markerp tracking)
           (marker-position tracking)
           (eq (marker-buffer tracking) data-buffer))
      tracking)
     ((and (markerp position)
           (marker-position position)
           (eq (marker-buffer position) data-buffer))
      position))))

(defun mevedel-view-stream-ensure-progress-for-fsm (fsm)
  "Ensure the request progress row for top-level FSM is visible."
  (when-let* ((info (and fsm (fboundp 'gptel-fsm-info)
                         (gptel-fsm-info fsm)))
              (data-buffer (plist-get info :buffer))
              ((buffer-live-p data-buffer))
              ((not (mevedel-view--agent-fsm-p info data-buffer)))
              (view-buffer (buffer-local-value 'mevedel--view-buffer
                                                data-buffer))
              ((buffer-live-p view-buffer)))
    (with-current-buffer view-buffer
      (unless mevedel-view--agent-transcript-p
        (unless (and (markerp mevedel-view--data-turn-start)
                     (marker-position mevedel-view--data-turn-start))
          (when-let* ((marker (mevedel-view-stream-active-response-marker
                               info data-buffer)))
            (setq mevedel-view--data-turn-start (copy-marker marker nil))))
        (unless (mevedel-view--in-flight-turn-start-position)
          (setq mevedel-view--in-flight-turn-start
                (copy-marker (mevedel-view--history-insertion-marker) nil)))
        (setq mevedel-view--request-progress-suppressed nil)
        (mevedel-view--ensure-request-progress data-buffer)))))

(defun mevedel-view-stream-begin-turn (view-start data-start &optional no-progress)
  "Begin an active streamed turn at VIEW-START and DATA-START.
When NO-PROGRESS is non-nil, record no active progress state."
  (when (markerp mevedel-view--in-flight-turn-start)
    (set-marker mevedel-view--in-flight-turn-start nil))
  (when (markerp mevedel-view--data-turn-start)
    (set-marker mevedel-view--data-turn-start nil))
  (setq mevedel-view--in-flight-turn-start
        (and (not no-progress) (copy-marker view-start nil)))
  (setq mevedel-view--data-turn-start
        (and (not no-progress) (copy-marker data-start nil)))
  (unless no-progress
    (mevedel-view--start-spinner)))

(defun mevedel-view-stream-stop ()
  "Stop active streaming UI and release all turn markers."
  (mevedel-view--stop-request-progress)
  (mevedel-view--stop-spinner-timer)
  (mevedel-view--cancel-stream-render)
  (mevedel-view--cancel-tool-boundary-render)
  (setq mevedel-view--pending-tool-calls nil)
  (mevedel-view--delete-pending-tool-live-lines)
  (when (markerp mevedel-view--in-flight-turn-start)
    (set-marker mevedel-view--in-flight-turn-start nil))
  (setq mevedel-view--in-flight-turn-start nil)
  (when (markerp mevedel-view--data-turn-start)
    (set-marker mevedel-view--data-turn-start nil))
  (setq mevedel-view--data-turn-start nil))

(defun mevedel-view-stream-render-response (start end)
  "Finish and render gptel response bounds START and END."
  (mevedel-view-stream--retry-pending-execution-terminals (current-buffer))
  (when-let* ((view-buf (buffer-local-value 'mevedel--view-buffer
                                            (current-buffer)))
              ((buffer-live-p view-buf)))
    (let ((data-buf (current-buffer)))
      (with-current-buffer view-buf
        (mevedel-view--debug-log
         'render-response-begin
         :start start
         :end end
         :state (mevedel-view--debug-state data-buf start end))
        (with-current-buffer data-buf
          (setq-local mevedel--compaction-in-flight nil))
        (mevedel-view--stop-request-progress)
        (mevedel-view--debug-log
         'render-response-after-spinner
         :state (mevedel-view--debug-state data-buf start end))
        (mevedel-view--cancel-stream-render)
        (mevedel-view--cancel-tool-boundary-render)
        (setq mevedel-view--pending-tool-calls nil)
        (mevedel-view--delete-pending-tool-live-lines)
        (setq end (or (mevedel-view--append-request-summary data-buf start)
                      end))
        (mevedel-view--render-incremental data-buf start end)
        (mevedel-view--debug-log
         'render-response-after-incremental
         :state (mevedel-view--debug-state data-buf start end))
        (mevedel-view--stop-spinner-timer)
        (when (markerp mevedel-view--in-flight-turn-start)
          (set-marker mevedel-view--in-flight-turn-start nil))
        (setq mevedel-view--in-flight-turn-start nil)
        (when (markerp mevedel-view--data-turn-start)
          (set-marker mevedel-view--data-turn-start nil))
        (setq mevedel-view--data-turn-start nil))))
  nil)

(provide 'mevedel-view-stream)
;;; mevedel-view-stream.el ends here
