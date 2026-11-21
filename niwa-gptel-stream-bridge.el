;;; niwa-gptel-stream-bridge.el -- gptel stream compatibility -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the version-sensitive advice between gptel's private streaming
;; lifecycle and niwa data buffers.  View progress and redraw scheduling
;; remain in niwa-view-stream.el.

;;; Code:

(require 'niwa-telemetry)
(require 'niwa-utilities)

;; `gptel'
(declare-function gptel-curl--stream-filter
                  "ext:gptel-request" (process output))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel--request-alist)
(defvar gptel-post-stream-hook)

;; `niwa-agents'
(declare-function niwa-agent-invocation-path "niwa-agents" (cl-x))

;; `niwa-structs'
(defvar niwa--data-buffer)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-telemetry'
(declare-function niwa-telemetry-current-session
                  "niwa-telemetry" (&optional buffer))
(declare-function niwa-telemetry-record
                  "niwa-telemetry" (session event &rest props))
(autoload 'niwa-telemetry-current-session "niwa-telemetry")
(autoload 'niwa-telemetry-record "niwa-telemetry")

;; `niwa-transport'
(declare-function niwa-transport-run-at-time
                  "niwa-transport" (seconds function &rest args))
(declare-function niwa-transport-pause-process "niwa-transport" (process seconds))
(declare-function niwa-transport-resume-process "niwa-transport" (process))
(declare-function niwa-transport-resume-paused "niwa-transport" (&optional within))
(autoload 'niwa-transport-run-at-time "niwa-transport")
(autoload 'niwa-transport-pause-process "niwa-transport")
(autoload 'niwa-transport-resume-process "niwa-transport")
(autoload 'niwa-transport-resume-paused "niwa-transport")

;; `niwa-utilities'
(declare-function niwa--timer-pending-p "niwa-utilities" (timer))
(declare-function niwa--warn-once
                  "niwa-utilities" (scope key format &rest args))
(autoload 'niwa--timer-pending-p "niwa-utilities")
(autoload 'niwa--warn-once "niwa-utilities")
(eval-when-compile (require 'niwa-utilities))

(require 'niwa-transcript-source)

(defcustom niwa-gptel-stream-bridge-insert-batch-delay 0.4
  "Seconds to batch consecutive string stream inserts in data buffers.

When positive, niwa coalesces adjacent plain text stream chunks before
letting gptel insert them into the authoritative transcript buffer.  nil
or zero disables batching and preserves immediate insertion.

Each flush is a timer wakeup, and every wakeup redisplays; a pgtk frame
repaints its whole surface each time even though the data buffer is not
shown.  The view renders a flushed batch in the same wakeup, so text
appears at most this long after it arrives.  The same delay paces reading
a local curl stream on systems with job control: see
`niwa-gptel-stream-bridge--pace-reading'.  Streaming 32 chunks a second
cost 59% editor CPU unpaced, 27% at 0.2 s and 17% at 0.4 s."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'niwa)
(defvar niwa-gptel-stream-bridge--gptel-stream-advice-installed nil
  "Non-nil when niwa's gptel stream repair advice should be active.")

(defconst niwa-gptel-stream-bridge--gptel-stream-filter-max-retries 100
  "Maximum deferred flush attempts for early gptel stream chunks.")

(defvar niwa-gptel-stream-bridge-flushing nil
  "Non-nil while a batched stream insert reaches gptel.
Stream observers can render at once: the batch already coalesced the
chunks.")

(defvar niwa-gptel-stream-bridge--after-flush nil
  "Box (FUNCTIONS) for work after a batch its own timer is flushing, or nil.")

(defvar niwa-gptel-stream-bridge--insert-batching-suspended nil
  "Non-nil means nested gptel stream insert calls should not batch.")

(defun niwa-gptel-stream-bridge--gptel-data-buffer (buffer)
  "Return BUFFER's niwa data buffer, or nil when BUFFER is unrelated.
If BUFFER is a view buffer, return its backing data buffer.  If BUFFER
already is a niwa data buffer, return BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((and (derived-mode-p 'niwa-view-mode)
             (boundp 'niwa--data-buffer)
             niwa--data-buffer
             (buffer-live-p niwa--data-buffer))
        niwa--data-buffer)
       ((and (boundp 'niwa--view-buffer)
             niwa--view-buffer
             (buffer-live-p niwa--view-buffer)
             (with-current-buffer niwa--view-buffer
               (and (derived-mode-p 'niwa-view-mode)
                    (eq niwa--data-buffer buffer))))
        buffer)))))

(defun niwa-gptel-stream-bridge--live-marker-p (marker)
  "Return non-nil when MARKER points into a live buffer."
  (and (markerp marker)
       (marker-position marker)
       (buffer-live-p (marker-buffer marker))))

(defun niwa-gptel-stream-bridge--gptel-stream-info-p (info)
  "Return non-nil when INFO belongs to a niwa gptel stream."
  (when-let* ((buffer (and (consp info) (plist-get info :buffer)))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (or (bound-and-true-p niwa--session)
          (niwa-gptel-stream-bridge--gptel-data-buffer buffer)))))

(defun niwa-gptel-stream-bridge--repair-gptel-stream-info (info)
  "Repair detached stream markers in gptel INFO when it belongs to niwa.

gptel's streaming insertion path expects `:position' to point somewhere
and calls `goto-char' on it before niwa gets control back.  A detached
marker can happen after request teardown or buffer reconstruction races.
For niwa streams, recover by appending future chunks to the data buffer
and clear stale tracking markers so gptel reinitializes them."
  (when (niwa-gptel-stream-bridge--gptel-stream-info-p info)
    (let ((buffer (plist-get info :buffer)))
      (niwa-gptel-stream-bridge--wrap-gptel-stream-transformer info)
      (unless (niwa-gptel-stream-bridge--live-marker-p (plist-get info :position))
        (with-current-buffer buffer
          (plist-put info :position (copy-marker (point-max) nil))))
      (dolist (key '(:tracking-marker :reasoning-marker))
        (let ((marker (plist-get info key)))
          (when (and marker
                     (not (niwa-gptel-stream-bridge--live-marker-p marker)))
            (plist-put info key nil))))))
  info)

(defun niwa-gptel-stream-bridge--wrap-gptel-stream-transformer (info)
  "Wrap INFO's stream transformer so stale cleanup does not signal.

gptel's streaming Org converter owns an internal temporary buffer.  In
some teardown orders that buffer is killed before the final callback
reuses the transformer.  Let the response finish by returning the raw
chunk when that stale transformer fails."
  (when-let* ((transformer (and (consp info)
                                (plist-get info :transformer)))
              ((functionp transformer))
              ((not (plist-get info :niwa-transformer-wrapped)))
              (scope (niwa-diagnostic-scope (plist-get info :buffer))))
    (plist-put info :niwa-transformer-wrapped t)
    (plist-put
     info :transformer
     (lambda (str)
       (condition-case err
           (funcall transformer str)
         (error
          (niwa--warn-once
           scope 'stream-bridge-transformer
           "Ignoring stale gptel stream transformer: %s"
           (error-message-string err))
          str))))))

(defun niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
    (orig-fn response info &optional raw)
  "Repair niwa stream INFO before invoking ORIG-FN with RESPONSE."
  (niwa-gptel-stream-bridge--repair-gptel-stream-info info)
  (cond
   ((and (stringp response)
         (not raw)
         (niwa-gptel-stream-bridge--gptel-stream-info-p info)
         (not niwa-gptel-stream-bridge--insert-batching-suspended)
         (numberp niwa-gptel-stream-bridge-insert-batch-delay)
         (> niwa-gptel-stream-bridge-insert-batch-delay 0))
    (niwa-gptel-stream-bridge--queue-gptel-stream-insert-batch
     orig-fn response info))
   (t
    (when (niwa-gptel-stream-bridge--gptel-stream-info-p info)
      (niwa-gptel-stream-bridge--flush-gptel-stream-insert-batch info))
    (let ((inhibit-modification-hooks
           (or inhibit-modification-hooks
               (niwa-gptel-stream-bridge--gptel-stream-info-p info)))
          (niwa-gptel-stream-bridge--insert-batching-suspended
           (or niwa-gptel-stream-bridge--insert-batching-suspended
               (not (stringp response)))))
      (niwa-gptel-stream-bridge--insert-tracked
       orig-fn response info raw)))))

(defun niwa-gptel-stream-bridge--insert-tracked (orig-fn response info raw)
  "Call ORIG-FN with RESPONSE, INFO and RAW, tracking its silent insert.
Under `inhibit-modification-hooks', a string insert is announced to transcript
change proofs as one change from the tracking marker, or the initial position
before that marker exists.  The transformer and post-stream hooks are foreign
code that may edit anywhere or redirect insertion, so each runs under
`niwa-transcript-source-call-with-unproven-changes': an edit by either
fails the proofs closed whatever its range, as does a point or buffer change.
A transformer that downstream
code replaced meanwhile is not restored over that replacement."
  (let ((position (plist-get info :position)))
    (if (not (and (stringp response)
                  inhibit-modification-hooks
                  (markerp position)
                  (marker-position position)
                  (buffer-live-p (marker-buffer position))))
        (funcall orig-fn response info raw)
      (with-current-buffer (marker-buffer position)
        (let* ((tracking (plist-get info :tracking-marker))
               (start (if tracking
                          (if (and (markerp tracking)
                                   (eq (marker-buffer tracking) (current-buffer)))
                              (marker-position tracking)
                            (point-min))
                        (marker-position position)))
               (finish (niwa-transcript-source-begin-change start (point-max)))
               (transformer (plist-get info :transformer))
               (hooks gptel-post-stream-hook)
               (wrapped
                (and (functionp transformer)
                     (lambda (text)
                       (niwa-transcript-source-call-with-unproven-changes
                        (lambda () (funcall transformer text)))))))
          (when wrapped
            (plist-put info :transformer wrapped))
          (unwind-protect
              (let ((gptel-post-stream-hook
                     (list
                      (lambda ()
                        ;; Observers can derive caches now.  Adopt the insert's
                        ;; tick first, but never adopt later hook edits.
                        (funcall finish)
                        (niwa-transcript-source-call-with-unproven-changes
                         (lambda ()
                           (let ((gptel-post-stream-hook hooks))
                             (run-hooks 'gptel-post-stream-hook))))))))
                (funcall orig-fn response info raw))
            (funcall finish)
            (when (and wrapped (eq (plist-get info :transformer) wrapped))
              (plist-put info :transformer transformer))))))))

(defun niwa-gptel-stream-bridge--queue-gptel-stream-insert-batch
    (orig-fn response info)
  "Queue non-raw string RESPONSE for ORIG-FN as a batched stream insert."
  (plist-put info :niwa-stream-insert-orig orig-fn)
  (plist-put info :niwa-stream-insert-parts
             (cons response
                   (plist-get info :niwa-stream-insert-parts)))
  ;; This runs inside the curl stream filter, which can execute while TRAMP
  ;; has timers suspended; the transport holds a timer armed in that window
  ;; until the remote command returns.
  (unless (niwa--timer-pending-p
           (plist-get info :niwa-stream-insert-timer))
    (plist-put info :niwa-stream-diagnostic-scope
               (niwa-diagnostic-scope (plist-get info :buffer)))
    (plist-put
     info :niwa-stream-insert-timer
     (niwa-transport-run-at-time
      niwa-gptel-stream-bridge-insert-batch-delay
      #'niwa-gptel-stream-bridge--flush-batch-due info))))

(defun niwa-gptel-stream-bridge-after-flush (function)
  "Call FUNCTION once the batch being flushed has been inserted.
In the batch's own timer FUNCTION runs before that timer returns.  Emacs
redisplays after each pass over its due timers, and a timer created during
one runs in the next pass, so a zero-delay timer cost a second redisplay
per batch, which pgtk presents in full.  Elsewhere FUNCTION runs from a
zero-delay timer."
  (if niwa-gptel-stream-bridge--after-flush
      (push function (car niwa-gptel-stream-bridge--after-flush))
    (niwa-transport-run-at-time 0 function)))

(defun niwa-gptel-stream-bridge--flush-batch-due (info)
  "Insert INFO's due batch, run its follow-up work and continue paced streams.
One timer pass inserts, renders and continues the paced curl or Claude
Code adapter process: its own resume timer, due at another time, would
cost one more redisplay per batch.  The stream feeding this batch was
paused for the batch delay when the batch began, so the flush continues
processes due within half that delay, or 100 ms: the Claude Code adapter
stays stopped 50 ms past its batch.  Continuing every paused stream
read each of N concurrent streams N times per delay; a stream continued
early falls in step with this one, sharing its passes from then on."
  (let ((after (list nil)))
    (unwind-protect
        (let ((niwa-gptel-stream-bridge--after-flush after))
          (niwa-gptel-stream-bridge--flush-gptel-stream-insert-batch info))
      (dolist (function (nreverse (car after)))
        (condition-case-unless-debug err
            (funcall function)
          (error (niwa-diagnostic-message
                  (or (plist-get info :niwa-stream-diagnostic-scope) 'unavailable)
                  "Stream batch follow-up failed: %s"
                  (error-message-string err)))))
      (let ((delay niwa-gptel-stream-bridge-insert-batch-delay))
        (niwa-transport-resume-paused
         (and (numberp delay) (> delay 0) (max (/ delay 2.0) 0.1)))))))

(defun niwa-gptel-stream-bridge--flush-gptel-stream-insert-batch (info)
  "Flush any pending batched string stream insert on INFO."
  (when-let* ((timer (plist-get info :niwa-stream-insert-timer))
              ((timerp timer)))
    (cancel-timer timer))
  (plist-put info :niwa-stream-insert-timer nil)
  (when-let* ((parts (plist-get info :niwa-stream-insert-parts))
              (orig-fn (plist-get info :niwa-stream-insert-orig))
              ((functionp orig-fn)))
    (plist-put info :niwa-stream-insert-parts nil)
    (let ((inhibit-modification-hooks t)
          (niwa-gptel-stream-bridge-flushing t)
          (niwa-gptel-stream-bridge--insert-batching-suspended t))
      (niwa-gptel-stream-bridge--repair-gptel-stream-info info)
      (when (niwa-gptel-stream-bridge--gptel-stream-info-p info)
        (niwa-gptel-stream-bridge--insert-tracked
         orig-fn (apply #'concat (nreverse parts)) info nil)))))

(defun niwa-gptel-stream-bridge-discard-batch (info)
  "Discard provisional inserts owned by the failed attempt INFO."
  (when-let* ((timer (plist-get info :niwa-stream-insert-timer)))
    (cancel-timer timer))
  (plist-put info :niwa-stream-insert-timer nil)
  (plist-put info :niwa-stream-insert-parts nil))

(defconst niwa-gptel-stream-bridge--pace-backlog-bytes 4096
  "Reads at least this large mean curl has a backlog to deliver.
Pausing after such a read capped a large stream at one pipe read per
pause, about 72 KB a second; Responses API events alone can approach that.")

(defun niwa-gptel-stream-bridge--stream-info (process)
  "Return the gptel request INFO of PROCESS, or nil."
  (when-let* ((fsm (niwa-gptel-stream-bridge--gptel-stream-filter-registered-p
                    process)))
    (and (fboundp 'gptel-fsm-info)
         (ignore-errors (gptel-fsm-info fsm)))))

(defun niwa-gptel-stream-bridge--note-stream-start (process)
  "Remember the stop reason PROCESS's request had before its first read.
gptel keeps one INFO for every tool round and never clears its stop
reason, so the previous round's would otherwise end pacing at once."
  (unless (process-get process 'niwa-gptel-stream-bridge--started)
    (process-put process 'niwa-gptel-stream-bridge--started t)
    (process-put process 'niwa-gptel-stream-bridge--stale-stop-reason
                 (plist-get (niwa-gptel-stream-bridge--stream-info process)
                            :stop-reason))))

(defun niwa-gptel-stream-bridge--stream-ended-p (process)
  "Return non-nil when PROCESS has delivered its complete response.
gptel records a stop reason as soon as the Responses, Anthropic, Gemini
and Bedrock streams finish; OpenAI-compatible chat streams end with
`data: [DONE]'.  A reason left from an earlier tool round does not count."
  (or (when-let* ((reason (plist-get (niwa-gptel-stream-bridge--stream-info process)
                                     :stop-reason)))
        (not (eq reason (process-get
                         process 'niwa-gptel-stream-bridge--stale-stop-reason))))
      (when-let* ((buffer (process-buffer process))
                  ((buffer-live-p buffer)))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-max))
            (search-backward "data: [DONE]" (max (point-min) (- (point-max) 256))
                             t))))))

(defun niwa-gptel-stream-bridge--pace-reading (process output)
  "Pace local curl PROCESS after reading OUTPUT until the next batch is due.
See `niwa-transport-pause-process': a stream of 32 chunks a second held
the editor at 59% CPU with nothing animating, and 17% paced.  Only a small
read pauses: a large one means curl has more to deliver, and pausing after
every read held a 1.1 MB stream to 15.6 s.  A finished response continues
at once, even when the read that stopped curl held only part of the final
burst: curl must exit for gptel to settle the request and run its tools.
Only niwa's own streams are paced; gptel's sentinel ignores the stop
and continue events."
  (cond
   ((niwa-gptel-stream-bridge--stream-ended-p process)
    (niwa-transport-resume-process process))
   ((< (string-bytes output) niwa-gptel-stream-bridge--pace-backlog-bytes)
    (when (niwa-gptel-stream-bridge--gptel-stream-info-p
           (niwa-gptel-stream-bridge--stream-info process))
      ;; The batch's flush continues PROCESS; its own resume timer is the
      ;; fallback when no text is queued.
      (niwa-transport-pause-process
       process niwa-gptel-stream-bridge-insert-batch-delay)))))

(defun niwa-gptel-stream-bridge--gptel-stream-cleanup-advice (orig-fn process status)
  "Call ORIG-FN after wrapping stream transformers for PROCESS.
STATUS is passed through unchanged.  Stop and continue events from
`niwa-gptel-stream-bridge--pace-reading' do not end the stream and
never reach ORIG-FN, which treats every call as the end.

Runs with GC batched: the cleanup drives the terminal settlement --
response parsing, FSM transitions, and the renders they trigger -- and
was a leading allocator in a profiled session."
  (unless (memq (process-status process) '(stop run))
    (niwa-gptel-stream-bridge--cleanup process status orig-fn)))

(defun niwa-gptel-stream-bridge--cleanup (process status orig-fn)
  "Settle PROCESS's stream with STATUS through gptel's ORIG-FN sentinel."
  (niwa-transport-resume-process process)
  (niwa--with-gc-batched
   (let* ((entry (alist-get process gptel--request-alist))
          (fsm (car-safe entry))
          (info (and fsm (fboundp 'gptel-fsm-info)
                     (ignore-errors (gptel-fsm-info fsm))))
          (chat-buffer (plist-get info :buffer))
          (session (and (buffer-live-p chat-buffer)
                        (niwa-telemetry-current-session chat-buffer))))
     (when (niwa-gptel-stream-bridge--gptel-stream-info-p info)
       (niwa-gptel-stream-bridge--flush-gptel-stream-insert-batch info)
       (niwa-gptel-stream-bridge--wrap-gptel-stream-transformer info))
     (prog1 (funcall orig-fn process status)
       (when session
         (niwa-telemetry-record
          session 'provider-stream-ended
          :request-id (plist-get info :niwa-request-id)
          :agent-path
          (when-let* ((invocation (plist-get info :niwa-agent-invocation)))
            (niwa-agent-invocation-path invocation))
          :provider-status status
          :first-byte-seen
          (and (process-get process 'niwa-telemetry-first-byte) t)))))))

(defun niwa-gptel-stream-bridge--gptel-stream-filter-registered-p (process)
  "Return non-nil when PROCESS has a registered gptel FSM."
  (and (boundp 'gptel--request-alist)
       (car-safe (alist-get process gptel--request-alist))))

(defun niwa-gptel-stream-bridge--schedule-gptel-stream-filter-flush (process)
  "Schedule a deferred gptel stream filter flush for PROCESS."
  ;; Same hazard as the insert-batch timer: this is armed from the stream
  ;; filter, so let the transport hold it across a remote command.
  (unless (niwa--timer-pending-p
           (process-get process 'niwa-gptel-stream-bridge--filter-timer))
    (process-put
     process 'niwa-gptel-stream-bridge--filter-timer
     (niwa-transport-run-at-time
      0 #'niwa-gptel-stream-bridge--flush-gptel-stream-filter process))))

(defun niwa-gptel-stream-bridge--flush-gptel-stream-filter (process)
  "Flush buffered early stream chunks for PROCESS once gptel is ready."
  (process-put process 'niwa-gptel-stream-bridge--filter-timer nil)
  (when (process-get process 'niwa-gptel-stream-bridge--pending-output)
    (cond
     ((not (process-live-p process))
      (process-put process 'niwa-gptel-stream-bridge--pending-output nil)
      (process-put process 'niwa-gptel-stream-bridge--filter-retries nil))
     ((niwa-gptel-stream-bridge--gptel-stream-filter-registered-p process)
      (process-put process 'niwa-gptel-stream-bridge--filter-retries nil)
      (gptel-curl--stream-filter process ""))
     (t
      (let ((retries
             (1+ (or (process-get process
                                  'niwa-gptel-stream-bridge--filter-retries)
                     0))))
        (if (> retries niwa-gptel-stream-bridge--gptel-stream-filter-max-retries)
            (progn
              (process-put process 'niwa-gptel-stream-bridge--pending-output nil)
              (process-put process 'niwa-gptel-stream-bridge--filter-retries nil)
              (niwa--warn-once
               'unavailable 'stream-bridge-orphan-chunk
               "Dropping gptel stream chunk without registered request FSM"))
          (process-put process 'niwa-gptel-stream-bridge--filter-retries retries)
          (process-put
           process 'niwa-gptel-stream-bridge--filter-timer
           (niwa-transport-run-at-time
            0.01 #'niwa-gptel-stream-bridge--flush-gptel-stream-filter
            process))))))))

(defun niwa-gptel-stream-bridge--gptel-stream-filter-advice (orig-fn process output)
  "Delay ORIG-FN until gptel has registered PROCESS's FSM.
OUTPUT is the stream chunk passed to gptel's process filter.

`gptel-curl-get-response' installs the streaming process filter before
it records PROCESS in `gptel--request-alist'.  If curl produces an
early chunk in that gap, gptel's filter sees a nil FSM.  Preserve the
chunk and replay it once the request entry exists.

Chunk parsing and insertion is steady allocation for the whole stream;
the entire filter runs with collections batched."
  (niwa--with-gc-batched
   (when (and (> (length output) 0)
              (not (process-get process 'niwa-telemetry-first-byte)))
     (when-let* ((entry (and (boundp 'gptel--request-alist)
                             (alist-get process gptel--request-alist)))
                 (fsm (car-safe entry))
                 (info (and (fboundp 'gptel-fsm-info)
                            (ignore-errors (gptel-fsm-info fsm))))
                 (chat-buffer (plist-get info :buffer))
                 ((buffer-live-p chat-buffer)))
       (process-put process 'niwa-telemetry-first-byte t)
       (with-current-buffer chat-buffer
         (when (niwa-telemetry-current-session chat-buffer)
           (niwa-telemetry-record
            (niwa-telemetry-current-session chat-buffer)
            'provider-first-byte
            :request-id (plist-get info :niwa-request-id)
            :agent-path
            (when-let* ((invocation
                         (plist-get info :niwa-agent-invocation)))
              (niwa-agent-invocation-path invocation))
            :chunk-bytes (string-bytes output))))))
   (let ((pending (process-get process
                               'niwa-gptel-stream-bridge--pending-output)))
     (if (niwa-gptel-stream-bridge--gptel-stream-filter-registered-p process)
         (progn
           (when pending
             (setq output (concat pending output))
             (process-put process 'niwa-gptel-stream-bridge--pending-output nil))
           (process-put process 'niwa-gptel-stream-bridge--filter-retries nil)
           (niwa-gptel-stream-bridge--note-stream-start process)
           (funcall orig-fn process output)
           (niwa-gptel-stream-bridge--pace-reading process output))
       (process-put process 'niwa-gptel-stream-bridge--pending-output
                    (concat pending output))
       (niwa-gptel-stream-bridge--schedule-gptel-stream-filter-flush process)))))


(defun niwa-gptel-stream-bridge--advice-add-if-bound (symbol where function)
  "Add advice FUNCTION to SYMBOL at WHERE when SYMBOL is fbound."
  (when (and (fboundp symbol)
             (not (advice-member-p function symbol)))
    (advice-add symbol where function)))

(defun niwa-gptel-stream-bridge--advice-remove-if-bound (symbol function)
  "Remove advice FUNCTION from SYMBOL when SYMBOL is fbound."
  (when (fboundp symbol)
    (advice-remove symbol function)))

(defun niwa-gptel-stream-bridge--install-advice ()
  "Install gptel stream marker repair advice."
  (niwa-gptel-stream-bridge--advice-add-if-bound
   'gptel-curl--stream-insert-response
   :around #'niwa-gptel-stream-bridge--gptel-stream-insert-response-advice)
  (niwa-gptel-stream-bridge--advice-add-if-bound
   'gptel-curl--stream-cleanup
   :around #'niwa-gptel-stream-bridge--gptel-stream-cleanup-advice)
  (niwa-gptel-stream-bridge--advice-add-if-bound
   'gptel-curl--stream-filter
   :around #'niwa-gptel-stream-bridge--gptel-stream-filter-advice))

(defun niwa-gptel-stream-bridge--install-if-enabled ()
  "Install gptel stream marker repair advice when enabled."
  (when niwa-gptel-stream-bridge--gptel-stream-advice-installed
    (niwa-gptel-stream-bridge--install-advice)))

(defun niwa-gptel-stream-bridge--uninstall-advice ()
  "Remove gptel stream marker repair advice."
  (niwa-gptel-stream-bridge--advice-remove-if-bound
   'gptel-curl--stream-insert-response
   #'niwa-gptel-stream-bridge--gptel-stream-insert-response-advice)
  (niwa-gptel-stream-bridge--advice-remove-if-bound
   'gptel-curl--stream-cleanup
   #'niwa-gptel-stream-bridge--gptel-stream-cleanup-advice)
  (niwa-gptel-stream-bridge--advice-remove-if-bound
   'gptel-curl--stream-filter
   #'niwa-gptel-stream-bridge--gptel-stream-filter-advice))

(defun niwa-gptel-stream-bridge-install ()
  "Install gptel stream compatibility advice."
  (setq niwa-gptel-stream-bridge--gptel-stream-advice-installed t)
  (niwa-gptel-stream-bridge--install-if-enabled)
  (with-eval-after-load 'gptel
    (niwa-gptel-stream-bridge--install-if-enabled))
  (with-eval-after-load 'gptel-request
    (niwa-gptel-stream-bridge--install-if-enabled)))

(defun niwa-gptel-stream-bridge-uninstall ()
  "Remove gptel stream compatibility advice.
Paced streams continue first: without the advice, gptel's sentinel would
take their continue event for the end of the response."
  (niwa-transport-resume-paused)
  (setq niwa-gptel-stream-bridge--gptel-stream-advice-installed nil)
  (niwa-gptel-stream-bridge--uninstall-advice))

(provide 'niwa-gptel-stream-bridge)

;;; niwa-gptel-stream-bridge.el ends here
