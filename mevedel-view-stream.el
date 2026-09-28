;;; mevedel-view-stream.el --- Streaming view lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Owns active-turn progress state and coalesced streaming/tool-boundary
;; redraws.  The gptel compatibility layer and durable execution transcripts
;; have dedicated owners; transcript interpretation and composer editing remain
;; behind the view module's rendering interface.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-execution-transcript)
(require 'mevedel-view-animation)
(require 'mevedel-view-power)
(require 'mevedel-view-zone)

;; `cl-extra'
(declare-function cl-subseq "cl-extra" (sequence start &optional end))

;; `gptel'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `mevedel-compact-run'
(defvar mevedel-compact-run-in-flight)

;; `mevedel-execution-transcript'
(declare-function mevedel-execution-transcript-handle-event
                  "mevedel-execution-transcript" (event))
(declare-function mevedel-execution-transcript-retry-pending-terminals
                  "mevedel-execution-transcript" (data-buffer))

;; `mevedel-structs'
(declare-function mevedel-request-active-elapsed-seconds
                  "mevedel-structs" (request &optional now))
(declare-function mevedel-request-active-work-pause-started-at
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-current-session
                  "mevedel-telemetry" (&optional buffer))
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-utilities'
(declare-function mevedel--timer-pending-p "mevedel-utilities" (timer))
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))

;; `mevedel-view'
(declare-function mevedel-view--cancel-scheduled-render "mevedel-view" ())
(declare-function mevedel-view--schedule-render
                  "mevedel-view" (kind data-buffer delay))
(declare-function mevedel-view--tool-status-string "mevedel-view" (tool-name args))
(declare-function mevedel-view--unattended-p "mevedel-view" (&optional buffer))
(declare-function mevedel-view-rerender "mevedel-view" (&optional buffer))
(defvar mevedel-view--display-map)
(defvar mevedel-view--pending-render-kind)
(defvar mevedel-view--pending-tool-rows)
(defvar mevedel-view-pending-tools-visible-max)
(defvar mevedel-view-rerender-debounce)
(defvar mevedel-view-spinner-animate)
(defvar mevedel-view-spinner-style)
(defvar mevedel-view-tool-spinner-style)
(defvar mevedel-view-spinner-framerate)
(defvar mevedel-view-spinner-battery-framerate)
(defvar mevedel-view-spinner-power-policy)

;; `mevedel-view-agent'
(declare-function mevedel-view--agent-status-counts "mevedel-view-agent" ())
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-composer'
(declare-function mevedel-view--agent-fsm-p
                  "mevedel-view-composer" (info data-buffer))
(declare-function mevedel-view--call-preserving-input-point
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--call-preserving-user-view-state
                  "mevedel-view-composer" (thunk))

;; `mevedel-view-render'
(declare-function mevedel-view-render-mutate
                  "mevedel-view-render" (key function &optional replacement cleanup))
(declare-function mevedel-view-render-terminal "mevedel-view-render" ())
(declare-function mevedel-view-render--settle-now
                  "mevedel-view-render" (data-buf start end))
(defvar mevedel-view-render--owner)
(defvar mevedel-view-render--terminal-p)
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
(declare-function mevedel-view--refresh-tool-row
                  "mevedel-view-render" (data-buffer tool-use-id))
(declare-function mevedel-view--request-progress-anchor
                  "mevedel-view-render" ())
(declare-function mevedel-view-render-invalidate-live-tail
                  "mevedel-view-render" ())
(declare-function mevedel-view-render-live-update
                  "mevedel-view-render" (data-buf))
(declare-function mevedel-view-render-settle
                  "mevedel-view-render" (data-buf start end))

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-clear "mevedel-view-zone" (namespace))
(declare-function mevedel-view-zone-forget "mevedel-view-zone" (&optional namespace))
(declare-function mevedel-view-zone-reconcile "mevedel-view-zone" (namespace start end fragments))
(declare-function mevedel-view-zone-region "mevedel-view-zone" (namespace))
(declare-function mevedel-view-zone-start "mevedel-view-zone" (namespace))

(defvar-local mevedel-view--spinner-status nil
  "Current base status text shown by the request-progress row.")

(defvar-local mevedel-view--spinner-owner nil
  "Subsystem that last wrote the request-progress status text.")

(defvar-local mevedel-view--spinner-generation 0
  "Generation of the latest semantic request-progress status update.")

(defvar mevedel-view--status-owner-override nil
  "Dynamically bound subsystem owner for one status update.")

(defvar-local mevedel-view--spinner-start-time nil
  "Fallback wall-clock start time for spinner elapsed display.
Used before a `mevedel-request' exists, or in tests that exercise the
spinner without a data-buffer request.")

(defvar-local mevedel-view--spinner-timer nil
  "Buffer-local timer animating visible spinner frames.")

(defvar-local mevedel-view--spinner-phase-start nil
  "Wall-clock time used to derive animation phase across timer stalls.")

(defvar-local mevedel-view--spinner-frozen-seconds nil
  "Animation phase held while decorative motion is disabled.")

(defvar-local mevedel-view--spinner-timer-period nil
  "Current visual timer cadence, or nil when no timer is running.")

(defvar-local mevedel-view--spinner-main-color-p nil
  "Non-nil when the main style renders color rather than a glyph fallback.")

(defvar-local mevedel-view--spinner-label-target nil
  "Marker for the foreground label's animation property.")

(defvar-local mevedel-view--spinner-metadata-target nil
  "Markers for the foreground label's separate elapsed metadata span.")

(defvar-local mevedel-view--spinner-tool-targets nil
  "Markers for the visible pending-tool indicator properties.")

(defvar-local mevedel-view--spinner-tool-samples nil
  "Last displayed tool phases, keyed by pending-tool zone id.")

(defvar-local mevedel-view--spinner-last-second nil
  "Elapsed label last rendered on a timer tick.")

(defvar-local mevedel-view--spinner-last-sample-seconds nil
  "Phase of the most recently sampled request-label display frame.")

(defvar-local mevedel-view--spinner-theme-stale-p nil
  "Non-nil when a theme change has invalidated the displayed color sample.")

(defvar-local mevedel-view--spinner-sample-frame nil
  "Display frame used for the request label's last color sample.
The value `:multiple' means the portable multi-frame fallback was used.")

(defvar-local mevedel-view--spinner-rendered-tool-style nil
  "Style of the current pending-tool indicator fragments.")

(defvar-local mevedel-view--spinner-rendered-state nil
  "Last (STATUS PREFIX) rendered in the request-progress fragment.")

(defvar-local mevedel-view--request-progress-suppressed nil
  "Non-nil means the request progress row must not be recreated.
Set during terminal cleanup; cleared when a new progress row is
explicitly started.")

(defvar-local mevedel-view--in-flight-turn-start nil
  "View-buffer marker at which the current assistant turn's render begins.

Set by the send path right after the user turn is echoed, consumed by
`mevedel-view-render-live-update' to bound the delete-and-re-render
region for each progress update, and cleared when the final
`gptel-post-response-functions' render completes.  Nil outside an
active exchange.")

(defvar-local mevedel-view--data-turn-start nil
  "Data-buffer marker at which the current assistant turn starts.

Anchored just after the user prompt was forwarded to the data
buffer, so `mevedel-view-render-live-update' can extract only the
in-flight assistant portion (not the whole conversation) when
rebuilding the view.  Nil outside an active exchange.")

(defvar-local mevedel-view--pending-tool-serial 0
  "Counter distinguishing pending tool calls that share a fingerprint.")

(defvar-local mevedel-view--pending-tool-calls nil
  "Alist of in-flight tool calls, one entry per call.
Each entry is `(KEY . TOOL-NAME)' where KEY is
`((NAME . ARGS-PRINT) . SERIAL)' and TOOL-NAME is the displayed tool
name.  Identical parallel calls share the fingerprint half and are
told apart by the serial.

Pre-tool hook adds an entry; post-tool hook removes one.  The
render path walks this alist and emits one `Calling X…' line per
entry in arrival order, respecting
`mevedel-view-pending-tools-visible-max' for truncation when many
tools are in flight in parallel.")

(defvar-local mevedel-view--execution-event-entries 0
  "Number of entries retained in `mevedel-view--execution-events'.")

(defvar-local mevedel-view--execution-events nil
  "Latest transient tool progress keyed by durable tool-use id.")

(defcustom mevedel-view-stream-render-delay 0.4
  "Seconds to wait before rendering a batch of stream chunks.

The `gptel-post-stream-hook' path fires once per streamed chunk (up to
dozens per second).  `mevedel-view-stream-schedule' lets chunks arriving
inside one pending window share a single incremental render.  Tune higher
if the render cost is visible in your environment; lower for snappier updates.

Tool boundaries use a shorter delay but join the same pending render."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-view-tool-boundary-render-delay 0.05
  "Seconds to coalesce incremental renders around tool boundaries.
Pre/post tool hooks update their lightweight pending-tool status lines
immediately, then use this delay for the heavier transcript render."
  :type 'number
  :group 'mevedel)

(defun mevedel-view--animation-seconds ()
  "Return continuous seconds since the current progress animation started."
  (max 0.0 (- (float-time) (or mevedel-view--spinner-phase-start
                                (float-time)))))

(defun mevedel-view--animation-display-seconds ()
  "Return the phase for a newly rendered progress label.
Semantic redraws must not advance the glyph when motion is disabled.
The underlying phase continues to advance and resumes without restarting."
  (if (zerop (mevedel-view-power-framerate
             mevedel-view-spinner-framerate
             mevedel-view-spinner-battery-framerate
             mevedel-view-spinner-power-policy
             mevedel-view-spinner-animate))
      (or mevedel-view--spinner-frozen-seconds
          (setq mevedel-view--spinner-frozen-seconds
                (or mevedel-view--spinner-last-sample-seconds
                    (mevedel-view--animation-seconds))))
    (setq mevedel-view--spinner-frozen-seconds nil)
    (setq mevedel-view--spinner-last-sample-seconds
          (mevedel-view--animation-seconds))))

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

(defun mevedel-view--spinner-request ()
  "Return the request that owns the current visible spinner, or nil."
  (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                             mevedel--data-buffer))
              ((buffer-live-p data-buf)))
    (buffer-local-value 'mevedel--current-request data-buf)))

(defun mevedel-view--spinner-elapsed-label ()
  "Return the elapsed-time label for the current spinner, or nil."
  (when-let* ((seconds
               (if-let* ((request (mevedel-view--spinner-request)))
                   (mevedel-request-active-elapsed-seconds request)
                 (and mevedel-view--spinner-start-time
                      (float-time
                       (time-subtract
                        (current-time)
                        mevedel-view--spinner-start-time))))))
    (mevedel-view--duration-label
     seconds)))

(defun mevedel-view--request-progress-active-p (&optional data-buf)
  "Return non-nil when the current view should show request progress.
DATA-BUF defaults to this view's data buffer.  The predicate accepts
both fully materialized requests and the short pre-WAIT interval where
the view has already inserted the in-flight markers."
  (and (not mevedel-view--agent-transcript-p)
       (not mevedel-view--request-progress-suppressed)
       (or mevedel-view--spinner-start-time
           (mevedel-view-stream-in-flight-turn-start-position)
           (let ((buf (or data-buf
                          (and (boundp 'mevedel--data-buffer)
                               mevedel--data-buffer))))
             (and buf
                  (buffer-live-p buf)
                  (buffer-local-value 'mevedel--current-request buf))))))

(defun mevedel-view--request-progress-visible-p ()
  "Return non-nil when a request-progress fragment is visible."
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
  (and (mevedel-view--request-progress-visible-p)
       (mevedel-view-zone-start 'progress)))

(defun mevedel-view--request-progress-fragments (status &optional display-status)
  "Return the fragment list for request-progress STATUS."
  (list (list :namespace 'progress
              :id 'request
              :priority 0
              :body (mevedel-view--format-spinner-block status display-status)
              :keymap mevedel-view--display-map
              :navigatable nil)))

(defun mevedel-view--render-request-progress ()
  "Render the current request-progress row from buffer-local state."
  (when mevedel-view--spinner-status
    (let* ((display-status
            (mevedel-view--spinner-display-status
             mevedel-view--spinner-status))
           (render-state (list display-status
                               (mevedel-view--request-progress-prefix))))
      (unless (and (equal render-state
                          mevedel-view--spinner-rendered-state)
                   (mevedel-view-zone-region 'progress))
        (let ((anchor (mevedel-view--request-progress-anchor)))
          (mevedel-view-zone-reconcile
           'progress anchor anchor
           (mevedel-view--request-progress-fragments
            mevedel-view--spinner-status display-status))
          (setq mevedel-view--spinner-rendered-state
                render-state)
          (mevedel-view--capture-request-animation-target))))))

(defun mevedel-view--clear-request-progress ()
  "Remove the fragment-managed request-progress row."
  (mevedel-view-zone-clear 'progress)
  (setq mevedel-view--spinner-rendered-state nil
        mevedel-view--spinner-label-target nil
        mevedel-view--spinner-metadata-target nil
        mevedel-view--spinner-sample-frame nil))

(defun mevedel-view--forget-request-progress-region ()
  "Forget the request-progress region after a larger redraw deleted it."
  (mevedel-view-zone-forget 'progress)
  (setq mevedel-view--spinner-rendered-state nil
        mevedel-view--spinner-label-target nil
        mevedel-view--spinner-metadata-target nil
        mevedel-view--spinner-sample-frame nil))

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
  (let* ((base
          (if-let* ((request (mevedel-view--spinner-request))
                    ((mevedel-request-active-work-pause-started-at request)))
              "Waiting for input"
            (mevedel-view--spinner-base-status status)))
         (elapsed (mevedel-view--spinner-elapsed-label))
         (agents (mevedel-view--spinner-agent-count-label)))
    (string-join (delq nil (list base elapsed agents)) " · ")))

(defun mevedel-view--format-spinner-line (status &optional face display-status)
  "Return propertized spinner line for STATUS.
FACE defaults to `mevedel-view-spinner'."
  (let* ((face (or face 'mevedel-view-spinner))
         (display-status (or display-status
                             (mevedel-view--spinner-display-status status)))
         (base (if (string-prefix-p "Waiting for input" display-status)
                   "Waiting for input"
                 (mevedel-view--spinner-base-status status)))
         (label (propertize base
                            'font-lock-face face
                            'mevedel-view-spinner-frame t
                            'display (mevedel-view-animation-frame
                                      mevedel-view-spinner-style base
                                      (mevedel-view--animation-display-seconds) face
                                      (mevedel-view--animation-buffer-frame))
                            'read-only t
                            'keymap mevedel-view--display-map
                            'front-sticky '(read-only keymap)
                            'rear-nonsticky '(read-only keymap))))
    (concat label
            (propertize (concat (substring display-status (length base)) "\n")
                        'font-lock-face face
                        'mevedel-view-spinner-status
                        (mevedel-view--spinner-base-status status)
                        'read-only t
                        'keymap mevedel-view--display-map
                        'front-sticky '(read-only keymap)
                        'rear-nonsticky '(read-only keymap)))))

(defun mevedel-view--request-progress-prefix ()
  "Return separator text before the request progress row."
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

(defun mevedel-view--format-spinner-block (status &optional display-status)
  "Return request-progress spinner text for STATUS at point."
  (concat (mevedel-view--request-progress-prefix)
          (mevedel-view--format-spinner-line status nil display-status)))

(defun mevedel-view--spinner-active-p ()
  "Return non-nil when this view buffer has visible spinner work."
  (or mevedel-view--pending-tool-calls
      (mevedel-view--request-progress-visible-p)
      (mevedel-view--request-progress-active-p)))

(defun mevedel-view--capture-request-animation-target ()
  "Remember the label and metadata spans after a semantic progress render."
  (setq mevedel-view--spinner-label-target nil
        mevedel-view--spinner-metadata-target nil
        mevedel-view--spinner-sample-frame nil)
  (when-let* ((region (mevedel-view-zone-region 'progress))
              (pos (text-property-any (overlay-start region)
                                      (overlay-end region)
                                      'mevedel-view-spinner-frame t)))
    (let ((label-end (or (next-single-property-change
                          pos 'mevedel-view-spinner-frame nil
                          (overlay-end region))
                         (overlay-end region)))
          (metadata-end (1- (overlay-end region))))
      (setq mevedel-view--spinner-label-target
            (cons (copy-marker pos) (copy-marker label-end t))
            mevedel-view--spinner-sample-frame
            (mevedel-view--animation-buffer-frame))
      ;; The fragment ends with a newline.  Its suffix can remain visible
      ;; after horizontal scrolling hides the decorative label.
      (when (< label-end metadata-end)
        (setq mevedel-view--spinner-metadata-target
              (cons (copy-marker label-end)
                    (copy-marker metadata-end t)))))))

(defun mevedel-view--capture-tool-animation-targets ()
  "Remember the bounded pending-tool spans after live-tail reconciliation."
  (setq mevedel-view--spinner-tool-targets nil)
  (when-let* ((region (mevedel-view-zone-region 'history-live)))
    (let ((pos (overlay-start region))
          (end (overlay-end region)))
      (while (and pos (< pos end)
                  (setq pos (text-property-any
                             pos end 'mevedel-view-inline-spinner-frame t)))
        (let ((span-end (or (next-single-property-change
                             pos 'mevedel-view-inline-spinner-frame nil end)
                            end)))
          (push (cons (copy-marker pos) (copy-marker span-end t))
                mevedel-view--spinner-tool-targets)
          (setq pos span-end))))
    (setq mevedel-view--spinner-tool-targets
          (nreverse mevedel-view--spinner-tool-targets)))
  (setq mevedel-view--spinner-tool-samples
        (cl-remove-if-not
         (lambda (sample)
           (cl-some
            (lambda (target)
              (equal (car sample)
                     (get-text-property (marker-position (car target))
                                        'mevedel-view-zone-id)))
            mevedel-view--spinner-tool-targets))
         mevedel-view--spinner-tool-samples)))

(defun mevedel-view--tool-sample-seconds (start)
  "Return the displayed phase for the pending tool indicator at START.
Newly inserted tool rows begin at phase zero."
  (or (cdr (assoc (get-text-property start 'mevedel-view-zone-id)
                  mevedel-view--spinner-tool-samples))
      0.0))

(defun mevedel-view--record-tool-sample (start seconds)
  "Record the displayed phase SECONDS for the tool indicator at START."
  (let* ((id (get-text-property start 'mevedel-view-zone-id))
         (sample (assoc id mevedel-view--spinner-tool-samples)))
    (if sample
        (setcdr sample seconds)
      (push (cons id seconds) mevedel-view--spinner-tool-samples))))

(defun mevedel-view--snapshot-tool-animation-targets ()
  "Return live tool (ID DISPLAY PHASE) samples before replacing their rows."
  (when (eq mevedel-view--spinner-rendered-tool-style
            mevedel-view-tool-spinner-style)
    (delq nil
          (mapcar (lambda (target)
                    (when-let* ((start (marker-position (car target)))
                                ((eq (marker-buffer (car target))
                                     (current-buffer)))
                                (id (get-text-property
                                     start 'mevedel-view-zone-id)))
                      (list id (get-text-property start 'display)
                            (mevedel-view--tool-sample-seconds start))))
                  mevedel-view--spinner-tool-targets))))

(defun mevedel-view--restore-tool-animation-targets (previous)
  "Restore surviving tool DISPLAY and PHASE entries from PREVIOUS."
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t)
        (inhibit-modification-hooks t)
        (buffer-undo-list t))
    (unwind-protect
        (dolist (target mevedel-view--spinner-tool-targets)
          (let* ((start (marker-position (car target)))
                 (entry (assoc (get-text-property start 'mevedel-view-zone-id)
                               previous)))
            (when entry
              (unless (equal-including-properties
                       (cadr entry) (get-text-property start 'display))
                (put-text-property start (marker-position (cdr target))
                                   'display (cadr entry)))
              (mevedel-view--record-tool-sample start (caddr entry)))))
      (set-buffer-modified-p modified))))

(defun mevedel-view--animation-buffer-frame ()
  "Return a visible display frame for this buffer, or `:multiple'.
Before a span has been inserted, use portable glyphs when no frame or
more than one distinct frame can display the buffer."
  (let (found)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (when (eq (frame-visible-p (window-frame window)) t)
        (if (and found (not (eq found (window-frame window))))
            (setq found :multiple)
          (unless found (setq found (window-frame window))))))
    (or found :multiple)))

(defun mevedel-view--animation-span-in-window-p (start end window)
  "Return non-nil if the animated span START..END appears in WINDOW.
Replacement display strings map every character back to their source span,
so buffer positions alone cannot reveal which animated characters survive
horizontal scrolling.  Skip pixel positioning in unscrolled windows."
  (and (< start end)
       (<= (window-start window) start)
       (< start (or (window-end window) (point-min)))
       (or (zerop (window-hscroll window))
           (let ((display (get-text-property start 'display)))
             (if (not (stringp display))
                 ;; The elapsed suffix is ordinary buffer text.
                 (cl-some
                  (lambda (position)
                    (when-let* ((sample (posn-at-point position window))
                                (xy (posn-x-y sample)))
                      (and (null (posn-area sample))
                           (<= 0 (car xy))
                           (< (car xy) (window-body-width window t)))))
                  (list start (1- end)))
               (when-let* ((sample (or (posn-at-point start window)
                                       (posn-at-point (1- end) window)
                                       (posn-at-point
                                        (+ start (/ (- end start) 2)) window)))
                           (xy (posn-x-y sample))
                           (left (posn-at-x-y 0 (cdr xy) window))
                           (point (posn-point left))
                           ((and (integerp point) (<= start point) (< point end)))
                           (first (posn-string left))
                           (index (cdr first)))
                 (let* ((style mevedel-view-spinner-style)
                        (tool (get-text-property
                               start 'mevedel-view-inline-spinner-frame))
                        (animated-start
                         (if (and (not tool) (eq style 'ellipsis))
                             (max 0 (- (length display) 3))
                           0))
                        (animated-end
                         (cond (tool (length display))
                               ((memq style '(shimmer breathe bounce))
                                (if (get-text-property 0 'face display)
                                    ;; A combining mark crossing the limit
                                    ;; leaves the final cluster uncolored.
                                    (let ((limit (min (length display)
                                                      mevedel-view-animation--prefix-limit)))
                                      (while (and (> limit 0)
                                                  (not (get-text-property
                                                        (1- limit) 'face display)))
                                        (setq limit (1- limit)))
                                      limit)
                                  2))
                               ((eq style 'dots) 5)
                               ((memq style '(braille ascii)) 2)
                               ((eq style 'ellipsis) (length display))
                               (t 0))))
                   (and (< index animated-end)
                        (or (zerop animated-start)
                            (let* ((right
                                    (posn-at-x-y
                                     (max 0 (1- (window-body-width window t)))
                                     (cdr xy) window))
                                   (right-point (and right (posn-point right)))
                                   (right-string (and right (posn-string right))))
                              (or (and (integerp right-point)
                                       (>= right-point end))
                                  (and right-string
                                       (>= (cdr right-string)
                                           animated-start)))))))))))))

(defun mevedel-view--animation-window-attended-p (window)
  "Return non-nil when WINDOW can show animation to an attentive reader.
Unlike the transcript's buffer-wide attention gate, this check must apply
to the same window as the target's on-screen position."
  (let* ((frame (window-frame window))
         (top frame))
    (while (frame-parent top)
      (setq top (frame-parent top)))
    (and (eq (frame-visible-p frame) t)
         (or (not (display-graphic-p top))
             (frame-focus-state top)))))

(defun mevedel-view--animation-target-visible-p (target property &optional any-value)
  "Return non-nil when TARGET is a visible span marked PROPERTY.
If ANY-VALUE is non-nil, accept any non-nil PROPERTY value, not just t."
  (when-let* ((start (car-safe target))
              ((eq (marker-buffer start) (current-buffer)))
              (pos (marker-position start))
              ((if any-value
                   (get-text-property pos property)
                 (eq (get-text-property pos property) t))))
    (cl-some (lambda (window)
               (and (mevedel-view--animation-window-attended-p window)
                    (mevedel-view--animation-span-in-window-p
                     pos (marker-position (cdr target)) window)))
             (get-buffer-window-list (current-buffer) nil t))))

(defun mevedel-view--spinner-metadata-visible-p ()
  "Return non-nil when the request's elapsed suffix is on screen."
  (mevedel-view--animation-target-visible-p
   mevedel-view--spinner-metadata-target
   'mevedel-view-spinner-status t))

(defun mevedel-view--animation-target-frame (target &optional all)
  "Return TARGET's visible frame, or `:multiple' across display frames.
Color display properties cannot use two palettes simultaneously, so the
caller uses a glyph fallback when more than one frame shows the span.
When ALL is non-nil, return the frames actually showing TARGET instead;
glyph styles can check their display support without inspecting unrelated
windows."
  (let ((position (marker-position (car target)))
        (end (marker-position (cdr target)))
        found frames)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (when (and (eq (frame-visible-p (window-frame window)) t)
                 (mevedel-view--animation-span-in-window-p position end window))
        (when all (cl-pushnew (window-frame window) frames))
        (if (and found (not (eq found (window-frame window))))
            (setq found :multiple)
          (unless found (setq found (window-frame window))))))
    (if all (nreverse frames) found)))

(defun mevedel-view--animation-visible-p ()
  "Return non-nil when a progress or pending-tool animation can be seen."
  (or (mevedel-view--animation-target-visible-p
       mevedel-view--spinner-label-target 'mevedel-view-spinner-frame)
      (cl-some (lambda (target)
                 (mevedel-view--animation-target-visible-p
                  target 'mevedel-view-inline-spinner-frame))
               mevedel-view--spinner-tool-targets)))

(defun mevedel-view--animation-target-in-window-rows-p (window)
  "Return non-nil if any registered target occupies WINDOW's visible rows."
  (cl-some
   (lambda (target)
     (when-let* ((start (car-safe target))
                 ((eq (marker-buffer start) (current-buffer)))
                 (pos (marker-position start)))
       (and (<= (window-start window) pos)
            (< pos (or (window-end window) (point-min))))))
   (cons mevedel-view--spinner-label-target
         mevedel-view--spinner-tool-targets)))

(defun mevedel-view--animation-hscrolled-p ()
  "Return non-nil if a target is vertically in a hscrolled view window."
  (cl-some
   (lambda (window)
     (and (mevedel-view--animation-window-attended-p window)
          (> (window-hscroll window) 0)
          (mevedel-view--animation-target-in-window-rows-p window)))
   (get-buffer-window-list (current-buffer) nil t)))

(defun mevedel-view--resume-on-horizontal-redisplay (window)
  "Rearm after automatic horizontal scrolling reveals a target in WINDOW.
Emacs can update hscroll internally without calling `set-window-hscroll'
or `window-scroll-functions'.  Defer the full scheduler until redisplay
finishes; at most one probe timer belongs to this view in the meantime."
  (when (and (eq (window-buffer window) (current-buffer))
             (mevedel-view--animation-window-attended-p window)
             (not (and (mevedel--ui-timer-pending-p mevedel-view--spinner-timer)
                       (not (equal mevedel-view--spinner-timer-period 1.0))))
             (mevedel-view--animation-target-in-window-rows-p window)
             (or (zerop (window-hscroll window))
                 (mevedel-view--animation-visible-p)))
    (remove-hook 'pre-redisplay-functions
                 #'mevedel-view--resume-on-horizontal-redisplay t)
    ;; A visible elapsed suffix may already own a one-second timer.  Transfer
    ;; ownership to the deferred probe instead of leaving that timer queued.
    (when (timerp mevedel-view--spinner-timer)
      (mevedel--ui-timer-cancel mevedel-view--spinner-timer))
    (setq mevedel-view--spinner-timer nil
          mevedel-view--spinner-timer-period nil)
    (let ((buffer (current-buffer)) timer)
      (setq timer (timer-create))
      (timer-set-time timer (current-time))
      (timer-set-function
       timer
       (lambda ()
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (eq timer mevedel-view--spinner-timer)
               (setq mevedel-view--spinner-timer nil)
               (mevedel-view--start-spinner-timer t))))))
      (mevedel--ui-timer-activate timer)
      (setq mevedel-view--spinner-timer timer))))

(defun mevedel-view--spinner-visual-period (style &optional main)
  "Return effective time between visual frames for STYLE, or nil.
MAIN means use the actual color/glyph rendering of the request label."
  (let ((rate (mevedel-view-power-framerate
               mevedel-view-spinner-framerate
               mevedel-view-spinner-battery-framerate
               mevedel-view-spinner-power-policy
               mevedel-view-spinner-animate)))
    (when (and (> rate 0) (not (eq style 'static)))
      (max (/ 1.0 rate)
           (if (and main (memq style '(shimmer breathe bounce))
                    (not mevedel-view--spinner-main-color-p))
               0.12
             (mevedel-view-animation-period style))))))

(defun mevedel-view--animation-wants-power-p (visible)
  "Return non-nil when VISIBLE animation needs automatic power status."
  (and visible mevedel-view-spinner-animate
       (eq mevedel-view-spinner-power-policy 'auto)
       (or (and (mevedel-view--animation-target-visible-p
                 mevedel-view--spinner-label-target
                 'mevedel-view-spinner-frame)
                (not (eq mevedel-view-spinner-style 'static)))
           (and (cl-some (lambda (target)
                           (mevedel-view--animation-target-visible-p
                            target 'mevedel-view-inline-spinner-frame))
                         mevedel-view--spinner-tool-targets)
                (not (eq mevedel-view-tool-spinner-style 'static))))))

(defun mevedel-view--stop-spinner-timer ()
  "Stop the buffer-local spinner animation timer."
  (remove-hook 'pre-redisplay-functions
               #'mevedel-view--resume-on-horizontal-redisplay t)
  (when (timerp mevedel-view--spinner-timer)
    (mevedel--ui-timer-cancel mevedel-view--spinner-timer))
  (setq mevedel-view--spinner-timer nil
        mevedel-view--spinner-timer-period nil)
  (mevedel-view-power-unwatch (current-buffer)))

(defun mevedel-view--refresh-themed-status (&optional force)
  "Repaint visible indicator spans at their current or frozen phase.
FORCE also checks frozen glyph fallbacks on a visibility rearm, even
without a theme change.  Hidden targets retain the pending refresh;
neither the status row nor the composer is rebuilt."
  (when (or force mevedel-view--spinner-theme-stale-p)
    (let ((seconds (mevedel-view--animation-display-seconds))
          (frozen (zerop (mevedel-view-power-framerate
                          mevedel-view-spinner-framerate
                          mevedel-view-spinner-battery-framerate
                          mevedel-view-spinner-power-policy
                          mevedel-view-spinner-animate)))
          (pending nil))
      (when (and mevedel-view--spinner-status
                 (not (eq mevedel-view-spinner-style 'static)))
        (let ((target mevedel-view--spinner-label-target))
          (if (mevedel-view--animation-target-visible-p
               target 'mevedel-view-spinner-frame)
              (let* ((start (marker-position (car target)))
                     (end (marker-position (cdr target)))
                     (style mevedel-view-spinner-style)
                     (label (buffer-substring-no-properties start end))
                     (display-frame (mevedel-view--animation-target-frame
                                     target (eq style 'dots)))
                     (frame (mevedel-view-animation-frame
                             style label seconds 'mevedel-view-spinner
                             display-frame)))
                (unless (equal-including-properties
                         frame (get-text-property start 'display))
                  (let ((inhibit-read-only t)
                        (buffer-undo-list t))
                    (with-silent-modifications
                      (put-text-property start end 'display frame))))
                (setq mevedel-view--spinner-last-sample-seconds seconds
                      mevedel-view--spinner-sample-frame display-frame))
            (setq pending t))))
      (unless (eq mevedel-view-tool-spinner-style 'static)
        (dolist (target mevedel-view--spinner-tool-targets)
          (if (mevedel-view--animation-target-visible-p
               target 'mevedel-view-inline-spinner-frame)
              (let* ((start (marker-position (car target)))
                     (end (marker-position (cdr target)))
                     (display-frame (mevedel-view--animation-target-frame
                                     target (eq mevedel-view-tool-spinner-style
                                                'dots)))
                     (frame (mevedel-view-animation-frame
                             mevedel-view-tool-spinner-style ""
                             (if frozen
                                 (mevedel-view--tool-sample-seconds start)
                               seconds)
                             'mevedel-view-ephemeral display-frame)))
                (unless (equal-including-properties
                         frame (get-text-property start 'display))
                  (let ((inhibit-read-only t)
                        (buffer-undo-list t))
                    (with-silent-modifications
                      (put-text-property start end 'display frame))))
                (unless frozen
                  (mevedel-view--record-tool-sample start seconds)))
            (setq pending t))))
      (setq mevedel-view--spinner-theme-stale-p pending))))

(defun mevedel-view--start-spinner-timer (&optional resumed)
  "Start one view timer at the next needed visual or metadata cadence.
RESUMED means visibility or focus changed, so frozen glyph support can be
rechecked without changing the displayed animation phase."
  ;; A shared display property cannot carry separate palettes for two frames.
  ;; Repaint on a frame move even when this view has no decorative or elapsed
  ;; timer to notice it; defer the repaint while the label is hidden.
  (when (and mevedel-view--spinner-status
             (memq mevedel-view-spinner-style
                   '(shimmer breathe bounce braille dots))
             (mevedel-view--animation-target-visible-p
              mevedel-view--spinner-label-target 'mevedel-view-spinner-frame)
             (not (equal mevedel-view--spinner-sample-frame
                         (mevedel-view--animation-target-frame
                          mevedel-view--spinner-label-target
                          (eq mevedel-view-spinner-style 'dots)))))
    (setq mevedel-view--spinner-theme-stale-p t))
  (let ((frozen-glyphs
         (and resumed
              (zerop (mevedel-view-power-framerate
                      mevedel-view-spinner-framerate
                      mevedel-view-spinner-battery-framerate
                      mevedel-view-spinner-power-policy
                      mevedel-view-spinner-animate))
              (or (memq mevedel-view-spinner-style '(braille dots))
                  (and mevedel-view--spinner-tool-targets
                       (memq mevedel-view-tool-spinner-style
                             '(braille dots)))))))
    (when (and frozen-glyphs
               (or (eq mevedel-view-spinner-style 'dots)
                   (eq mevedel-view-tool-spinner-style 'dots)))
      (mevedel-view-animation-reset-glyph-support))
    (when (or mevedel-view--spinner-theme-stale-p frozen-glyphs)
      (mevedel-view--refresh-themed-status frozen-glyphs)))
  (when (and (mevedel-view--spinner-active-p)
             (not mevedel-view--spinner-phase-start))
    (setq mevedel-view--spinner-phase-start (float-time)))
  ;; Latch on a zero-fps transition even when the next redraw is delayed.
  ;; An ordinary rearm must not record the clock phase as a displayed sample.
  (if (zerop (mevedel-view-power-framerate
             mevedel-view-spinner-framerate
             mevedel-view-spinner-battery-framerate
             mevedel-view-spinner-power-policy
             mevedel-view-spinner-animate))
      (mevedel-view--animation-display-seconds)
    (setq mevedel-view--spinner-frozen-seconds nil))
  (let* ((visible (mevedel-view--animation-visible-p))
         (main-visible (and visible
                            (mevedel-view--animation-target-visible-p
                             mevedel-view--spinner-label-target
                             'mevedel-view-spinner-frame)))
         (main-color (and main-visible
                          (memq mevedel-view-spinner-style
                                '(shimmer breathe bounce))
                          (let* ((target mevedel-view--spinner-label-target)
                                 (label (buffer-substring-no-properties
                                         (marker-position (car target))
                                         (marker-position (cdr target)))))
                            (mevedel-view-animation-color-available-p
                             mevedel-view-spinner-style label
                             'mevedel-view-spinner
                             (mevedel-view--animation-target-frame target)))))
         (main (progn
                 (setq mevedel-view--spinner-main-color-p main-color)
                 (when main-visible
                   (mevedel-view--spinner-visual-period
                    mevedel-view-spinner-style t))))
         (tool (and visible
                    (cl-some (lambda (target)
                               (mevedel-view--animation-target-visible-p
                                target 'mevedel-view-inline-spinner-frame))
                             mevedel-view--spinner-tool-targets)
                    (mevedel-view--spinner-visual-period
                     mevedel-view-tool-spinner-style)))
         (metadata (and mevedel-view--spinner-status
                        (mevedel-view--spinner-metadata-visible-p)
                        (not (and-let* ((request (mevedel-view--spinner-request)))
                               (mevedel-request-active-work-pause-started-at
                                request)))
                        1.0))
         (periods (delq nil (list main tool metadata)))
         (period (and periods (apply #'min periods))))
    (if (and (not visible) (mevedel-view--spinner-active-p)
             (mevedel-view--animation-hscrolled-p))
        (add-hook 'pre-redisplay-functions
                  #'mevedel-view--resume-on-horizontal-redisplay nil t)
      (remove-hook 'pre-redisplay-functions
                   #'mevedel-view--resume-on-horizontal-redisplay t))
    (when (mevedel-view--animation-wants-power-p visible)
      (mevedel-view-power-watch (current-buffer)
                                #'mevedel-view--start-spinner-timer))
    (unless (mevedel-view--animation-wants-power-p visible)
      (mevedel-view-power-unwatch (current-buffer)))
    (unless (and period
                 (equal period mevedel-view--spinner-timer-period)
                 (mevedel--ui-timer-pending-p mevedel-view--spinner-timer))
      (when (timerp mevedel-view--spinner-timer)
        (mevedel--ui-timer-cancel mevedel-view--spinner-timer))
      (setq mevedel-view--spinner-timer nil
            mevedel-view--spinner-timer-period period)
      (when period
        (let ((buffer (current-buffer)) timer)
          (setq timer (timer-create))
          (timer-set-time timer (time-add nil (seconds-to-time period)))
          (timer-set-function
           timer
           (lambda ()
             (if (not (buffer-live-p buffer))
                 (mevedel--ui-timer-cancel timer)
               (with-current-buffer buffer
                 (if (and (eq timer mevedel-view--spinner-timer)
                          (or mevedel-view--spinner-status
                              mevedel-view--pending-tool-calls)
                          (or (mevedel-view--animation-visible-p)
                              (mevedel-view--spinner-metadata-visible-p)))
                     (condition-case nil
                         (progn
                           (mevedel-view--spinner-tick)
                           ;; A one-shot timer cannot replay deadlines missed
                           ;; during a stall.  Reuse its object after each
                           ;; delivered tick to avoid frame-rate allocation.
                           (when (eq timer mevedel-view--spinner-timer)
                             (timer-set-time
                              timer (time-add (current-time)
                                              (seconds-to-time period)))
                             (mevedel--ui-timer-activate timer)))
                       (error (mevedel-view--stop-spinner-timer)))
                   (mevedel--ui-timer-cancel timer)
                   (when (eq timer mevedel-view--spinner-timer)
                     (mevedel-view--stop-spinner-timer)
                     (when (and (derived-mode-p 'mevedel-view-mode)
                                (mevedel-view--spinner-active-p))
                       (mevedel-view--start-spinner-timer))))))))
          (mevedel--ui-timer-activate timer)
          (setq mevedel-view--spinner-timer timer))))))

(defun mevedel-view--refresh-animation-options ()
  "Apply changed animation settings to a live view without restarting work."
  (when (derived-mode-p 'mevedel-view-mode)
    (mevedel-view--animation-display-seconds)
    (setq mevedel-view--spinner-rendered-state nil)
    (when (mevedel-view--spinner-active-p)
      (mevedel-view--render-request-progress)
      ;; A policy/rate change does not change an existing tool row's shape.
      ;; Keep its last displayed glyph, including target-frame fallbacks.
      (unless (eq mevedel-view--spinner-rendered-tool-style
                  mevedel-view-tool-spinner-style)
        (mevedel-view--refresh-pending-tool-lines))
      (mevedel-view--start-spinner-timer))))

(defun mevedel-view--refresh-animation-on-theme (&rest _)
  "Repaint active color labels after the theme has invalidated frame banks.
The work is event-driven, not part of a decorative frame callback.  A
hidden label retains its pending refresh until visibility rearms its view."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (derived-mode-p 'mevedel-view-mode)
                   (or (and mevedel-view--spinner-status
                            (not (eq mevedel-view-spinner-style 'static)))
                       (and mevedel-view--spinner-tool-targets
                            (not (eq mevedel-view-tool-spinner-style 'static)))))
          (setq mevedel-view--spinner-theme-stale-p t)
          (condition-case nil
              (mevedel-view--start-spinner-timer)
            (error nil)))))))

;; Animation cache invalidation is installed first by the animation module.
;; Prepare the new visible sample only after that cache has been discarded.
(add-hook 'enable-theme-functions
          #'mevedel-view--refresh-animation-on-theme t)
(add-hook 'disable-theme-functions
          #'mevedel-view--refresh-animation-on-theme t)

(defun mevedel-view--spinner-inherits-face-p (face frame)
  "Return non-nil if the spinner inherits FACE on FRAME, directly or indirectly."
  (let (seen)
    (cl-labels ((includes (candidate)
                  (cond ((eq candidate face) t)
                        ((consp candidate) (cl-some #'includes candidate))
                        ((and (symbolp candidate)
                              (not (memq candidate seen))
                              (facep candidate))
                         (push candidate seen)
                         (or (includes (get candidate 'face-alias))
                             (includes (face-attribute candidate :inherit
                                                      frame)))))))
      (condition-case nil
          (includes 'mevedel-view-spinner)
        (error nil)))))

(defun mevedel-view--refresh-animation-on-face (face _frame &rest attributes)
  "Repaint color labels when FACE's resolved colors change outside a theme.
Customize applies face specs through `set-face-attribute', even if no
animation or elapsed timer remains to notice the new colors."
  (when (or (plist-member attributes :foreground)
            (plist-member attributes :background)
            (plist-member attributes :inherit))
    (let (seen)
      ;; An edit through a face alias changes its target, but advice receives
      ;; the alias name rather than the target's name.
      (while (and (symbolp face) (get face 'face-alias)
                  (not (memq face seen)))
        (push face seen)
        (setq face (get face 'face-alias)))
      (when (or (eq face 'default)
                (cl-some (lambda (frame)
                           (mevedel-view--spinner-inherits-face-p face frame))
                         (frame-list)))
        (mevedel-view-animation-invalidate)
        (mevedel-view--refresh-animation-on-theme)))))

(defun mevedel-view--spinner-tick ()
  "Update display spans; reconcile semantic metadata at most once a second."
  (let ((second (floor (float-time))))
    (unless (eql second mevedel-view--spinner-last-second)
      (setq mevedel-view--spinner-last-second second)
      (mevedel-view-animation-check-colors)
      (when (and mevedel-view--spinner-status
                 (or (mevedel-view--animation-target-visible-p
                      mevedel-view--spinner-label-target
                      'mevedel-view-spinner-frame)
                     (mevedel-view--spinner-metadata-visible-p)))
        (mevedel-view--call-preserving-user-view-state
         (lambda () (mevedel-view--ensure-request-progress))))
      ;; Theme/face changes can turn a color bank into a glyph fallback (or
      ;; back).  Re-evaluate the actual cadence only on this semantic tick.
      (mevedel-view--start-spinner-timer)))
  (let ((seconds (mevedel-view--animation-seconds))
        (was-modified (buffer-modified-p))
        (inhibit-read-only t)
        (inhibit-modification-hooks t)
        (buffer-undo-list t))
    (unwind-protect
        (progn
          (when (and (mevedel-view--spinner-visual-period
                      mevedel-view-spinner-style t)
                     (mevedel-view--animation-target-visible-p
                      mevedel-view--spinner-label-target
                      'mevedel-view-spinner-frame))
            (let* ((target mevedel-view--spinner-label-target)
                   (start (marker-position (car target)))
                   (end (marker-position (cdr target)))
                   (label (buffer-substring-no-properties start end))
                   (style mevedel-view-spinner-style)
                   (display-frame (mevedel-view--animation-target-frame
                                   target (eq style 'dots))))
              (when (or (not (memq style '(shimmer breathe bounce)))
                        (mevedel-view-animation-color-ready-p
                         style label 'mevedel-view-spinner display-frame))
                (let ((frame (mevedel-view-animation-frame
                              style label seconds 'mevedel-view-spinner
                              display-frame)))
                  (setq mevedel-view--spinner-last-sample-seconds seconds)
                  (setq mevedel-view--spinner-sample-frame display-frame)
                  (unless (equal-including-properties
                           frame (get-text-property start 'display))
                    (put-text-property start end 'display frame))))))
          (when (mevedel-view--spinner-visual-period
                 mevedel-view-tool-spinner-style)
            (dolist (target mevedel-view--spinner-tool-targets)
              (when (mevedel-view--animation-target-visible-p
                     target 'mevedel-view-inline-spinner-frame)
                (let* ((start (marker-position (car target)))
                       (display-frame
                        (mevedel-view--animation-target-frame
                         target (eq mevedel-view-tool-spinner-style 'dots)))
                       (frame (mevedel-view-animation-frame
                               mevedel-view-tool-spinner-style "" seconds
                               'mevedel-view-ephemeral display-frame)))
                  (mevedel-view--record-tool-sample start seconds)
                  (unless (equal-including-properties
                           frame (get-text-property start 'display))
                    (put-text-property start (marker-position (cdr target))
                                       'display frame)))))))
      (set-buffer-modified-p was-modified))))

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
     (unless mevedel-view--spinner-phase-start
       (setq mevedel-view--spinner-phase-start (float-time)))
     (cl-incf mevedel-view--spinner-generation)
     (setq mevedel-view--spinner-status (or status "Thinking...")
           mevedel-view--spinner-owner 'request)
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

(defun mevedel-view--update-spinner (status &optional owner)
  "Update request progress to show STATUS text owned by OWNER."
  (mevedel-view--call-preserving-input-point
   (lambda ()
     (mevedel-view--debug-log
      'spinner-update
      :status status
      :state (mevedel-view--debug-state mevedel--data-buffer))
     (when-let* ((session
                  (and (buffer-live-p mevedel--data-buffer)
                       (buffer-local-value 'mevedel--session
                                           mevedel--data-buffer)))
                 ((fboundp 'mevedel-telemetry-record)))
       (mevedel-telemetry-record
        session 'status-transition
        :previous-owner mevedel-view--spinner-owner
        :previous-status mevedel-view--spinner-status
        :owner (or owner mevedel-view--status-owner-override 'request)
        :status status))
     (setq mevedel-view--request-progress-suppressed nil)
     (unless mevedel-view--spinner-start-time
       (setq mevedel-view--spinner-start-time (current-time)))
     (unless mevedel-view--spinner-phase-start
       (setq mevedel-view--spinner-phase-start (float-time)))
     (cl-incf mevedel-view--spinner-generation)
     (setq mevedel-view--spinner-status status
           mevedel-view--spinner-owner
           (or owner mevedel-view--status-owner-override 'request))
     (save-excursion
       (mevedel-view--render-request-progress))
     (mevedel-view--start-spinner-timer))))

(defun mevedel-view--spinner-status-snapshot ()
  "Return a snapshot of the current request-progress status."
  (when mevedel-view--spinner-status
    (list :generation mevedel-view--spinner-generation
          :status mevedel-view--spinner-status
          :owner mevedel-view--spinner-owner)))

(defun mevedel-view--claim-spinner-status (snapshot status owner)
  "Replace SNAPSHOT with STATUS owned by OWNER when it is still current.
Return non-nil when the status was acquired."
  (when (and snapshot owner
             (= (plist-get snapshot :generation)
                mevedel-view--spinner-generation))
    (mevedel-view--update-spinner status owner)
    t))

(defun mevedel-view--restore-spinner-status (owner snapshot)
  "Restore SNAPSHOT when OWNER still owns the request-progress status.
Return non-nil when the status was restored."
  (when (and snapshot owner (eq mevedel-view--spinner-owner owner))
    (mevedel-view--update-spinner
     (plist-get snapshot :status)
     (or (plist-get snapshot :owner) 'request))
    t))

(defun mevedel-view--stop-spinner ()
  "Remove request progress if present."
  (mevedel-view--call-preserving-input-point
   (lambda ()
     (mevedel-view--debug-log
      'spinner-stop-delete
      :spinner (mevedel-view--debug-spinner-state)
      :state (mevedel-view--debug-state mevedel--data-buffer))
     (mevedel-view--clear-request-progress)
     (when-let* ((session
                  (and (buffer-live-p mevedel--data-buffer)
                       (buffer-local-value 'mevedel--session
                                           mevedel--data-buffer)))
                 ((fboundp 'mevedel-telemetry-record)))
       (mevedel-telemetry-record
        session 'status-transition
        :previous-owner mevedel-view--spinner-owner
        :previous-status mevedel-view--spinner-status
        :owner nil :status nil))
     (cl-incf mevedel-view--spinner-generation)
     (setq mevedel-view--spinner-status nil
           mevedel-view--spinner-owner nil
           mevedel-view--spinner-phase-start nil
           mevedel-view--spinner-frozen-seconds nil
           mevedel-view--spinner-last-sample-seconds nil
           mevedel-view--spinner-sample-frame nil
           mevedel-view--spinner-theme-stale-p nil)
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
      (unless (and (mevedel-view-stream-in-flight-turn-start-position)
                   (markerp mevedel-view--data-turn-start)
                   (marker-position mevedel-view--data-turn-start))
        (let ((summary (mevedel-view--tool-status-string tool-name args)))
          (mevedel-view--update-spinner summary)))))
  ;; Return nil so the hook does not interfere with tool execution.
  nil)

(defun mevedel-view-stream-in-flight-turn-start-position ()
  "Return the current in-flight turn start position, or nil."
  (when (markerp mevedel-view--in-flight-turn-start)
    (marker-position mevedel-view--in-flight-turn-start)))

(defun mevedel-view-stream-set-in-flight-turn-start (position)
  "Set `mevedel-view--in-flight-turn-start' to POSITION as a marker.
POSITION may be an integer or marker."
  (setq mevedel-view--in-flight-turn-start
        (copy-marker position nil)))

(defun mevedel-view--refresh-pending-tool-lines (&optional previous)
  "Refresh pending-tool live-tail lines, restoring surviving PREVIOUS samples.
Without PREVIOUS, capture displayed samples before replacing the live rows."
  (let ((previous (or previous (mevedel-view--snapshot-tool-animation-targets))))
    (mevedel-view--delete-pending-tool-live-lines)
    (when mevedel-view--pending-tool-calls
      (let* ((cap mevedel-view-pending-tools-visible-max)
             (visible (cl-subseq mevedel-view--pending-tool-calls
                                 0
                                 (min cap
                                      (length
                                       mevedel-view--pending-tool-calls)))))
        (mevedel-view--insert-pending-tool-lines visible previous)))
    (unless mevedel-view--pending-tool-calls
      (mevedel-view--capture-tool-animation-targets)
      (setq mevedel-view--spinner-rendered-tool-style
            mevedel-view-tool-spinner-style)
      (mevedel-view--start-spinner-timer))))

(defun mevedel-view-stream--execution-view-buffer (data-buffer)
  "Return the visible view backed by DATA-BUFFER, or nil."
  (and (buffer-live-p data-buffer)
       (cl-find-if
        (lambda (buffer)
          (and (buffer-live-p buffer)
               (eq data-buffer
                   (buffer-local-value 'mevedel--data-buffer buffer))))
        (buffer-list))))

(defun mevedel-view-stream--cache-execution-progress (event)
  "Cache bounded transient tool progress from EVENT in the current view."
  (when-let* ((tool-use-id (plist-get event :tool-use-id)))
    (unless (hash-table-p mevedel-view--execution-events)
      (setq mevedel-view--execution-events (make-hash-table :test #'equal)))
    (mevedel-view--cache-put
     mevedel-view--execution-events tool-use-id
     (list :type 'progress
           :facts (copy-tree (plist-get event :facts))
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

(defun mevedel-view-stream--schedule-execution-row-recovery (data-buffer)
  "Schedule one incremental render to recover a missing execution row."
  (when (and (mevedel-view-stream-in-flight-turn-start-position)
             (markerp mevedel-view--data-turn-start))
    (mevedel-view--schedule-render
     'incremental data-buffer mevedel-view-stream-render-delay)))

(defun mevedel-view-stream-handle-execution-event (event)
  "Apply Bash EVENT to its authoritative row and visible view.
Always return nil; only the mailbox sink may acknowledge durable delivery."
  (mevedel-execution-transcript-handle-event event)
  (when (eq (plist-get event :type) 'progress)
    (setq event
          (plist-put
           (copy-sequence event) :output-tail
           (string-join
            (last (string-lines (or (plist-get event :output-tail) "")) 5)
            "\n"))))
  (mevedel-view-stream-handle-tool-progress event))

(defun mevedel-view-stream-handle-tool-progress (event)
  "Apply transient tool progress EVENT to its visible aggregate row."
  (let* ((type (plist-get event :type))
         (tool-use-id (plist-get event :tool-use-id))
         (data-buffer (plist-get event :data-buffer))
         (view-buffer
          (mevedel-view-stream--execution-view-buffer data-buffer)))
    (when view-buffer
      (with-current-buffer view-buffer
        (pcase type
          ('progress
           (mevedel-view-stream--cache-execution-progress event))
          ('terminal
           (mevedel-view-stream--remove-execution-progress tool-use-id)))
        (cond
         ;; Keep only the row's identity while unattended.  Its latest
         ;; cached progress is enough to refresh it when focus returns;
         ;; rebuilding unrelated history would turn that return into a stall.
         ((mevedel-view--unattended-p)
          (when tool-use-id
            (cl-pushnew tool-use-id mevedel-view--pending-tool-rows :test #'equal))
          (mevedel-view--schedule-render
           (if tool-use-id 'tools 'full)
           data-buffer mevedel-view-rerender-debounce))
         ((and tool-use-id
               (mevedel-view--refresh-tool-row data-buffer tool-use-id)))
         (t
          (mevedel-view-stream--schedule-execution-row-recovery
           data-buffer))))))
  nil)

(defun mevedel-view--render-stream-update (data-buf)
  "Incrementally render DATA-BUF, isolating observer-view failures."
  (mevedel-execution-transcript-retry-pending-terminals data-buf)
  (if (not mevedel-view--agent-transcript-p)
      (mevedel-view-render-live-update data-buf)
    (condition-case err
        (atomic-change-group
          (mevedel-view-render-live-update data-buf))
      (error
       (mevedel--warn-once
        'view-stream-agent-render
        "Live agent transcript render failed: %s"
        (error-message-string err))))))

(defun mevedel-view--schedule-tool-boundary-render (data-buf)
  "Schedule a coalesced incremental render for DATA-BUF."
  (when (and (buffer-live-p data-buf)
             (mevedel-view-stream-in-flight-turn-start-position)
             (markerp mevedel-view--data-turn-start))
    (mevedel-view--schedule-render
     'incremental data-buf mevedel-view-tool-boundary-render-delay)))

(defun mevedel-view-stream-schedule ()
  "Schedule a debounced incremental render driven by the stream hook.

Intended for `gptel-post-stream-hook', which fires once per streamed
chunk in the data buffer.  Defers the incremental render by
`mevedel-view-stream-render-delay' seconds so chunks in the same pending
window share one refresh instead of rebuilding the view per token."
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
      (when (and (mevedel-view-stream-in-flight-turn-start-position)
                 (markerp mevedel-view--data-turn-start))
        (mevedel-view--schedule-render
         'incremental data-buf mevedel-view-stream-render-delay))))
  nil)

(defun mevedel-view--pending-tool-fingerprint (info)
  "Return the pending-tool fingerprint for tool INFO.

gptel builds its tool-hook arguments from the name, the arguments, and
the result, so there is no call id to key on: identical parallel calls
produce the same fingerprint."
  (cons (plist-get info :name)
        (let ((print-level 4)
              (print-length 32)
              (print-circle t))
          (prin1-to-string (plist-get info :args)))))

(defun mevedel-view--pending-tool-claim-key (info)
  "Return a fresh pending-tool key for tool INFO.

The key pairs INFO's fingerprint with a serial, so two identical
parallel calls hold distinct keys.  The serial matters to rendering: a
live-tail fragment is identified by this key, and a key that shifted
when an earlier call finished would rebuild every row below it."
  (cons (mevedel-view--pending-tool-fingerprint info)
        (cl-incf mevedel-view--pending-tool-serial)))

(defun mevedel-view--pending-tool-entry (info)
  "Return the pending entry matching tool INFO, or nil.
Identical parallel calls share a fingerprint, so the first entry still
holding it is the one that finished."
  (let ((fingerprint (mevedel-view--pending-tool-fingerprint info)))
    (cl-find-if (lambda (entry) (equal (car (car entry)) fingerprint))
                mevedel-view--pending-tool-calls)))

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
      (unless (equal name "Agent")
        (let ((key (mevedel-view--pending-tool-claim-key args))
              (label (mevedel-view--tool-status-string
                      name (plist-get args :args))))
          ;; One entry per call, not per key: identical parallel calls
          ;; share a fingerprint, and collapsing them would drop the
          ;; live line while the other call is still running.
          (setq mevedel-view--pending-tool-calls
                (append mevedel-view--pending-tool-calls
                        (list (cons key label))))))
      ;; Keep the request-level progress row visible; pending-tool lines
      ;; are detail rows below it, not a replacement for elapsed request
      ;; progress.
      (mevedel-view--ensure-request-progress data-buf)
      (mevedel-view--start-spinner-timer)
      (when (and (mevedel-view-stream-in-flight-turn-start-position)
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
  (mevedel-execution-transcript-retry-pending-terminals (current-buffer))
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
      ;; Remove the one call that finished, not every call sharing its
      ;; fingerprint.
      (when-let* ((entry (mevedel-view--pending-tool-entry args)))
        (setq mevedel-view--pending-tool-calls
              (delq entry mevedel-view--pending-tool-calls)))
      (unless mevedel-view--pending-tool-calls
        (mevedel-view--delete-pending-tool-live-lines))
      (unless (or mevedel-view--pending-tool-calls
                  (mevedel-view--request-progress-visible-p))
        (mevedel-view--stop-spinner-timer))
      (when (and (mevedel-view-stream-in-flight-turn-start-position)
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
  (mevedel-view-zone-clear 'history-live)
  (setq mevedel-view--spinner-tool-targets nil))

(defun mevedel-view--insert-pending-tool-lines (entries &optional previous)
  "Render fragment-backed pending tool live-tail rows for ENTRIES.
ENTRIES is a subset of `mevedel-view--pending-tool-calls' (head N).
PREVIOUS holds displayed tool samples captured before any transcript deletion.
When the full list exceeds `mevedel-view-pending-tools-visible-max',
the caller passes only the visible head and a tail-summary row is
appended.

Pending-tool rows are part of the in-flight transcript live tail, so
they fall back to the history/status boundary rather than the input
  marker when no render insertion marker is dynamically bound."
  (let ((anchor (mevedel-view--pending-tool-insertion-target)))
    (mevedel-view-zone-reconcile
     'history-live anchor anchor
     (mevedel-view--pending-tool-fragments entries)))
  (mevedel-view--capture-tool-animation-targets)
  ;; Newly inserted rows display their initial glyph until a frame tick.  A
  ;; surviving row retains both its display and phase across reconciliation.
  (setq mevedel-view--spinner-tool-samples nil)
  (mevedel-view--restore-tool-animation-targets previous)
  (setq mevedel-view--spinner-rendered-tool-style
        mevedel-view-tool-spinner-style)
  (mevedel-view--start-spinner-timer))

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
        (unless (mevedel-view-stream-in-flight-turn-start-position)
          (setq mevedel-view--in-flight-turn-start
                (copy-marker (mevedel-view--history-insertion-marker) nil)))
        (setq mevedel-view--request-progress-suppressed nil)
        (mevedel-view--ensure-request-progress data-buffer)))))

(defun mevedel-view-stream-begin-turn (view-start data-start &optional no-progress)
  "Begin an active streamed turn at VIEW-START and DATA-START.
When NO-PROGRESS is non-nil, record no active progress state."
  (let ((view-start (unless mevedel-view-render--owner view-start))
        (data-start (copy-marker data-start)))
    (mevedel-view-render-mutate
     'begin-turn
     (lambda ()
       (mevedel-view-stream--stop-now)
       (setq mevedel-view-render--terminal-p nil)
       (mevedel-view-stream--begin-turn-now
        (or view-start (mevedel-view--history-insertion-marker))
        data-start no-progress))
     t)))

(defun mevedel-view-stream--begin-turn-now (view-start data-start no-progress)
  "Anchor VIEW-START and DATA-START while holding projection ownership.
NO-PROGRESS suppresses active-turn presentation."
  (when (markerp mevedel-view--in-flight-turn-start)
    (set-marker mevedel-view--in-flight-turn-start nil))
  (when (markerp mevedel-view--data-turn-start)
    (set-marker mevedel-view--data-turn-start nil))
  (mevedel-view-render-invalidate-live-tail)
  (setq mevedel-view--in-flight-turn-start
        (and (not no-progress) (copy-marker view-start nil)))
  (setq mevedel-view--data-turn-start
        (and (not no-progress) (copy-marker data-start nil)))
  (unless no-progress
    (mevedel-view--start-spinner)))

(defun mevedel-view-stream-stop ()
  "Stop active streaming UI and release all turn markers."
  (mevedel-view-render-terminal)
  (mevedel-view-render-mutate
   'stop #'mevedel-view-stream--stop-now nil #'mevedel-view-stream--release-turn))

(defun mevedel-view-stream--stop-now ()
  "Release active streaming presentation with projection ownership held."
  (unwind-protect
      (progn
        (mevedel-view--stop-request-progress)
        (mevedel-view--cancel-scheduled-render)
        (setq mevedel-view--pending-tool-calls nil)
        (mevedel-view--delete-pending-tool-live-lines))
    (mevedel-view-stream--release-turn)))

(defun mevedel-view-stream--release-turn ()
  "Release terminal resources even when its projection was superseded."
  (mevedel-view-render-invalidate-live-tail)
  ;; A superseding full render must not recreate progress from the old
  ;; spinner timestamp or a request that has not finished settling yet.
  (setq mevedel-view--pending-tool-calls nil
        mevedel-view--request-progress-suppressed t
        mevedel-view--spinner-start-time nil
        mevedel-view--spinner-phase-start nil
        mevedel-view--spinner-frozen-seconds nil
        mevedel-view--spinner-last-sample-seconds nil)
  ;; Retire obsolete streamed work, but keep a full recovery requested
  ;; by the terminal render's error handler before this mandatory release.
  (when (eq mevedel-view--pending-render-kind 'incremental)
    (mevedel-view--cancel-scheduled-render))
  (mevedel-view--stop-spinner-timer)
  (when (markerp mevedel-view--in-flight-turn-start)
    (set-marker mevedel-view--in-flight-turn-start nil))
  (setq mevedel-view--in-flight-turn-start nil)
  (when (markerp mevedel-view--data-turn-start)
    (set-marker mevedel-view--data-turn-start nil))
  (setq mevedel-view--data-turn-start nil))

(defun mevedel-view-stream-render-response (start end)
  "Finish and render gptel response bounds START and END."
  (let ((data-buf (current-buffer))
        (start (copy-marker start))
        (end (copy-marker end t)))
    (when-let* ((view-buf mevedel--view-buffer)
                ((buffer-live-p view-buf)))
      (with-current-buffer view-buf
        (mevedel-view-render-terminal)
        (mevedel-view-render-mutate
         'terminal
         (lambda ()
           (when (buffer-live-p data-buf)
             (with-current-buffer data-buf
               (let ((mevedel--view-buffer view-buf))
                 (mevedel-view-stream--render-response-now start end)))))
         nil #'mevedel-view-stream--release-turn))))
  nil)

(defun mevedel-view-stream--render-response-now (start end)
  "Finish response START..END with its view's projection ownership held."
  (mevedel-execution-transcript-retry-pending-terminals (current-buffer))
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
          (setq-local mevedel-compact-run-in-flight nil))
        ;; Releasing this turn is not optional.  While a pending tool call
        ;; remains, stopping the progress row leaves the spinner timer
        ;; running, and a stale in-flight marker keeps the view looking
        ;; active; an escaping error would also skip the post-response
        ;; observers that follow, including gptel's font-lock refresh and
        ;; the Plan proposal presentation.  Everything fallible therefore
        ;; runs inside the guard, including the zone mutations.
        (unwind-protect
            (condition-case err
                (progn
                  (mevedel-view--stop-request-progress)
                  (mevedel-view--debug-log
                   'render-response-after-spinner
                   :state (mevedel-view--debug-state data-buf start end))
                  (mevedel-view--cancel-scheduled-render)
                  ;; Clear the list before dropping its rows, or the
                  ;; render below rebuilds the live tail from it.
                  (setq mevedel-view--pending-tool-calls nil)
                  (mevedel-view--delete-pending-tool-live-lines)
                  (setq end
                        (or (mevedel-view--append-request-summary
                             data-buf start)
                            end))
                  (mevedel-view-render--settle-now data-buf start end)
                  (mevedel-view--debug-log
                   'render-response-after-incremental
                   :state (mevedel-view--debug-state data-buf start end)))
              (error
               ;; The fallback rerender is debounced by
               ;; `mevedel-view-rerender-debounce', so it normally runs
               ;; after the release below.
               (mevedel--warn-once
                'view-stream-terminal-render
                "Terminal response render failed: %s"
                (error-message-string err))
               (mevedel-view-rerender view-buf)))
          (mevedel-view-stream--release-turn)))))
  nil)

(provide 'mevedel-view-stream)
;;; mevedel-view-stream.el ends here
