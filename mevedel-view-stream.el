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
(require 'mevedel-present)
(require 'mevedel-view-power)
(require 'mevedel-ptc-interpreter)
(require 'mevedel-tool-registry)
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

;; `mevedel-gptel-stream-bridge'
(declare-function mevedel-gptel-stream-bridge-after-flush
                  "mevedel-gptel-stream-bridge" (function))

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

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-segment-bounds
                  "mevedel-tool-render-data" (tool-use-id))

;; `mevedel-utilities'
(declare-function mevedel--duration-label "mevedel-utilities" (seconds))
(declare-function mevedel--timer-pending-p "mevedel-utilities" (timer))
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))

;; `mevedel-view'
(declare-function mevedel-view--cancel-scheduled-render "mevedel-view"
                  (&optional keep-tool-rows))
(declare-function mevedel-view--schedule-render
                  "mevedel-view" (kind data-buffer delay))
(declare-function mevedel-view--tool-status-string "mevedel-view" (tool-name args))
(declare-function mevedel-view--unattended-p "mevedel-view" (&optional buffer))
(declare-function mevedel-view-rerender "mevedel-view" (&optional buffer))
(defvar mevedel-view--display-map)
(defvar mevedel-view--pending-render-kind)
(defvar mevedel-view--pending-tool-rows)
(defvar mevedel-view--status-marker)
(defvar mevedel-view-rerender-debounce)
(defvar mevedel-view-spinner-animate)
(defvar mevedel-view-spinner-style)
(defvar mevedel-view-tool-spinner-style)
(defvar mevedel-view-spinner-framerate)
(defvar mevedel-view-spinner-battery-framerate)
(defvar mevedel-view-spinner-power-policy)

;; `mevedel-view-native'
(declare-function mevedel-view-native-sample "mevedel-view-native" (target))
(declare-function mevedel-view-native-stop "mevedel-view-native" ())
(declare-function mevedel-view-native-sync "mevedel-view-native" (specs elapsed rearm))
(autoload 'mevedel-view-native-sync "mevedel-view-native")

;; `mevedel-view-composer'
(declare-function mevedel-view--agent-fsm-p
                  "mevedel-view-composer" (info data-buffer))
(autoload 'mevedel-view--agent-fsm-p "mevedel-view-composer")
(declare-function mevedel-view--call-preserving-input-point
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--call-preserving-user-view-state
                  "mevedel-view-composer" (thunk &optional region))
(declare-function mevedel-view--call-with-render-boundaries-advancing
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--composer-mode-badge
                  "mevedel-view-composer" (&optional mode))
(autoload 'mevedel-view--composer-mode-badge "mevedel-view-composer")
(defvar mevedel-view--input-marker)

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-count-label
                  "mevedel-view-interaction" (&optional actionable))
(declare-function mevedel-view-jump-to-interaction "mevedel-view-interaction" ())
(autoload 'mevedel-view--interaction-count-label "mevedel-view-interaction")

;; `mevedel-view-audit'
(declare-function mevedel-view--hook-audit-records-from-text
                  "mevedel-view-audit" (text &optional type data-buf source-start))
(declare-function mevedel-view--insert-hook-audit-block
                  "mevedel-view-audit" (record &optional source expanded))
(declare-function mevedel-view-audit-breadcrumb-present-p
                  "mevedel-view-audit" (record before))

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
(declare-function mevedel-view--refresh-tool-row
                  "mevedel-view-render" (data-buffer tool-use-id))
(declare-function mevedel-view--render-live-region
                  "mevedel-view-render" (data-buf settle-p &optional start end))
(declare-function mevedel-view--request-progress-anchor
                  "mevedel-view-render" ())
(declare-function mevedel-view--retire-pending-decorations
                  "mevedel-view-render" (start end))
(declare-function mevedel-view-render-invalidate-live-tail
                  "mevedel-view-render" ())
(declare-function mevedel-view-render-live-update
                  "mevedel-view-render" (data-buf))
(declare-function mevedel-view-render-settle
                  "mevedel-view-render" (data-buf start end))

;; `mevedel-view-segments'
(declare-function mevedel-view-segments-display-buffer
                  "mevedel-view-segments" ())

;; `mevedel-transcript-audit'
(defvar mevedel--hook-audit-open)

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
  "Fastest cadence of the running timer, or nil when none runs.
A shimmer moves at this cadence only during its sweep.")

(defvar-local mevedel-view--spinner-timer-plan nil
  "Plan of the running timer; see `mevedel-view--spinner-next-delay'.")

(defvar mevedel-view--spinner-delivering nil
  "The spinner timer whose callback is running, which rearms it afterwards.
A delivered one-shot timer has left the timer list, so without this the
once-a-second semantic rescheduling inside the callback replaced the
timer and its closure on every tick, even with an unchanged plan.")

(defvar-local mevedel-view--spinner-probe-timer nil
  "Timer rechecking a spinner whose spans were not yet on screen.")

(defvar-local mevedel-view--spinner-probed-targets nil
  "Spinner spans the visibility probe last rechecked.")

(defvar-local mevedel-view--spinner-main-color-p nil
  "Non-nil when the main style renders color rather than a glyph fallback.")

(defvar-local mevedel-view--spinner-label-target nil
  "Marker for the foreground label's animation property.")

(defvar-local mevedel-view--spinner-metadata-target nil
  "Markers for the foreground label's separate elapsed metadata span.")

(defvar-local mevedel-view--spinner-tool-targets nil
  "Markers for the animated spans of running tool rows.")

(defvar-local mevedel-view--spinner-tool-samples nil
  "Tool phases to display, keyed by `mevedel-view-tool-animation-id'.
A freeze replaces them with resting phases before they are drawn.")

(defvar-local mevedel-view--spinner-last-second nil
  "Elapsed label last rendered on a timer tick.")

(defvar-local mevedel-view--spinner-last-sample-seconds nil
  "Phase of the most recently sampled request-label display frame.")

(defvar-local mevedel-view--spinner-theme-stale-p nil
  "Non-nil when displayed colors need a repaint.
A theme change or frame move invalidates the color sample, and a freeze
moves spans drawn mid-sweep to their resting phase.")

(defvar-local mevedel-view--spinner-sample-frame nil
  "Display frame used for the request label's last color sample.
The value `:multiple' means the portable multi-frame fallback was used.")

(defvar-local mevedel-view--spinner-rendered-tool-style nil
  "Style the current running tool rows were drawn with.")

(defvar-local mevedel-view--carried-tool-samples nil
  "Running-row samples displayed before a full projection removed the rows.
The live render that redraws the rows restores their displayed phase.")

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
  "In-flight tool calls in arrival order, one entry per call.
Each entry is (KEY NAME DETAIL METADATA) where KEY is
`((NAME . ARGS-PRINT) . SERIAL)', NAME is the tool name, and DETAIL is
its displayed argument or nil.  Identical parallel calls share the
fingerprint half and are told apart by the serial, which also keys the
row's animation phase across redraws.  METADATA holds the pipeline's
durable `:tool-use-id' and accepted `:permission-approvals', when known.

Pre-tool hooks add entries; pipeline admission binds their identities.
Pipeline settlement marks a bound entry `:settled'; post-tool hooks, which
precede the result reaching the transcript, remove settled or unbound
entries.  The live render draws the entries as calls of the turn's
trailing activity run: running, or finished while their result is due.")

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
A batch of parallel tool calls fires all its pre-tool hooks, and later
all its post-tool hooks, at once; each batch then shares the one
transcript render that draws its calls running or settled."
  :type 'number
  :group 'mevedel)

(defun mevedel-view--animation-seconds ()
  "Return continuous seconds since the current progress animation started."
  (max 0.0 (- (float-time) (or mevedel-view--spinner-phase-start
                                (float-time)))))

(defvar-local mevedel-view--native-animation-targets nil
  "Targets currently presented independently of Emacs redisplay.")

(defun mevedel-view--capture-native-animation ()
  "Retain the last native samples before freezing or rebuilding their spans.
Once a freeze has latched its resting phases, native samples are stale."
  (when (and (featurep 'mevedel-view-native)
             (not mevedel-view--spinner-frozen-seconds))
    (dolist (target mevedel-view--native-animation-targets)
      (when-let* ((seconds (mevedel-view-native-sample target)))
        (if (eq target mevedel-view--spinner-label-target)
            (setq mevedel-view--spinner-last-sample-seconds seconds)
          (when (marker-position (car target))
            (mevedel-view--record-tool-sample (marker-position (car target)) seconds)))))))

(defun mevedel-view--animation-frozen-p ()
  "Return non-nil when motion is disabled or the request awaits input."
  (or (and-let* ((request (mevedel-view--spinner-request)))
        (mevedel-request-active-work-pause-started-at request))
      (zerop (mevedel-view-power-framerate
              mevedel-view-spinner-framerate
              mevedel-view-spinner-battery-framerate
              mevedel-view-spinner-power-policy
              mevedel-view-spinner-animate))))

(defun mevedel-view--animation-rest-seconds (style seconds target face)
  "Rest color STYLE at SECONDS for TARGET and FACE, preserving glyph fallbacks."
  (let ((target (and target (marker-position (car target)) target)))
    (if (and (memq style '(shimmer breathe))
             (if target
                 ;; Use the displayed sample even after its window disappears.
                 ;; Prepared color samples carry faces; fallback glyphs do not.
                 (let ((display (get-text-property (car target) 'display)))
                   (or (memq target mevedel-view--native-animation-targets)
                       (and (stringp display) (> (length display) 0)
                            (get-text-property 0 'face display))))
               (mevedel-view-animation-color-available-p
                style "" face (mevedel-view--animation-buffer-frame))))
        (mevedel-view-animation-rest-seconds style seconds)
      seconds)))

(defun mevedel-view--animation-display-seconds ()
  "Return the phase for a newly rendered progress label.
Semantic redraws must not advance the glyph when motion is disabled.
The underlying phase continues to advance and resumes without restarting."
  (when (mevedel-view--animation-frozen-p)
    (mevedel-view--capture-native-animation))
  (if (mevedel-view--animation-frozen-p)
      (or mevedel-view--spinner-frozen-seconds
          (progn
            ;; Rest instead of holding a faded band for the whole pause, and
            ;; repaint spans already drawn mid-sweep at the next rearm.
            (dolist (target mevedel-view--spinner-tool-targets)
              (when-let* ((start (marker-position (car target)))
                          (sample (assoc (get-text-property start 'mevedel-view-tool-animation-id)
                                         mevedel-view--spinner-tool-samples)))
                (setcdr sample
                        (mevedel-view--animation-rest-seconds
                         mevedel-view-tool-spinner-style (cdr sample)
                         target (mevedel-view--tool-animation-face target)))))
            (setq mevedel-view--spinner-theme-stale-p t)
            (setq mevedel-view--spinner-frozen-seconds
                  (mevedel-view--animation-rest-seconds
                   mevedel-view-spinner-style
                   (or mevedel-view--spinner-last-sample-seconds
                       (mevedel-view--animation-seconds))
                   mevedel-view--spinner-label-target 'mevedel-view-spinner))))
    (setq mevedel-view--spinner-frozen-seconds nil)
    (setq mevedel-view--spinner-last-sample-seconds
          (mevedel-view--animation-seconds))))

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
    (mevedel--duration-label seconds)))

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

(defun mevedel-view--request-progress-fragments (body)
  "Return the permanent rail fragment containing BODY."
  (list (list :namespace 'progress
              :id 'request
              :priority 0
              :body body
              :keymap mevedel-view--display-map
              :navigatable nil)))

(defun mevedel-view--render-request-progress ()
  "Render the permanent composer rail, changing text only on semantic updates."
  (when (and (not mevedel-view--agent-transcript-p)
             (markerp mevedel-view--input-marker)
             (marker-position mevedel-view--input-marker))
    (let* ((display-status
            (and mevedel-view--spinner-status
                 (mevedel-view--spinner-display-status
                  mevedel-view--spinner-status)))
           (badge (mevedel-view--composer-mode-badge))
           (attention (mevedel-view--interaction-count-label t))
           (windows (get-buffer-window-list (current-buffer) nil t))
           (width (max 1 (1- (if windows
                                 (apply #'min (mapcar #'window-body-width windows))
                               (window-body-width)))))
           (prefix (mevedel-view--request-progress-prefix))
           (render-state (list display-status badge attention width prefix)))
      (unless (and (equal render-state
                          mevedel-view--spinner-rendered-state)
                   (mevedel-view-zone-region 'progress))
        (let ((anchor (mevedel-view--request-progress-anchor)))
          (mevedel-view-zone-reconcile
           'progress anchor anchor
           (mevedel-view--request-progress-fragments
            (concat prefix
                    (mevedel-view--format-composer-rail
                     badge attention width mevedel-view--spinner-status
                     display-status))))
          (setq mevedel-view--spinner-rendered-state
                render-state)
          (mevedel-view--capture-request-animation-target))))))

(defun mevedel-view--forget-request-progress-region ()
  "Forget the request-progress region after a larger redraw deleted it."
  (mevedel-view-zone-forget 'progress)
  (mevedel-view--release-animation-targets
   mevedel-view--spinner-label-target mevedel-view--spinner-metadata-target)
  (setq mevedel-view--spinner-rendered-state nil
        mevedel-view--spinner-label-target nil
        mevedel-view--spinner-metadata-target nil
        mevedel-view--spinner-sample-frame nil))

(defun mevedel-view--request-progress-mutation-region ()
  "Return the view text a request-progress redraw can replace.
The rail is reconciled at its anchor, from wherever its live region is."
  (let ((anchor (mevedel-view--request-progress-anchor))
        (zone (mevedel-view-zone-region 'progress)))
    (cons (min anchor (if zone (overlay-start zone) anchor))
          (max anchor (if zone (overlay-end zone) anchor)))))

(defun mevedel-view--ensure-request-progress (&optional data-buf status)
  "Ensure the foreground request progress row is visible.
DATA-BUF is the authoritative data buffer for elapsed-time lookup.
STATUS is the base label to show; nil preserves the current label or
falls back to \"Working...\"."
  (let ((mevedel--data-buffer (or data-buf
                                (and (boundp 'mevedel--data-buffer)
                                     mevedel--data-buffer))))
    (when (mevedel-view--request-progress-active-p data-buf)
      (setq mevedel-view--spinner-status
            (or status mevedel-view--spinner-status "Working...")))
    (mevedel-view--render-request-progress)
    (when (mevedel-view--request-progress-active-p data-buf)
      (mevedel-view--start-spinner-timer))))

(defun mevedel-view--spinner-dynamic-label-p (text)
  "Return non-nil when TEXT is a generated spinner metadata label."
  (or (string-match-p "\\`[0-9]+s\\'" text)
      (string-match-p "\\`[0-9]+m [0-9][0-9]s\\'" text)
      (string-match-p "\\`[0-9]+h [0-9][0-9]m\\'" text)))

(defun mevedel-view--spinner-base-status (status)
  "Return STATUS without generated elapsed-time labels."
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
  "Return STATUS decorated with elapsed time.
Background work is counted by the panel right above, not here."
  (let* ((base
          (if-let* ((request (mevedel-view--spinner-request))
                    ((mevedel-request-active-work-pause-started-at request)))
              "Waiting for input"
            (mevedel-view--spinner-base-status status)))
         (elapsed (mevedel-view--spinner-elapsed-label)))
    (string-join (delq nil (list base elapsed)) " · ")))

(defun mevedel-view--format-spinner-line (status &optional face display-status width)
  "Return propertized spinner line for STATUS.
FACE defaults to `mevedel-view-spinner'.  When WIDTH is non-nil, drop
elapsed metadata before truncating the activity to fit that many columns."
  (let* ((face (or face 'mevedel-view-spinner))
         (display-status (or display-status
                             (mevedel-view--spinner-display-status status)))
         (base (if (string-prefix-p "Waiting for input" display-status)
                   "Waiting for input"
                 (mevedel-view--spinner-base-status status)))
         (metadata (substring display-status (length base)))
         (base (replace-regexp-in-string "[\n\r\t]+" " " base))
         (frame (mevedel-view--animation-buffer-frame))
         (seconds (mevedel-view--animation-display-seconds))
         (sample (mevedel-view-animation-frame
                  mevedel-view-spinner-style base seconds face frame)))
    (when width
      (when (> (+ (string-width sample) (string-width metadata)) width)
        (setq metadata ""))
      (when (> (string-width sample) width)
        (setq base (truncate-string-to-width
                    base (max 0 (- width (- (string-width sample)
                                           (string-width base))))
                    nil nil "…")
              sample (mevedel-view-animation-frame
                      mevedel-view-spinner-style base seconds face frame))))
    (let ((label (propertize base
                            'font-lock-face face
                            'mevedel-view-spinner-frame t
                            'display sample
                            'read-only t
                            'keymap mevedel-view--display-map
                            'front-sticky '(read-only keymap)
                            'rear-nonsticky '(read-only keymap))))
    (concat label
            (propertize (concat metadata "\n")
                        'font-lock-face face
                        'mevedel-view-spinner-status
                        (mevedel-view--spinner-base-status status)
                        'read-only t
                        'keymap mevedel-view--display-map
                        'front-sticky '(read-only keymap)
                        'rear-nonsticky '(read-only keymap))))))

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

(defun mevedel-view--repair-request-progress-separator ()
  "Redraw the rail when the text above it now needs another separator.
The separator depends on how the text before the rail ends, which any
transcript writer can change without redrawing the rail.  A stale one
leaves an extra blank line, or none, until some later rail update
removes it and moves the transcript a row again."
  (when (and mevedel-view--spinner-rendered-state
             (mevedel-view-zone-region 'progress)
             (not (equal (nth 4 mevedel-view--spinner-rendered-state)
                         (mevedel-view--request-progress-prefix))))
    (mevedel-view--render-request-progress)))

(defun mevedel-view--format-composer-rail (badge attention width status display-status)
  "Format a one-line rail within WIDTH columns.
BADGE and actionable ATTENTION take priority over STATUS and DISPLAY-STATUS.
Only activity is animated; the controls remain ordinary interactive text."
  (let* ((badge (truncate-string-to-width
                 (replace-regexp-in-string "[\n\r\t]+" " " badge)
                 (max 1 (- width (if attention (min 12 (/ width 2)) 0)))
                 nil nil "…"))
         (room (- width (string-width badge)))
         (notice
          (when (and attention (> room 4))
            (let* ((available (- room 3))
                   (hint " · C-c C-j")
                   (text (concat "⚠ " attention hint)))
              (when (> (string-width text) available)
                (setq text
                      (if (>= available (+ 3 (string-width hint)))
                          (concat "⚠ "
                                  (truncate-string-to-width
                                   attention (- available 2 (string-width hint))
                                   nil nil "…")
                                  hint)
                        (truncate-string-to-width "⚠ C-c C-j" available
                                                  nil nil "…"))))
              (propertize text
                          'font-lock-face 'warning
                          'mouse-face 'highlight
                          'help-echo (concat attention "; C-c C-j to visit")
                          'mevedel-view-zone-activate
                          #'mevedel-view-jump-to-interaction)))))
    (when notice
      (setq badge (concat badge " · " notice)
            room (- width (string-width badge))))
    (if (and status (>= room 13))
        (concat badge " · "
                (mevedel-view--format-spinner-line
                 status nil display-status (- room 3)))
      (concat badge "\n"))))

(defun mevedel-view--spinner-active-p ()
  "Return non-nil when this view buffer has visible spinner work."
  (or mevedel-view--pending-tool-calls
      (mevedel-view--request-progress-active-p)))

(defun mevedel-view--release-animation-targets (&rest targets)
  "Detach the markers of every (START . END) span in TARGETS.
Spans are recaptured on every semantic render, about once a second for
the label; an abandoned marker stays attached to the view until garbage
collection, which mevedel defers while busy, and slows every edit there."
  (dolist (target targets)
    (when (consp target)
      (when (markerp (car target)) (set-marker (car target) nil))
      (when (markerp (cdr target)) (set-marker (cdr target) nil)))))

(defun mevedel-view--capture-request-animation-target ()
  "Remember the label and metadata spans after a semantic progress render."
  (mevedel-view--release-animation-targets
   mevedel-view--spinner-label-target mevedel-view--spinner-metadata-target)
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
      ;; Text streamed in just before the row lands at the label's start.
      ;; A start marker that stayed put took it into the span, whose start
      ;; then lacked the label property: the label read as hidden and froze
      ;; until the next semantic render, most of the time while a reply
      ;; streamed.  Rewrites of the label itself recapture these markers.
      (setq mevedel-view--spinner-label-target
            (cons (copy-marker pos t) (copy-marker label-end t))
            mevedel-view--spinner-sample-frame
            (mevedel-view--animation-buffer-frame))
      ;; The fragment ends with a newline.  Its suffix can remain visible
      ;; after horizontal scrolling hides the decorative label.
      (when (< label-end metadata-end)
        (setq mevedel-view--spinner-metadata-target
              (cons (copy-marker label-end)
                    (copy-marker metadata-end t)))))))

(defun mevedel-view--capture-tool-animation-targets (start end)
  "Remember the running tool spans the live render drew in START..END."
  (apply #'mevedel-view--release-animation-targets
         mevedel-view--spinner-tool-targets)
  (setq mevedel-view--spinner-tool-targets nil)
  (let ((pos start))
    (while (and pos (< pos end)
                (setq pos (text-property-any
                           pos end 'mevedel-view-inline-spinner-frame t)))
      (let ((span-end (or (next-single-property-change
                           pos 'mevedel-view-inline-spinner-frame nil end)
                          end)))
        ;; As for the label: text inserted at a row's start precedes it.
        (push (cons (copy-marker pos t) (copy-marker span-end t))
              mevedel-view--spinner-tool-targets)
        (setq pos span-end))))
  (setq mevedel-view--spinner-tool-targets
        (nreverse mevedel-view--spinner-tool-targets))
  (setq mevedel-view--spinner-tool-samples
        (cl-remove-if-not
         (lambda (sample)
           (cl-some
            (lambda (target)
              (equal (car sample)
                     (get-text-property (marker-position (car target))
                                        'mevedel-view-tool-animation-id)))
            mevedel-view--spinner-tool-targets))
         mevedel-view--spinner-tool-samples)))

(defun mevedel-view--tool-sample-seconds (start)
  "Return the displayed phase for the pending tool indicator at START.
Newly inserted tool rows begin at phase zero."
  (or (cdr (assoc (get-text-property start 'mevedel-view-tool-animation-id)
                  mevedel-view--spinner-tool-samples))
      0.0))

(defun mevedel-view--record-tool-sample (start seconds)
  "Record the displayed phase SECONDS for the tool indicator at START."
  (let* ((id (get-text-property start 'mevedel-view-tool-animation-id))
         (sample (assoc id mevedel-view--spinner-tool-samples)))
    (if sample
        (setcdr sample seconds)
      (push (cons id seconds) mevedel-view--spinner-tool-samples))))

(defun mevedel-view--snapshot-tool-animation-targets ()
  "Return live tool (ID DISPLAY PHASE LABEL) samples before replacing rows."
  (mevedel-view--capture-native-animation)
  (when (eq mevedel-view--spinner-rendered-tool-style
            mevedel-view-tool-spinner-style)
    (delq nil
          (mapcar (lambda (target)
                    (when-let* ((start (marker-position (car target)))
                                ((eq (marker-buffer (car target))
                                     (current-buffer)))
                                (id (get-text-property
                                     start 'mevedel-view-tool-animation-id)))
                      (let* ((seconds (mevedel-view--tool-sample-seconds start))
                             (label (mevedel-view--tool-animation-label target))
                             (display
                              (if (memq target mevedel-view--native-animation-targets)
                                  (mevedel-view-animation-frame
                                   mevedel-view-tool-spinner-style label seconds
                                   (mevedel-view--tool-animation-face target)
                                   (mevedel-view--animation-target-frame target))
                                (get-text-property start 'display))))
                        (list id display seconds label))))
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
                 (entry (assoc (get-text-property start 'mevedel-view-tool-animation-id)
                               previous)))
            (when entry
              (let* ((label (mevedel-view--tool-animation-label target))
                     ;; Stable fragment identity does not imply stable text:
                     ;; overflow counts change even while motion is frozen.
                     (display (if (equal label (nth 3 entry))
                                  (cadr entry)
                                (mevedel-view-animation-frame
                                 mevedel-view-tool-spinner-style label
                                 (caddr entry) (mevedel-view--tool-animation-face target)
                                 (mevedel-view--animation-buffer-frame)))))
                (unless (equal-including-properties
                         display (get-text-property start 'display))
                  (mevedel-view--put-decorative-display
                   start (marker-position (cdr target)) display)))
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
  "Return non-nil if START..END overlaps WINDOW's visible buffer range.
Use the completed redisplay's range without forcing glyph layout.  Partial
rows and horizontally clipped spans deliberately remain eligible: deciding
whether to animate must cost less than the decorative update itself."
  (and (< start end)
       (< start (or (window-end window) (point-min)))
       (> end (window-start window))))

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
                 (mevedel-view--animation-span-in-window-p
                  position end window))
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

(defun mevedel-view--spinner-visual-period (style &optional glyph-fallback)
  "Return effective time between visual frames for STYLE, or nil.
GLYPH-FALLBACK means a color STYLE is drawn by its glyph fallback."
  (let ((rate (mevedel-view-power-framerate
               mevedel-view-spinner-framerate
               mevedel-view-spinner-battery-framerate
               mevedel-view-spinner-power-policy
               mevedel-view-spinner-animate)))
    (when (and (> rate 0) (not (eq style 'static)))
      (max (/ 1.0 rate)
           (cond ((and glyph-fallback (memq style '(shimmer breathe bounce)))
                  (mevedel-view-animation-period 'braille))
                 ;; Their 8 fps bounded what each stock PGTK frame cost.
                 ((and (memq style '(breathe bounce)) (mevedel-present-active-p)) 0)
                 (t (mevedel-view-animation-period style)))))))

(defun mevedel-view--spinner-next-delay (plan seconds)
  "Return seconds until PLAN's next visible change at animation SECONDS.
PLAN contains (STYLE . PERIOD) entries for eligible indicators and metadata.
Color and glyph tool rows can coexist on different display frames; each
retains its own cadence, including while color shimmer rests."
  (apply #'min
         (mapcar (lambda (entry)
                   (mevedel-view-animation-next-delay
                    (car entry) seconds (cdr entry)))
                 plan)))

(defun mevedel-view--tool-animation-label (target)
  "Return the text a pending-tool indicator TARGET animates.
Glyph styles draw a prefix of their own; shimmer animates the row's verb
and tool name."
  (if (eq mevedel-view-tool-spinner-style 'shimmer)
      (buffer-substring-no-properties (marker-position (car target))
                                      (marker-position (cdr target)))
    ""))

(defun mevedel-view--tool-animation-face (target)
  "Return the face a running-tool indicator TARGET animates in.
A span names its own face, so a shimmering tool name keeps the color it
has once settled; spans without one use `mevedel-view-ephemeral'."
  (or (get-text-property (marker-position (car target))
                         'mevedel-view-tool-animation-face)
      'mevedel-view-ephemeral))

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
  (when (featurep 'mevedel-view-native) (mevedel-view-native-stop))
  (setq mevedel-view--native-animation-targets nil)
  (when (timerp mevedel-view--spinner-timer)
    (mevedel--ui-timer-cancel mevedel-view--spinner-timer))
  (when (timerp mevedel-view--spinner-probe-timer)
    (mevedel--ui-timer-cancel mevedel-view--spinner-probe-timer))
  (setq mevedel-view--spinner-timer nil
        mevedel-view--spinner-timer-period nil
        mevedel-view--spinner-timer-plan nil
        mevedel-view--spinner-probe-timer nil)
  (mevedel-view-power-unwatch (current-buffer)))

(defun mevedel-view--refresh-themed-status (&optional force)
  "Repaint visible indicator spans at their current or frozen phase.
FORCE also checks frozen glyph fallbacks on a visibility rearm, even
without a theme change.  Hidden targets retain the pending refresh;
neither the status row nor the composer is rebuilt."
  (when (or force mevedel-view--spinner-theme-stale-p)
    (let ((seconds (mevedel-view--animation-display-seconds))
          (frozen (mevedel-view--animation-frozen-p))
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
                      (mevedel-view--put-decorative-display start end frame))))
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
                             mevedel-view-tool-spinner-style
                             (mevedel-view--tool-animation-label target)
                             (if frozen
                                 (mevedel-view--tool-sample-seconds start)
                               seconds)
                             (mevedel-view--tool-animation-face target) display-frame)))
                (unless (equal-including-properties
                         frame (get-text-property start 'display))
                  (let ((inhibit-read-only t)
                        (buffer-undo-list t))
                    (with-silent-modifications
                      (mevedel-view--put-decorative-display start end frame))))
                (unless frozen
                  (mevedel-view--record-tool-sample start seconds)))
            (setq pending t))))
      (setq mevedel-view--spinner-theme-stale-p pending))))

(defun mevedel-view--recheck-spinner-after-redisplay ()
  "Rearm this view's spinner once the pending redisplay has completed.
Visibility uses the last completed redisplay's window range.  A scroll
hook still sees the old range, and so does a label or tool row inserted
since, so deciding there would freeze a spinner that is about to appear."
  (unless (mevedel--ui-timer-pending-p mevedel-view--spinner-probe-timer)
    (let ((buffer (current-buffer))
          (timer (timer-create)))
      (timer-set-time timer (time-add nil (seconds-to-time 0.2)))
      (timer-set-function
       timer
       (lambda ()
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (eq timer mevedel-view--spinner-probe-timer)
               (setq mevedel-view--spinner-probe-timer nil)
               (when (mevedel-view--spinner-active-p)
                 (mevedel-view--start-spinner-timer t)))))))
      (mevedel--ui-timer-activate timer)
      (setq mevedel-view--spinner-probe-timer timer))))

(defun mevedel-view--probe-spinner-visibility ()
  "Recheck an active spinner whose spans are not yet known to be on screen.
Nothing else rearms a view that stays attended and unscrolled, so without
this a new label would stay frozen, elapsed time included.  Each set of
spans is probed once: a span still off screen waits for scrolling or
a window resize."
  (let ((targets (list mevedel-view--spinner-label-target
                       mevedel-view--spinner-metadata-target
                       mevedel-view--spinner-tool-targets)))
    (when (and (mevedel-view--spinner-active-p)
               (cl-some #'car-safe targets)
               (not (equal targets mevedel-view--spinner-probed-targets))
               (cl-some #'mevedel-view--animation-window-attended-p
                        (get-buffer-window-list (current-buffer) nil t)))
      (setq mevedel-view--spinner-probed-targets targets)
      (mevedel-view--recheck-spinner-after-redisplay))))

(defun mevedel-view--sync-native-animation (main tools)
  "Offload the visible MAIN label and TOOLS when native presentation is ready."
  (let* ((rate (mevedel-view-power-framerate
                mevedel-view-spinner-framerate mevedel-view-spinner-battery-framerate
                mevedel-view-spinner-power-policy mevedel-view-spinner-animate))
         (seconds (mevedel-view--animation-seconds))
         specs)
    (when (and (> rate 0) (not (mevedel-view--animation-frozen-p)))
      (when main
        (push (list mevedel-view--spinner-label-target mevedel-view-spinner-style
                    (buffer-substring-no-properties
                     (car mevedel-view--spinner-label-target)
                     (cdr mevedel-view--spinner-label-target))
                    'mevedel-view-spinner
                    (if (memq mevedel-view-spinner-style '(shimmer breathe bounce))
                        (/ 1.0 rate) main)) specs))
      (dolist (target tools)
        (when-let* ((period (mevedel-view--spinner-visual-period mevedel-view-tool-spinner-style)))
          (push (list target mevedel-view-tool-spinner-style
                      (mevedel-view--tool-animation-label target)
                      (mevedel-view--tool-animation-face target) period) specs))))
    (setq mevedel-view--native-animation-targets
          (when (or (not noninteractive) (featurep 'mevedel-view-native))
            (mevedel-view-native-sync specs seconds #'mevedel-view--start-spinner-timer)))
    mevedel-view--native-animation-targets))

(defun mevedel-view--arm-spinner-timer (timer plan)
  "Arm TIMER for PLAN's next visual change or shared metadata tick."
  (let ((metadata-only (equal plan '((metadata . 1.0)))))
    (timer-set-time
     timer (if metadata-only
               (timer-next-integral-multiple-of-time nil 1)
             (time-add nil (seconds-to-time
                            (mevedel-view--spinner-next-delay
                             plan (mevedel-view--animation-seconds))))))
    (mevedel--ui-timer-activate timer metadata-only)))

(defun mevedel-view--start-spinner-timer (&optional resumed)
  "Start one view timer at the next needed visual or metadata cadence.
RESUMED means visibility or focus changed, so frozen glyph support can be
rechecked without changing the displayed animation phase."
  (mevedel-view--capture-native-animation)
  ;; Keep every registered label pinned, including the overflow row and
  ;; main label.  The row cap bounds this working set; a fixed four-bank
  ;; cache predates distinct tool labels and evicted still-visible rows.
  (mevedel-view-animation-reserve
   (1+ (length mevedel-view--spinner-tool-targets)))
  ;; A shared display property cannot carry separate palettes for two frames.
  ;; Repaint on a frame move even when this view has no decorative or elapsed
  ;; timer to notice it; defer the repaint while the label is hidden.
  (when (and mevedel-view--spinner-status
             (memq mevedel-view-spinner-style
                   '(shimmer breathe bounce braille dots)))
    (let* ((target mevedel-view--spinner-label-target)
           (moving (mevedel-view--animation-target-visible-p
                    target 'mevedel-view-spinner-frame))
           (frame (and moving
                       (mevedel-view--animation-target-frame
                        target (eq mevedel-view-spinner-style 'dots)))))
      (when (and moving
                 (not (equal mevedel-view--spinner-sample-frame frame)))
        (setq mevedel-view--spinner-theme-stale-p t))))
  (let ((frozen-glyphs
         (and resumed
              (mevedel-view--animation-frozen-p)
              (or (memq mevedel-view-spinner-style '(braille dots))
                  (and mevedel-view--spinner-tool-targets
                       (memq mevedel-view-tool-spinner-style
                             '(braille dots)))))))
    (when (and frozen-glyphs
               (or (eq mevedel-view-spinner-style 'dots)
                   (eq mevedel-view-tool-spinner-style 'dots)))
      (mevedel-view-animation-reset-glyph-support))
    ;; A hidden color label can remain stale while tools keep animating.  A
    ;; theme event or visibility rearm probes its rows once, not on each tool
    ;; frame; both enter this scheduler with RESUMED non-nil.
    (let ((tool-colors (and resumed mevedel-view--spinner-tool-targets
                            (eq mevedel-view-tool-spinner-style 'shimmer))))
      (when (or (and resumed mevedel-view--spinner-theme-stale-p)
                frozen-glyphs tool-colors)
        (mevedel-view--refresh-themed-status (or frozen-glyphs tool-colors)))))
  (when (and (mevedel-view--spinner-active-p)
             (not mevedel-view--spinner-phase-start))
    (setq mevedel-view--spinner-phase-start (float-time)))
  ;; Latch on a zero-fps transition even when the next redraw is delayed.
  ;; An ordinary rearm must not record the clock phase as a displayed sample.
  (if (mevedel-view--animation-frozen-p)
      (progn
        (mevedel-view--animation-display-seconds)
        ;; Move spans drawn before the freeze to their resting phase.
        (mevedel-view--refresh-themed-status))
    (setq mevedel-view--spinner-frozen-seconds nil))
  (let* ((paused (and-let* ((request (mevedel-view--spinner-request)))
                    (mevedel-request-active-work-pause-started-at request)))
         ;; Waiting for the user is not work: hold every indicator still,
         ;; as the elapsed time already is, instead of animating for as long
         ;; as a prompt stays unanswered.
         (visible (and (not paused) (mevedel-view--animation-visible-p)))
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
                    mevedel-view-spinner-style (not main-color)))))
         (tool-targets (and visible
                            (cl-remove-if-not
                             (lambda (target)
                               (mevedel-view--animation-target-visible-p
                                target 'mevedel-view-inline-spinner-frame))
                             mevedel-view--spinner-tool-targets)))
         (native (mevedel-view--sync-native-animation main tool-targets))
         ;; Prepare every visible label before frame callbacks.  Some spans
         ;; can use color while others need the portable multi-frame glyph.
         (tool-kinds
          (let (kinds)
            (dolist (target (cl-set-difference tool-targets native :test #'eq))
              (cl-pushnew
               (if (and (eq mevedel-view-tool-spinner-style 'shimmer)
                        (mevedel-view-animation-color-available-p
                         'shimmer (mevedel-view--tool-animation-label target)
                         (mevedel-view--tool-animation-face target)
                         (mevedel-view--animation-target-frame target)))
                   'shimmer 'glyph)
               kinds))
            kinds))
         (plan
          (delq nil
                (append
                 (list (and main (not (memq mevedel-view--spinner-label-target native))
                            (cons (if main-color mevedel-view-spinner-style
                                    'glyph) main)))
                 (mapcar
                  (lambda (kind)
                    (when-let* ((period (mevedel-view--spinner-visual-period
                                        mevedel-view-tool-spinner-style
                                        (eq kind 'glyph))))
                      (cons kind period)))
                  (sort tool-kinds #'string-lessp))
                 (list (and mevedel-view--spinner-status
                            (not paused)
                            (mevedel-view--spinner-metadata-visible-p)
                            '(metadata . 1.0))))))
         (period (and plan (apply #'min (mapcar #'cdr plan)))))
    (unless period
      (mevedel-view--probe-spinner-visibility))
    (when (mevedel-view--animation-wants-power-p visible)
      (mevedel-view-power-watch (current-buffer)
                                #'mevedel-view--start-spinner-timer))
    (unless (mevedel-view--animation-wants-power-p visible)
      (mevedel-view-power-unwatch (current-buffer)))
    (unless (and plan
                 (equal plan mevedel-view--spinner-timer-plan)
                 (or (mevedel--ui-timer-pending-p mevedel-view--spinner-timer)
                     (and mevedel-view--spinner-timer
                          (eq mevedel-view--spinner-timer
                              mevedel-view--spinner-delivering))))
      (when (timerp mevedel-view--spinner-timer)
        (mevedel--ui-timer-cancel mevedel-view--spinner-timer))
      (setq mevedel-view--spinner-timer nil
            mevedel-view--spinner-timer-period period
            mevedel-view--spinner-timer-plan plan)
      (when period
        (let ((buffer (current-buffer)) timer)
          (setq timer (timer-create))
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
                           (let ((mevedel-view--spinner-delivering timer))
                             (mevedel-view--spinner-tick))
                           ;; A one-shot timer cannot replay deadlines missed
                           ;; during a stall.  Reuse its object after each
                           ;; delivered tick to avoid frame-rate allocation.
                           (when (eq timer mevedel-view--spinner-timer)
                             (mevedel-view--arm-spinner-timer timer plan)))
                       (error (mevedel-view--stop-spinner-timer)))
                   (mevedel--ui-timer-cancel timer)
                   (when (eq timer mevedel-view--spinner-timer)
                     (mevedel-view--stop-spinner-timer)
                     (when (and (derived-mode-p 'mevedel-view-mode)
                                (mevedel-view--spinner-active-p))
                       (mevedel-view--start-spinner-timer))))))))
          (mevedel-view--arm-spinner-timer timer plan)
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
      ;; A new style changes the running marker, so redraw the live turn.
      (when (and mevedel-view--pending-tool-calls
                 (not (eq mevedel-view--spinner-rendered-tool-style
                          mevedel-view-tool-spinner-style))
                 (boundp 'mevedel--data-buffer)
                 (buffer-live-p mevedel--data-buffer))
        (mevedel-view--schedule-tool-boundary-render mevedel--data-buffer))
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
              (mevedel-view--start-spinner-timer t)
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
         (lambda () (mevedel-view--ensure-request-progress))
         (mevedel-view--request-progress-mutation-region)))
      ;; Theme/face changes can turn a color bank into a glyph fallback (or
      ;; back).  Re-evaluate the actual cadence only on this semantic tick.
      (mevedel-view--start-spinner-timer)))
  (let ((seconds (mevedel-view--animation-seconds))
        (moving (not (mevedel-view--animation-frozen-p)))
        (was-modified (buffer-modified-p))
        (inhibit-read-only t)
        (inhibit-modification-hooks t)
        (buffer-undo-list t))
    (unwind-protect
        (progn
          (when (and moving
                     (not (memq mevedel-view--spinner-label-target
                                mevedel-view--native-animation-targets))
                     (mevedel-view--spinner-visual-period
                      mevedel-view-spinner-style
                      (not mevedel-view--spinner-main-color-p))
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
                    (mevedel-view--put-decorative-display start end frame))))))
          (when (and moving (mevedel-view--spinner-visual-period
                             mevedel-view-tool-spinner-style))
            (dolist (target mevedel-view--spinner-tool-targets)
              (when (and (not (memq target mevedel-view--native-animation-targets))
                         (mevedel-view--animation-target-visible-p
                          target 'mevedel-view-inline-spinner-frame))
                (let* ((start (marker-position (car target)))
                       (style mevedel-view-tool-spinner-style)
                       (label (mevedel-view--tool-animation-label target))
                       (display-frame
                        (mevedel-view--animation-target-frame
                         target (eq style 'dots))))
                  ;; Like the label: a color bank is prepared at semantic
                  ;; cadence, never inside a frame callback.
                  (when (or (not (eq style 'shimmer))
                            (mevedel-view-animation-color-ready-p
                             style label (mevedel-view--tool-animation-face target) display-frame))
                    (let ((frame (mevedel-view-animation-frame
                                  style label seconds
                                  (mevedel-view--tool-animation-face target) display-frame)))
                      (mevedel-view--record-tool-sample start seconds)
                      (unless (equal-including-properties
                               frame (get-text-property start 'display))
                        (mevedel-view--put-decorative-display
                         start (marker-position (cdr target)) frame)))))))))
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
     ;; Reconciliation restores semantic positions inside the replaced rail.
     ;; An outer `save-excursion' would overwrite that with its collapsed marker.
     (mevedel-view--render-request-progress)
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
     (mevedel-view--render-request-progress)
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
  "Return the permanent rail to idle and release request animation."
  (mevedel-view--call-preserving-input-point
   (lambda ()
     (mevedel-view--debug-log
      'spinner-stop-delete
      :spinner (mevedel-view--debug-spinner-state)
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
        (mevedel-view--stop-spinner-timer))
     (mevedel-view--render-request-progress))))

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
      ;; In a live turn `mevedel-view-stream-pre-tool' draws the call as a
      ;; running row; the progress label would only repeat it.
      (unless (or (mevedel-view--routine-pending-poll-p info)
                  (and (mevedel-view-stream-in-flight-turn-start-position)
                       (markerp mevedel-view--data-turn-start)
                       (marker-position mevedel-view--data-turn-start)))
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
           :output-preview-truncated-p
           (plist-get event :output-preview-truncated-p)
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

(defun mevedel-view-stream--schedule-execution-row-recovery (data-buffer tool-use-id)
  "Recover TOOL-USE-ID's missing view row when DATA-BUFFER has its source.
A live call may not have entered the transcript yet.  Its progress stays
cached until source arrival and normal stream/tool-boundary rendering;
repeatedly rebuilding an unchanged transcript cannot materialize that row.
Nested calls recover through their source-owning parent.  A nil TOOL-USE-ID
requests generic recovery, without a particular source identity to check."
  (when (and (buffer-live-p data-buffer)
             (mevedel-view-stream-in-flight-turn-start-position)
             (markerp mevedel-view--data-turn-start)
             (or (null tool-use-id)
                 (with-current-buffer data-buffer
                   (save-restriction
                     (widen)
                     (let ((owner tool-use-id) source)
                       (while (and owner (not source))
                         (setq source (mevedel-tool-render-data-segment-bounds owner)
                               owner (and (string-match "/[0-9]+\\'" owner)
                                          (substring owner 0 (match-beginning 0)))))
                       source)))))
    (mevedel-view--schedule-render
     'incremental data-buffer mevedel-view-stream-render-delay)))

(defun mevedel-view-stream--terminal-breadcrumb (event)
  "Project EVENT's newly persisted breadcrumb into an open current view.
Only the transcript audit supplies its identity and source range.  A view
opened later, or one inspecting a historical segment, uses normal source
projection instead of maintaining a second completion registry.  While
the view renders a live turn, its tail renders the record from source
wherever the transcript placed it, so an activity group can count it."
  (when-let* ((data (plist-get event :data-buffer))
              ((buffer-live-p data))
              (view (mevedel-view-stream--execution-view-buffer data)))
    (with-current-buffer view
      (when (eq data (mevedel-view-segments-display-buffer))
        (if (and (mevedel-view-stream-in-flight-turn-start-position)
                 (markerp mevedel-view--data-turn-start))
            (mevedel-view--schedule-tool-boundary-render data)
          ;; Without a live turn the transcript received the record at its
          ;; end, as its last audit block.
          (when-let* ((execution-id
                       (plist-get (plist-get event :facts) :execution-id))
                      (record
                       (with-current-buffer data
                         (save-restriction
                           (widen)
                           (save-excursion
                             (goto-char (point-max))
                             (when (search-backward mevedel--hook-audit-open nil t)
                               (cl-find-if
                                (lambda (item)
                                  (and (equal execution-id
                                              (plist-get item :execution-id))
                                       (equal (plist-get event :tool-use-id)
                                              (plist-get item :tool-use-id))
                                       (equal (plist-get event :owner)
                                              (plist-get item :owner))))
                                (mevedel-view--hook-audit-records-from-text
                                 (buffer-substring (point) (point-max))
                                 'execution-breadcrumb data (point)))))))))
            (mevedel-view-render-mutate
             (list 'execution-breadcrumb execution-id)
             (lambda ()
               (unless (mevedel-view-audit-breadcrumb-present-p
                        record (mevedel-view--history-insertion-marker))
                 (let ((at (marker-position
                            (mevedel-view--history-insertion-marker))))
                   (mevedel-view--call-preserving-user-view-state
                    (lambda ()
                      (mevedel-view--call-with-render-boundaries-advancing
                       (lambda ()
                         (let ((inhibit-read-only t)
                               (inhibit-modification-hooks t))
                           (save-excursion
                             (goto-char at)
                             (mevedel-view--insert-hook-audit-block
                              record (plist-get record :source)))))))
                    ;; A pure insertion: text on either side is unchanged.
                    (cons at at))))))))))))

(defun mevedel-view-stream-handle-execution-event (event)
  "Apply Bash EVENT to its authoritative row and visible view.
Always return nil; only the mailbox sink may acknowledge durable delivery."
  (mevedel-execution-transcript-handle-event event)
  ;; The execution owner already bounds the tail and marks its own truncation.
  ;; Do not introduce an undisclosed second, line-based output limit here.
  ;; A Monitor batch changes nothing its row shows: the event count reaches
  ;; the status zone through the execution state hook, and the events reach
  ;; the model as mail.  Refreshing the row would redraw it unchanged.
  (unless (eq (plist-get event :type) 'monitor-batch)
    (mevedel-view-stream-handle-tool-progress event))
  (when (eq (plist-get event :type) 'terminal)
    (mevedel-view-stream--terminal-breadcrumb event))
  nil)

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
           data-buffer tool-use-id))))))
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
window share one refresh instead of rebuilding the view per token.  A
batch flushed by `mevedel-gptel-stream-bridge' already coalesced its
chunks: it renders through `mevedel-gptel-stream-bridge-after-flush',
inside the batch's own timer, instead of waking the editor again, and
shows the text sooner."
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
        (if (bound-and-true-p mevedel-gptel-stream-bridge-flushing)
            (mevedel-gptel-stream-bridge-after-flush
             (lambda ()
               (when (buffer-live-p view-buf)
                 (with-current-buffer view-buf
                   (mevedel-view--schedule-render 'incremental data-buf 0)))))
          (mevedel-view--schedule-render
           'incremental data-buf mevedel-view-stream-render-delay)))))
  nil)

(defun mevedel-view--routine-pending-poll-p (info)
  "Return non-nil when tool INFO is solely an empty-input WriteStdin poll.
Read a direct ToolCall with the bounded guest reader, but do not execute it.
Ambiguous expressions retain their pending row.  The settled result remains
visible if the control operation fails."
  (let ((name (plist-get info :name))
        (args (plist-get info :args)))
    (or (and (equal name "WriteStdin")
             (listp args)
             (zerop (% (length args) 2))
             (let ((keys (cl-loop for (key _) on args by #'cddr
                                  collect key)))
               (and (cl-every (lambda (key)
                                (memq key '(:execution_id :chars
                                            :yield_time_ms)))
                              keys)
                    (= (length keys) (length (delete-dups keys)))))
             (stringp (plist-get args :execution_id))
             (not (string-empty-p (plist-get args :execution_id)))
             (equal (or (plist-get args :chars) "") "")
             (or (not (plist-member args :yield_time_ms))
                 (let ((wait (plist-get args :yield_time_ms)))
                   (and (integerp wait) (<= 250 wait 300000)))))
        (and (equal name "ToolCall")
             (consp args)
             (eq (car args) :expression)
             (null (cddr args))
             (when-let* ((script (plist-get args :expression))
                         ((stringp script)))
               (condition-case nil
                   (let* ((forms (cdr (car (mevedel-ptc--read script))))
                          (form (car forms))
                          (operands (cdr form)))
                     (when (and (null (cdr forms))
                                (consp form)
                                (symbolp (car form))
                                (equal (symbol-name (car form)) "WriteStdin")
                                (zerop (% (length operands) 2)))
                       (let* ((pairs
                               (cl-loop for (key value) on operands by #'cddr
                                        collect
                                        (cons (and (symbolp key)
                                                   (symbol-name key))
                                              value)))
                              (id (cdr (assoc ":execution_id" pairs)))
                              (chars (assoc ":chars" pairs))
                              (wait (assoc ":yield_time_ms" pairs)))
                         (and (cl-every
                               (lambda (pair)
                                 (member (car pair)
                                         '(":execution_id" ":chars"
                                           ":yield_time_ms")))
                               pairs)
                              (= (length pairs)
                                 (length (delete-dups (mapcar #'car pairs))))
                              (stringp id) (not (string-empty-p id))
                              (or (null chars) (equal (cdr chars) ""))
                              (or (null wait)
                                  (and (integerp (cdr wait))
                                       (<= 250 (cdr wait) 300000)))))))
                 (error nil)))))))

(defun mevedel-view--pending-tool-fingerprint (info)
  "Return the pending-tool fingerprint for tool INFO.

gptel builds its tool-hook arguments from the name, the arguments, and
the result, so there is no call id to key on: identical parallel calls
produce the same fingerprint.  Hook and pipeline-event arguments for one
call agree under `mevedel-tool-args-key'; compare with `equal'."
  (cons (plist-get info :name)
        (mevedel-tool-args-key (plist-get info :args))))

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
Durable IDs are exact.  ID-less hooks may only claim unbound entries:
an out-of-order completion must not consume a bound identical sibling."
  (let ((id (plist-get info :tool-use-id))
        (fingerprint (mevedel-view--pending-tool-fingerprint info)))
    (cl-find-if
     (lambda (entry)
       (let ((bound-id (plist-get (nth 3 entry) :tool-use-id)))
         (if id (equal id bound-id)
           (and (null bound-id) (equal (car (car entry)) fingerprint)))))
     mevedel-view--pending-tool-calls)))

(defun mevedel-view--pending-tool-finished-entry (info)
  "Return the pending entry an ID-less post-tool hook for INFO completes.
A call the pipeline already reported finished goes first; otherwise an
unbound call with INFO's fingerprint.  A bound sibling still running is
never taken: its completion arrives out of order."
  (let ((fingerprint (mevedel-view--pending-tool-fingerprint info)))
    (or (cl-find-if (lambda (entry)
                      (and (plist-get (nth 3 entry) :settled)
                           (equal (car (car entry)) fingerprint)))
                    mevedel-view--pending-tool-calls)
        (mevedel-view--pending-tool-entry info))))

(defun mevedel-view-stream-handle-pipeline-event (event)
  "Bind pending rows and display accepted approval provenance from EVENT.
Only top-level invocations own pending rows.  PTC publishes nested progress
on its canonical child records; it must not decorate the whole envelope."
  (let ((data (plist-get event :data-buffer))
        (id (plist-get event :tool-use-id)))
    (when (and (buffer-live-p data) id
               (not (plist-get event :parent-tool-use-id)))
      (dolist (view (buffer-list))
        (when (eq data (buffer-local-value 'mevedel--data-buffer view))
          (with-current-buffer view
            (let ((entry (mevedel-view--pending-tool-entry event)) changed)
              (pcase (plist-get event :type)
                ('started
                 (unless (or entry
                             (equal (plist-get event :name) "Agent")
                             (mevedel-view--routine-pending-poll-p event))
                   ;; Admission order matches pre-hook order, even when
                   ;; approvals and completions later arrive out of order.
                   (setq entry
                         (mevedel-view--pending-tool-entry
                          (list :name (plist-get event :name)
                                :args (plist-get event :args))))
                   (unless entry
                     (setq entry
                           (list (mevedel-view--pending-tool-claim-key event)
                                 (plist-get event :name)
                                 (mevedel-tool-display-string
                                  (plist-get event :name) (plist-get event :args))))
                     (setq mevedel-view--pending-tool-calls
                           (append mevedel-view--pending-tool-calls (list entry))))
                   (setcdr (nthcdr 2 entry) (list (list :tool-use-id id)))
                   (setq changed t)))
                ('permission-approved
                 (when entry
                   (setf (nth 3 entry)
                         (plist-put (nth 3 entry) :permission-approvals
                                    (copy-tree (plist-get event :permission-approvals))))
                   (setq changed t)))
                ('settled
                 ;; The result reaches the transcript only after gptel's
                 ;; post-tool hook (for a parallel batch, after the whole
                 ;; batch), which removes the entry.  Until then the call
                 ;; keeps its place in the run, shown finished.
                 (when entry
                   (setf (nth 3 entry)
                         (plist-put (nth 3 entry) :settled
                                    (or (plist-get event :status) t)))
                   (setq changed t))))
              (when (and changed
                         (eq data (mevedel-view-segments-display-buffer))
                         (mevedel-view-stream-in-flight-turn-start-position)
                         (markerp mevedel-view--data-turn-start))
                (mevedel-view--schedule-tool-boundary-render data))))))))
  nil)

(add-hook 'mevedel-pipeline-tool-event-hook
          #'mevedel-view-stream-handle-pipeline-event)

(defun mevedel-view-stream-pre-tool (args)
  "Mark an in-flight tool call from ARGS and schedule a view render.

Runs as a `gptel-pre-tool-call-functions' hook in the data buffer.
Adds an entry to `mevedel-view--pending-tool-calls' on the
associated view buffer.  gptel runs the hook for every call of a batch
at once, and the batch shares one debounced render that draws its calls
running in the turn's trailing activity run."
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
      (unless (or (equal name "Agent")
                  (mevedel-view--routine-pending-poll-p args))
        (let ((key (mevedel-view--pending-tool-claim-key args))
              ;; WaitAgent settles as `WaitAgent: agents (...)'.
              (detail (if (equal name "WaitAgent")
                          "agents"
                        (mevedel-tool-display-string
                         name (plist-get args :args)))))
          ;; One entry per call, not per key: identical parallel calls
          ;; share a fingerprint, and collapsing them would drop the
          ;; running row while the other call is still running.
          (setq mevedel-view--pending-tool-calls
                (append mevedel-view--pending-tool-calls
                        (list (list key name detail))))))
      ;; Keep the request-level progress row visible; running tool rows
      ;; do not replace elapsed request progress.
      (mevedel-view--ensure-request-progress data-buf)
      (mevedel-view--start-spinner-timer)
      (when (and (mevedel-view-stream-in-flight-turn-start-position)
                 (markerp mevedel-view--data-turn-start))
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
ARGS is the tool-call plist.  gptel runs the hook for every call of a
finished batch and then writes their results, so the batch's shared
render draws the calls settled; until then their rows keep running."
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
      (when-let* ((entry (mevedel-view--pending-tool-finished-entry args)))
        (setq mevedel-view--pending-tool-calls
              (delq entry mevedel-view--pending-tool-calls)))
      (unless (or mevedel-view--pending-tool-calls
                  (mevedel-view--request-progress-visible-p))
        (mevedel-view--stop-spinner-timer))
      (when (and (mevedel-view-stream-in-flight-turn-start-position)
                 (markerp mevedel-view--data-turn-start))
        (mevedel-view--schedule-tool-boundary-render data-buf)
        (mevedel-view--debug-log
         'post-tool-hook-after-schedule
         :state (mevedel-view--debug-state data-buf)))))
  ;; gptel post-tool hooks must return nil unless they intentionally
  ;; provide a control plist.
  nil)

(defun mevedel-view--adopt-tool-animation-targets (start end previous)
  "Animate the running tool rows a live render drew in START..END.
PREVIOUS holds the samples displayed before the render replaced the rows."
  (mevedel-view--capture-tool-animation-targets start end)
  ;; Newly drawn rows display their initial glyph until a frame tick.  A
  ;; row that survives a redraw retains both its display and its phase.
  (setq mevedel-view--spinner-tool-samples nil)
  (mevedel-view--restore-tool-animation-targets previous)
  (setq mevedel-view--spinner-rendered-tool-style
        mevedel-view-tool-spinner-style)
  (mevedel-view--start-spinner-timer))

(defun mevedel-view--retire-pending-tool-rows ()
  "Remove running tool decorations no render will replace.
A turn that stopped without settling, or a live render with nothing to
redraw, leaves them behind.  Only the in-flight turn can hold them."
  (apply #'mevedel-view--release-animation-targets
         mevedel-view--spinner-tool-targets)
  (setq mevedel-view--spinner-tool-targets nil)
  (when-let* (((markerp mevedel-view--status-marker))
              (end (marker-position mevedel-view--status-marker)))
    (let ((inhibit-modification-hooks t))
      (mevedel-view--retire-pending-decorations
       (min end (or (mevedel-view-stream-in-flight-turn-start-position)
                    (point-min)))
       end))))

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
        (mevedel-view--cancel-scheduled-render t)
        (when (and mevedel-view--pending-tool-calls
                   (buffer-live-p mevedel--data-buffer)
                   (eq mevedel--data-buffer (mevedel-view-segments-display-buffer))
                   (mevedel-view-stream-in-flight-turn-start-position)
                   (markerp mevedel-view--data-turn-start))
          ;; Running calls counted toward their group's summary.  An abort
          ;; renders nothing after this, so redraw the live turn from what
          ;; reached the transcript, or a group would keep counting calls
          ;; that never ran.
          (setq mevedel-view--pending-tool-calls nil)
          (condition-case err
              (mevedel-view--render-live-region mevedel--data-buffer nil)
            ;; Stopping must not fail; retiring the rows below still removes
            ;; every running decoration.
            (error
             (mevedel--warn-once
              'view-stream-stop-render
              "Stopped turn render failed: %s" (error-message-string err)))))
        (mevedel-view--retire-pending-tool-rows)
        (setq mevedel-view--pending-tool-calls nil))
    (mevedel-view-stream--release-turn)))

(defun mevedel-view-stream--release-turn ()
  "Release terminal resources even when its projection was superseded."
  (mevedel-view-render-invalidate-live-tail)
  ;; A superseding full render must not recreate progress from the old
  ;; spinner timestamp or a request that has not finished settling yet.
  (setq mevedel-view--pending-tool-calls nil
        mevedel-view--carried-tool-samples nil
        mevedel-view--request-progress-suppressed t
        mevedel-view--spinner-start-time nil
        mevedel-view--spinner-phase-start nil
        mevedel-view--spinner-frozen-seconds nil
        mevedel-view--spinner-last-sample-seconds nil)
  ;; Retire obsolete streamed work; a pending full render survives.
  (mevedel-view--cancel-scheduled-render t)
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
                  (mevedel-view--cancel-scheduled-render t)
                  ;; Settling draws only what reached the transcript.
                  (mevedel-view--retire-pending-tool-rows)
                  (setq mevedel-view--pending-tool-calls nil)
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
