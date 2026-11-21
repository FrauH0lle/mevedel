;;; niwa-view-stream.el --- Streaming view lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Owns active-turn progress state and coalesced streaming/tool-boundary
;; redraws.  The gptel compatibility layer and durable execution transcripts
;; have dedicated owners; transcript interpretation and composer editing remain
;; behind the view module's rendering interface.
;;
;; Modules outside the view package report request progress through the
;; `niwa-view-stream-{show,ensure,end,claim,release}-progress' verbs; see
;; the comment above `niwa-view-stream-show-progress' for their contracts.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-execution-transcript)
(require 'niwa-telemetry)
(require 'niwa-view-animation)
(require 'niwa-present)
(require 'niwa-view-power)
(require 'niwa-view-execution)
(require 'niwa-ptc-interpreter)
(require 'niwa-tool-registry)
(require 'niwa-utilities)
(require 'niwa-view-render)
(require 'niwa-view-zone)

;; `cl-extra'
(declare-function cl-subseq "cl-extra" (sequence start &optional end))

;; `gptel'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `niwa-compact-run'
(defvar niwa-compact-run-in-flight)

;; `niwa-execution-transcript'
(declare-function niwa-execution-transcript-handle-event
                  "niwa-execution-transcript" (event))
(declare-function niwa-execution-transcript-retry-pending-terminals
                  "niwa-execution-transcript" (data-buffer))

;; `niwa-gptel-stream-bridge'
(declare-function niwa-gptel-stream-bridge-after-flush
                  "niwa-gptel-stream-bridge" (function))

;; `niwa-structs'
(declare-function niwa-request-active-elapsed-seconds
                  "niwa-structs" (request &optional now))
(declare-function niwa-request-active-work-pause-started-at
                  "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--data-buffer)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-telemetry'
(declare-function niwa-telemetry-current-session
                  "niwa-telemetry" (&optional buffer))
(declare-function niwa-telemetry-record
                  "niwa-telemetry" (session event &rest props))

;; `niwa-tool-render-data'
(declare-function niwa-tool-render-data-segment-bounds
                  "niwa-tool-render-data" (tool-use-id))

;; `niwa-utilities'
(declare-function niwa--duration-label "niwa-utilities" (seconds))
(declare-function niwa--timer-pending-p "niwa-utilities" (timer))
(declare-function niwa--warn-once
                  "niwa-utilities" (scope key format &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(autoload 'niwa--warn-once "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")


;; `niwa-view'
(declare-function niwa-view--cancel-scheduled-render "niwa-view"
                  (&optional keep-tool-rows))
(declare-function niwa-view--schedule-render
                  "niwa-view" (kind data-buffer delay))
(declare-function niwa-view--tool-status-string "niwa-view" (tool-name args))
(declare-function niwa-view--unattended-p "niwa-view" (&optional buffer))
(declare-function niwa-view--window-attended-p "niwa-view" (window))
(declare-function niwa-view-rerender "niwa-view" (&optional buffer))
(defvar niwa-view--display-map)
(defvar niwa-view--pending-render-kind)
(defvar niwa-view--pending-tool-rows)
(defvar niwa-view--status-marker)
(defvar niwa-view-rerender-debounce)
(defvar niwa-view-spinner-animate)
(defvar niwa-view-spinner-style)
(defvar niwa-view-tool-spinner-style)
(defvar niwa-view-spinner-framerate)
(defvar niwa-view-spinner-battery-framerate)
(defvar niwa-view-spinner-power-policy)

;; `niwa-view-native'
(declare-function niwa-view-native-sample "niwa-view-native" (target))
(declare-function niwa-view-native-stop "niwa-view-native" ())
(declare-function niwa-view-native-sync "niwa-view-native" (specs elapsed rearm))
(autoload 'niwa-view-native-sync "niwa-view-native")

;; `niwa-view-composer'
(declare-function niwa-view--agent-fsm-p
                  "niwa-view-composer" (info data-buffer))
(autoload 'niwa-view--agent-fsm-p "niwa-view-composer")
(declare-function niwa-view-call-preserving-user-view-state
                  "niwa-view-composer" (thunk &optional region))
(declare-function niwa-view--call-with-render-boundaries-advancing
                  "niwa-view-composer" (thunk))
(declare-function niwa-view--composer-mode-badge
                  "niwa-view-composer" (&optional mode))
(autoload 'niwa-view--composer-mode-badge "niwa-view-composer")
(declare-function niwa-view-composer-buffer "niwa-view-composer" (&optional view))
(autoload 'niwa-view-composer-buffer "niwa-view-composer")
(declare-function niwa-composer-set-attention "niwa-view-composer" (attention))
(defvar niwa-composer--view)

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-count-label
                  "niwa-view-interaction" (&optional actionable))
(declare-function niwa-view-jump-to-interaction "niwa-view-interaction" ())
(autoload 'niwa-view--interaction-count-label "niwa-view-interaction")

;; `niwa-view-audit'
(declare-function niwa-view--hook-audit-records-from-text
                  "niwa-view-audit" (text &optional type data-buf source-start))
(declare-function niwa-view--insert-hook-audit-block
                  "niwa-view-audit" (record &optional source expanded))
(declare-function niwa-view-audit-breadcrumb-present-p
                  "niwa-view-audit" (record before))

;; `niwa-view-render'
(declare-function niwa-view-render--mouse-held-p "niwa-view-render" ())
(declare-function niwa-view-render-mutate
                  "niwa-view-render" (key function &optional replacement cleanup))
(declare-function niwa-view-render-terminal "niwa-view-render" ())
(declare-function niwa-view-render--settle-now
                  "niwa-view-render" (data-buf start end))
(defvar niwa-view-render--owner)
(defvar niwa-view-render--terminal-p)
(declare-function niwa-view--append-request-summary
                  "niwa-view-render" (data-buf start))
(declare-function niwa-view--cache-put
                  "niwa-view-render" (table key value counter-symbol))
(declare-function niwa-view--debug-log
                  "niwa-view-render" (event &rest data))
(declare-function niwa-view--debug-state
                  "niwa-view-render" (&optional data-buf start end))
(declare-function niwa-view--history-insertion-marker
                  "niwa-view-render" ())
(declare-function niwa-view--refresh-tool-row
                  "niwa-view-render" (data-buffer tool-use-id))
(declare-function niwa-view--render-live-region
                  "niwa-view-render" (data-buf settle-p &optional start end))
(declare-function niwa-view--retire-pending-decorations
                  "niwa-view-render" (start end))
(declare-function niwa-view-render-invalidate-live-tail
                  "niwa-view-render" ())
(declare-function niwa-view-render-live-update
                  "niwa-view-render" (data-buf))
(declare-function niwa-view-render-settle
                  "niwa-view-render" (data-buf start end))

;; `niwa-view-segments'
(declare-function niwa-view-segments-display-buffer
                  "niwa-view-segments" ())

;; `niwa-transcript-audit'
(defvar niwa--hook-audit-open)

;; `niwa-view-zone'
(declare-function niwa-view-zone-clear "niwa-view-zone" (namespace))
(declare-function niwa-view-zone-reconcile "niwa-view-zone" (namespace start end fragments))
(declare-function niwa-view-zone-region "niwa-view-zone" (namespace))

(defvar-local niwa-view--spinner-status nil
  "Current base status text shown by the request-progress row.")

(defvar-local niwa-view--spinner-owner nil
  "Subsystem that last wrote the request-progress status text.")

(defvar-local niwa-view--spinner-generation 0
  "Generation of the latest semantic request-progress status update.")

(defvar-local niwa-view--spinner-start-time nil
  "Fallback wall-clock start time for spinner elapsed display.
Used before a `niwa-request' exists, or in tests that exercise the
spinner without a data-buffer request.")

(defvar-local niwa-view--spinner-timer nil
  "Buffer-local timer animating visible spinner frames.")

(defvar-local niwa-view--spinner-phase-start nil
  "Wall-clock time used to derive animation phase across timer stalls.")

(defvar-local niwa-view--spinner-frozen-seconds nil
  "Animation phase held while decorative motion is disabled.")

(defvar-local niwa-view--spinner-timer-period nil
  "Fastest cadence of the running timer, or nil when none runs.
A shimmer moves at this cadence only during its sweep.")

(defvar-local niwa-view--spinner-timer-plan nil
  "Plan of the running timer; see `niwa-view--spinner-next-delay'.")

(defvar niwa-view--spinner-delivering nil
  "The spinner timer whose callback is running, which rearms it afterwards.
A delivered one-shot timer has left the timer list, so without this the
once-a-second semantic rescheduling inside the callback replaced the
timer and its closure on every tick, even with an unchanged plan.")

(defvar-local niwa-view--spinner-probe-timer nil
  "Timer rechecking a spinner whose spans were not yet on screen.")

(defvar-local niwa-view--spinner-probed-targets nil
  "Spinner spans the visibility probe last rechecked.")

(defvar-local niwa-view--spinner-main-color-p nil
  "Non-nil when the main style renders color rather than a glyph fallback.")

(defvar-local niwa-view--spinner-label-target nil
  "Marker for the foreground label's animation property.")

(defvar-local niwa-view--spinner-metadata-target nil
  "Markers for the foreground label's separate elapsed metadata span.")

(defvar-local niwa-view--spinner-tool-targets nil
  "Markers for the animated spans of running tool rows.")

(defvar-local niwa-view--spinner-tool-samples nil
  "Tool phases to display, keyed by `niwa-view-tool-animation-id'.
A freeze replaces them with resting phases before they are drawn.")

(defvar-local niwa-view--spinner-last-second nil
  "Elapsed label last rendered on a timer tick.")

(defvar-local niwa-view--spinner-last-sample-seconds nil
  "Phase of the most recently sampled request-label display frame.")

(defvar-local niwa-view--spinner-theme-stale-p nil
  "Non-nil when displayed colors need a repaint.
A theme change or frame move invalidates the color sample, and a freeze
moves spans drawn mid-sweep to their resting phase.")

(defvar-local niwa-view--spinner-sample-frame nil
  "Display frame used for the request label's last color sample.
The value `:multiple' means the portable multi-frame fallback was used.")

(defvar-local niwa-view--spinner-rendered-tool-style nil
  "Style the current running tool rows were drawn with.")

(defvar-local niwa-view--carried-tool-samples nil
  "Running-row samples displayed before a full projection removed the rows.
The live render that redraws the rows restores their displayed phase.")

(defvar-local niwa-view--spinner-rendered-state nil
  "Last (STATUS PREFIX) rendered in the request-progress fragment.")

(defvar-local niwa-view--request-progress-suppressed nil
  "Non-nil means the request progress row must not be recreated.
Set during terminal cleanup; cleared when a new progress row is
explicitly started.")

(defvar-local niwa-view--in-flight-turn-start nil
  "View-buffer marker at which the current assistant turn's render begins.

Set by the send path right after the user turn is echoed, consumed by
`niwa-view-render-live-update' to bound the delete-and-re-render
region for each progress update, and cleared when the final
`gptel-post-response-functions' render completes.  Nil outside an
active exchange.")

(defvar-local niwa-view--data-turn-start nil
  "Data-buffer marker at which the current assistant turn starts.

Anchored just after the user prompt was forwarded to the data
buffer, so `niwa-view-render-live-update' can extract only the
in-flight assistant portion (not the whole conversation) when
rebuilding the view.  Nil outside an active exchange.")

(defvar-local niwa-view--pending-tool-serial 0
  "Counter distinguishing pending tool calls that share a fingerprint.")

(defvar-local niwa-view--pending-tool-calls nil
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

(defvar-local niwa-view--execution-event-entries 0
  "Number of entries retained in `niwa-view--execution-events'.")

(defvar-local niwa-view--execution-events nil
  "Latest transient tool progress keyed by durable tool-use id.")

(defcustom niwa-view-stream-render-delay 0.4
  "Seconds to wait before rendering a batch of stream chunks.

The `gptel-post-stream-hook' path fires once per streamed chunk (up to
dozens per second).  `niwa-view-stream-schedule' lets chunks arriving
inside one pending window share a single incremental render.  Tune higher
if the render cost is visible in your environment; lower for snappier updates.

Tool boundaries use a shorter delay but join the same pending render."
  :type 'number
  :group 'niwa)

(defcustom niwa-view-tool-boundary-render-delay 0.05
  "Seconds to coalesce incremental renders around tool boundaries.
A batch of parallel tool calls fires all its pre-tool hooks, and later
all its post-tool hooks, at once; each batch then shares the one
transcript render that draws its calls running or settled."
  :type 'number
  :group 'niwa)

(defun niwa-view--animation-seconds ()
  "Return continuous seconds since the current progress animation started."
  (max 0.0 (- (float-time) (or niwa-view--spinner-phase-start
                                (float-time)))))

(defvar-local niwa-view--native-animation-targets nil
  "Targets currently presented independently of Emacs redisplay.")

(defun niwa-view--capture-native-animation ()
  "Retain the last native samples before freezing or rebuilding their spans.
Once a freeze has latched its resting phases, native samples are stale."
  (when (and (featurep 'niwa-view-native)
             (not niwa-view--spinner-frozen-seconds))
    (dolist (target niwa-view--native-animation-targets)
      (when-let* ((buffer (marker-buffer (car target)))
                  (seconds (with-current-buffer buffer
                             (niwa-view-native-sample target))))
        (if (eq target niwa-view--spinner-label-target)
            (setq niwa-view--spinner-last-sample-seconds seconds)
          (when (marker-position (car target))
            (niwa-view--record-tool-sample (marker-position (car target)) seconds)))))))

(defun niwa-view--animation-frozen-p ()
  "Return non-nil when motion is disabled or the request awaits input."
  (or (and-let* ((request (niwa-view--spinner-request)))
        (niwa-request-active-work-pause-started-at request))
      (zerop (niwa-view--spinner-framerate))))

(defun niwa-view--animation-rest-seconds (style seconds target face)
  "Rest color STYLE at SECONDS for TARGET and FACE, preserving glyph fallbacks."
  (let ((target (and target (marker-position (car target)) target)))
    (if (and (memq style '(shimmer breathe))
             (if target
                 ;; Use the displayed sample even after its window disappears.
                 ;; Prepared color samples carry faces; fallback glyphs do not.
                 (let ((display (niwa-view--animation-target-property target 'display)))
                   (or (memq target niwa-view--native-animation-targets)
                       (and (stringp display) (> (length display) 0)
                            (get-text-property 0 'face display))))
               (niwa-view-animation-color-available-p
                style "" face (niwa-view--animation-buffer-frame))))
        (niwa-view-animation-rest-seconds style seconds)
      seconds)))

(defun niwa-view--animation-display-seconds ()
  "Return the phase for a newly rendered progress label.
Semantic redraws must not advance the glyph when motion is disabled.
The underlying phase continues to advance and resumes without restarting."
  (when (niwa-view--animation-frozen-p)
    (niwa-view--capture-native-animation))
  (if (niwa-view--animation-frozen-p)
      (or niwa-view--spinner-frozen-seconds
          (progn
            ;; Rest instead of holding a faded band for the whole pause, and
            ;; repaint spans already drawn mid-sweep at the next rearm.
            (dolist (target niwa-view--spinner-tool-targets)
              (when-let* ((start (marker-position (car target)))
                          (sample (assoc (get-text-property start 'niwa-view-tool-animation-id)
                                         niwa-view--spinner-tool-samples)))
                (setcdr sample
                        (niwa-view--animation-rest-seconds
                         niwa-view-tool-spinner-style (cdr sample)
                         target (niwa-view--tool-animation-face target)))))
            (setq niwa-view--spinner-theme-stale-p t)
            (setq niwa-view--spinner-frozen-seconds
                  (niwa-view--animation-rest-seconds
                   niwa-view-spinner-style
                   (or niwa-view--spinner-last-sample-seconds
                       (niwa-view--animation-seconds))
                   niwa-view--spinner-label-target 'niwa-view-spinner))))
    (setq niwa-view--spinner-frozen-seconds nil)
    (setq niwa-view--spinner-last-sample-seconds
          (niwa-view--animation-seconds))))

(defun niwa-view--spinner-request ()
  "Return the request that owns the current visible spinner, or nil."
  (when-let* ((data-buf (and (boundp 'niwa--data-buffer)
                             niwa--data-buffer))
              ((buffer-live-p data-buf)))
    (buffer-local-value 'niwa--current-request data-buf)))

(defun niwa-view--spinner-elapsed-label ()
  "Return the elapsed-time label for the current spinner, or nil."
  (when-let* ((seconds
               (if-let* ((request (niwa-view--spinner-request)))
                   (niwa-request-active-elapsed-seconds request)
                 (and niwa-view--spinner-start-time
                      (float-time
                       (time-subtract
                        (current-time)
                        niwa-view--spinner-start-time))))))
    (niwa--duration-label seconds)))

(defun niwa-view--request-progress-active-p (&optional data-buf)
  "Return non-nil when the current view should show request progress.
DATA-BUF defaults to this view's data buffer.  The predicate accepts
both fully materialized requests and the short pre-WAIT interval where
the view has already inserted the in-flight markers."
  (and (not niwa-view--agent-transcript-p)
       (not niwa-view--request-progress-suppressed)
       (or niwa-view--spinner-start-time
           (niwa-view-stream-in-flight-turn-start-position)
           (let ((buf (or data-buf
                          (and (boundp 'niwa--data-buffer)
                               niwa--data-buffer))))
             (and buf
                  (buffer-live-p buf)
                  (buffer-local-value 'niwa--current-request buf))))))

(defun niwa-view--render-request-progress ()
  "Update the composer's first line without changing text or undo positions."
  (when-let* ((composer (niwa-view-composer-buffer)))
    (let* ((status niwa-view--spinner-status)
           (display-status (and status (niwa-view--spinner-display-status status)))
           (badge (niwa-view--composer-mode-badge))
           (attention (niwa-view--interaction-count-label t))
           (windows (niwa-view--windows 'visibility composer))
           (width (max 1 (1- (if windows
                                 (apply #'min (mapcar #'window-body-width windows))
                               (frame-width)))))
           (state (list display-status badge attention width)))
      (unless (equal-including-properties state niwa-view--spinner-rendered-state)
        (let* ((frame (niwa-view--animation-buffer-frame composer))
               (line (niwa-view--format-composer-rail
                      badge attention width status display-status frame)))
          (niwa-view--write-request-rail composer line attention)
          (setq niwa-view--spinner-sample-frame frame
                niwa-view--spinner-rendered-state state))))))

(defun niwa-view--write-request-rail (composer line attention)
  "Paint LINE on COMPOSER's fixed cells and cache its ATTENTION label."
  (let* ((end (1- (length line)))
         (label (text-property-any 0 end 'niwa-view-spinner-frame t line))
         (suffix (and label (next-single-property-change
                             label 'niwa-view-spinner-frame line end)))
         (metadata (and suffix (< suffix end) (substring line suffix end)))
         (new-targets (and label (not niwa-view--spinner-label-target)))
         label-target metadata-target)
    (with-current-buffer composer
      (niwa-composer-set-attention attention)
      (save-restriction
        (widen)
        (let ((start (point-min)))
          (when new-targets
            (setq label-target (cons (copy-marker (+ start 1)) (copy-marker (+ start 2)))
                  metadata-target (cons (copy-marker (+ start 2)) (copy-marker (+ start 3)))))
          (with-silent-modifications
            (put-text-property start (+ start 1) 'display (substring line 0 (or label end)))
            (add-text-properties (+ start 1) (+ start 2)
                                 (list 'display (if label (get-text-property label 'display line) "")
                                       'niwa-view-spinner-frame (and label t)
                                       'niwa-view-spinner-label
                                       (and label (substring-no-properties line label suffix))))
            (add-text-properties (+ start 2) (+ start 3)
                                 (list 'display (or metadata "")
                                       'niwa-view-spinner-status
                                       (and label (get-text-property end 'niwa-view-spinner-status line))))))))
    (when new-targets
      (setq niwa-view--spinner-label-target label-target
            niwa-view--spinner-metadata-target metadata-target))))

(defun niwa-view--animation-target-property (target property)
  "Read PROPERTY at TARGET's start, even outside a buffer restriction."
  (when-let* ((start (car-safe target)) (buffer (marker-buffer start)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (get-text-property start property)))))

(defun niwa-view--rail-animation-label (target)
  "Return the activity label stored on TARGET's composer cell."
  (niwa-view--animation-target-property target 'niwa-view-spinner-label))

(defun niwa-view--paint-rail-sample (start sample)
  "Paint SAMPLE on the fixed rail cell at START."
  (with-current-buffer (marker-buffer start)
    (let ((pos (marker-position start)))
      (unless (equal-including-properties sample (get-text-property pos 'display))
        (with-silent-modifications
          (put-text-property pos (1+ pos) 'display sample))))))

(defun niwa-view--rearm-composer-spinner ()
  "Recheck the animation timer of the current composer's owning view."
  (when (buffer-live-p niwa-composer--view)
    (with-current-buffer niwa-composer--view
      (niwa-view--start-spinner-timer t))))

(defun niwa-view-stream-composer-window-change (_window &optional start)
  "Refit the rail after layout changes and recheck animation visibility.
With START (a scroll notification), wait for redisplay before doing any work."
  (when (buffer-live-p niwa-composer--view)
    (with-current-buffer niwa-composer--view
      (unless start (niwa-view--render-request-progress))
      (when (niwa-view--spinner-active-p)
        (niwa-view--recheck-spinner-after-redisplay)))))

(defun niwa-view--ensure-request-progress (&optional data-buf status)
  "Ensure the foreground request progress row is visible.
DATA-BUF is the authoritative data buffer for elapsed-time lookup.
STATUS is the base label to show; nil preserves the current label or
falls back to \"Working...\"."
  (let ((niwa--data-buffer (or data-buf
                                (and (boundp 'niwa--data-buffer)
                                     niwa--data-buffer))))
    (when (niwa-view--request-progress-active-p data-buf)
      (setq niwa-view--spinner-status
            (or status niwa-view--spinner-status "Working...")))
    (niwa-view--render-request-progress)
    (when (niwa-view--request-progress-active-p data-buf)
      (niwa-view--start-spinner-timer))))

(defun niwa-view--spinner-dynamic-label-p (text)
  "Return non-nil when TEXT is a generated spinner metadata label."
  (or (string-match-p "\\`[0-9]+s\\'" text)
      (string-match-p "\\`[0-9]+m [0-9][0-9]s\\'" text)
      (string-match-p "\\`[0-9]+h [0-9][0-9]m\\'" text)))

(defun niwa-view--spinner-base-status (status)
  "Return STATUS without generated elapsed-time labels."
  (let* ((status (if (or (null status) (string-empty-p status))
                     "Thinking..."
                   status))
         (parts (string-split status " · " t "[ \t\n]+")))
    (while (and (cdr parts)
                (niwa-view--spinner-dynamic-label-p (car (last parts))))
      (setq parts (butlast parts)))
    (let ((base (string-join parts " · ")))
      (if (or (string-empty-p base)
              (string= base "Thinking..."))
          "Working..."
        base))))

(defun niwa-view--spinner-display-status (status)
  "Return STATUS decorated with elapsed time.
Background work is counted by the panel right above, not here."
  (let* ((base
          (if-let* ((request (niwa-view--spinner-request))
                    ((niwa-request-active-work-pause-started-at request)))
              "Waiting for input"
            (niwa-view--spinner-base-status status)))
         (elapsed (niwa-view--spinner-elapsed-label)))
    (string-join (delq nil (list base elapsed)) " · ")))

(defun niwa-view--format-spinner-line (status &optional face display-status width frame)
  "Return propertized spinner line for STATUS.
FACE defaults to `niwa-view-spinner'.  When WIDTH is non-nil, drop
elapsed metadata before truncating the activity to fit that many columns.
FRAME selects the display palette for the animated label."
  (let* ((face (or face 'niwa-view-spinner))
         (display-status (or display-status
                             (niwa-view--spinner-display-status status)))
         (base (if (string-prefix-p "Waiting for input" display-status)
                   "Waiting for input"
                 (niwa-view--spinner-base-status status)))
         (metadata (substring display-status (length base)))
         (base (replace-regexp-in-string "[\n\r\t]+" " " base))
         (frame (or frame (niwa-view--animation-buffer-frame)))
         (seconds (niwa-view--animation-display-seconds))
         (sample (niwa-view-animation-frame
                  niwa-view-spinner-style base seconds face frame)))
    (when width
      (when (> (+ (string-width sample) (string-width metadata)) width)
        (setq metadata ""))
      (when (> (string-width sample) width)
        (setq base (truncate-string-to-width
                    base (max 0 (- width (- (string-width sample)
                                           (string-width base))))
                    nil nil "…")
              sample (niwa-view-animation-frame
                      niwa-view-spinner-style base seconds face frame))))
    (let ((label (propertize base
                            'font-lock-face face
                            'niwa-view-spinner-frame t
                            'display sample
                            'read-only t
                            'keymap niwa-view--display-map
                            'front-sticky '(read-only keymap)
                            'rear-nonsticky '(read-only keymap))))
    (concat label
            (propertize (concat metadata "\n")
                        'face face
                        'niwa-view-spinner-status
                        (niwa-view--spinner-base-status status)
                        'read-only t
                        'keymap niwa-view--display-map
                        'front-sticky '(read-only keymap)
                        'rear-nonsticky '(read-only keymap))))))

(defun niwa-view--click-rail-attention (event)
  "Visit the next pending card from the composer clicked in EVENT."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (niwa-view-jump-to-interaction))

(defvar-keymap niwa-view--rail-attention-map
  ;; A press would first move the draft caret onto the rail.
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'niwa-view--click-rail-attention
  "<mouse-2>" #'niwa-view--click-rail-attention
  "<header-line> <mouse-1>" #'niwa-view--click-rail-attention
  "<header-line> <mouse-2>" #'niwa-view--click-rail-attention)

(defun niwa-view--format-composer-rail (badge attention width status display-status &optional frame)
  "Format a one-line rail within WIDTH columns.
BADGE and actionable ATTENTION take priority over STATUS and DISPLAY-STATUS.
Only activity is animated; FRAME selects its display palette."
  (let* ((badge (truncate-string-to-width
                 (replace-regexp-in-string "[\n\r\t]+" " " badge)
                 (max 1 (- width (if attention (min 12 (/ width 2)) 0)))
                 nil nil "…"))
         (room (- width (string-width badge)))
         (separator (propertize " · " 'face 'niwa-view-spinner))
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
                          'face 'warning
                          'mouse-face 'highlight
                          'keymap niwa-view--rail-attention-map
                          'help-echo (concat attention "; C-c C-j to visit")
                          'niwa-view-zone-activate
                          #'niwa-view-jump-to-interaction)))))
    (when notice
      (setq badge (concat badge separator notice)
            room (- width (string-width badge))))
    (if (and status (>= room 13))
        (concat badge separator
                (niwa-view--format-spinner-line
                 status nil display-status (- room 3) frame))
      (concat badge "\n"))))

(defun niwa-view--spinner-active-p ()
  "Return non-nil when this view buffer has visible spinner work."
  (or niwa-view--pending-tool-calls
      niwa-view-execution--elapsed-targets
      (niwa-view--request-progress-active-p)))

(defun niwa-view--release-animation-targets (&rest targets)
  "Detach the markers of every (START . END) span in TARGETS.
Spans are recaptured on every semantic render, about once a second for
the label; an abandoned marker stays attached to the view until garbage
collection, which niwa defers while busy, and slows every edit there."
  (dolist (target targets)
    (when (consp target)
      (when (markerp (car target)) (set-marker (car target) nil))
      (when (markerp (cdr target)) (set-marker (cdr target) nil)))))

(defun niwa-view--capture-tool-animation-targets (start end)
  "Remember the running tool spans the live render drew in START..END."
  (apply #'niwa-view--release-animation-targets
         niwa-view--spinner-tool-targets)
  (setq niwa-view--spinner-tool-targets nil)
  (let ((pos start))
    (while (and pos (< pos end)
                (setq pos (text-property-any
                           pos end 'niwa-view-inline-spinner-frame t)))
      (let ((span-end (or (next-single-property-change
                           pos 'niwa-view-inline-spinner-frame nil end)
                          end)))
        ;; As for the label: text inserted at a row's start precedes it.
        (push (cons (copy-marker pos t) (copy-marker span-end t))
              niwa-view--spinner-tool-targets)
        (setq pos span-end))))
  (setq niwa-view--spinner-tool-targets
        (nreverse niwa-view--spinner-tool-targets))
  (setq niwa-view--spinner-tool-samples
        (cl-remove-if-not
         (lambda (sample)
           (cl-some
            (lambda (target)
              (equal (car sample)
                     (get-text-property (marker-position (car target))
                                        'niwa-view-tool-animation-id)))
            niwa-view--spinner-tool-targets))
         niwa-view--spinner-tool-samples)))

(defun niwa-view--tool-sample-seconds (start)
  "Return the displayed phase for the pending tool indicator at START.
Newly inserted tool rows begin at phase zero."
  (or (cdr (assoc (get-text-property start 'niwa-view-tool-animation-id)
                  niwa-view--spinner-tool-samples))
      0.0))

(defun niwa-view--record-tool-sample (start seconds)
  "Record the displayed phase SECONDS for the tool indicator at START."
  (let* ((id (get-text-property start 'niwa-view-tool-animation-id))
         (sample (assoc id niwa-view--spinner-tool-samples)))
    (if sample
        (setcdr sample seconds)
      (push (cons id seconds) niwa-view--spinner-tool-samples))))

(defun niwa-view--snapshot-tool-animation-targets ()
  "Return live tool (ID DISPLAY PHASE LABEL) samples before replacing rows."
  (niwa-view--capture-native-animation)
  (when (eq niwa-view--spinner-rendered-tool-style
            niwa-view-tool-spinner-style)
    (delq nil
          (mapcar (lambda (target)
                    (when-let* ((start (marker-position (car target)))
                                ((eq (marker-buffer (car target))
                                     (current-buffer)))
                                (id (get-text-property
                                     start 'niwa-view-tool-animation-id)))
                      (let* ((seconds (niwa-view--tool-sample-seconds start))
                             (label (niwa-view--tool-animation-label target))
                             (display
                              (if (memq target niwa-view--native-animation-targets)
                                  (niwa-view-animation-frame
                                   niwa-view-tool-spinner-style label seconds
                                   (niwa-view--tool-animation-face target)
                                   (niwa-view--animation-target-frame target))
                                (get-text-property start 'display))))
                        (list id display seconds label))))
                  niwa-view--spinner-tool-targets))))

(defun niwa-view--restore-tool-animation-targets (previous)
  "Restore surviving tool DISPLAY and PHASE entries from PREVIOUS."
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t)
        (inhibit-modification-hooks t)
        (buffer-undo-list t))
    (unwind-protect
        (dolist (target niwa-view--spinner-tool-targets)
          (let* ((start (marker-position (car target)))
                 (entry (assoc (get-text-property start 'niwa-view-tool-animation-id)
                               previous)))
            (when entry
              (let* ((label (niwa-view--tool-animation-label target))
                     ;; Stable fragment identity does not imply stable text:
                     ;; overflow counts change even while motion is frozen.
                     (display (if (equal label (nth 3 entry))
                                  (cadr entry)
                                (niwa-view-animation-frame
                                 niwa-view-tool-spinner-style label
                                 (caddr entry) (niwa-view--tool-animation-face target)
                                 (niwa-view--animation-buffer-frame)))))
                (unless (equal-including-properties
                         display (get-text-property start 'display))
                  (niwa-view--put-decorative-display
                   start (marker-position (cdr target)) display)))
              (niwa-view--record-tool-sample start (caddr entry)))))
      (set-buffer-modified-p modified))))

(defun niwa-view--animation-buffer-frame (&optional buffer)
  "Return a visible display frame for BUFFER, or `:multiple'.
BUFFER defaults to the current buffer.
Before a span has been inserted, use portable glyphs when no frame or
more than one distinct frame can display the buffer."
  (let (found)
    (dolist (window (niwa-view--windows 'visibility buffer))
      (when (eq (frame-visible-p (window-frame window)) t)
        (let ((frame (window-frame window)))
          (if (and found (not (eq found frame)))
              (setq found :multiple)
            (unless found (setq found frame))))))
    (or found :multiple)))

(defun niwa-view--animation-span-in-window-p (start end window)
  "Return non-nil if START..END overlaps WINDOW's visible buffer range.
Use the completed redisplay's range without forcing glyph layout.  Partial
rows and horizontally clipped spans deliberately remain eligible: deciding
whether to animate must cost less than the decorative update itself."
  (and (< start end)
       (< start (or (window-end window) (point-min)))
       (> end (window-start window))))

(defun niwa-view--animation-target-visible-p (target property &optional any-value)
  "Return non-nil when TARGET is a visible span marked PROPERTY.
If ANY-VALUE is non-nil, accept any non-nil PROPERTY value, not just t."
  (when-let* ((start (car-safe target))
              (buffer (marker-buffer start))
              (pos (marker-position start))
              (value (with-current-buffer buffer
                       (and (<= (point-min) pos (1- (point-max)))
                            (get-text-property pos property))))
              ((or any-value (eq value t))))
    (cl-some (lambda (window)
               (and (niwa-view--window-attended-p window)
                    (niwa-view--animation-span-in-window-p
                     pos (marker-position (cdr target)) window)))
             (niwa-view--windows 'visibility buffer))))

(defun niwa-view--spinner-metadata-visible-p ()
  "Return non-nil when the request's elapsed suffix is on screen."
  (let ((target niwa-view--spinner-metadata-target))
    (and (niwa-view--animation-target-visible-p target 'niwa-view-spinner-status t)
         (not (equal "" (niwa-view--animation-target-property target 'display))))))

(defun niwa-view--background-metadata-visible-p ()
  "Return non-nil when a running background row is on screen and attended."
  (cl-some (lambda (target)
             (niwa-view--animation-target-visible-p
              target 'niwa-view-background-elapsed t))
           niwa-view-execution--elapsed-targets))

(defun niwa-view--animation-target-frame (target &optional all)
  "Return TARGET's visible frame, or `:multiple' across display frames.
Color display properties cannot use two palettes simultaneously, so the
caller uses a glyph fallback when more than one frame shows the span.
When ALL is non-nil, return the frames actually showing TARGET instead;
glyph styles can check their display support without inspecting unrelated
windows."
  (let ((position (marker-position (car target)))
        (end (marker-position (cdr target)))
        found frames)
    (dolist (window (niwa-view--windows 'visibility (marker-buffer (car target))))
      (when (and (eq (frame-visible-p (window-frame window)) t)
                 (niwa-view--animation-span-in-window-p
                  position end window))
        (let ((frame (window-frame window)))
          (when all (cl-pushnew frame frames))
          (if (and found (not (eq found frame)))
              (setq found :multiple)
            (unless found (setq found frame))))))
    (if all (nreverse frames) found)))

(defun niwa-view--animation-visible-p ()
  "Return non-nil when a progress or pending-tool animation can be seen."
  (or (niwa-view--animation-target-visible-p
       niwa-view--spinner-label-target 'niwa-view-spinner-frame)
      (cl-some (lambda (target)
                 (niwa-view--animation-target-visible-p
                  target 'niwa-view-inline-spinner-frame))
               niwa-view--spinner-tool-targets)))

(defun niwa-view--spinner-framerate ()
  "Return the spinner's effective frame rate under the user's power policy."
  (niwa-view-power-framerate
   niwa-view-spinner-framerate
   niwa-view-spinner-battery-framerate
   niwa-view-spinner-power-policy
   niwa-view-spinner-animate))

(defun niwa-view--spinner-visual-period (style &optional glyph-fallback)
  "Return effective time between visual frames for STYLE, or nil.
GLYPH-FALLBACK means a color STYLE is drawn by its glyph fallback."
  (let ((rate (niwa-view--spinner-framerate)))
    (when (and (> rate 0) (not (eq style 'static)))
      (max (/ 1.0 rate)
           (cond ((and glyph-fallback (memq style '(shimmer breathe bounce)))
                  (niwa-view-animation-period 'braille))
                 ;; Their 8 fps bounded what each stock PGTK frame cost.
                 ((and (memq style '(breathe bounce)) (niwa-present-active-p)) 0)
                 (t (niwa-view-animation-period style)))))))

(defun niwa-view--spinner-next-delay (plan seconds)
  "Return seconds until PLAN's next visible change at animation SECONDS.
PLAN contains (STYLE . PERIOD) entries for eligible indicators and metadata.
Color and glyph tool rows can coexist on different display frames; each
retains its own cadence, including while color shimmer rests."
  (apply #'min
         (mapcar (lambda (entry)
                   (niwa-view-animation-next-delay
                    (car entry) seconds (cdr entry)))
                 plan)))

(defun niwa-view--tool-animation-label (target)
  "Return the text a pending-tool indicator TARGET animates.
Glyph styles draw a prefix of their own; shimmer animates the row's verb
and tool name."
  (if (eq niwa-view-tool-spinner-style 'shimmer)
      (buffer-substring-no-properties (marker-position (car target))
                                      (marker-position (cdr target)))
    ""))

(defun niwa-view--tool-animation-face (target)
  "Return the face a running-tool indicator TARGET animates in.
A span names its own face, so a shimmering tool name keeps the color it
has once settled; spans without one use `niwa-view-ephemeral'."
  (or (get-text-property (marker-position (car target))
                         'niwa-view-tool-animation-face)
      'niwa-view-ephemeral))

(defun niwa-view--animation-wants-power-p (visible)
  "Return non-nil when VISIBLE animation needs automatic power status."
  (and visible niwa-view-spinner-animate
       (eq niwa-view-spinner-power-policy 'auto)
       (or (and (niwa-view--animation-target-visible-p
                 niwa-view--spinner-label-target
                 'niwa-view-spinner-frame)
                (not (eq niwa-view-spinner-style 'static)))
           (and (cl-some (lambda (target)
                           (niwa-view--animation-target-visible-p
                            target 'niwa-view-inline-spinner-frame))
                         niwa-view--spinner-tool-targets)
                (not (eq niwa-view-tool-spinner-style 'static))))))

(defun niwa-view--stop-spinner-timer ()
  "Stop the buffer-local spinner animation timer."
  (when (featurep 'niwa-view-native)
    (niwa-view-native-stop)
    (when-let* ((composer (niwa-view-composer-buffer)))
      (with-current-buffer composer (niwa-view-native-stop))))
  (setq niwa-view--native-animation-targets nil)
  (when (timerp niwa-view--spinner-timer)
    (niwa--ui-timer-cancel niwa-view--spinner-timer))
  (when (timerp niwa-view--spinner-probe-timer)
    (niwa--ui-timer-cancel niwa-view--spinner-probe-timer))
  (setq niwa-view--spinner-timer nil
        niwa-view--spinner-timer-period nil
        niwa-view--spinner-timer-plan nil
        niwa-view--spinner-probe-timer nil)
  (niwa-view-power-unwatch (current-buffer)))

(defun niwa-view--refresh-themed-status (&optional force)
  "Repaint visible indicator spans at their current or frozen phase.
FORCE also checks frozen glyph fallbacks on a visibility rearm, even
without a theme change.  Hidden targets retain the pending refresh;
neither the status row nor the composer is rebuilt."
  (when (or force niwa-view--spinner-theme-stale-p)
    (let ((seconds (niwa-view--animation-display-seconds))
          (frozen (niwa-view--animation-frozen-p))
          (pending nil))
      (when (and niwa-view--spinner-status
                 (not (eq niwa-view-spinner-style 'static)))
        (let ((target niwa-view--spinner-label-target))
          (if (niwa-view--animation-target-visible-p
               target 'niwa-view-spinner-frame)
              (let* ((start (car target))
                     (style niwa-view-spinner-style)
                     (label (niwa-view--rail-animation-label target))
                     (display-frame (niwa-view--animation-target-frame
                                     target (eq style 'dots)))
                     (frame (niwa-view-animation-frame
                             style label seconds 'niwa-view-spinner
                             display-frame)))
                (niwa-view--paint-rail-sample start frame)
                (setq niwa-view--spinner-last-sample-seconds seconds
                      niwa-view--spinner-sample-frame display-frame))
            (setq pending t))))
      (unless (eq niwa-view-tool-spinner-style 'static)
        (dolist (target niwa-view--spinner-tool-targets)
          (if (niwa-view--animation-target-visible-p
               target 'niwa-view-inline-spinner-frame)
              (let* ((start (marker-position (car target)))
                     (end (marker-position (cdr target)))
                     (display-frame (niwa-view--animation-target-frame
                                     target (eq niwa-view-tool-spinner-style
                                                'dots)))
                     (frame (niwa-view-animation-frame
                             niwa-view-tool-spinner-style
                             (niwa-view--tool-animation-label target)
                             (if frozen
                                 (niwa-view--tool-sample-seconds start)
                               seconds)
                             (niwa-view--tool-animation-face target) display-frame)))
                (unless (equal-including-properties
                         frame (get-text-property start 'display))
                  (let ((inhibit-read-only t)
                        (buffer-undo-list t))
                    (with-silent-modifications
                      (niwa-view--put-decorative-display start end frame))))
                (unless frozen
                  (niwa-view--record-tool-sample start seconds)))
            (setq pending t))))
      (setq niwa-view--spinner-theme-stale-p pending))))

(defun niwa-view--recheck-spinner-after-redisplay ()
  "Rearm this view's spinner once the pending redisplay has completed.
Visibility uses the last completed redisplay's window range.  A scroll
hook still sees the old range, and so does a label or tool row inserted
since, so deciding there would freeze a spinner that is about to appear."
  (unless (niwa--ui-timer-pending-p niwa-view--spinner-probe-timer)
    (let ((buffer (current-buffer))
          (timer (timer-create)))
      (timer-set-time timer (time-add nil (seconds-to-time 0.2)))
      (timer-set-function
       timer
       (lambda ()
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (eq timer niwa-view--spinner-probe-timer)
               (setq niwa-view--spinner-probe-timer nil)
               (when (niwa-view--spinner-active-p)
                 (niwa-view--start-spinner-timer t)))))))
      (niwa--ui-timer-activate
       timer nil (niwa-diagnostic-scope niwa--data-buffer))
      (setq niwa-view--spinner-probe-timer timer))))

(defun niwa-view--probe-spinner-visibility ()
  "Recheck an active spinner whose spans are not yet known to be on screen.
Nothing else rearms a view that stays attended and unscrolled, so without
this a new label would stay frozen, elapsed time included.  Each set of
spans is probed once: a span still off screen waits for scrolling or
a window resize."
  (let ((targets (list niwa-view--spinner-label-target
                       niwa-view--spinner-metadata-target
                       niwa-view--spinner-tool-targets
                       niwa-view-execution--elapsed-targets)))
    (when (and (niwa-view--spinner-active-p)
               (cl-some #'car-safe targets)
               (not (equal targets niwa-view--spinner-probed-targets))
               (cl-some #'niwa-view--window-attended-p
                        (niwa-view--windows 'visibility)))
      (setq niwa-view--spinner-probed-targets targets)
      (niwa-view--recheck-spinner-after-redisplay))))

(defun niwa-view--sync-native-animation (main tools)
  "Offload the visible MAIN label and TOOLS when native presentation is ready."
  (let ((rate (niwa-view--spinner-framerate))
        (seconds (niwa-view--animation-seconds))
        (composer (niwa-view-composer-buffer))
        main-spec tool-specs)
    (when (and (> rate 0) (not (niwa-view--animation-frozen-p)))
      (when main
        (setq main-spec
              (list niwa-view--spinner-label-target niwa-view-spinner-style
                    (niwa-view--rail-animation-label niwa-view--spinner-label-target)
                    'niwa-view-spinner
                    (if (memq niwa-view-spinner-style '(shimmer breathe bounce))
                        (/ 1.0 rate) main))))
      (dolist (target tools)
        (when-let* ((period (niwa-view--spinner-visual-period niwa-view-tool-spinner-style)))
          (push (list target niwa-view-tool-spinner-style
                      (niwa-view--tool-animation-label target)
                      (niwa-view--tool-animation-face target) period) tool-specs))))
    (setq niwa-view--native-animation-targets
          (when (or (not noninteractive) (featurep 'niwa-view-native))
            (append (when composer
                      (with-current-buffer composer
                        (niwa-view-native-sync (and main-spec (list main-spec)) seconds
                                                  #'niwa-view--rearm-composer-spinner)))
                    (niwa-view-native-sync tool-specs seconds #'niwa-view--start-spinner-timer))))
    niwa-view--native-animation-targets))

(defun niwa-view--arm-spinner-timer (timer plan)
  "Arm TIMER for PLAN's next visual change or shared metadata tick."
  (let ((metadata-only (equal plan '((metadata . 1.0)))))
    (timer-set-time
     timer (if metadata-only
               (timer-next-integral-multiple-of-time nil 1)
             (time-add nil (seconds-to-time
                            (niwa-view--spinner-next-delay
                             plan (niwa-view--animation-seconds))))))
    (niwa--ui-timer-activate
     timer metadata-only (niwa-diagnostic-scope niwa--data-buffer))))

(defun niwa-view--start-spinner-timer (&optional resumed)
  "Start one view timer at the next needed visual or metadata cadence.
RESUMED means visibility or focus changed, so frozen glyph support can be
rechecked without changing the displayed animation phase."
  (niwa-view--capture-native-animation)
  ;; Keep every registered label pinned, including the overflow row and
  ;; main label.  The row cap bounds this working set; a fixed four-bank
  ;; cache predates distinct tool labels and evicted still-visible rows.
  (niwa-view-animation-reserve
   (1+ (length niwa-view--spinner-tool-targets)))
  ;; A shared display property cannot carry separate palettes for two frames.
  ;; Repaint on a frame move even when this view has no decorative or elapsed
  ;; timer to notice it; defer the repaint while the label is hidden.
  (when (and niwa-view--spinner-status
             (memq niwa-view-spinner-style
                   '(shimmer breathe bounce braille dots)))
    (let* ((target niwa-view--spinner-label-target)
           (moving (niwa-view--animation-target-visible-p
                    target 'niwa-view-spinner-frame))
           (frame (and moving
                       (niwa-view--animation-target-frame
                        target (eq niwa-view-spinner-style 'dots)))))
      (when (and moving
                 (not (equal niwa-view--spinner-sample-frame frame)))
        (setq niwa-view--spinner-theme-stale-p t))))
  (let ((frozen-glyphs
         (and resumed
              (niwa-view--animation-frozen-p)
              (or (memq niwa-view-spinner-style '(braille dots))
                  (and niwa-view--spinner-tool-targets
                       (memq niwa-view-tool-spinner-style
                             '(braille dots)))))))
    (when (and frozen-glyphs
               (or (eq niwa-view-spinner-style 'dots)
                   (eq niwa-view-tool-spinner-style 'dots)))
      (niwa-view-animation-reset-glyph-support))
    ;; A hidden color label can remain stale while tools keep animating.  A
    ;; theme event or visibility rearm probes its rows once, not on each tool
    ;; frame; both enter this scheduler with RESUMED non-nil.
    (let ((tool-colors (and resumed niwa-view--spinner-tool-targets
                            (eq niwa-view-tool-spinner-style 'shimmer))))
      (when (or (and resumed niwa-view--spinner-theme-stale-p)
                frozen-glyphs tool-colors)
        (niwa-view--refresh-themed-status (or frozen-glyphs tool-colors)))))
  (when (and (niwa-view--spinner-active-p)
             (not niwa-view--spinner-phase-start))
    (setq niwa-view--spinner-phase-start (float-time)))
  ;; Latch on a zero-fps transition even when the next redraw is delayed.
  ;; An ordinary rearm must not record the clock phase as a displayed sample.
  (if (niwa-view--animation-frozen-p)
      (progn
        (niwa-view--animation-display-seconds)
        ;; Move spans drawn before the freeze to their resting phase.
        (niwa-view--refresh-themed-status))
    (setq niwa-view--spinner-frozen-seconds nil))
  (let* ((paused (and-let* ((request (niwa-view--spinner-request)))
                    (niwa-request-active-work-pause-started-at request)))
         ;; Waiting for the user is not work: hold every indicator still,
         ;; as the elapsed time already is, instead of animating for as long
         ;; as a prompt stays unanswered.
         (visible (and (not paused) (niwa-view--animation-visible-p)))
         (main-visible (and visible
                            (niwa-view--animation-target-visible-p
                             niwa-view--spinner-label-target
                             'niwa-view-spinner-frame)))
         (main-color (and main-visible
                          (memq niwa-view-spinner-style
                                '(shimmer breathe bounce))
                          (let* ((target niwa-view--spinner-label-target)
                                 (label (niwa-view--rail-animation-label target)))
                            (niwa-view-animation-color-available-p
                             niwa-view-spinner-style label
                             'niwa-view-spinner
                             (niwa-view--animation-target-frame target)))))
         (main (progn
                 (setq niwa-view--spinner-main-color-p main-color)
                 (when main-visible
                   (niwa-view--spinner-visual-period
                    niwa-view-spinner-style (not main-color)))))
         (tool-targets (and visible
                            (cl-remove-if-not
                             (lambda (target)
                               (niwa-view--animation-target-visible-p
                                target 'niwa-view-inline-spinner-frame))
                             niwa-view--spinner-tool-targets)))
         (native (niwa-view--sync-native-animation main tool-targets))
         ;; Prepare every visible label before frame callbacks.  Some spans
         ;; can use color while others need the portable multi-frame glyph.
         (tool-kinds
          (let (kinds)
            (dolist (target (cl-set-difference tool-targets native :test #'eq))
              (cl-pushnew
               (if (and (eq niwa-view-tool-spinner-style 'shimmer)
                        (niwa-view-animation-color-available-p
                         'shimmer (niwa-view--tool-animation-label target)
                         (niwa-view--tool-animation-face target)
                         (niwa-view--animation-target-frame target)))
                   'shimmer 'glyph)
               kinds))
            kinds))
         (plan
          (delq nil
                (append
                 (list (and main (not (memq niwa-view--spinner-label-target native))
                            (cons (if main-color niwa-view-spinner-style
                                    'glyph) main)))
                 (mapcar
                  (lambda (kind)
                    (when-let* ((period (niwa-view--spinner-visual-period
                                        niwa-view-tool-spinner-style
                                        (eq kind 'glyph))))
                      (cons kind period)))
                  (sort tool-kinds #'string-lessp))
                 (list (and (or (and niwa-view--spinner-status
                                     (not paused)
                                     (niwa-view--spinner-metadata-visible-p))
                                (niwa-view--background-metadata-visible-p))
                            '(metadata . 1.0))))))
         (period (and plan (apply #'min (mapcar #'cdr plan)))))
    (unless period
      (niwa-view--probe-spinner-visibility))
    (when (niwa-view--animation-wants-power-p visible)
      (niwa-view-power-watch (current-buffer)
                                #'niwa-view--start-spinner-timer))
    (unless (niwa-view--animation-wants-power-p visible)
      (niwa-view-power-unwatch (current-buffer)))
    (unless (and plan
                 (equal plan niwa-view--spinner-timer-plan)
                 (or (niwa--ui-timer-pending-p niwa-view--spinner-timer)
                     (and niwa-view--spinner-timer
                          (eq niwa-view--spinner-timer
                              niwa-view--spinner-delivering))))
      (when (timerp niwa-view--spinner-timer)
        (niwa--ui-timer-cancel niwa-view--spinner-timer))
      (setq niwa-view--spinner-timer nil
            niwa-view--spinner-timer-period period
            niwa-view--spinner-timer-plan plan)
      (when period
        (let ((buffer (current-buffer)) timer)
          (setq timer (timer-create))
          (timer-set-function
           timer
           (lambda ()
             (if (not (buffer-live-p buffer))
                 (niwa--ui-timer-cancel timer)
               (with-current-buffer buffer
                 (if (and (eq timer niwa-view--spinner-timer)
                          (niwa-view--spinner-active-p)
                          (or (niwa-view--animation-visible-p)
                              (niwa-view--spinner-metadata-visible-p)
                              (niwa-view--background-metadata-visible-p)))
                     (condition-case nil
                         (progn
                           (let ((niwa-view--spinner-delivering timer))
                             (niwa-view--spinner-tick))
                           ;; A one-shot timer cannot replay deadlines missed
                           ;; during a stall.  Reuse its object after each
                           ;; delivered tick to avoid frame-rate allocation.
                           (when (eq timer niwa-view--spinner-timer)
                             (niwa-view--arm-spinner-timer timer plan)))
                       (error (niwa-view--stop-spinner-timer)))
                   (niwa--ui-timer-cancel timer)
                   (when (eq timer niwa-view--spinner-timer)
                     (niwa-view--stop-spinner-timer)
                     (when (and (derived-mode-p 'niwa-view-mode)
                                (niwa-view--spinner-active-p))
                       (niwa-view--start-spinner-timer))))))))
          (niwa-view--arm-spinner-timer timer plan)
          (setq niwa-view--spinner-timer timer))))))

(defun niwa-view--refresh-animation-options ()
  "Apply changed animation settings to a live view without restarting work."
  (when (derived-mode-p 'niwa-view-mode)
    (niwa-view--animation-display-seconds)
    (setq niwa-view--spinner-rendered-state nil)
    (when (niwa-view--spinner-active-p)
      (niwa-view--render-request-progress)
      ;; A policy/rate change does not change an existing tool row's shape.
      ;; Keep its last displayed glyph, including target-frame fallbacks.
      ;; A new style changes the running marker, so redraw the live turn.
      (when (and niwa-view--pending-tool-calls
                 (not (eq niwa-view--spinner-rendered-tool-style
                          niwa-view-tool-spinner-style))
                 (boundp 'niwa--data-buffer)
                 (buffer-live-p niwa--data-buffer))
        (niwa-view--schedule-tool-boundary-render niwa--data-buffer))
      (niwa-view--start-spinner-timer))))

(defun niwa-view--refresh-animation-on-theme (&rest _)
  "Repaint active color labels after the theme has invalidated frame banks.
The work is event-driven, not part of a decorative frame callback.  A
hidden label retains its pending refresh until visibility rearms its view."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (derived-mode-p 'niwa-view-mode)
                   (or (and niwa-view--spinner-status
                            (not (eq niwa-view-spinner-style 'static)))
                       (and niwa-view--spinner-tool-targets
                            (not (eq niwa-view-tool-spinner-style 'static)))))
          (setq niwa-view--spinner-theme-stale-p t)
          (condition-case nil
              (niwa-view--start-spinner-timer t)
            (error nil)))))))

;; Animation cache invalidation is installed first by the animation module.
;; Prepare the new visible sample only after that cache has been discarded.
(add-hook 'enable-theme-functions
          #'niwa-view--refresh-animation-on-theme t)
(add-hook 'disable-theme-functions
          #'niwa-view--refresh-animation-on-theme t)

(defun niwa-view--spinner-inherits-face-p (face frame)
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
          (includes 'niwa-view-spinner)
        (error nil)))))

(defun niwa-view--refresh-animation-on-face (face _frame &rest attributes)
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
                           (niwa-view--spinner-inherits-face-p face frame))
                         (frame-list)))
        (niwa-view-animation-invalidate)
        (niwa-view--refresh-animation-on-theme)))))

(defun niwa-view--spinner-tick ()
  "Update display spans; reconcile semantic metadata at most once a second."
  ;; A queued release carries a raw position from before any text rewrite.
  ;; Keep its second unconsumed, then catch up through the bounded mouse hold.
  (if (and (not niwa-view-render--owner)
           (niwa-view-render--mouse-held-p))
      (niwa-view-render-mutate 'elapsed-tick #'niwa-view--spinner-tick)
    (let ((second (floor (float-time))))
      (unless (eql second niwa-view--spinner-last-second)
        (setq niwa-view--spinner-last-second second)
        (niwa-view-animation-check-colors)
        (when (niwa-view--background-metadata-visible-p)
          (niwa-view-execution-tick))
        (when (and niwa-view--spinner-status
                   (or (niwa-view--animation-target-visible-p
                        niwa-view--spinner-label-target
                        'niwa-view-spinner-frame)
                       (niwa-view--spinner-metadata-visible-p)))
          (niwa-view--render-request-progress))
        ;; Theme/face changes can turn a color bank into a glyph fallback (or
        ;; back).  Re-evaluate the actual cadence only on this semantic tick.
        (niwa-view--start-spinner-timer))))
  (let ((seconds (niwa-view--animation-seconds))
        (moving (not (niwa-view--animation-frozen-p)))
        (was-modified (buffer-modified-p))
        (inhibit-read-only t)
        (inhibit-modification-hooks t)
        (buffer-undo-list t))
    (unwind-protect
        (progn
          (when (and moving
                     (not (memq niwa-view--spinner-label-target
                                niwa-view--native-animation-targets))
                     ;; Tool rows can wake the shared timer sooner than the
                     ;; label's own period.
                     (when-let* ((period (niwa-view--spinner-visual-period
                                          niwa-view-spinner-style
                                          (not niwa-view--spinner-main-color-p))))
                       (let ((last niwa-view--spinner-last-sample-seconds))
                         (not (and last (< 0 (- seconds last) (* 0.9 period))))))
                     (niwa-view--animation-target-visible-p
                      niwa-view--spinner-label-target
                      'niwa-view-spinner-frame))
            (let* ((target niwa-view--spinner-label-target)
                   (start (car target))
                   (label (niwa-view--rail-animation-label target))
                   (style niwa-view-spinner-style)
                   (display-frame (niwa-view--animation-target-frame
                                   target (eq style 'dots))))
              (when (or (not (memq style '(shimmer breathe bounce)))
                        (niwa-view-animation-color-ready-p
                         style label 'niwa-view-spinner display-frame))
                (let ((frame (niwa-view-animation-frame
                              style label seconds 'niwa-view-spinner
                              display-frame)))
                  (setq niwa-view--spinner-last-sample-seconds seconds)
                  (setq niwa-view--spinner-sample-frame display-frame)
                  (niwa-view--paint-rail-sample start frame)))))
          (when (and moving (niwa-view--spinner-visual-period
                             niwa-view-tool-spinner-style))
            (dolist (target niwa-view--spinner-tool-targets)
              (when (and (not (memq target niwa-view--native-animation-targets))
                         (niwa-view--animation-target-visible-p
                          target 'niwa-view-inline-spinner-frame))
                (let* ((start (marker-position (car target)))
                       (style niwa-view-tool-spinner-style)
                       (label (niwa-view--tool-animation-label target))
                       (display-frame
                        (niwa-view--animation-target-frame
                         target (eq style 'dots))))
                  ;; Like the label: a color bank is prepared at semantic
                  ;; cadence, never inside a frame callback.
                  (when (or (not (eq style 'shimmer))
                            (niwa-view-animation-color-ready-p
                             style label (niwa-view--tool-animation-face target) display-frame))
                    (let ((frame (niwa-view-animation-frame
                                  style label seconds
                                  (niwa-view--tool-animation-face target) display-frame)))
                      (niwa-view--record-tool-sample start seconds)
                      (unless (equal-including-properties
                               frame (get-text-property start 'display))
                        (niwa-view--put-decorative-display
                         start (marker-position (cdr target)) frame)))))))))
      ;; Decorative frames do not invalidate the transcript's chrome.
      (restore-buffer-modified-p was-modified))))

(defun niwa-view--start-spinner (&optional status)
  "Show request progress with STATUS text in the view buffer.
STATUS defaults to \"Thinking...\"."
  (niwa-view--debug-log
   'spinner-start
   :status status
   :state (niwa-view--debug-state niwa--data-buffer))
  (when niwa-view--request-progress-suppressed
    (setq niwa-view--spinner-start-time nil))
  (setq niwa-view--request-progress-suppressed nil)
  (unless niwa-view--spinner-start-time
    (setq niwa-view--spinner-start-time (current-time)))
  (unless niwa-view--spinner-phase-start
    (setq niwa-view--spinner-phase-start (float-time)))
  (cl-incf niwa-view--spinner-generation)
  (setq niwa-view--spinner-status (or status "Thinking...")
        niwa-view--spinner-owner 'request)
  (niwa-view--render-request-progress)
  (niwa-view--start-spinner-timer))

(defun niwa-view--update-spinner (status &optional owner)
  "Update request progress to show STATUS text owned by OWNER."
  (niwa-view--debug-log
   'spinner-update
   :status status
   :state (niwa-view--debug-state niwa--data-buffer))
  (when-let* ((session
               (and (buffer-live-p niwa--data-buffer)
                    (buffer-local-value 'niwa--session
                                        niwa--data-buffer))))
    (niwa-telemetry-record
     session 'status-transition
     :previous-owner niwa-view--spinner-owner
     :previous-status niwa-view--spinner-status
     :owner (or owner 'request)
     :status status))
  (setq niwa-view--request-progress-suppressed nil)
  (unless niwa-view--spinner-start-time
    (setq niwa-view--spinner-start-time (current-time)))
  (unless niwa-view--spinner-phase-start
    (setq niwa-view--spinner-phase-start (float-time)))
  (cl-incf niwa-view--spinner-generation)
  (setq niwa-view--spinner-status status
        niwa-view--spinner-owner
        (or owner 'request))
  (niwa-view--render-request-progress)
  (niwa-view--start-spinner-timer))

(defun niwa-view--spinner-status-snapshot ()
  "Return a snapshot of the current request-progress status."
  (when niwa-view--spinner-status
    (list :generation niwa-view--spinner-generation
          :status niwa-view--spinner-status
          :owner niwa-view--spinner-owner)))

(defun niwa-view--claim-spinner-status (snapshot status owner)
  "Replace SNAPSHOT with STATUS owned by OWNER when it is still current.
Return non-nil when the status was acquired."
  (when (and snapshot owner
             (= (plist-get snapshot :generation)
                niwa-view--spinner-generation))
    (niwa-view--update-spinner status owner)
    t))

(defun niwa-view--restore-spinner-status (owner snapshot)
  "Restore SNAPSHOT when OWNER still owns the request-progress status.
Return non-nil when the status was restored."
  (when (and snapshot owner (eq niwa-view--spinner-owner owner))
    (niwa-view--update-spinner
     (plist-get snapshot :status)
     (or (plist-get snapshot :owner) 'request))
    t))

(defun niwa-view--stop-spinner ()
  "Return the permanent rail to idle and release request animation."
  (niwa-view--debug-log
   'spinner-stop-delete
   :state (niwa-view--debug-state niwa--data-buffer))
  (when-let* ((session
               (and (buffer-live-p niwa--data-buffer)
                    (buffer-local-value 'niwa--session
                                        niwa--data-buffer))))
    (niwa-telemetry-record
     session 'status-transition
     :previous-owner niwa-view--spinner-owner
     :previous-status niwa-view--spinner-status
     :owner nil :status nil))
  (cl-incf niwa-view--spinner-generation)
  (setq niwa-view--spinner-status nil
        niwa-view--spinner-owner nil
        niwa-view--spinner-phase-start nil
        niwa-view--spinner-frozen-seconds nil
        niwa-view--spinner-last-sample-seconds nil
        niwa-view--spinner-sample-frame nil
        niwa-view--spinner-theme-stale-p nil)
  (unless niwa-view--pending-tool-calls
    (unless (and (boundp 'niwa--data-buffer)
                 niwa--data-buffer
                 (buffer-live-p niwa--data-buffer)
                 (buffer-local-value 'niwa--current-request
                                     niwa--data-buffer))
      (setq niwa-view--spinner-start-time nil))
    (if niwa-view-execution--elapsed-targets
        (niwa-view--start-spinner-timer)
      (niwa-view--stop-spinner-timer)))
  (niwa-view--render-request-progress))

(defun niwa-view--stop-request-progress ()
  "Stop and suppress the current request progress row."
  (setq niwa-view--request-progress-suppressed t)
  (niwa-view--stop-spinner)
  (setq niwa-view--spinner-start-time nil))

;;;; Request-progress verbs for modules outside the view package
;;
;; Callers resolve their own VIEW buffer and pass it; every verb does nothing
;; unless VIEW is live, so no rendering or telemetry happens without a view.
;; `show' updates the label and un-suppresses the row; `ensure' only redraws
;; and keeps the current label.  `end' stops *and suppresses* the row, so it
;; stays hidden until a later `show' or request start; `release' without a
;; snapshot only returns the row to idle (not suppressed).  A claim token is
;; the previous status plus the owner; release checks the owner only, never
;; the generation, and never stops progress some other owner now holds.
;; `lease' and `claim-leased' are the stricter pair for a status taken later:
;; only request-owned progress can be leased, and the claim is refused unless
;; the progress is unchanged since the lease; release is the same.

(defun niwa-view-stream-show-progress (view status &optional owner)
  "Show STATUS as request progress in VIEW, owned by OWNER (default `request').
Un-suppresses a hidden row.  Do nothing unless VIEW is live."
  (when (buffer-live-p view)
    (with-current-buffer view
      (niwa-view--update-spinner status owner))))

(defun niwa-view-stream-ensure-progress (view data-buf)
  "Redraw VIEW's progress row for DATA-BUF's request, keeping its label.
Unlike `niwa-view-stream-show-progress' this neither changes the
label nor un-suppresses the row.  Do nothing unless VIEW is live."
  (when (buffer-live-p view)
    (with-current-buffer view
      (niwa-view--ensure-request-progress data-buf))))

(defun niwa-view-stream-end-progress (view)
  "Stop and suppress request progress in VIEW.
Do nothing unless VIEW is live."
  (when (buffer-live-p view)
    (with-current-buffer view
      (niwa-view--stop-request-progress))))

(defun niwa-view-stream-claim-progress (view status owner)
  "Show STATUS owned by OWNER in VIEW and return a token for releasing it.
The token remembers what the claim replaced (possibly nothing).  Return
nil, with no side effects, unless VIEW is live."
  (when (buffer-live-p view)
    (with-current-buffer view
      (let ((snapshot (niwa-view--spinner-status-snapshot)))
        (niwa-view--update-spinner status owner)
        (list :owner owner :snapshot snapshot)))))

(defun niwa-view-stream-release-progress (view token)
  "Release TOKEN's claim in VIEW, which the caller resolves again now.
With a remembered status, restore it; otherwise return to idle.  Either
way only while TOKEN's owner still owns VIEW's progress.  Do nothing
unless VIEW is live and TOKEN is non-nil."
  (when (and token (buffer-live-p view))
    (with-current-buffer view
      (let ((owner (plist-get token :owner))
            (snapshot (plist-get token :snapshot)))
        (if snapshot
            (niwa-view--restore-spinner-status owner snapshot)
          (when (eq niwa-view--spinner-owner owner)
            (niwa-view--stop-spinner)))))))

(defun niwa-view-stream-lease-progress (view)
  "Return a lease on VIEW's request-owned progress, or nil.
Only an active status owned by `request' can be leased, so a lease never
covers another owner's temporary status.  Take it early, then pass it to
`niwa-view-stream-claim-leased-progress'.  Do nothing unless VIEW is
live."
  (when (buffer-live-p view)
    (with-current-buffer view
      (when (ignore-errors (niwa-view--spinner-active-p))
        (let ((snapshot (ignore-errors
                          (niwa-view--spinner-status-snapshot))))
          (when (eq (plist-get snapshot :owner) 'request)
            snapshot))))))

(defun niwa-view-stream-claim-leased-progress (view lease status owner)
  "Show STATUS owned by OWNER in VIEW if LEASE is still current.
Unlike `niwa-view-stream-claim-progress' this refuses, returning nil,
when LEASE is nil or the progress changed since it was leased.  On
success return a token for `niwa-view-stream-release-progress'."
  (when (and lease (buffer-live-p view))
    (with-current-buffer view
      (when (niwa-view--claim-spinner-status lease status owner)
        (list :owner owner :snapshot lease)))))

(defun niwa-view-stream-spinner-hook (info)
  "Update spinner from `gptel-pre-tool-call-functions'.
INFO is a plist with at least :name and :args."
  (when-let* ((view-buf (buffer-local-value 'niwa--view-buffer
                                            (current-buffer)))
              (_ (buffer-live-p view-buf))
              (tool-name (plist-get info :name))
              (args (plist-get info :args)))
    (with-current-buffer view-buf
      ;; In a live turn `niwa-view-stream-pre-tool' draws the call as a
      ;; running row; the progress label would only repeat it.
      (unless (or (niwa-view--routine-pending-poll-p info)
                  (and (niwa-view-stream-in-flight-turn-start-position)
                       (markerp niwa-view--data-turn-start)
                       (marker-position niwa-view--data-turn-start)))
        (let ((summary (niwa-view--tool-status-string tool-name args)))
          (niwa-view--update-spinner summary)))))
  ;; Return nil so the hook does not interfere with tool execution.
  nil)

(defun niwa-view-stream-in-flight-turn-start-position ()
  "Return the current in-flight turn start position, or nil."
  (when (markerp niwa-view--in-flight-turn-start)
    (marker-position niwa-view--in-flight-turn-start)))

(defun niwa-view-stream-set-in-flight-turn-start (position)
  "Set `niwa-view--in-flight-turn-start' to POSITION as a marker.
POSITION may be an integer or marker."
  (setq niwa-view--in-flight-turn-start
        (copy-marker position nil)))

(defun niwa-view-stream--execution-view-buffer (data-buffer)
  "Return the transcript view backed by DATA-BUFFER, or nil."
  (and (buffer-live-p data-buffer)
       (cl-find-if
        (lambda (buffer)
          (and (buffer-live-p buffer)
               (eq data-buffer
                   (buffer-local-value 'niwa--data-buffer buffer))
               (with-current-buffer buffer
                 (derived-mode-p 'niwa-view-mode))))
        (buffer-list))))

(defun niwa-view-stream--cache-execution-progress (event)
  "Cache bounded transient tool progress from EVENT in the current view."
  (when-let* ((tool-use-id (plist-get event :tool-use-id)))
    (unless (hash-table-p niwa-view--execution-events)
      (setq niwa-view--execution-events (make-hash-table :test #'equal)))
    (niwa-view--cache-put
     niwa-view--execution-events tool-use-id
     (list :type 'progress
           :facts (copy-tree (plist-get event :facts))
           :output-preview-truncated-p
           (plist-get event :output-preview-truncated-p)
           :output-tail (plist-get event :output-tail))
     'niwa-view--execution-event-entries)))

(defun niwa-view-stream--remove-execution-progress (tool-use-id)
  "Remove TOOL-USE-ID's transient progress from the current view."
  (when (and tool-use-id
             (hash-table-p niwa-view--execution-events)
             (gethash tool-use-id niwa-view--execution-events))
    (remhash tool-use-id niwa-view--execution-events)
    (setq niwa-view--execution-event-entries
          (max 0 (1- niwa-view--execution-event-entries)))))

(defun niwa-view-stream--schedule-execution-row-recovery (data-buffer tool-use-id)
  "Recover TOOL-USE-ID's missing view row when DATA-BUFFER has its source.
A live call may not have entered the transcript yet.  Its progress stays
cached until source arrival and normal stream/tool-boundary rendering;
repeatedly rebuilding an unchanged transcript cannot materialize that row.
Nested calls recover through their source-owning parent.  A nil TOOL-USE-ID
requests generic recovery, without a particular source identity to check."
  (when (and (buffer-live-p data-buffer)
             (niwa-view-stream-in-flight-turn-start-position)
             (markerp niwa-view--data-turn-start)
             (or (null tool-use-id)
                 (with-current-buffer data-buffer
                   (save-restriction
                     (widen)
                     (let ((owner tool-use-id) source)
                       (while (and owner (not source))
                         (setq source (niwa-tool-render-data-segment-bounds owner)
                               owner (and (string-match "/[0-9]+\\'" owner)
                                          (substring owner 0 (match-beginning 0)))))
                       source)))))
    (niwa-view--schedule-render
     'incremental data-buffer niwa-view-stream-render-delay)))

(defun niwa-view-stream--terminal-breadcrumb (event)
  "Project EVENT's newly persisted breadcrumb into an open current view.
Only the transcript audit supplies its identity and source range.  A view
opened later, or one inspecting a historical segment, uses normal source
projection instead of maintaining a second completion registry.  While
the view renders a live turn, its tail renders the record from source
wherever the transcript placed it, so an activity group can count it."
  (when-let* ((data (plist-get event :data-buffer))
              ((buffer-live-p data))
              (view (niwa-view-stream--execution-view-buffer data)))
    (with-current-buffer view
      (when (eq data (niwa-view-segments-display-buffer))
        (if (and (niwa-view-stream-in-flight-turn-start-position)
                 (markerp niwa-view--data-turn-start))
            (niwa-view--schedule-tool-boundary-render data)
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
                             (when (search-backward niwa--hook-audit-open nil t)
                               (cl-find-if
                                (lambda (item)
                                  (and (equal execution-id
                                              (plist-get item :execution-id))
                                       (equal (plist-get event :tool-use-id)
                                              (plist-get item :tool-use-id))
                                       (equal (plist-get event :owner)
                                              (plist-get item :owner))))
                                (niwa-view--hook-audit-records-from-text
                                 (buffer-substring (point) (point-max))
                                 'execution-breadcrumb data (point)))))))))
            (niwa-view-render-mutate
             (list 'execution-breadcrumb execution-id)
             (lambda ()
               (unless (niwa-view-audit-breadcrumb-present-p
                        record (niwa-view--history-insertion-marker))
                 (let ((at (marker-position
                            (niwa-view--history-insertion-marker))))
                   (niwa-view-call-preserving-user-view-state
                    (lambda ()
                      (niwa-view--call-with-render-boundaries-advancing
                       (lambda ()
                         (let ((inhibit-read-only t)
                               (inhibit-modification-hooks t))
                           (save-excursion
                             (goto-char at)
                             (niwa-view--insert-hook-audit-block
                              record (plist-get record :source)))))))
                    ;; A pure insertion: text on either side is unchanged.
                    (cons at at))))))))))))

(defun niwa-view-stream-handle-execution-event (event)
  "Apply Bash EVENT to its authoritative row and visible view.
Always return nil; only the mailbox sink may acknowledge durable delivery."
  (niwa-execution-transcript-handle-event event)
  ;; The execution owner already bounds the tail and marks its own truncation.
  ;; Do not introduce an undisclosed second, line-based output limit here.
  ;; A Monitor batch changes nothing its row shows: the event count reaches
  ;; the status zone through the execution state hook, and the events reach
  ;; the model as mail.  Refreshing the row would redraw it unchanged.
  (unless (eq (plist-get event :type) 'monitor-batch)
    (niwa-view-stream-handle-tool-progress event))
  (when (eq (plist-get event :type) 'terminal)
    (niwa-view-stream--terminal-breadcrumb event))
  nil)

(defun niwa-view-stream-handle-tool-progress (event)
  "Apply transient tool progress EVENT to its visible aggregate row."
  (let* ((type (plist-get event :type))
         (tool-use-id (plist-get event :tool-use-id))
         (data-buffer (plist-get event :data-buffer))
         (view-buffer
          (niwa-view-stream--execution-view-buffer data-buffer)))
    (when view-buffer
      (with-current-buffer view-buffer
        (pcase type
          ('progress
           (niwa-view-stream--cache-execution-progress event))
          ('terminal
           (niwa-view-stream--remove-execution-progress tool-use-id)))
        (cond
         ;; Keep only the row's identity while unattended.  Its latest
         ;; cached progress is enough to refresh it when focus returns;
         ;; rebuilding unrelated history would turn that return into a stall.
         ((niwa-view--unattended-p)
          (when tool-use-id
            (cl-pushnew tool-use-id niwa-view--pending-tool-rows :test #'equal))
          (niwa-view--schedule-render
           (if tool-use-id 'tools 'full)
           data-buffer niwa-view-rerender-debounce))
         ((and tool-use-id
               (niwa-view--refresh-tool-row data-buffer tool-use-id)))
         (t
          (niwa-view-stream--schedule-execution-row-recovery
           data-buffer tool-use-id))))))
  nil)

(defun niwa-view--render-stream-update (data-buf)
  "Incrementally render DATA-BUF, isolating observer-view failures."
  (niwa-execution-transcript-retry-pending-terminals data-buf)
  (if (not niwa-view--agent-transcript-p)
      (niwa-view-render-live-update data-buf)
    (condition-case err
        (atomic-change-group
          (niwa-view-render-live-update data-buf))
      (error
       (niwa--warn-once
        (niwa-diagnostic-scope data-buf)
        'view-stream-agent-render
        "%s (Live agent transcript render failed)"
        (error-message-string err))))))

(defun niwa-view--schedule-tool-boundary-render (data-buf)
  "Schedule a coalesced incremental render for DATA-BUF."
  (when (and (buffer-live-p data-buf)
             (niwa-view-stream-in-flight-turn-start-position)
             (markerp niwa-view--data-turn-start))
    (niwa-view--schedule-render
     'incremental data-buf niwa-view-tool-boundary-render-delay)))

(defun niwa-view-stream-schedule ()
  "Schedule a debounced incremental render driven by the stream hook.

Intended for `gptel-post-stream-hook', which fires once per streamed
chunk in the data buffer.  Defers the incremental render by
`niwa-view-stream-render-delay' seconds so chunks in the same pending
window share one refresh instead of rebuilding the view per token.  A
batch flushed by `niwa-gptel-stream-bridge' already coalesced its
chunks: it renders through `niwa-gptel-stream-bridge-after-flush',
inside the batch's own timer, instead of waking the editor again, and
shows the text sooner."
  (when-let* ((view-buf (and (boundp 'niwa--view-buffer)
                             niwa--view-buffer))
              ((buffer-live-p view-buf))
              (data-buf (current-buffer)))
    (with-current-buffer view-buf
      (niwa-view--debug-log
       'stream-render-schedule
       :state (niwa-view--debug-state data-buf))
      ;; Only schedule when a turn is in-flight.  Before the first
      ;; user send -- or after the final post-response cleanup -- the
      ;; incremental markers are nil and rendering would no-op.
      (when (and (niwa-view-stream-in-flight-turn-start-position)
                 (markerp niwa-view--data-turn-start))
        (if (bound-and-true-p niwa-gptel-stream-bridge-flushing)
            (niwa-gptel-stream-bridge-after-flush
             (lambda ()
               (when (buffer-live-p view-buf)
                 (with-current-buffer view-buf
                   (niwa-view--schedule-render 'incremental data-buf 0)))))
          (niwa-view--schedule-render
           'incremental data-buf niwa-view-stream-render-delay)))))
  nil)

(defun niwa-view--routine-pending-poll-p (info)
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
                   (let* ((forms (cdr (car (niwa-ptc--read script))))
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

(defun niwa-view--pending-tool-fingerprint (info)
  "Return the pending-tool fingerprint for tool INFO.

gptel builds its tool-hook arguments from the name, the arguments, and
the result, so there is no call id to key on: identical parallel calls
produce the same fingerprint.  Hook and pipeline-event arguments for one
call agree under `niwa-tool-args-key'; compare with `equal'."
  (cons (plist-get info :name)
        (niwa-tool-args-key (plist-get info :args))))

(defun niwa-view--pending-tool-claim-key (info)
  "Return a fresh pending-tool key for tool INFO.

The key pairs INFO's fingerprint with a serial, so two identical
parallel calls hold distinct keys.  The serial matters to rendering: a
live-tail fragment is identified by this key, and a key that shifted
when an earlier call finished would rebuild every row below it."
  (cons (niwa-view--pending-tool-fingerprint info)
        (cl-incf niwa-view--pending-tool-serial)))

(defun niwa-view--pending-tool-entry (info)
  "Return the pending entry matching tool INFO, or nil.
Durable IDs are exact.  ID-less hooks may only claim unbound entries:
an out-of-order completion must not consume a bound identical sibling."
  (let ((id (plist-get info :tool-use-id))
        (fingerprint (niwa-view--pending-tool-fingerprint info)))
    (cl-find-if
     (lambda (entry)
       (let ((bound-id (plist-get (nth 3 entry) :tool-use-id)))
         (if id (equal id bound-id)
           (and (null bound-id) (equal (car (car entry)) fingerprint)))))
     niwa-view--pending-tool-calls)))

(defun niwa-view--pending-tool-finished-entry (info)
  "Return the pending entry an ID-less post-tool hook for INFO completes.
A call the pipeline already reported finished goes first; otherwise an
unbound call with INFO's fingerprint.  A bound sibling still running is
never taken: its completion arrives out of order."
  (let ((fingerprint (niwa-view--pending-tool-fingerprint info)))
    (or (cl-find-if (lambda (entry)
                      (and (plist-get (nth 3 entry) :settled)
                           (equal (car (car entry)) fingerprint)))
                    niwa-view--pending-tool-calls)
        (niwa-view--pending-tool-entry info))))

(defun niwa-view-stream-handle-pipeline-event (event)
  "Bind pending rows and display accepted approval provenance from EVENT.
Only top-level invocations own pending rows.  PTC publishes nested progress
on its canonical child records; it must not decorate the whole envelope."
  (let ((data (plist-get event :data-buffer))
        (id (plist-get event :tool-use-id)))
    (when (and (buffer-live-p data) id
               (not (plist-get event :parent-tool-use-id)))
      (dolist (view (buffer-list))
        (when (eq data (buffer-local-value 'niwa--data-buffer view))
          (with-current-buffer view
            (let ((entry (niwa-view--pending-tool-entry event)) changed)
              (pcase (plist-get event :type)
                ('started
                 (unless (or entry
                             (equal (plist-get event :name) "Agent")
                             (niwa-view--routine-pending-poll-p event))
                   ;; Admission order matches pre-hook order, even when
                   ;; approvals and completions later arrive out of order.
                   (setq entry
                         (niwa-view--pending-tool-entry
                          (list :name (plist-get event :name)
                                :args (plist-get event :args))))
                   (unless entry
                     (setq entry
                           (list (niwa-view--pending-tool-claim-key event)
                                 (plist-get event :name)
                                 (niwa-tool-display-string
                                  (plist-get event :name) (plist-get event :args)
                                  (niwa-diagnostic-scope data))))
                     (setq niwa-view--pending-tool-calls
                           (append niwa-view--pending-tool-calls (list entry))))
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
                         (eq data (niwa-view-segments-display-buffer))
                         (niwa-view-stream-in-flight-turn-start-position)
                         (markerp niwa-view--data-turn-start))
                (niwa-view--schedule-tool-boundary-render data))))))))
  nil)

(add-hook 'niwa-pipeline-tool-event-hook
          #'niwa-view-stream-handle-pipeline-event)

(defun niwa-view-stream-pre-tool (args)
  "Mark an in-flight tool call from ARGS and schedule a view render.

Runs as a `gptel-pre-tool-call-functions' hook in the data buffer.
Adds an entry to `niwa-view--pending-tool-calls' on the
associated view buffer.  gptel runs the hook for every call of a batch
at once, and the batch shares one debounced render that draws its calls
running in the turn's trailing activity run."
  (when-let* ((view-buf (and (boundp 'niwa--view-buffer)
                             niwa--view-buffer))
              ((buffer-live-p view-buf))
              (name (plist-get args :name))
              (data-buf (current-buffer)))
    (with-current-buffer view-buf
      (niwa-view--debug-log
       'pre-tool-hook
       :args (list :id (plist-get args :id)
                   :call-id (plist-get args :call-id)
                   :name name)
       :state (niwa-view--debug-state data-buf))
      (unless (or (equal name "Agent")
                  (niwa-view--routine-pending-poll-p args))
        (let ((key (niwa-view--pending-tool-claim-key args))
              ;; WaitAgent settles as `WaitAgent: agents (...)'.
              (detail (if (equal name "WaitAgent")
                          "agents"
                        (niwa-tool-display-string
                         name (plist-get args :args) (niwa-diagnostic-scope data-buf)))))
          ;; One entry per call, not per key: identical parallel calls
          ;; share a fingerprint, and collapsing them would drop the
          ;; running row while the other call is still running.
          (setq niwa-view--pending-tool-calls
                (append niwa-view--pending-tool-calls
                        (list (list key name detail))))))
      ;; Keep the request-level progress row visible; running tool rows
      ;; do not replace elapsed request progress.
      (niwa-view--ensure-request-progress data-buf)
      (niwa-view--start-spinner-timer)
      (when (and (niwa-view-stream-in-flight-turn-start-position)
                 (markerp niwa-view--data-turn-start))
        (niwa-view--schedule-tool-boundary-render data-buf)
        (niwa-view--debug-log
         'pre-tool-hook-after-schedule
         :state (niwa-view--debug-state data-buf)))))
  ;; gptel pre-tool hooks must return nil unless they intentionally
  ;; provide a control plist.
  nil)

(defun niwa-view-stream-post-tool (args)
  "Clear the in-flight tool marker and schedule a view render.

Runs as a `gptel-post-tool-call-functions' hook in the data buffer.
ARGS is the tool-call plist.  gptel runs the hook for every call of a
finished batch and then writes their results, so the batch's shared
render draws the calls settled; until then their rows keep running."
  (niwa-execution-transcript-retry-pending-terminals (current-buffer))
  (when-let* ((view-buf (and (boundp 'niwa--view-buffer)
                             niwa--view-buffer))
              ((buffer-live-p view-buf))
              (name (plist-get args :name))
              (data-buf (current-buffer)))
    (with-current-buffer view-buf
      (niwa-view--debug-log
       'post-tool-hook
       :args (list :id (plist-get args :id)
                   :call-id (plist-get args :call-id)
                   :name name)
       :state (niwa-view--debug-state data-buf))
      ;; Remove the one call that finished, not every call sharing its
      ;; fingerprint.
      (when-let* ((entry (niwa-view--pending-tool-finished-entry args)))
        (setq niwa-view--pending-tool-calls
              (delq entry niwa-view--pending-tool-calls)))
      (unless (or niwa-view--pending-tool-calls
                  (niwa-view--request-progress-active-p))
        (if niwa-view-execution--elapsed-targets
            (niwa-view--start-spinner-timer)
          (niwa-view--stop-spinner-timer)))
      (when (and (niwa-view-stream-in-flight-turn-start-position)
                 (markerp niwa-view--data-turn-start))
        (niwa-view--schedule-tool-boundary-render data-buf)
        (niwa-view--debug-log
         'post-tool-hook-after-schedule
         :state (niwa-view--debug-state data-buf)))))
  ;; gptel post-tool hooks must return nil unless they intentionally
  ;; provide a control plist.
  nil)

(defun niwa-view--adopt-tool-animation-targets (start end previous)
  "Animate the running tool rows a live render drew in START..END.
PREVIOUS holds the samples displayed before the render replaced the rows."
  (niwa-view--capture-tool-animation-targets start end)
  ;; Newly drawn rows display their initial glyph until a frame tick.  A
  ;; row that survives a redraw retains both its display and its phase.
  (setq niwa-view--spinner-tool-samples nil)
  (niwa-view--restore-tool-animation-targets previous)
  (setq niwa-view--spinner-rendered-tool-style
        niwa-view-tool-spinner-style)
  (niwa-view--start-spinner-timer))

(defun niwa-view--retire-pending-tool-rows ()
  "Remove running tool decorations no render will replace.
A turn that stopped without settling, or a live render with nothing to
redraw, leaves them behind.  Only the in-flight turn can hold them."
  (apply #'niwa-view--release-animation-targets
         niwa-view--spinner-tool-targets)
  (setq niwa-view--spinner-tool-targets nil)
  (when-let* (((markerp niwa-view--status-marker))
              (end (marker-position niwa-view--status-marker)))
    (let ((inhibit-modification-hooks t))
      (niwa-view--retire-pending-decorations
       (min end (or (niwa-view-stream-in-flight-turn-start-position)
                    (point-min)))
       end))))

(defun niwa-view-stream-active-response-marker (info data-buffer)
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

(defun niwa-view-stream-ensure-progress-for-fsm (fsm)
  "Ensure the request progress row for top-level FSM is visible."
  (when-let* ((info (and fsm (fboundp 'gptel-fsm-info)
                         (gptel-fsm-info fsm)))
              (data-buffer (plist-get info :buffer))
              ((buffer-live-p data-buffer))
              ((not (niwa-view--agent-fsm-p info data-buffer)))
              (view-buffer (buffer-local-value 'niwa--view-buffer
                                                data-buffer))
              ((buffer-live-p view-buffer)))
    (with-current-buffer view-buffer
      (unless niwa-view--agent-transcript-p
        (unless (and (markerp niwa-view--data-turn-start)
                     (marker-position niwa-view--data-turn-start))
          (when-let* ((marker (niwa-view-stream-active-response-marker
                               info data-buffer)))
            (setq niwa-view--data-turn-start (copy-marker marker nil))))
        (unless (niwa-view-stream-in-flight-turn-start-position)
          (setq niwa-view--in-flight-turn-start
                (copy-marker (niwa-view--history-insertion-marker) nil)))
        (setq niwa-view--request-progress-suppressed nil)
        (niwa-view--ensure-request-progress data-buffer)))))

(defun niwa-view-stream-begin-turn (view-start data-start &optional no-progress)
  "Begin an active streamed turn at VIEW-START and DATA-START.
When NO-PROGRESS is non-nil, record no active progress state."
  (let ((view-start (unless niwa-view-render--owner view-start))
        (data-start (copy-marker data-start)))
    (niwa-view-render-mutate
     'begin-turn
     (lambda ()
       (niwa-view-stream--stop-now)
       (setq niwa-view-render--terminal-p nil)
       (niwa-view-stream--begin-turn-now
        (or view-start (niwa-view--history-insertion-marker))
        data-start no-progress))
     t)))

(defun niwa-view-stream--begin-turn-now (view-start data-start no-progress)
  "Anchor VIEW-START and DATA-START while holding projection ownership.
NO-PROGRESS suppresses active-turn presentation."
  (when (markerp niwa-view--in-flight-turn-start)
    (set-marker niwa-view--in-flight-turn-start nil))
  (when (markerp niwa-view--data-turn-start)
    (set-marker niwa-view--data-turn-start nil))
  (niwa-view-render-invalidate-live-tail)
  (setq niwa-view--in-flight-turn-start
        (and (not no-progress) (copy-marker view-start nil)))
  (setq niwa-view--data-turn-start
        (and (not no-progress) (copy-marker data-start nil)))
  (unless no-progress
    (niwa-view--start-spinner)))

(defun niwa-view-stream-stop ()
  "Stop active streaming UI and release all turn markers."
  (niwa-view-render-terminal)
  (niwa-view-render-mutate
   'stop #'niwa-view-stream--stop-now nil #'niwa-view-stream--release-turn))

(defun niwa-view-stream--stop-now ()
  "Release active streaming presentation with projection ownership held."
  (unwind-protect
      (progn
        (niwa-view--stop-request-progress)
        (niwa-view--cancel-scheduled-render t)
        (when (and niwa-view--pending-tool-calls
                   (buffer-live-p niwa--data-buffer)
                   (eq niwa--data-buffer (niwa-view-segments-display-buffer))
                   (niwa-view-stream-in-flight-turn-start-position)
                   (markerp niwa-view--data-turn-start))
          ;; Running calls counted toward their group's summary.  An abort
          ;; renders nothing after this, so redraw the live turn from what
          ;; reached the transcript, or a group would keep counting calls
          ;; that never ran.
          (setq niwa-view--pending-tool-calls nil)
          (condition-case err
              (niwa-view--render-live-region niwa--data-buffer nil)
            ;; Stopping must not fail; retiring the rows below still removes
            ;; every running decoration.
            (error
             (niwa--warn-once
              (niwa-diagnostic-scope niwa--data-buffer)
              'view-stream-stop-render
              "%s (Stopped turn render failed)" (error-message-string err)))))
        (niwa-view--retire-pending-tool-rows)
        (setq niwa-view--pending-tool-calls nil))
    (niwa-view-stream--release-turn)))

(defun niwa-view-stream--release-turn ()
  "Release terminal resources even when its projection was superseded."
  (niwa-view-render-invalidate-live-tail)
  ;; A superseding full render must not recreate progress from the old
  ;; spinner timestamp or a request that has not finished settling yet.
  (setq niwa-view--pending-tool-calls nil
        niwa-view--carried-tool-samples nil
        niwa-view--request-progress-suppressed t
        niwa-view--spinner-start-time nil
        niwa-view--spinner-phase-start nil
        niwa-view--spinner-frozen-seconds nil
        niwa-view--spinner-last-sample-seconds nil)
  ;; Retire obsolete streamed work; a pending full render survives.
  (niwa-view--cancel-scheduled-render t)
  (niwa-view--stop-spinner-timer)
  ;; Terminal cleanup can run after a failed or superseded projection, before
  ;; its ordinary spinner stop.  Background metadata must not revive stale
  ;; foreground spans when it rearms the shared timer below.
  (apply #'niwa-view--release-animation-targets
         niwa-view--spinner-label-target niwa-view--spinner-metadata-target
         niwa-view--spinner-tool-targets)
  (setq niwa-view--spinner-status nil
        niwa-view--spinner-owner nil
        niwa-view--spinner-label-target nil
        niwa-view--spinner-metadata-target nil
        niwa-view--spinner-tool-targets nil)
  (when (markerp niwa-view--in-flight-turn-start)
    (set-marker niwa-view--in-flight-turn-start nil))
  (setq niwa-view--in-flight-turn-start nil)
  (when (markerp niwa-view--data-turn-start)
    (set-marker niwa-view--data-turn-start nil))
  (setq niwa-view--data-turn-start nil)
  (when niwa-view-execution--elapsed-targets
    (niwa-view--start-spinner-timer)))

(defun niwa-view-stream-render-response (start end)
  "Finish and render gptel response bounds START and END."
  (let ((data-buf (current-buffer))
        (start (copy-marker start))
        (end (copy-marker end t)))
    (when-let* ((view-buf niwa--view-buffer)
                ((buffer-live-p view-buf)))
      (with-current-buffer view-buf
        (niwa-view-render-terminal)
        (niwa-view-render-mutate
         'terminal
         (lambda ()
           (when (buffer-live-p data-buf)
             (with-current-buffer data-buf
               (let ((niwa--view-buffer view-buf))
                 (niwa-view-stream--render-response-now start end)))))
         nil #'niwa-view-stream--release-turn))))
  nil)

(defun niwa-view-stream--render-response-now (start end)
  "Finish response START..END with its view's projection ownership held."
  (niwa-execution-transcript-retry-pending-terminals (current-buffer))
  (when-let* ((view-buf (buffer-local-value 'niwa--view-buffer
                                            (current-buffer)))
              ((buffer-live-p view-buf)))
    (let ((data-buf (current-buffer)))
      (with-current-buffer view-buf
        (niwa-view--debug-log
         'render-response-begin
         :start start
         :end end
         :state (niwa-view--debug-state data-buf start end))
        (with-current-buffer data-buf
          (setq-local niwa-compact-run-in-flight nil))
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
                  (niwa-view--stop-request-progress)
                  (niwa-view--debug-log
                   'render-response-after-spinner
                   :state (niwa-view--debug-state data-buf start end))
                  (niwa-view--cancel-scheduled-render t)
                  ;; Settling draws only what reached the transcript.
                  (niwa-view--retire-pending-tool-rows)
                  (setq niwa-view--pending-tool-calls nil)
                  (setq end
                        (or (niwa-view--append-request-summary
                             data-buf start)
                            end))
                  (niwa-view-render--settle-now data-buf start end)
                  (niwa-view--debug-log
                   'render-response-after-incremental
                   :state (niwa-view--debug-state data-buf start end)))
              (error
               ;; The fallback rerender is debounced by
               ;; `niwa-view-rerender-debounce', so it normally runs
               ;; after the release below.
               (niwa--warn-once
                (niwa-diagnostic-scope data-buf)
                'view-stream-terminal-render
                "%s (Terminal response render failed)"
                (error-message-string err))
               (niwa-view-rerender view-buf)))
          (niwa-view-stream--release-turn)))))
  nil)

(provide 'niwa-view-stream)
;;; niwa-view-stream.el ends here
