;;; mevedel-view-stream.el --- Streaming view lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Owns active-turn progress state and coalesced streaming/tool-boundary
;; redraws.  The gptel compatibility layer and durable execution transcripts
;; have dedicated owners; transcript interpretation and composer editing remain
;; behind the view module's rendering interface.

;;; Code:

(eval-when-compile (require 'cl-lib))

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

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--tool-segment-bounds
                  "mevedel-pipeline" (tool-use-id))

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

;; `mevedel-tool-exec'
(declare-function mevedel-tool-exec-format-execution-metadata
                  "mevedel-tool-exec" (facts))

;; `mevedel-view'
(declare-function mevedel-view--cancel-scheduled-render "mevedel-view" ())
(declare-function mevedel-view--schedule-render
                  "mevedel-view" (kind data-buffer delay))
(declare-function mevedel-view--tool-status-string "mevedel-view" (tool-name args))
(defvar mevedel-view--display-map)
(defvar mevedel-view-pending-tools-visible-max)
(defvar mevedel-view-spinner-animate)
(defvar mevedel-view-spinner-frames)
(defvar mevedel-view-spinner-interval)

;; `mevedel-view-agent'
(declare-function mevedel-view--agent-status-counts "mevedel-view-agent" ())
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-composer'
(declare-function mevedel-view--agent-fsm-p
                  "mevedel-view-composer" (info data-buffer))
(declare-function mevedel-view--call-preserving-input-point
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
(declare-function mevedel-view--refresh-tool-row
                  "mevedel-view-render" (data-buffer tool-use-id))
(declare-function mevedel-view--render-incremental
                  "mevedel-view-render" (data-buf &optional start end))
(declare-function mevedel-view--request-progress-anchor
                  "mevedel-view-render" ())

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

(defvar-local mevedel-view--spinner-frame-index 0
  "Current frame index for animated view buffer spinners.")

(defvar-local mevedel-view--spinner-rendered-display-status nil
  "Last textual status rendered in the request-progress fragment.")

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
    (require 'mevedel-view-zone)
    (let ((display-status
           (mevedel-view--spinner-display-status
            mevedel-view--spinner-status)))
      (unless (and (equal display-status
                          mevedel-view--spinner-rendered-display-status)
                   (mevedel-view-zone-region 'progress))
        (let ((anchor (mevedel-view--request-progress-anchor)))
          (mevedel-view-zone-reconcile
           'progress anchor anchor
           (mevedel-view--request-progress-fragments
            mevedel-view--spinner-status display-status))
          (setq mevedel-view--spinner-rendered-display-status
                display-status))))))

(defun mevedel-view--clear-request-progress ()
  "Remove the fragment-managed request-progress row."
  (require 'mevedel-view-zone)
  (mevedel-view-zone-clear 'progress)
  (setq mevedel-view--spinner-rendered-display-status nil))

(defun mevedel-view--forget-request-progress-region ()
  "Forget the request-progress region after a larger redraw deleted it."
  (require 'mevedel-view-zone)
  (mevedel-view-zone-forget 'progress)
  (setq mevedel-view--spinner-rendered-display-status nil))

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
  (let* ((frame (mevedel-view--spinner-frame))
         (face (or face 'mevedel-view-spinner))
         (display-status (or display-status
                             (mevedel-view--spinner-display-status status))))
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

(defun mevedel-view--format-spinner-block (status &optional display-status)
  "Return request-progress spinner text for STATUS at point."
  (concat (mevedel-view--request-progress-prefix)
          (mevedel-view--format-spinner-line status nil display-status)))

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
  (when-let* ((region (mevedel-view-zone-region 'history-live)))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (mevedel-view--refresh-spinner-frame-spans
       'mevedel-view-inline-spinner-frame
       (overlay-start region)
       (overlay-end region)
       'mevedel-view-ephemeral))))

(defun mevedel-view--refresh-request-spinner-frame ()
  "Refresh the request-progress spinner frame without rewriting its text."
  (when-let* ((region (mevedel-view-zone-region 'progress)))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (mevedel-view--refresh-spinner-frame-spans
       'mevedel-view-spinner-frame
       (overlay-start region)
       (overlay-end region)
       'mevedel-view-spinner))))

(defun mevedel-view--spinner-tick ()
  "Advance visible spinner frames in the current view buffer."
  (mevedel-view--call-preserving-user-view-state
   (lambda ()
     (setq mevedel-view--spinner-frame-index
           (mod (1+ mevedel-view--spinner-frame-index)
                (max 1 (length mevedel-view-spinner-frames))))
     (mevedel-view--ensure-request-progress)
     (mevedel-view--refresh-request-spinner-frame)
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
           mevedel-view--spinner-owner nil)
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

(defun mevedel-view-stream--cache-execution-progress (event)
  "Cache bounded transient progress from EVENT in the current view."
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
               (mevedel-tool-exec-format-execution-metadata facts)
               (and (stringp tail)
                    (not (string-empty-p tail))
                    (concat "\n" tail))))
      (mevedel-view--refresh-pending-tool-lines))))

(defun mevedel-view-stream--schedule-execution-row-recovery (data-buffer)
  "Schedule one incremental render to recover a missing execution row."
  (when (and (mevedel-view-stream-in-flight-turn-start-position)
             (markerp mevedel-view--data-turn-start))
    (mevedel-view--schedule-render
     'incremental data-buffer mevedel-view-stream-render-delay)))

(defun mevedel-view-stream-handle-execution-event (event)
  "Apply Bash EVENT to its authoritative row and visible view.
Always return nil; only the mailbox sink may acknowledge durable delivery."
  (require 'mevedel-execution-transcript)
  (mevedel-execution-transcript-handle-event event)
  (let* ((type (plist-get event :type))
         (tool-use-id (plist-get event :tool-use-id))
         (data-buffer (plist-get event :data-buffer))
         (view-buffer
          (mevedel-view-stream--execution-view-buffer data-buffer)))
    (when view-buffer
      (with-current-buffer view-buffer
        (pcase type
          ('progress
           (setq event
                 (plist-put
                  (copy-sequence event) :output-tail
                  (string-join
                   (last (string-lines
                          (or (plist-get event :output-tail) "")) 5)
                   "\n")))
           (mevedel-view-stream--cache-execution-progress event)
           (mevedel-view-stream--update-execution-pending-row event))
          ('terminal
           (mevedel-view-stream--remove-execution-progress tool-use-id)))
        (unless (and tool-use-id
                     (mevedel-view--refresh-tool-row data-buffer tool-use-id))
          (mevedel-view-stream--schedule-execution-row-recovery
           data-buffer)))))
  nil)

(defun mevedel-view--render-stream-update (data-buf)
  "Incrementally render DATA-BUF, isolating observer-view failures."
  (require 'mevedel-execution-transcript)
  (mevedel-execution-transcript-retry-pending-terminals data-buf)
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
  (require 'mevedel-execution-transcript)
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
      (let ((key (mevedel-view--pending-tool-key args)))
        (setq mevedel-view--pending-tool-calls
              (assoc-delete-all key mevedel-view--pending-tool-calls)))
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
        (unless (mevedel-view-stream-in-flight-turn-start-position)
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
  (mevedel-view--cancel-scheduled-render)
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
  (require 'mevedel-execution-transcript)
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
        (mevedel-view--stop-request-progress)
        (mevedel-view--debug-log
         'render-response-after-spinner
         :state (mevedel-view--debug-state data-buf start end))
        (mevedel-view--cancel-scheduled-render)
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
