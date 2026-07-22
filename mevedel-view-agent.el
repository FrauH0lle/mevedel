;;; mevedel-view-agent.el --- Agent presentation and transcript views -*- lexical-binding: t -*-

;;; Commentary:

;; Owns agent transcript lookup and inspection, aggregate live-agent status,
;; attribution controls, and targeted agent refreshes in mevedel view buffers.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `cl-seq'
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-active-activity-p
                  "mevedel-agent-control" (activity))
(declare-function mevedel-agent-control-retained-buffer-p
                  "mevedel-agent-control" (session buffer))
(declare-function mevedel-agent-record-activity
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-conversation-buffer
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-id
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-invocation
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-parent-path
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-path
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-role
                  "mevedel-agent-control" (cl-x) t)

;; `mevedel-agent-conversation'
(defvar mevedel--agent-invocation)

;; `mevedel-agent-persistence'
(declare-function mevedel-agent-persistence-transcript-path-p
                  "mevedel-agent-persistence" (path save-path))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-agent "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-call-count "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-description "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-started-at "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-verdict "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--find-file-noselect "mevedel-session-persistence" (file))

;; `mevedel-structs'
(declare-function mevedel-session-agent-transcripts "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-agent-registry "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-queue
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-tool-ui'
(declare-function mevedel-tool-ui--render-agent
                  "mevedel-tool-ui" (name args result render-data))

;; `mevedel-transcript'
(declare-function mevedel-transcript-segments "mevedel-transcript" (start end))

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-properties
                  "mevedel-transcript-restore" (&optional only-if-missing))

;; `mevedel-view'
(declare-function mevedel-view--display-fragment-keymap "mevedel-view" (&rest maps))
(declare-function mevedel-view--ensure "mevedel-view" (data-buf &optional view-name options))
(declare-function mevedel-view--indent-region-lines
                  "mevedel-view" (start end prefix))
(declare-function mevedel-view--render-status "mevedel-view" (&optional data-buf))
(declare-function mevedel-view--status-trailing-newline-suffix "mevedel-view" (body))
(declare-function mevedel-view--zone-separator "mevedel-view" (label))
(declare-function mevedel-view-activate-at-point "mevedel-view" (&optional event))
(declare-function mevedel-view-rerender "mevedel-view" (&optional buffer))
(defvar mevedel-view-mode-map)

;; `mevedel-view-composer'
(declare-function mevedel-view--call-preserving-input-point "mevedel-view-composer" (thunk))
(declare-function mevedel-view--call-preserving-input-text "mevedel-view-composer" (thunk))
(declare-function mevedel-view--call-preserving-window-state "mevedel-view-composer" (thunk))
(declare-function mevedel-view--transcript-gptel-send-blocked
                  "mevedel-view-composer" (&optional arg))
(defvar mevedel-view--input-marker)

;; `mevedel-view-render'
(declare-function mevedel-view--add-display-region-properties
                  "mevedel-view-render" (start end &optional type))
(declare-function mevedel-view--debug-log "mevedel-view-render" (event &rest data))
(declare-function mevedel-view--full-rerender "mevedel-view-render" ())
(declare-function mevedel-view--insert-rendered-tool "mevedel-view-render" (rendering source))
(declare-function mevedel-view--record-source-collapse-state "mevedel-view-render" (source vtype collapsed))
(declare-function mevedel-view--rendering-with-collapse-state "mevedel-view-render" (rendering source))
(declare-function mevedel-view--section-bounds "mevedel-view-render" ())
(declare-function mevedel-view--segment-rendering "mevedel-view-render" (data-buf seg-start seg-end &optional collapsed-only))
(declare-function mevedel-view--source-collapse-state-entry "mevedel-view-render" (source vtype))
(declare-function mevedel-view--tool-call-parse "mevedel-view-render" (data-buf seg-start seg-end &optional raw))
(declare-function mevedel-view-next-display "mevedel-view-render" ())
(declare-function mevedel-view-previous-display "mevedel-view-render" ())
(declare-function mevedel-view-toggle-section "mevedel-view-render" ())
(declare-function mevedel-view-toggle-transcript "mevedel-view-render" ())

;; `mevedel-view-stream'
(declare-function mevedel-view--in-flight-turn-start-position
                  "mevedel-view-stream" ())
(declare-function mevedel-view--set-in-flight-turn-start
                  "mevedel-view-stream" (position))
(declare-function mevedel-view-stream-post-tool
                  "mevedel-view-stream" (args))
(declare-function mevedel-view-stream-pre-tool
                  "mevedel-view-stream" (args))
(declare-function mevedel-view-stream-schedule "mevedel-view-stream" ())
(declare-function mevedel-view-stream-stop "mevedel-view-stream" ())
(defvar mevedel-view--data-turn-start)
(defvar mevedel-view--in-flight-turn-start)

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-collapse-state "mevedel-view-zone" (key &optional default))
(declare-function mevedel-view-zone-collapse-state-set-p "mevedel-view-zone" (key))
(declare-function mevedel-view-zone-set-collapse-state "mevedel-view-zone" (key collapsed))


;;
;;; Customization

(defcustom mevedel-agent-view-display-action
  '(display-buffer-in-side-window
    (side . right) (slot . 0) (window-width . 0.4))
  "Action passed to `pop-to-buffer' when opening an agent transcript.
Consulted by `mevedel-view-open-agent-transcript' so callers do not
need to set `display-buffer-overriding-action'.  Power users can
override globally or via `display-buffer-alist'."
  :type 'sexp
  :group 'mevedel)


(defcustom mevedel-view-agent-refresh-delay 0.05
  "Seconds to coalesce live agent handle refreshes.
Agent tool start/finish hooks can arrive in bursts.  This delay keeps those
bursts from repainting the same handle repeatedly while still updating live
badges promptly."
  :type 'number
  :group 'mevedel)


;;
;;; Presentation state


(defvar-local mevedel-view--agent-transcript-p nil
  "Non-nil when this view buffer renders an agent transcript for inspection.")

(defvar-local mevedel-view--agent-path nil
  "Canonical agent path rendered by an agent transcript inspection view.")

(defvar-local mevedel-view--agent-transcript-info nil
  "Resolved transcript metadata plist for an agent transcript inspection view.")

(defvar-local mevedel-view--agent-transcript-parent-view nil
  "Parent session view buffer that opened this transcript inspection view.")

(defvar-local mevedel-view--agent-transcript-window nil
  "Side window currently displaying an agent transcript for this view buffer.")

(defvar mevedel-view--agent-transcript-data-kill-in-progress nil
  "Non-nil while transcript view/data kill hooks are already paired.")


;;;; Status helpers

(defun mevedel-view-agent--handle-badge (render-data)
  "Return a propertized state badge for RENDER-DATA, or an empty string."
  (let* ((status (plist-get render-data :status))
         (blocked-reason (plist-get render-data :blocked-reason))
         (calls (plist-get render-data :calls))
         (elapsed (plist-get render-data :elapsed))
         (reason (plist-get render-data :reason))
         (verdict (plist-get render-data :verdict))
         (calls-suffix (if (and calls (> calls 0))
                           (format " · %d calls" calls)
                         ""))
         (elapsed-suffix (if (and elapsed (> elapsed 0))
                             (format " · %.1fs" elapsed)
                           "")))
    (if blocked-reason
        (propertize (format "[blocked · awaiting %s]" blocked-reason)
                    'font-lock-face 'mevedel-view-handle-blocked)
      (pcase status
        ('running
         (propertize (format "[running%s]" calls-suffix)
                     'font-lock-face 'mevedel-view-handle-running))
        ('completed
         (pcase verdict
           ('fail
            (propertize (format "✗ verdict FAIL%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-error))
           ('partial
            (propertize (format "○ verdict PARTIAL%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-error))
           ('pass
            (propertize (format "✓ verdict PASS%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-done))
           (_
            (propertize (format "✓ done%s%s" elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-done))))
        ('error
         (propertize (format "✗ error%s"
                             (if reason (format " · %s" reason) ""))
                     'font-lock-face 'mevedel-view-handle-error))
        ('aborted
         (propertize "✗ aborted"
                     'font-lock-face 'mevedel-view-handle-error))
        ('incomplete
         (propertize "○ incomplete"
                     'font-lock-face 'mevedel-view-handle-error))
        (_ "")))))

(defun mevedel-view-agent--blocked-reason (path session)
  "Return the visible blocked reason for canonical PATH in SESSION, or nil."
  (when (and path session)
    (when (cl-some (lambda (entry)
                     (equal (plist-get entry :origin) path))
                   (mevedel-session-permission-queue session))
      "permission")))


;;;; Aggregate state and keymaps

(defconst mevedel-view--status-agent-collapse-key '(status agents)
  "Stable fragment collapse key for the aggregate agent status block.")

(defvar-local mevedel-view--agent-refresh-timers nil
  "Hash table of pending coalesced agent refresh timers by canonical path.")


(defvar-keymap mevedel-view--agent-handle-map
  :doc "Keymap active on non-label text in Agent handles.
The visible agent path carries its own transcript-opening
keymap; the rest of the handle remains navigable without opening the
transcript on click."
  "TAB" #'mevedel-view-toggle-section
  "n" #'mevedel-view-next-display
  "p" #'mevedel-view-previous-display
  "t" #'mevedel-view-toggle-transcript
  "q" #'mevedel-view-close-agent-transcript)

(defvar-keymap mevedel-view--agent-label-map
  :doc "Keymap active on the visible path in Agent handles."
  "RET" #'mevedel-view-activate-at-point
  "<mouse-1>" #'mevedel-view-activate-at-point
  "<mouse-2>" #'mevedel-view-activate-at-point)


(defun mevedel-view-agent-initialize (options data-buffer)
  "Initialize agent presentation state for DATA-BUFFER from OPTIONS."
  (setq-local mevedel-view--agent-transcript-p
              (and (plist-get options :agent-transcript-p) t))
  (setq-local mevedel-view--agent-path (plist-get options :agent-path))
  (setq-local mevedel-view--agent-transcript-info
              (plist-get options :transcript-info))
  (setq-local mevedel-view--agent-transcript-parent-view
              (plist-get options :parent-view))
  (setq-local mevedel-view--agent-refresh-timers
              (make-hash-table :test #'equal))
  (when mevedel-view--agent-transcript-p
    (use-local-map (copy-keymap mevedel-view-mode-map))
    (local-set-key (kbd "q") #'mevedel-view-close-agent-transcript)
    (local-set-key [remap gptel-send]
                   #'mevedel-view--transcript-gptel-send-blocked)
    (setq header-line-format
          '(:eval (mevedel-view--agent-transcript-header-line)))
    (when (plist-get options :preserve-data-view-buffer)
      (with-current-buffer data-buffer
        (add-hook 'kill-buffer-hook
                  #'mevedel-view--on-agent-transcript-data-killed nil t)))))


;;
;;; Transcript inspection

(defun mevedel-view--agent-terminal-status-p (status)
  "Return non-nil when STATUS names a terminal agent transcript state."
  (memq status '(completed error aborted incomplete)))

(defun mevedel-view--agent-effective-status (inv entry)
  "Return the effective visible status from live INV and sidecar ENTRY.
Terminal live invocation state wins over stale sidecar metadata.  If
there is no terminal live state, terminal sidecar metadata wins over a
non-terminal live state so a finalized transcript does not regress to
running in the UI."
  (let ((inv-status (and inv
                         (mevedel-agent-invocation-transcript-status inv)))
        (entry-status (and entry (plist-get entry :status))))
    (cond
     ((mevedel-view--agent-terminal-status-p inv-status) inv-status)
     ((mevedel-view--agent-terminal-status-p entry-status) entry-status)
     (inv-status inv-status)
     (t entry-status))))

(defun mevedel-view--agent-transcript-header-line ()
  "Return the header-line string for a transcript inspection view."
  (let* ((info (mevedel-view--agent-transcript-current-info))
         (agent-path (or mevedel-view--agent-path
                         (plist-get info :agent-path)))
         (badge (mevedel-view-agent--handle-badge
                 (list :status (plist-get info :status)
                       :calls (plist-get info :calls)
                       :elapsed (plist-get info :elapsed)
                       :reason (plist-get info :reason)
                       :blocked-reason
                       (plist-get info :blocked-reason))))
         (calls (plist-get info :calls))
         (elapsed (plist-get info :elapsed))
         (session-label
          (or (plist-get info :session-label)
              (when-let* ((session (plist-get info :session)))
                (or (mevedel-session-session-id session)
                    (mevedel-session-name session)))
              (when-let* ((parent mevedel-view--agent-transcript-parent-view)
                          ((buffer-live-p parent))
                          (parent-data
                           (buffer-local-value 'mevedel--data-buffer parent))
                          ((buffer-live-p parent-data))
                          (session (buffer-local-value 'mevedel--session
                                                       parent-data)))
                (or (mevedel-session-session-id session)
                    (mevedel-session-name session)))
              "unknown")))
    (string-join
     (delq nil
           (list (format "Agent %s" agent-path)
                 (unless (string-empty-p badge) badge)
                 (when (integerp calls) (format "%d calls" calls))
                 (when (numberp elapsed) (format "%.1fs" elapsed))
                 (format "session %s" session-label)))
     "  ")))

(defun mevedel-view--agent-transcript-current-info ()
  "Return transcript metadata refreshed from the live invocation."
  (let* ((info (copy-sequence mevedel-view--agent-transcript-info))
         (data-buf mevedel--data-buffer)
         (inv (and (buffer-live-p data-buf)
                   (buffer-local-value 'mevedel--agent-invocation data-buf))))
    (when (and (plist-get info :live-buffer)
               (mevedel-agent-invocation-p inv))
      (let* ((started (mevedel-agent-invocation-started-at inv))
             (session (and (buffer-live-p data-buf)
                           (buffer-local-value 'mevedel--session data-buf)))
             (blocked-reason
              (mevedel-view-agent--blocked-reason
               mevedel-view--agent-path session)))
        (setq info
              (append
               (list :status
                     (mevedel-agent-invocation-transcript-status inv)
                     :calls (mevedel-agent-invocation-call-count inv)
                     :reason (mevedel-agent-invocation-terminal-reason inv)
                     :blocked-reason blocked-reason)
               (when started
                 (list :elapsed
                       (float-time
                        (time-subtract (current-time) started))))
               info))))
    info))

(defun mevedel-view--clear-parent-transcript-window ()
  "Clear this transcript view from its parent view's singleton slot."
  (when-let* ((parent mevedel-view--agent-transcript-parent-view)
              ((buffer-live-p parent)))
    (with-current-buffer parent
      (setq mevedel-view--agent-transcript-window nil))))

(defun mevedel-view--retained-agent-data-p (data-buffer transcript-info)
  "Return non-nil when TRANSCRIPT-INFO's session retains DATA-BUFFER."
  (and (fboundp 'mevedel-agent-control-retained-buffer-p)
       (mevedel-agent-control-retained-buffer-p
        (plist-get transcript-info :session) data-buffer)))

(defun mevedel-view-agent-handle-view-kill ()
  "Clean up the current transcript view and return non-nil when handled."
  (when (bound-and-true-p mevedel-view--agent-transcript-p)
    (let ((data-buffer mevedel--data-buffer)
          (view-buffer (current-buffer))
          (live-p (plist-get mevedel-view--agent-transcript-info
                             :live-buffer)))
      (mevedel-view--clear-parent-transcript-window)
      (when (buffer-live-p data-buffer)
        (with-current-buffer data-buffer
          (when (eq mevedel--view-buffer view-buffer)
            (setq mevedel--view-buffer nil)))
        (unless (or live-p
                    (mevedel-view--retained-agent-data-p
                     data-buffer mevedel-view--agent-transcript-info)
                    mevedel-view--agent-transcript-data-kill-in-progress)
          (let ((mevedel-view--agent-transcript-data-kill-in-progress t))
            (kill-buffer data-buffer)))))
    t))

(defun mevedel-view--agent-transcript-view-p (buffer)
  "Return non-nil when BUFFER is an agent transcript inspection view."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (bound-and-true-p mevedel-view--agent-transcript-p))))

(defun mevedel-view--agent-transcript-views-for-parent (parent-view)
  "Return transcript inspection views opened from PARENT-VIEW."
  (let (views)
    (dolist (buf (buffer-list) (nreverse views))
      (when (and (mevedel-view--agent-transcript-view-p buf)
                 (with-current-buffer buf
                   (eq mevedel-view--agent-transcript-parent-view
                       parent-view)))
        (push buf views)))))

(defun mevedel-view--agent-transcript-views-for-data (data-buffer)
  "Return transcript inspection views rendering DATA-BUFFER."
  (let (views)
    (dolist (buf (buffer-list) (nreverse views))
      (when (and (mevedel-view--agent-transcript-view-p buf)
                 (with-current-buffer buf
                   (eq mevedel--data-buffer data-buffer)))
        (push buf views)))))

(defun mevedel-view--kill-agent-transcript-view
    (view-buffer &optional kill-retained)
  "Kill transcript inspection VIEW-BUFFER and disposable data.
Also kill retained conversation data when KILL-RETAINED is non-nil."
  (when (mevedel-view--agent-transcript-view-p view-buffer)
    (let (data-buffer live-p transcript-info)
      (with-current-buffer view-buffer
        (setq data-buffer mevedel--data-buffer
              transcript-info mevedel-view--agent-transcript-info
              live-p (plist-get mevedel-view--agent-transcript-info
                                :live-buffer)))
      (dolist (win (get-buffer-window-list view-buffer nil t))
        (ignore-errors
          (quit-window nil win)))
      (when (buffer-live-p view-buffer)
        (kill-buffer view-buffer))
      (when (and (not live-p)
                 data-buffer
                 (buffer-live-p data-buffer)
                 (or kill-retained
                     (not (mevedel-view--retained-agent-data-p
                           data-buffer transcript-info))))
        (let ((mevedel-view--agent-transcript-data-kill-in-progress t))
          (kill-buffer data-buffer))))))

(defun mevedel-view-agent-cleanup-parent (parent-view)
  "Kill every transcript inspection view opened from PARENT-VIEW."
  (dolist (view (mevedel-view--agent-transcript-views-for-parent parent-view))
    (mevedel-view--kill-agent-transcript-view view t)))

(defun mevedel-view--on-agent-transcript-data-killed ()
  "Kill live transcript inspection views when their data buffer dies."
  (unless mevedel-view--agent-transcript-data-kill-in-progress
    (let ((data-buffer (current-buffer))
          (mevedel-view--agent-transcript-data-kill-in-progress t))
      (dolist (view (mevedel-view--agent-transcript-views-for-data data-buffer))
        (when (buffer-live-p view)
          (kill-buffer view))))))

(defun mevedel-view--agent-live-transcript-dispatch (function &rest args)
  "Call stream FUNCTION with ARGS for every view of the current agent buffer."
  (let ((agent-buf (current-buffer)))
    (dolist (view (mevedel-view--agent-transcript-views-for-data agent-buf))
      (condition-case err
          (with-current-buffer view
            (atomic-change-group
              (with-current-buffer agent-buf
                (let ((mevedel--view-buffer view))
                  (apply function args))))
            (force-mode-line-update t))
        (error
         (display-warning
          'mevedel
          (format "Live agent transcript update failed: %s"
                  (error-message-string err))
          :warning)))))
  nil)

(defun mevedel-view-agent-live-transcript-stream ()
  "Fan out a streamed agent chunk to open transcript inspection views."
  (mevedel-view--agent-live-transcript-dispatch
   #'mevedel-view-stream-schedule))

(defun mevedel-view-agent-live-transcript-pre-tool (args)
  "Fan out the agent pre-tool hook ARGS to open transcript views."
  (mevedel-view--agent-live-transcript-dispatch
   #'mevedel-view-stream-pre-tool args))

(defun mevedel-view-agent-live-transcript-post-tool (args)
  "Fan out the agent post-tool hook ARGS to open transcript views."
  (mevedel-view--agent-live-transcript-dispatch
   #'mevedel-view-stream-post-tool args))

(defun mevedel-view--agent-transcript-start-streaming ()
  "Anchor incremental rendering to the last rendered assistant turn."
  (let ((pos (point-min))
        last-assistant)
    (while (< pos (point-max))
      (when (eq (get-text-property pos 'mevedel-view-turn-role) 'assistant)
        (setq last-assistant pos))
      (setq pos (or (next-single-property-change
                     pos 'mevedel-view-turn-role nil (point-max))
                    (point-max))))
    (when last-assistant
      (let ((turn-id (get-text-property last-assistant
                                        'mevedel-view-turn-id))
            (source-pos last-assistant)
            source)
        (while (and (< source-pos (point-max))
                    (eq (get-text-property source-pos
                                           'mevedel-view-turn-id)
                        turn-id)
                    (not source))
          (setq source (get-text-property source-pos 'mevedel-view-source))
          (setq source-pos (1+ source-pos)))
        (when (and (consp source) (integerp (car source)))
          (mevedel-view--set-in-flight-turn-start last-assistant)
          (setq mevedel-view--data-turn-start
                (copy-marker (car source) nil)))))
    (unless (mevedel-view--in-flight-turn-start-position)
      (mevedel-view--set-in-flight-turn-start mevedel-view--input-marker)
      (setq mevedel-view--data-turn-start
            (with-current-buffer mevedel--data-buffer
              (copy-marker (point-max) nil))))))

(defun mevedel-view-agent-live-transcript-finalize (invocation)
  "Refresh live transcript views for finalized INVOCATION.
Returns non-nil when a live transcript view still references the
agent buffer and should keep that buffer alive.  The view is marked
non-live so closing it after finalization kills the retained data
buffer."
  (when-let* (((mevedel-agent-invocation-p invocation))
              (agent-buf (mevedel-agent-invocation-buffer invocation))
              ((buffer-live-p agent-buf)))
    (let* ((views (mevedel-view--agent-transcript-views-for-data agent-buf))
           (status (mevedel-agent-invocation-transcript-status invocation))
           (started (mevedel-agent-invocation-started-at invocation))
           (elapsed (and started
                         (float-time
                          (time-subtract (current-time) started))))
           (reason (mevedel-agent-invocation-terminal-reason invocation))
           (calls (mevedel-agent-invocation-call-count invocation)))
      (dolist (view views)
        (when (buffer-live-p view)
          (condition-case err
              (with-current-buffer view
                (mevedel-view-stream-stop)
                (setq mevedel-view--agent-transcript-info
                      (append (list :live-buffer nil
                                    :status status
                                    :calls calls
                                    :elapsed elapsed
                                    :reason reason)
                              mevedel-view--agent-transcript-info))
                (when (buffer-live-p mevedel--data-buffer)
                  (atomic-change-group
                    (mevedel-view--full-rerender))))
            (error
             (display-warning
              'mevedel
              (format "Final live agent transcript update failed: %s"
                      (error-message-string err))
              :warning)))))
      views)))

(defun mevedel-view-close-agent-transcript ()
  "Close the selected transcript side window.
Saved transcript file buffers are killed with the view.  Live running
agent buffers are left alone."
  (interactive)
  (unless mevedel-view--agent-transcript-p
    (user-error "Not an agent transcript view"))
  (mevedel-view--kill-agent-transcript-view (current-buffer)))


;;
;;; Transcript opening and status

(defun mevedel-view-open-agent-transcript-at-point (&optional event)
  "Open the transcript referenced by the attribution at point or EVENT.

When EVENT is a mouse event outside an attribution, just move point as
usual.  Keyboard invocation outside an attribution signals a user error."
  (interactive (list last-nonmenu-event))
  (let* ((event-pos (and event
                         (eventp event)
                         (posn-point (event-end event))))
         (pos (if (integer-or-marker-p event-pos) event-pos (point)))
         (agent-path (get-text-property pos 'mevedel-view-agent-path)))
    (cond
     (agent-path
     (when (and event (eventp event))
        (mouse-set-point event))
      (mevedel-view-agent-handle-activate agent-path))
     ((and event (eventp event))
      (mouse-set-point event))
     (t
      (user-error "No transcript at point")))))

(defun mevedel-view--session ()
  "Return the root session displayed by the current view, or nil."
  (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                             mevedel--data-buffer))
              ((buffer-live-p data-buf)))
    (buffer-local-value 'mevedel--session data-buf)))

(defun mevedel-view--agent-record (agent-path)
  "Return the retained record for canonical AGENT-PATH, or nil."
  (when-let* ((session (mevedel-view--session)))
    (cdr (assoc agent-path (mevedel-session-agent-registry session)))))

(defun mevedel-view--lookup-transcript-pair (agent-path)
  "Return the transcript sidecar pair for canonical AGENT-PATH."
  (when-let* ((session (mevedel-view--session)))
    (let ((entries (mevedel-session-agent-transcripts session)))
      (if-let* ((record (mevedel-view--agent-record agent-path)))
          (assoc (mevedel-agent-record-id record) entries)
        (cl-find-if
         (lambda (entry)
           (equal agent-path (plist-get (cdr entry) :agent-path)))
         entries)))))

(defun mevedel-view--resolve-agent-transcript (agent-path)
  "Return transcript info for canonical AGENT-PATH.
Retained agents resolve through their conversation buffer when it is resident;
otherwise their validated transcript file is opened.
Signals `user-error' when no transcript source can be opened."
  (require 'mevedel-agent-persistence)
  (let* ((data-buf (and (boundp 'mevedel--data-buffer)
                        mevedel--data-buffer))
         (session (and data-buf (buffer-live-p data-buf)
                       (buffer-local-value 'mevedel--session data-buf)))
         (record (and session (mevedel-view--agent-record agent-path)))
         (pair (mevedel-view--lookup-transcript-pair agent-path))
         (entry (cdr pair))
         (inv (and record (mevedel-view--agent-invocation agent-path)))
         (status (mevedel-view--agent-effective-status inv entry))
         (conversation-buffer
          (and record (mevedel-agent-record-conversation-buffer record)))
         (active-p
          (and record
               (mevedel-agent-control-active-activity-p
                (mevedel-agent-record-activity record))))
         (save-path (and session (mevedel-session-save-path session)))
         (rel-path (and entry (plist-get entry :path))))
    (unless (or record pair)
      (user-error "Unknown agent path: %s" agent-path))
    (unless (or entry (buffer-live-p conversation-buffer))
      (user-error "No transcript entry for agent path: %s" agent-path))
    (if (buffer-live-p conversation-buffer)
        (append (list :agent-path agent-path
                      :status status
                      :entry entry
                      :session session
                      :buffer conversation-buffer
                      :live-buffer active-p
                      :calls (or (and inv
                                      (mevedel-agent-invocation-call-count inv))
                                 (plist-get entry :calls))
                      :elapsed (mevedel-view--agent-row-elapsed inv entry)
                      :reason (or (and inv
                                       (mevedel-agent-invocation-terminal-reason
                                        inv))
                                  (plist-get entry :reason))
                      :session-label (or (and session
                                              (mevedel-session-session-id
                                               session))
                                         (and session
                                              (mevedel-session-name session))
                                         "unknown"))
                entry)
      (unless save-path
        (user-error "Parent session has no save-path"))
      (unless (mevedel-agent-persistence-transcript-path-p
               rel-path save-path)
        (user-error "Transcript path failed validation: %s" rel-path))
      (let ((abs (expand-file-name rel-path save-path)))
        (unless (file-exists-p abs)
          (user-error "Transcript file missing: %s" abs))
        (append (list :agent-path agent-path
                      :status status
                      :entry entry
                      :session session
                      :save-path save-path
                      :relative-path rel-path
                      :absolute-path abs
                      :session-label (or (mevedel-session-session-id session)
                                         (mevedel-session-name session)
                                         "unknown"))
                entry)))))

(defun mevedel-view--display-agent-transcript-view (view-buf)
  "Display transcript inspection VIEW-BUF in this view's singleton slot."
  (let ((parent-view (or (and (boundp 'mevedel-view--agent-transcript-parent-view)
                              mevedel-view--agent-transcript-parent-view)
                         (current-buffer))))
    (when (and (buffer-live-p parent-view)
               (not (eq parent-view view-buf)))
      (with-current-buffer parent-view
        (let ((win mevedel-view--agent-transcript-window))
          (setq win
                (cond
                 ((and (window-live-p win)
                       (not (window-dedicated-p win))
                       (buffer-live-p (window-buffer win))
                       (with-current-buffer (window-buffer win)
                         mevedel-view--agent-transcript-p))
                  (condition-case nil
                      (let ((old-view (window-buffer win)))
                        (set-window-buffer win view-buf)
                        (unless (eq old-view view-buf)
                          (mevedel-view--kill-agent-transcript-view
                           old-view))
                        win)
                    (error nil)))
                 ((window-live-p win)
                  (ignore-errors (quit-window nil win))
                  nil)
                 (t nil)))
          (unless (window-live-p win)
            (setq win
                  (condition-case nil
                      (display-buffer view-buf mevedel-agent-view-display-action)
                    (error
                     (pop-to-buffer view-buf mevedel-agent-view-display-action)
                     (selected-window)))))
          (setq mevedel-view--agent-transcript-window
                (and (window-live-p win) win))
          (when (window-live-p win)
            (select-window win)
            (with-current-buffer view-buf
              (goto-char (point-max))
              (set-window-point win (point)))))))))

(defun mevedel-view--ensure-agent-transcript-view (agent-path info parent-view)
  "Return a rendered transcript inspection view for AGENT-PATH and INFO.
PARENT-VIEW is the session view that opened the transcript."
  (let* ((live-p (plist-get info :live-buffer))
         (agent-data (or (plist-get info :buffer)
                         (mevedel-session-persistence--find-file-noselect
                          (plist-get info :absolute-path))))
         (view-name (format "*mevedel-agent:%s*" agent-path))
         (agent-view
          (mevedel-view--ensure
           agent-data view-name
           (list :agent-transcript-p t
                 :agent-path agent-path
                 :parent-view parent-view
                 :preserve-data-view-buffer live-p
                 :transcript-info info))))
    (with-current-buffer agent-data
      (when (eq major-mode 'so-long-mode)
        (org-mode))
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      ;; Saved transcripts rely on gptel-org's GPTEL_BOUNDS property
      ;; for the `gptel' text properties that distinguish user text,
      ;; responses, reasoning, and tool calls.  Restore just those
      ;; bounds; full `gptel-org--restore-state' also restores backend
      ;; and tool objects, which is unnecessary and noisy for a
      ;; read-only transcript view.
      (unless live-p
        (require 'mevedel-transcript-restore)
        (mevedel-transcript-restore-properties))
      (unless (or live-p buffer-read-only)
        (read-only-mode +1)))
    (with-current-buffer agent-view
      (mevedel-view--full-rerender)
      (when live-p
        (mevedel-view--agent-transcript-start-streaming))
      (setq buffer-read-only t))
    agent-view))

(defun mevedel-view-agent-handle-activate (&optional agent-path)
  "Open the rendered agent handle at point or AGENT-PATH."
  (interactive)
  (let ((path (or agent-path
                  (get-text-property (point) 'mevedel-view-agent-path))))
    (unless path
      (user-error "No agent handle at point"))
    (condition-case err
        (mevedel-view-open-agent-transcript path)
      (user-error
       (message "%s" (error-message-string err))))))

(defun mevedel-view-open-agent-transcript (agent-path)
  "Open canonical AGENT-PATH's read-only transcript inspection view.

Looks up the entry in the parent session's `agent-transcripts'
slot and validates the path through
`mevedel-agent-persistence-transcript-path-p' before opening
completed transcripts.  Running agents open from their live invocation
buffer."
  (interactive
   (list (completing-read
          "Agent transcript: "
          (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                                     mevedel--data-buffer))
                      ((buffer-live-p data-buf))
                      (session (buffer-local-value
                                'mevedel--session data-buf)))
            (delete-dups
             (append
              (mapcar #'car (mevedel-session-agent-registry session))
              (delq nil
                    (mapcar (lambda (entry)
                              (plist-get (cdr entry) :agent-path))
                            (mevedel-session-agent-transcripts session))))))
          nil t)))
  (let* ((parent-view (current-buffer))
         (info (mevedel-view--resolve-agent-transcript agent-path))
         (agent-view (mevedel-view--ensure-agent-transcript-view
                      agent-path info parent-view)))
    (mevedel-view--display-agent-transcript-view agent-view)))

(defun mevedel-view--agent-handle-paths-in-buffer ()
  "Return canonical paths whose source-backed handles are present in the view."
  (let (paths)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((path (and (not (mevedel-view--agent-status-region-position-p
                               (point)))
                         (get-text-property (point)
                                            'mevedel-view-agent-handle-p)
                         (get-text-property (point)
                                            'mevedel-view-agent-path))))
          (when (and path (not (member path paths)))
            (push path paths)))
        (goto-char (or (next-single-property-change
                        (point) 'mevedel-view-agent-path nil (point-max))
                       (point-max)))))
    (nreverse paths)))

(defun mevedel-view--agent-row-elapsed (inv entry)
  "Return elapsed seconds for INV or transcript ENTRY."
  (or (plist-get entry :elapsed)
      (and inv
           (mevedel-agent-invocation-started-at inv)
           (float-time
            (time-subtract (current-time)
                           (mevedel-agent-invocation-started-at inv))))))

(defun mevedel-view--agent-record-status (record)
  "Return RECORD's visible active status, or nil when it is idle."
  (pcase (mevedel-agent-record-activity record)
    ((or 'permission-blocked 'interaction-blocked) 'blocked)
    ('waiting 'waiting)
    ((or 'starting 'running) 'running)
    (_ nil)))

(defun mevedel-view--agent-path-depth (path)
  "Return PATH's display depth below a direct child of root."
  (max 0 (- (length (split-string path "/" t)) 2)))

(defun mevedel-view--agent-status-counts ()
  "Return a plist of active agent counts for the current view."
  (let ((session (mevedel-view--session))
        (blocked 0)
         (running 0))
    (dolist (entry (and session (mevedel-session-agent-registry session)))
      (pcase (mevedel-view--agent-record-status (cdr entry))
        ((or 'blocked 'waiting) (cl-incf blocked))
        ('running (cl-incf running))))
    (list :blocked blocked :running running)))

(defun mevedel-view--agent-status-collect ()
  "Collect aggregate agent status rows for agents without visible handles."
  (let* ((session (mevedel-view--session))
         (handle-paths (mevedel-view--agent-handle-paths-in-buffer))
         ;; Inline handles are the primary UI for agent status and
         ;; transcript opening.  The aggregate footer only covers
         ;; agents that have no visible handle in the rendered turn.
         rows)
    (dolist (pair (and session (mevedel-session-agent-registry session)))
      (let* ((path (car pair))
             (record (cdr pair))
             (status (mevedel-view--agent-record-status record)))
        (when (and status (not (member path handle-paths)))
          (push (list :path path
                      :status status
                      :depth (mevedel-view--agent-path-depth path))
                rows))))
    (sort rows (lambda (left right)
                 (string-lessp (plist-get left :path)
                               (plist-get right :path))))))

(defun mevedel-view--agent-status-summary (rows)
  "Return a compact summary label for aggregate ROWS."
  (let ((blocked 0)
        (running 0)
        (terminal 0))
    (dolist (row rows)
      (pcase (plist-get row :status)
        ('blocked (cl-incf blocked))
        ('waiting (cl-incf blocked))
        ('running (cl-incf running))
        (_ (cl-incf terminal))))
    (string-join
     (delq nil
           (list (when (> blocked 0) (format "%d blocked" blocked))
                 (when (> running 0) (format "%d running" running))
                 (when (> terminal 0) (format "%d finished" terminal))))
     " · ")))

(defun mevedel-view--agent-status-buttonize-toggle (header suffix)
  "Return HEADER with SUFFIX made into the aggregate-status toggle.
Only the visible `[+]' / `[-]' suffix is made clickable so the
status line behaves like other compact view buffer affordances."
  (let ((start (string-match (regexp-quote suffix) header))
        (map (make-sparse-keymap)))
    (when start
      (define-key map (kbd "RET") #'mevedel-view-activate-at-point)
      (define-key map [mouse-1] #'mevedel-view-activate-at-point)
      (define-key map [mouse-2] #'mevedel-view-activate-at-point)
      (add-text-properties
       start (+ start (length suffix))
       `(face link
         keymap ,map
         mouse-face highlight
         follow-link t
         help-echo "Expand or collapse agent status")
       header))
    header))

(defun mevedel-view--agent-status-string (rows expanded-p)
  "Return the collapsed aggregate status header for ROWS.
EXPANDED-P controls whether the toggle suffix displays an expanded or
collapsed marker; expanded row content is rendered from Agent handles."
  (let* ((summary (mevedel-view--agent-status-summary rows))
         (suffix (if expanded-p "[-]" "[+]"))
         (header (mevedel-view--zone-separator
                  (format "%d %s: %s%s"
                          (length rows)
                          (if (= 1 (length rows)) "agent" "agents")
                          summary
                          (concat " " suffix)))))
    (setq header (mevedel-view--agent-status-buttonize-toggle
                  header suffix))
    (concat header "\n")))

(defun mevedel-view--agent-status-row-rendering (row)
  "Return an Agent-handle rendering plist for aggregate status ROW."
  (let* ((agent-path (plist-get row :path))
         (status (plist-get row :status))
         (label (pcase status
                  ('blocked "Blocked")
                  ('waiting "Waiting")
                  (_ "Running"))))
    (list :header (format "%s %s" label agent-path)
          :body (format "Agent is %s." (downcase label))
          :vtype 'agent-handle
          :agent-path agent-path
          :agent-status status
          :initially-collapsed-p t)))

(defun mevedel-view--agent-status-handles-string (rows)
  "Return Agent-handle text for ROWS, preserving live expansion state."
  (let ((data-buffer
         (and (boundp 'mevedel--data-buffer) mevedel--data-buffer))
        (session
         (and (boundp 'mevedel--session) mevedel--session)))
    (with-temp-buffer
      (let ((mevedel--data-buffer data-buffer)
            (mevedel--session session)
            (mevedel-view--input-marker (copy-marker (point-max) t)))
        (dolist (row rows)
          (let ((depth (or (plist-get row :depth) 0)))
            (when-let* ((rendering
                         (mevedel-view--agent-status-row-rendering row)))
              (let ((start (point)))
                (mevedel-view--insert-rendered-tool rendering nil)
                (when (> depth 0)
                  (mevedel-view--indent-region-lines
                   start (point)
                   (propertize
                    (make-string (* 2 depth) ?\s)
                    'font-lock-face 'mevedel-view-tool-metadata)))))))
        (let ((inhibit-read-only t))
          (remove-text-properties
           (point-min) (point-max)
           '(mevedel-view-source nil mevedel-view-source-key nil))
          (mevedel-view--add-display-region-properties
           (point-min) (point-max) 'agent-handle)
          (remove-text-properties
           (point-min) (point-max)
           '(read-only nil front-sticky nil rear-nonsticky nil)))
        (buffer-string)))))



;;
;;; Aggregate status and targeted refresh

(defun mevedel-view-agent-status-fragment ()
  "Return the aggregate live-agent status fragment, or nil."
  (when-let* ((rows (mevedel-view--agent-status-collect)))
    (let* ((expanded-p (mevedel-view--status-agent-expanded-p))
           (body (if expanded-p
                     (mevedel-view--agent-status-handles-string rows)
                   (mevedel-view--agent-status-string rows nil)))
           (fragment
            (list :namespace 'status
                  :id 'agents
                  :priority 0
                  :body body
                  :keymap (mevedel-view--display-fragment-keymap)
                  :navigatable t
                  :activate #'mevedel-view-agent-status-toggle
                  :entry 'agents
                  :collapsible t
                  :collapse-key mevedel-view--status-agent-collapse-key
                  :collapsed (not expanded-p)))
           (suffix (mevedel-view--status-trailing-newline-suffix body)))
      (if suffix
          (plist-put fragment :body-suffix suffix)
        fragment))))

(defun mevedel-view--status-agent-expanded-p ()
  "Return non-nil when aggregate agent status should show handle rows."
  (require 'mevedel-view-zone)
  (if (mevedel-view-zone-collapse-state-set-p
       mevedel-view--status-agent-collapse-key)
      (not (mevedel-view-zone-collapse-state
            mevedel-view--status-agent-collapse-key nil))
    t))


(defun mevedel-view--render-agent-status ()
  "Render or remove the aggregate live agent status text."
  (mevedel-view--render-status))

(defun mevedel-view--agent-status-region-position-p (pos)
  "Return non-nil when POS is inside the aggregate status fragment."
  (and (integer-or-marker-p pos)
       (eq (get-text-property pos 'mevedel-view-zone-namespace) 'status)
       (eq (get-text-property pos 'mevedel-view-zone-id) 'agents)))

(defun mevedel-view--agent-source-present-p (agent-path)
  "Return non-nil if the data buffer has an Agent source for AGENT-PATH."
  (when (and (boundp 'mevedel--data-buffer)
             (buffer-live-p mevedel--data-buffer))
    (let ((data-buf mevedel--data-buffer))
      (with-current-buffer data-buf
        (save-restriction
          (widen)
          (catch 'found
            (dolist (seg (mevedel-transcript-segments (point-min) (point-max)))
              (when (eq (car seg) 'tool)
                (when-let* ((call (mevedel-view--tool-call-parse
                                   data-buf (cadr seg) (caddr seg))))
                  (when (and (equal (plist-get call :name) "Agent")
                             (equal (plist-get (plist-get call :render-data)
                                               :path)
                                    agent-path))
                    (throw 'found t)))))
            nil))))))

(defun mevedel-view--agent-handle-refresh-points (agent-path)
  "Return source-backed visible handle positions for AGENT-PATH.
The return value is (POINTS . STALE-P), where STALE-P means a visible
non-status handle existed but lacked usable source metadata."
  (let ((pos (point-min))
        points
        stale-p)
    (while (< pos (point-max))
      (let* ((path (get-text-property pos 'mevedel-view-agent-path))
             (handle-p (get-text-property pos 'mevedel-view-agent-handle-p))
             (source (get-text-property pos 'mevedel-view-source)))
        (when (and handle-p
                   (equal path agent-path)
                   (not (mevedel-view--agent-status-region-position-p pos)))
          (if (and (consp source)
                   (integer-or-marker-p (car source))
                   (integer-or-marker-p (cdr source)))
              (unless (cl-find-if
                       (lambda (point)
                         (eq (get-text-property point 'mevedel-view-source)
                             source))
                       points)
                (push pos points))
            (setq stale-p t))))
      (setq pos (or (next-single-property-change
                     pos 'mevedel-view-agent-path nil (point-max))
                    (point-max))))
    (cons (sort points #'>) stale-p)))

(defun mevedel-view--refresh-agent-handle-at (pos _agent-path)
  "Refresh the rendered source-backed agent handle at POS.
Return non-nil on success."
  (save-excursion
    (goto-char pos)
    (let* ((source (get-text-property pos 'mevedel-view-source))
           (bounds (mevedel-view--section-bounds))
           (data-buf mevedel--data-buffer)
           (current-collapsed (and (get-text-property
                                    pos 'mevedel-view-collapsed)
                                   t))
           (state (mevedel-view--source-collapse-state-entry
                   source 'agent-handle))
           (collapsed (if state (cdr state) current-collapsed))
           (turn-id (get-text-property pos 'mevedel-view-turn-id))
           (in-flight-after-section-p
            (and bounds
                 (when-let* ((start (mevedel-view--in-flight-turn-start-position)))
                   (<= (car bounds) start (cdr bounds)))))
           (rendering
            (and bounds
                 data-buf
                 (buffer-live-p data-buf)
                 (mevedel-view--segment-rendering
                  data-buf (car source) (cdr source) collapsed))))
      (when (and bounds rendering)
        (unless state
          (mevedel-view--record-source-collapse-state
           source 'agent-handle collapsed))
        (let* ((view-start (car bounds))
               (view-end (cdr bounds))
               (rendering (mevedel-view--rendering-with-collapse-state
                           (plist-put (copy-sequence rendering)
                                      :initially-collapsed-p collapsed)
                           source)))
          (goto-char view-start)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((ins-start (point)))
                (delete-region view-start view-end)
                (mevedel-view--insert-rendered-tool rendering source)
                (when turn-id
                  (put-text-property ins-start (point)
                                     'mevedel-view-turn-id turn-id))
                (when in-flight-after-section-p
                  (set-marker mevedel-view--in-flight-turn-start ins-start)))
            (set-marker-insertion-type mevedel-view--input-marker nil))
          t)))))

(defun mevedel-view--refresh-agent-rendering-now (agent-path)
  "Refresh visible rendering for AGENT-PATH in the current view buffer."
  (let ((start-time (float-time))
        stale-p)
    (mevedel-view--call-preserving-window-state
     (lambda ()
       (mevedel-view--call-preserving-input-point
       (lambda ()
         (mevedel-view--call-preserving-input-text
          (lambda ()
            (let ((inhibit-read-only t)
                  (inhibit-modification-hooks t))
              (pcase-let ((`(,points . ,stale)
                           (mevedel-view--agent-handle-refresh-points agent-path)))
                (setq stale-p (or stale
                                  (and (null points)
                                       (mevedel-view--agent-source-present-p
                                        agent-path))))
                (dolist (point points)
                  (unless (mevedel-view--refresh-agent-handle-at point agent-path)
                    (setq stale-p t))))
              (mevedel-view--render-agent-status))))))))
    (mevedel-view--debug-log
     'agent-refresh
     :agent-path agent-path
     :elapsed (- (float-time) start-time)
     :fallback stale-p)
    (when stale-p
      (mevedel-view-rerender (current-buffer)))
    (not stale-p)))

(defun mevedel-view-refresh-agent-rendering (view-buffer agent-path)
  "Refresh VIEW-BUFFER's visible rendering for AGENT-PATH.
Rapid calls for the same agent are coalesced so tool start/finish bursts update
one handle/status row without scheduling repeated full rerenders."
  (when (and agent-path (buffer-live-p view-buffer))
    (with-current-buffer view-buffer
      (unless (hash-table-p mevedel-view--agent-refresh-timers)
        (setq mevedel-view--agent-refresh-timers
              (make-hash-table :test #'equal)))
      (when-let* ((timer (gethash agent-path mevedel-view--agent-refresh-timers)))
        (when (timerp timer)
          (cancel-timer timer)))
      (if (or (not (numberp mevedel-view-agent-refresh-delay))
              (<= mevedel-view-agent-refresh-delay 0))
          (mevedel-view--refresh-agent-rendering-now agent-path)
        (puthash
         agent-path
         (run-at-time
          mevedel-view-agent-refresh-delay nil
          (lambda (buffer id)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (when (hash-table-p mevedel-view--agent-refresh-timers)
                  (remhash id mevedel-view--agent-refresh-timers))
                (mevedel-view--refresh-agent-rendering-now id))))
          view-buffer agent-path)
         mevedel-view--agent-refresh-timers)))))

(defun mevedel-view-agent-status-toggle ()
  "Toggle the aggregate live agent status rows."
  (interactive)
  (require 'mevedel-view-zone)
  (let ((collapsed (mevedel-view--status-agent-expanded-p)))
    (mevedel-view-zone-set-collapse-state
     mevedel-view--status-agent-collapse-key collapsed))
  (mevedel-view--render-agent-status))



;;
;;; Attribution

(defun mevedel-view--insert-attribution (agent-path)
  "Insert the `from PATH' attribution fragment for canonical AGENT-PATH.
Returns the propertized string (does not modify the buffer).
The path portion is always propertized as a click target.  Activation resolves
the current live or persisted transcript and reports any unavailable source."
  (let* ((header (concat "from " agent-path))
         (s (copy-sequence header)))
    (add-text-properties 0 (length s)
                         (list 'font-lock-face 'mevedel-view-attribution)
                         s)
    (let* ((from-prefix-len (length "from "))
           (id-end (length s))
           (open-fn
           (lambda ()
              (interactive)
              (mevedel-view-agent-handle-activate agent-path)))
           (map (make-sparse-keymap)))
      (define-key map [mouse-1] open-fn)
      (define-key map [mouse-2] open-fn)
      (define-key map (kbd "RET") open-fn)
      (add-text-properties
       from-prefix-len id-end
       `(face link
         follow-link t
         mouse-face highlight
         keymap ,map
         mevedel-view-agent-path ,agent-path
         help-echo "Open agent transcript")
       s))
    s))


;;;; Live invocation and blocked state

(defun mevedel-view--agent-invocation (agent-path)
  "Return the retained invocation identity for canonical AGENT-PATH."
  (when-let* ((record (mevedel-view--agent-record agent-path)))
    (or (mevedel-agent-record-invocation record)
        (when-let* ((buffer (mevedel-agent-record-conversation-buffer record))
                    ((buffer-live-p buffer)))
          (buffer-local-value 'mevedel--agent-invocation buffer)))))

(defun mevedel-view-reset-agent-ephemeral-state (&optional view-buffer)
  "Reset view-local ephemeral agent UI state in VIEW-BUFFER.
Defaults to the current buffer."
  (with-current-buffer (or view-buffer (current-buffer))
    (require 'mevedel-view-zone)
    (mevedel-view-zone-set-collapse-state
     mevedel-view--status-agent-collapse-key nil)
    (mevedel-view--render-status)))

(provide 'mevedel-view-agent)

;;; mevedel-view-agent.el ends here
