;;; mevedel-view-agent.el --- Agent presentation and transcript views -*- lexical-binding: t -*-

;;; Commentary:

;; Owns agent transcript lookup and inspection, aggregate live-agent status,
;; attribution controls, and targeted agent refreshes in mevedel view buffers.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `cl-seq'
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-position "cl-seq" (cl-item cl-seq &rest cl-keys))

;; `mevedel-agent-runtime'
(declare-function mevedel-agent-runtime--agent-invocation-at "mevedel-agent-runtime" (fsm))
(declare-function mevedel-agent-runtime--prune-stale-agents-fsm "mevedel-agent-runtime" ())
(declare-function mevedel-agent-runtime-display-label "mevedel-agent-runtime" (agent-id))
(defvar mevedel-agent-runtime--fsms)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-agent "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent-id "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-background-agents "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-call-count "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-description "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-context "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-started-at "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-verdict "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--find-file-noselect "mevedel-session-persistence" (file))
(declare-function mevedel-session-persistence--validate-transcript-path "mevedel-session-persistence" (path save-path))

;; `mevedel-structs'
(declare-function mevedel-session-agent-transcripts "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-queue "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-queue "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-tool-ui'
(declare-function mevedel-tool-ui--handle-badge "mevedel-tool-ui" (render-data))
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
;;; State and keymaps


(defvar-local mevedel-view--agent-transcript-p nil
  "Non-nil when this view buffer renders an agent transcript for inspection.")

(defvar-local mevedel-view--agent-id nil
  "Canonical agent id rendered by an agent transcript inspection view.")

(defvar-local mevedel-view--agent-transcript-info nil
  "Resolved transcript metadata plist for an agent transcript inspection view.")

(defvar-local mevedel-view--agent-transcript-parent-view nil
  "Parent session view buffer that opened this transcript inspection view.")

(defvar-local mevedel-view--agent-transcript-window nil
  "Side window currently displaying an agent transcript for this view buffer.")

(defvar mevedel-view--agent-transcript-data-kill-in-progress nil
  "Non-nil while transcript view/data kill hooks are already paired.")

(defconst mevedel-view--status-agent-collapse-key '(status agents)
  "Stable fragment collapse key for the aggregate agent status block.")

(defvar-local mevedel-view--agent-refresh-timers nil
  "Hash table of pending coalesced agent refresh timers by agent id.")


(defvar-keymap mevedel-view--agent-handle-map
  :doc "Keymap active on non-label text in Agent handles.
The visible agent type label carries its own transcript-opening
keymap; the rest of the handle remains navigable without opening the
transcript on click."
  "TAB" #'mevedel-view-toggle-section
  "n" #'mevedel-view-next-display
  "p" #'mevedel-view-previous-display
  "t" #'mevedel-view-toggle-transcript
  "q" #'mevedel-view-close-agent-transcript)

(defvar-keymap mevedel-view--agent-label-map
  :doc "Keymap active on the visible agent type label in Agent handles."
  "RET" #'mevedel-view-activate-at-point
  "<mouse-1>" #'mevedel-view-activate-at-point
  "<mouse-2>" #'mevedel-view-activate-at-point)


(defun mevedel-view-agent-initialize (options data-buffer)
  "Initialize agent presentation state for DATA-BUFFER from OPTIONS."
  (setq-local mevedel-view--agent-transcript-p
              (and (plist-get options :agent-transcript-p) t))
  (setq-local mevedel-view--agent-id (plist-get options :agent-id))
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
  (let* ((info mevedel-view--agent-transcript-info)
         (agent-id (or mevedel-view--agent-id
                       (plist-get info :agent-id)))
         (display-label (mevedel-view--display-label-for-agent agent-id))
         (badge (mevedel-tool-ui--handle-badge
                 (list :status (plist-get info :status)
                       :calls (plist-get info :calls)
                       :elapsed (plist-get info :elapsed)
                       :reason (plist-get info :reason))))
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
           (list (format "Agent %s" display-label)
                 (unless (string-empty-p badge) badge)
                 (when (integerp calls) (format "%d calls" calls))
                 (when (numberp elapsed) (format "%.1fs" elapsed))
                 (format "session %s" session-label)))
     "  ")))

(defun mevedel-view--clear-parent-transcript-window ()
  "Clear this transcript view from its parent view's singleton slot."
  (when-let* ((parent mevedel-view--agent-transcript-parent-view)
              ((buffer-live-p parent)))
    (with-current-buffer parent
      (setq mevedel-view--agent-transcript-window nil))))

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

(defun mevedel-view--kill-agent-transcript-view (view-buffer)
  "Kill transcript inspection VIEW-BUFFER and its non-live data buffer."
  (when (mevedel-view--agent-transcript-view-p view-buffer)
    (let (data-buffer live-p)
      (with-current-buffer view-buffer
        (setq data-buffer mevedel--data-buffer
              live-p (plist-get mevedel-view--agent-transcript-info
                                :live-buffer)))
      (dolist (win (get-buffer-window-list view-buffer nil t))
        (ignore-errors
          (quit-window nil win)))
      (when (buffer-live-p view-buffer)
        (kill-buffer view-buffer))
      (when (and (not live-p)
                 data-buffer
                 (buffer-live-p data-buffer))
        (let ((mevedel-view--agent-transcript-data-kill-in-progress t))
          (kill-buffer data-buffer))))))

(defun mevedel-view-agent-cleanup-parent (parent-view)
  "Kill every transcript inspection view opened from PARENT-VIEW."
  (dolist (view (mevedel-view--agent-transcript-views-for-parent parent-view))
    (mevedel-view--kill-agent-transcript-view view)))

(defun mevedel-view--on-agent-transcript-data-killed ()
  "Kill live transcript inspection views when their data buffer dies."
  (unless mevedel-view--agent-transcript-data-kill-in-progress
    (let ((data-buffer (current-buffer))
          (mevedel-view--agent-transcript-data-kill-in-progress t))
      (dolist (view (mevedel-view--agent-transcript-views-for-data data-buffer))
        (when (buffer-live-p view)
          (kill-buffer view))))))

(defun mevedel-view--agent-live-transcript-views (agent-buf &optional agent-id)
  "Return live transcript views displaying AGENT-BUF.
When AGENT-ID is non-nil, only return views for that agent id."
  (let (views)
    (dolist (buf (buffer-list) (nreverse views))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf
                   (and (bound-and-true-p mevedel-view--agent-transcript-p)
                        (eq mevedel--data-buffer agent-buf)
                        (or (not agent-id)
                            (equal mevedel-view--agent-id agent-id)
                            (equal (mevedel-view--display-label-for-agent
                                    mevedel-view--agent-id)
                                   agent-id)))))
        (push buf views)))))

(defun mevedel-view-agent-live-transcript-finalize (invocation)
  "Refresh live transcript views for finalized INVOCATION.
Returns non-nil when a live transcript view still references the
agent buffer and should keep that buffer alive.  The view is marked
non-live so closing it after finalization kills the retained data
buffer."
  (when-let* (((mevedel-agent-invocation-p invocation))
              (agent-buf (mevedel-agent-invocation-buffer invocation))
              ((buffer-live-p agent-buf)))
    (let* ((agent-id (mevedel-agent-invocation-agent-id invocation))
           (views (mevedel-view--agent-live-transcript-views
                   agent-buf agent-id))
           (status (mevedel-agent-invocation-transcript-status invocation))
           (started (mevedel-agent-invocation-started-at invocation))
           (elapsed (and started
                         (float-time
                          (time-subtract (current-time) started))))
           (reason (mevedel-agent-invocation-terminal-reason invocation))
           (calls (mevedel-agent-invocation-call-count invocation)))
      (dolist (view views)
        (when (buffer-live-p view)
          (with-current-buffer view
            (setq mevedel-view--agent-transcript-info
                  (plist-put (copy-sequence
                              mevedel-view--agent-transcript-info)
                             :live-buffer nil))
            (setq mevedel-view--agent-transcript-info
                  (plist-put mevedel-view--agent-transcript-info
                             :status status))
            (setq mevedel-view--agent-transcript-info
                  (plist-put mevedel-view--agent-transcript-info
                             :calls calls))
            (when elapsed
              (setq mevedel-view--agent-transcript-info
                    (plist-put mevedel-view--agent-transcript-info
                               :elapsed elapsed)))
            (when reason
              (setq mevedel-view--agent-transcript-info
                    (plist-put mevedel-view--agent-transcript-info
                               :reason reason)))
            (when (buffer-live-p mevedel--data-buffer)
              (mevedel-view--full-rerender)))))
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
         (agent-id (get-text-property pos 'mevedel-view-agent-id)))
    (cond
     (agent-id
      (when (and event (eventp event))
        (mouse-set-point event))
      (if (get-text-property pos 'mevedel-view-agent-handle-p)
          (mevedel-view-agent-handle-activate agent-id)
        (mevedel-view--open-agent-transcript-or-message
         agent-id
         (get-text-property pos 'mevedel-view-agent-live-click)
         (get-text-property pos 'mevedel-view-agent-calls))))
     ((and event (eventp event))
      (mouse-set-point event))
     (t
      (user-error "No transcript at point")))))

(defun mevedel-view--lookup-transcript-pair (agent-id)
  "Return the parent session's transcript pair for AGENT-ID.

Resolve the parent chat (data) buffer from the current view
buffer, read its `mevedel--session', and look up AGENT-ID in the
session's `agent-transcripts' alist.

AGENT-ID may be the canonical id (for example, `type--32hex') or the
display label (`type--8hex') shown in rendered view text.  Returns nil
if any link is missing."
  (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                             mevedel--data-buffer))
              ((buffer-live-p data-buf))
              (session (buffer-local-value 'mevedel--session data-buf))
              (entries (mevedel-session-agent-transcripts session)))
    (or (assoc agent-id entries)
        (cl-find-if
         (lambda (entry)
           (equal (mevedel-view--display-label-for-agent (car entry))
                  agent-id))
         entries))))

(defun mevedel-view--lookup-transcript-entry (agent-id)
  "Return the parent session's transcript entry plist for AGENT-ID.
See `mevedel-view--lookup-transcript-pair' for accepted id forms."
  (cdr (mevedel-view--lookup-transcript-pair agent-id)))

(defun mevedel-view--display-label-for-agent (agent-id)
  "Return the short display label for AGENT-ID."
  (or (and (fboundp 'mevedel-agent-runtime-display-label)
           (mevedel-agent-runtime-display-label agent-id))
      agent-id))

(defun mevedel-view--resolve-agent-transcript (agent-id)
  "Return transcript info for AGENT-ID.
Terminal agents resolve through their saved transcript file.  Running
agents resolve through their live invocation buffer when available.
Signals `user-error' when no transcript source can be opened."
  (let* ((data-buf (and (boundp 'mevedel--data-buffer)
                        mevedel--data-buffer))
         (session (and data-buf (buffer-live-p data-buf)
                       (buffer-local-value 'mevedel--session data-buf)))
         (pair (and session (mevedel-view--lookup-transcript-pair agent-id)))
         (canonical-id (or (car pair) agent-id))
         (entry (cdr pair))
         (inv (mevedel-view--agent-invocation agent-id))
         (status (mevedel-view--agent-effective-status inv entry))
         (live-buffer (and inv
                           (not (mevedel-view--agent-terminal-status-p status))
                           (mevedel-agent-invocation-buffer inv)))
         (save-path (and session (mevedel-session-save-path session)))
         (rel-path (and entry (plist-get entry :path))))
    (unless (or entry live-buffer)
      (user-error "No transcript entry for agent-id: %s" agent-id))
    (if (and live-buffer (buffer-live-p live-buffer))
        (append (list :agent-id canonical-id
                      :status status
                      :entry entry
                      :session session
                      :buffer live-buffer
                      :live-buffer t
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
      (unless (mevedel-session-persistence--validate-transcript-path
               rel-path save-path)
        (user-error "Transcript path failed validation: %s" rel-path))
      (let ((abs (expand-file-name rel-path save-path)))
        (unless (file-exists-p abs)
          (user-error "Transcript file missing: %s" abs))
        (append (list :agent-id canonical-id
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

(defun mevedel-view--ensure-agent-transcript-view (agent-id info parent-view)
  "Return a rendered transcript inspection view for AGENT-ID and INFO.
PARENT-VIEW is the session view that opened the transcript."
  (let* ((live-p (plist-get info :live-buffer))
         (agent-data (or (plist-get info :buffer)
                         (mevedel-session-persistence--find-file-noselect
                          (plist-get info :absolute-path))))
         (display-label (mevedel-view--display-label-for-agent agent-id))
         (view-name (format "*mevedel-agent:%s*" display-label))
         (agent-view
          (mevedel-view--ensure
           agent-data view-name
           (list :agent-transcript-p t
                 :agent-id agent-id
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
      (mevedel-view--full-rerender))
    agent-view))

(defun mevedel-view-agent-handle-activate (&optional agent-id)
  "Open the rendered agent handle at point or AGENT-ID."
  (interactive)
  (let ((id (or agent-id
                (get-text-property (point) 'mevedel-view-agent-id))))
    (unless id
      (user-error "No agent handle at point"))
    (condition-case err
        (mevedel-view-open-agent-transcript id)
      (user-error
       (message "%s" (error-message-string err))))))

(defun mevedel-view--open-agent-transcript-or-message
    (agent-id &optional _live-click-p calls)
  "Open AGENT-ID's transcript or explain why it is not openable.
CALLS is the optional number of tool calls to mention in fallback messages.

This is the click/RET path for attribution fragments."
  (let* ((entry (mevedel-view--lookup-transcript-entry agent-id))
         (inv (mevedel-view--agent-invocation agent-id))
         (status (mevedel-view--agent-effective-status inv entry))
         (session (and (boundp 'mevedel--data-buffer)
                       mevedel--data-buffer
                       (buffer-live-p mevedel--data-buffer)
                       (buffer-local-value 'mevedel--session
                                           mevedel--data-buffer)))
         (save-path (and session (mevedel-session-save-path session)))
         (rel-path (and entry (plist-get entry :path)))
         (path-ok (and entry save-path
                       (mevedel-session-persistence--validate-transcript-path
                        rel-path save-path)))
         (terminal-p (memq status '(completed error aborted incomplete)))
         (display-label (mevedel-view--display-label-for-agent agent-id))
         (count (or calls (and entry (plist-get entry :calls)))))
    (cond
     ((and path-ok terminal-p)
      (mevedel-view-open-agent-transcript agent-id))
     ((and (eq status 'running) inv)
      (mevedel-view-open-agent-transcript agent-id))
     ((eq status 'running)
      (message "Agent %s still running%s. Live buffer unavailable."
               display-label
               (if (integerp count)
                   (format " (%d tool calls)" count)
                 "")))
     ((not entry)
      (message "No transcript recorded for %s" display-label))
     ((not path-ok)
      (message "Transcript path is unavailable for %s" display-label))
     (t
      (message "Transcript unavailable for %s" display-label)))))

(defun mevedel-view-open-agent-transcript (agent-id)
  "Open AGENT-ID's transcript as a rendered read-only inspection view.

Looks up the entry in the parent session's `agent-transcripts'
slot and validates the path through
`mevedel-session-persistence--validate-transcript-path' before opening
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
            (mapcar #'car (mevedel-session-agent-transcripts session)))
          nil t)))
  (let* ((parent-view (current-buffer))
         (info (mevedel-view--resolve-agent-transcript agent-id))
         (canonical-id (plist-get info :agent-id))
         (agent-view (mevedel-view--ensure-agent-transcript-view
                      canonical-id info parent-view)))
    (mevedel-view--display-agent-transcript-view agent-view)))

(defun mevedel-view--agent-handle-ids-in-buffer ()
  "Return agent ids whose source-backed handles are present in the view."
  (let (ids)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((id (and (not (mevedel-view--agent-status-region-position-p
                             (point)))
                       (get-text-property (point) 'mevedel-view-agent-handle-p)
                       (get-text-property (point) 'mevedel-view-agent-id))))
          (when (and id (not (member id ids)))
            (push id ids)))
        (goto-char (or (next-single-property-change
                        (point) 'mevedel-view-agent-id nil (point-max))
                       (point-max)))))
    (nreverse ids)))

(defun mevedel-view--agent-row-description (_agent-id inv entry)
  "Return the agent row description from INV or transcript ENTRY."
  (or (and inv (mevedel-agent-invocation-description inv))
      (plist-get entry :description)
      ""))

(defun mevedel-view--agent-row-type (agent-id inv entry)
  "Return the agent type for AGENT-ID from INV, ENTRY, or the id prefix."
  (or (and inv
           (mevedel-agent-invocation-agent inv)
           (mevedel-agent-name (mevedel-agent-invocation-agent inv)))
      (plist-get entry :type)
      (plist-get entry :agent-type)
      (when (stringp agent-id)
        (if-let* ((sep (string-search "--" agent-id)))
            (substring agent-id 0 sep)
          agent-id))
      "?"))

(defun mevedel-view--agent-row-elapsed (inv entry)
  "Return elapsed seconds for INV or transcript ENTRY."
  (or (plist-get entry :elapsed)
      (and inv
           (mevedel-agent-invocation-started-at inv)
           (float-time
            (time-subtract (current-time)
                           (mevedel-agent-invocation-started-at inv))))))

(defun mevedel-view--agent-row-verdict (inv entry)
  "Return parsed verifier verdict for INV or transcript ENTRY."
  (or (and inv (mevedel-agent-invocation-verdict inv))
      (plist-get entry :verdict)))

(defun mevedel-view--agent-row-parent-id (inv entry)
  "Return parent agent id for INV or transcript ENTRY, when known."
  (or (plist-get entry :parent-agent-id)
      (when-let* (((mevedel-agent-invocation-p inv))
                  (parent (mevedel-agent-invocation-parent-context inv))
                  ((mevedel-agent-invocation-p parent)))
        (mevedel-agent-invocation-agent-id parent))))

(defun mevedel-view--agent-entry-has-visible-child-p (agent-id entries)
  "Return non-nil if ENTRIES include an active child of AGENT-ID."
  (catch 'found
    (dolist (pair entries nil)
      (let* ((entry (cdr pair))
             (parent-id (plist-get entry :parent-agent-id))
             (status (plist-get entry :status)))
        (when (and (equal parent-id agent-id)
                   (or (eq status 'running)
                       (mevedel-view--agent-terminal-status-p status)))
          (throw 'found t))))))

(defun mevedel-view--agent-status-counts ()
  "Return a plist of active agent counts for the current view."
  (let* ((data-buf (and (boundp 'mevedel--data-buffer) mevedel--data-buffer))
         (session (and data-buf (buffer-live-p data-buf)
                       (buffer-local-value 'mevedel--session data-buf)))
         (entries (and session (mevedel-session-agent-transcripts session)))
         (seen (make-hash-table :test #'equal))
         (blocked 0)
         (running 0))
    (cl-labels
        ((record (agent-id status)
           (when (and agent-id status)
             (puthash agent-id status seen))))
      (when entries
        (dolist (pair entries)
          (let* ((agent-id (car pair))
                 (entry (cdr pair))
                 (inv (mevedel-view--agent-invocation agent-id))
                 (status (mevedel-view--agent-effective-status inv entry)))
            (when (eq status 'running)
              (record agent-id
                      (if (mevedel-view--agent-status-blocked-p agent-id)
                          'blocked
                        'running))))))
      (when (and data-buf (buffer-live-p data-buf))
        (with-current-buffer data-buf
          (dolist (pair mevedel-agent-runtime--fsms)
            (let* ((agent-id (car pair))
                   (inv (ignore-errors
                         (mevedel-agent-runtime--agent-invocation-at (cdr pair))))
                   (entry (cdr (assoc agent-id entries)))
                   (status (mevedel-view--agent-effective-status inv entry)))
              (when (eq status 'running)
                (record agent-id
                        (if (mevedel-view--agent-status-blocked-p agent-id)
                            'blocked
                          'running)))))))
      (maphash (lambda (_agent-id status)
                 (pcase status
                   ('blocked (cl-incf blocked))
                   ('running (cl-incf running))))
               seen)
      (list :blocked blocked :running running))))

(defun mevedel-view--agent-status-collect ()
  "Collect aggregate agent status rows for agents without visible handles."
  (let* ((data-buf (and (boundp 'mevedel--data-buffer) mevedel--data-buffer))
         (session (and data-buf (buffer-live-p data-buf)
                       (buffer-local-value 'mevedel--session data-buf)))
         (entries (and session (mevedel-session-agent-transcripts session)))
         (handle-ids (mevedel-view--agent-handle-ids-in-buffer))
         ;; Inline handles are the primary UI for agent status and
         ;; transcript opening.  The aggregate footer only covers
         ;; agents that have no visible handle in the rendered turn.
         (live-ids (copy-sequence handle-ids))
         rows)
    (when (and data-buf (buffer-live-p data-buf))
      (with-current-buffer data-buf
        ;; Preserve the ids of terminal live invocations before pruning
        ;; removes their FSMs, so stale running sidecar metadata cannot
        ;; reappear as aggregate status rows.
        (dolist (pair mevedel-agent-runtime--fsms)
          (let* ((agent-id (car pair))
                 (inv (ignore-errors
                         (mevedel-agent-runtime--agent-invocation-at (cdr pair))))
                 (status (and inv
                              (mevedel-agent-invocation-transcript-status
                               inv))))
            (when (mevedel-view--agent-terminal-status-p status)
              (push agent-id live-ids))))
        (when (fboundp 'mevedel-agent-runtime--prune-stale-agents-fsm)
          (mevedel-agent-runtime--prune-stale-agents-fsm))
        (dolist (pair mevedel-agent-runtime--fsms)
          (let* ((agent-id (car pair))
                 (fsm (cdr pair))
                 (inv (ignore-errors
                        (mevedel-agent-runtime--agent-invocation-at fsm)))
                 (entry (cdr (assoc agent-id entries)))
                 (status (mevedel-view--agent-effective-status inv entry)))
            (when inv
              (push agent-id live-ids)
              (unless (member agent-id handle-ids)
                (when (eq status 'running)
                  (let ((blocked
                         (and session
                              (or (mevedel-view--queue-has-origin-p
                                   (mevedel-session-permission-queue session)
                                   agent-id)
                                  (mevedel-view--queue-has-origin-p
                                   (mevedel-session-plan-queue session)
                                   agent-id))))
                        (parent-id
                         (mevedel-view--agent-row-parent-id inv entry)))
                    (push (list :agent-id agent-id
                                :status (if blocked 'blocked 'running)
                                :agent-type
                                (mevedel-view--agent-row-type
                                 agent-id inv entry)
                                :description
                                (mevedel-view--agent-row-description agent-id inv entry)
                                :calls (mevedel-agent-invocation-call-count inv)
                                :elapsed (mevedel-view--agent-row-elapsed inv entry)
                                :verdict
                                (mevedel-view--agent-row-verdict inv entry)
                                :parent-agent-id parent-id
                                :depth (if parent-id 1 0)
                                :parent-turn (plist-get entry :parent-turn)
                                :reason
                                (or (mevedel-agent-invocation-terminal-reason inv)
                                    (plist-get entry :reason)))
                          rows)))))))))
    (dolist (pair entries)
      (let* ((agent-id (car pair))
             (entry (cdr pair))
             (inv (mevedel-view--agent-invocation agent-id))
             (status (mevedel-view--agent-effective-status inv entry))
             (blocked (and (eq status 'running)
                           (mevedel-view--agent-status-blocked-p agent-id)))
             (visible-status (if blocked 'blocked status))
             (parent-id (mevedel-view--agent-row-parent-id inv entry)))
        (when (and (or (not (member agent-id live-ids))
                       (mevedel-view--agent-entry-has-visible-child-p
                        agent-id entries))
                   (eq status 'running))
          (push agent-id live-ids)
          (push (list :agent-id agent-id
                      :status visible-status
                      :agent-type
                      (mevedel-view--agent-row-type agent-id inv entry)
                      :description
                      (mevedel-view--agent-row-description agent-id inv entry)
                      :calls (or (and inv
                                      (mevedel-agent-invocation-call-count inv))
                                 (plist-get entry :calls))
                      :elapsed (mevedel-view--agent-row-elapsed inv entry)
                      :verdict (mevedel-view--agent-row-verdict inv entry)
                      :parent-agent-id parent-id
                      :depth (if parent-id 1 0)
                      :parent-turn (plist-get entry :parent-turn)
                      :reason (plist-get entry :reason))
                rows))))
    (mevedel-view--agent-status-order-rows rows)))

(defun mevedel-view--agent-status-rank (row)
  "Return status sort rank for ROW."
  (or (cl-position (plist-get row :status)
                   '(blocked running error aborted incomplete completed))
      99))

(defun mevedel-view--agent-status-infer-parent (row rows)
  "Return inferred parent id for ROW from ROWS, or nil.

Parentage normally comes from recorded transcript metadata or the live
invocation parent context.  If that metadata is missing, live parent
invocations can still identify their running background children."
  (or (plist-get row :parent-agent-id)
      (let ((agent-id (plist-get row :agent-id)))
        (catch 'parent
          (dolist (candidate rows)
            (let ((candidate-id (plist-get candidate :agent-id)))
              (when (and candidate-id
                         (not (equal candidate-id agent-id)))
                (when-let* ((inv (mevedel-view--agent-invocation
                                  candidate-id))
                            ((mevedel-agent-invocation-p inv))
                            (children
                             (mevedel-agent-invocation-background-agents
                              inv))
                            ((member agent-id children)))
                  (throw 'parent candidate-id)))))
          nil))))

(defun mevedel-view--agent-status-order-rows (rows)
  "Return ROWS ordered as a parent-before-child hierarchy."
  (let ((index 0)
        (table (make-hash-table :test #'equal))
        (children (make-hash-table :test #'equal))
        ordered)
    (dolist (row rows)
      (unless (plist-member row :index)
        (setq row (plist-put row :index index))
        (cl-incf index))
      (puthash (plist-get row :agent-id) row table))
    (dolist (row rows)
      (when-let* ((parent-id (mevedel-view--agent-status-infer-parent
                              row rows))
                  ((gethash parent-id table)))
        (setq row (plist-put row :parent-agent-id parent-id))
        (puthash (plist-get row :agent-id) row table)))
    (maphash
     (lambda (_id row)
       (let ((parent-id (plist-get row :parent-agent-id)))
         (if (and parent-id (gethash parent-id table))
             (puthash parent-id
                      (cons row (gethash parent-id children))
                      children)
           (push row ordered))))
     table)
    (cl-labels
        ((sibling-less-p
          (a b)
          (let ((rank-a (mevedel-view--agent-status-rank a))
                (rank-b (mevedel-view--agent-status-rank b)))
            (if (= rank-a rank-b)
                (< (or (plist-get a :index) 0)
                   (or (plist-get b :index) 0))
              (< rank-a rank-b))))
         (depth-of
          (row)
          (if-let* ((parent-id (plist-get row :parent-agent-id))
                    (parent (gethash parent-id table)))
              (1+ (depth-of parent))
            0))
         (emit
          (row)
          (let ((row (plist-put (copy-sequence row) :depth (depth-of row))))
            (cons row
                  (mapcan #'emit
                          (sort (copy-sequence
                                 (gethash (plist-get row :agent-id)
                                          children))
                                #'sibling-less-p))))))
      (mapcan #'emit (sort ordered #'sibling-less-p)))))

(defun mevedel-view--agent-status-summary (rows)
  "Return a compact summary label for aggregate ROWS."
  (let ((blocked 0)
        (running 0)
        (terminal 0))
    (dolist (row rows)
      (pcase (plist-get row :status)
        ('blocked (cl-incf blocked))
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

(defun mevedel-view--agent-status-row-rendering (row &optional header-width)
  "Return an Agent-handle rendering plist for aggregate status ROW.
HEADER-WIDTH is the optional width used to align the row header."
  (let* ((agent-id (plist-get row :agent-id))
         (status (plist-get row :status))
         (render-status (if (eq status 'blocked) 'running status))
         (agent-type (or (plist-get row :agent-type)
                         (mevedel-view--agent-row-type agent-id nil nil)))
         (description (or (plist-get row :description) ""))
         (calls (plist-get row :calls))
         (elapsed (plist-get row :elapsed))
         (verdict (plist-get row :verdict))
         (reason (plist-get row :reason))
         (blocked-reason (and (eq status 'blocked) "interaction"))
         (render-data (append
                       (list :kind 'agent-transcript
                             :agent-id agent-id
                             :status render-status
                             :calls (or calls 0)
                             :background t
                             :omit-attribution t)
                       (when header-width
                         (list :header-width header-width))
                       (when elapsed (list :elapsed elapsed))
                       (when verdict (list :verdict verdict))
                       (when blocked-reason
                         (list :blocked-reason blocked-reason))
                       (when reason (list :reason reason))))
         (body (if (eq render-status 'running)
                   "Agent is running."
                 "Agent completed.")))
    (mevedel-tool-ui--render-agent
     "Agent"
     (list :subagent_type agent-type
           :description description)
     body
     render-data)))

(defun mevedel-view--agent-status-line-width ()
  "Return the maximum display width for one aggregate agent status row."
  (let* ((buffer (current-buffer))
         (selected (selected-window))
         (windows (get-buffer-window-list buffer nil t))
         (width (cond
                 ((eq (window-buffer selected) buffer)
                  (window-body-width selected))
                 (windows
                  (apply #'min (mapcar #'window-body-width windows)))
                 (t
                  (window-body-width)))))
    (max 20 (1- width))))

(defun mevedel-view--agent-status-handles-string (rows)
  "Return Agent-handle text for ROWS, preserving live expansion state."
  (let ((data-buffer
         (and (boundp 'mevedel--data-buffer) mevedel--data-buffer))
        (session
         (and (boundp 'mevedel--session) mevedel--session))
        (line-width (mevedel-view--agent-status-line-width)))
    (with-temp-buffer
      (let ((mevedel--data-buffer data-buffer)
            (mevedel--session session)
            (mevedel-view--input-marker (copy-marker (point-max) t)))
        (dolist (row rows)
          (let* ((depth (or (plist-get row :depth) 0))
                 (header-width (max 20 (- line-width
                                          4
                                          (* 2 depth)))))
            (when-let* ((rendering
                         (mevedel-view--agent-status-row-rendering
                          row header-width)))
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

(defun mevedel-view--agent-source-present-p (agent-id)
  "Return non-nil if the data buffer has an Agent source for AGENT-ID."
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
                                               :agent-id)
                                    agent-id))
                    (throw 'found t)))))
            nil))))))

(defun mevedel-view--agent-handle-refresh-points (agent-id)
  "Return source-backed visible handle positions for AGENT-ID.
The return value is (POINTS . STALE-P), where STALE-P means a visible
non-status handle existed but lacked usable source metadata."
  (let ((pos (point-min))
        points
        stale-p)
    (while (< pos (point-max))
      (let* ((id (get-text-property pos 'mevedel-view-agent-id))
             (handle-p (get-text-property pos 'mevedel-view-agent-handle-p))
             (source (get-text-property pos 'mevedel-view-source)))
        (when (and handle-p
                   (equal id agent-id)
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
                     pos 'mevedel-view-agent-id nil (point-max))
                    (point-max))))
    (cons (sort points #'>) stale-p)))

(defun mevedel-view--refresh-agent-handle-at (pos _agent-id)
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

(defun mevedel-view--refresh-agent-rendering-now (agent-id)
  "Refresh visible rendering for AGENT-ID in the current view buffer."
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
                           (mevedel-view--agent-handle-refresh-points agent-id)))
                (setq stale-p (or stale
                                  (and (null points)
                                       (mevedel-view--agent-source-present-p
                                        agent-id))))
                (dolist (point points)
                  (unless (mevedel-view--refresh-agent-handle-at point agent-id)
                    (setq stale-p t))))
              (mevedel-view--render-agent-status))))))))
    (mevedel-view--debug-log
     'agent-refresh
     :agent-id agent-id
     :elapsed (- (float-time) start-time)
     :fallback stale-p)
    (when stale-p
      (mevedel-view-rerender (current-buffer)))
    (not stale-p)))

(defun mevedel-view-refresh-agent-rendering (view-buffer agent-id)
  "Refresh VIEW-BUFFER's visible rendering for AGENT-ID.
Rapid calls for the same agent are coalesced so tool start/finish bursts update
one handle/status row without scheduling repeated full rerenders."
  (when (and agent-id (buffer-live-p view-buffer))
    (with-current-buffer view-buffer
      (unless (hash-table-p mevedel-view--agent-refresh-timers)
        (setq mevedel-view--agent-refresh-timers
              (make-hash-table :test #'equal)))
      (when-let* ((timer (gethash agent-id mevedel-view--agent-refresh-timers)))
        (when (timerp timer)
          (cancel-timer timer)))
      (if (or (not (numberp mevedel-view-agent-refresh-delay))
              (<= mevedel-view-agent-refresh-delay 0))
          (mevedel-view--refresh-agent-rendering-now agent-id)
        (puthash
         agent-id
         (run-at-time
          mevedel-view-agent-refresh-delay nil
          (lambda (buffer id)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (when (hash-table-p mevedel-view--agent-refresh-timers)
                  (remhash id mevedel-view--agent-refresh-timers))
                (mevedel-view--refresh-agent-rendering-now id))))
          view-buffer agent-id)
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

(defun mevedel-view--insert-attribution
    (agent-id &optional _live-click-p calls)
  "Insert the `from <type>--<idshort>' attribution fragment for AGENT-ID.
Returns the propertized string (does not modify the buffer).
The agent-id portion is always propertized as a click target.
Click dispatches through
`mevedel-view--open-agent-transcript-or-message', which either
opens via `mevedel-view-open-agent-transcript' or reports why the
transcript is not openable yet.

Running transcripts open from the live invocation buffer when present.
CALLS, when non-nil, is used in the running-state echo-area message."
  (let* ((display-label (mevedel-view--display-label-for-agent agent-id))
         (entry (mevedel-view--lookup-transcript-entry agent-id))
         (inv (mevedel-view--agent-invocation agent-id))
         (status (mevedel-view--agent-effective-status inv entry))
         (session (and (boundp 'mevedel--data-buffer)
                       mevedel--data-buffer
                       (buffer-live-p mevedel--data-buffer)
                       (buffer-local-value 'mevedel--session
                                           mevedel--data-buffer)))
         (save-path (and session (mevedel-session-save-path session)))
         (rel-path (and entry (plist-get entry :path)))
         (path-ok (and entry save-path
                       (mevedel-session-persistence--validate-transcript-path
                        rel-path save-path)))
         (terminal-p (memq status
                           '(completed error aborted incomplete)))
         (live-openable (and (eq status 'running) inv))
         (openable (or (and path-ok terminal-p) live-openable))
         (echo (cond
                (openable (format "Open transcript for %s" agent-id))
                ((eq status 'running)
                 (format
                  "Agent %s still running%s. Live buffer unavailable."
                  display-label
                  (if (integerp calls)
                      (format " (%d tool calls)" calls)
                    "")))
                ((not entry)
                 (format "No transcript recorded for %s" agent-id))
                ((not path-ok)
                 (format "Transcript path is unavailable for %s" agent-id))
                (t (format "Transcript unavailable for %s" agent-id))))
         (header (concat "from " display-label))
         (s (copy-sequence header)))
    (add-text-properties 0 (length s)
                         (list 'font-lock-face 'mevedel-view-attribution)
                         s)
    (let* ((from-prefix-len (length "from "))
           (id-end (length s))
           (open-fn
            (lambda ()
              (interactive)
              (mevedel-view--open-agent-transcript-or-message
               agent-id nil calls)))
           (map (make-sparse-keymap)))
      (define-key map [mouse-1] open-fn)
      (define-key map [mouse-2] open-fn)
      (define-key map (kbd "RET") open-fn)
      ;; Apply button-style properties directly to the agent-id
      ;; substring of s so the returned string carries them.  An
      ;; earlier draft passed a substring copy to make-text-button
      ;; which produced a properly-buttoned new string but threw
      ;; it away -- the returned s only had mouse-face.  Now uses
      ;; add-text-properties on the original string so the
      ;; keymap, click action, and link face all stick.
      (add-text-properties
       from-prefix-len id-end
       `(face link
         follow-link t
         mouse-face highlight
         keymap ,map
         mevedel-view-agent-id ,agent-id
         mevedel-view-agent-live-click nil
         mevedel-view-agent-calls ,calls
         help-echo ,echo)
       s))
    s))


;;;; Live invocation and blocked state

(defun mevedel-view--agent-invocation (agent-id)
  "Return the live invocation for AGENT-ID visible to this view."
  (cl-labels
      ((id-match-p
        (candidate)
        (or (equal candidate agent-id)
            (equal (mevedel-view--display-label-for-agent candidate)
                   agent-id)))
       (lookup-in-buffer
        (buf)
        (when (and buf (buffer-live-p buf))
          (with-current-buffer buf
            (catch 'found
              (dolist (pair mevedel-agent-runtime--fsms)
                (when (id-match-p (car pair))
                  (when-let* ((inv (ignore-errors
                                      (mevedel-agent-runtime--agent-invocation-at
                                       (cdr pair)))))
                    (throw 'found inv))))
              nil)))))
    (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                               mevedel--data-buffer))
                ((buffer-live-p data-buf)))
      (or (lookup-in-buffer data-buf)
          (let ((session (buffer-local-value 'mevedel--session data-buf)))
            (catch 'found
              (dolist (buf (buffer-list))
                (when (and (buffer-live-p buf)
                           (eq (buffer-local-value 'mevedel--session buf)
                               session))
                  (when-let* ((inv (lookup-in-buffer buf)))
                    (throw 'found inv))))
              nil))))))

(defun mevedel-view-reset-agent-ephemeral-state (&optional view-buffer)
  "Reset view-local ephemeral agent UI state in VIEW-BUFFER.
Defaults to the current buffer."
  (with-current-buffer (or view-buffer (current-buffer))
    (require 'mevedel-view-zone)
    (mevedel-view-zone-set-collapse-state
     mevedel-view--status-agent-collapse-key nil)
    (mevedel-view--render-status)))

(defun mevedel-view--queue-has-origin-p (queue origin)
  "Return non-nil if QUEUE has an entry with ORIGIN."
  (let (found)
    (while (and queue (not found))
      (setq found (equal (plist-get (car queue) :origin) origin))
      (setq queue (cdr queue)))
    found))


(defun mevedel-view--agent-status-blocked-p (agent-id)
  "Return non-nil when AGENT-ID is waiting on a parent interaction queue."
  (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                             mevedel--data-buffer))
              ((buffer-live-p data-buf))
              (session (buffer-local-value 'mevedel--session data-buf)))
    (or (mevedel-view--queue-has-origin-p
         (mevedel-session-permission-queue session) agent-id)
        (mevedel-view--queue-has-origin-p
         (mevedel-session-plan-queue session) agent-id))))

(provide 'mevedel-view-agent)

;;; mevedel-view-agent.el ends here
