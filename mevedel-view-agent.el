;;; mevedel-view-agent.el --- Agent presentation and transcript views -*- lexical-binding: t -*-

;;; Commentary:

;; Owns agent transcript lookup and inspection, live-agent status rows for the
;; background-work panel, attribution controls, and targeted agent refreshes
;; in mevedel view buffers.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-structs)
(require 'mevedel-view-zone)

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `cl-seq'
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-active-activity-p
                  "mevedel-agent-control" (activity))
(declare-function mevedel-agent-control-model-metadata
                  "mevedel-agent-control" (session path))
(declare-function mevedel-agent-control-record-model-metadata
                  "mevedel-agent-control" (record))
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
(autoload 'mevedel-agent-control-model-metadata "mevedel-agent-control")
(autoload 'mevedel-agent-control-record-model-metadata "mevedel-agent-control")

;; `mevedel-agent-conversation'
(defvar mevedel--agent-invocation)

;; `mevedel-agent-persistence'
(declare-function mevedel-agent-persistence-ensure-conversation
                  "mevedel-agent-persistence"
                  (session record root-buffer &optional readonly-p))
(autoload 'mevedel-agent-persistence-ensure-conversation "mevedel-agent-persistence")
(declare-function mevedel-agent-persistence-transcript-path-p
                  "mevedel-agent-persistence" (path save-path))
(autoload 'mevedel-agent-persistence-transcript-path-p
  "mevedel-agent-persistence")

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-agent "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-call-count "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-description "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-tool-use-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-render-data-end-marker
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-render-data-start-marker
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-started-at "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-verdict "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)

;; `mevedel-background-list'
(declare-function mevedel-background-list-refresh-session
                  "mevedel-background-list" (session &optional data-buffer))

;; `mevedel-collaboration-agent'
(declare-function mevedel-collaboration-notify-agents-changed
                  "mevedel-collaboration-agent" (session))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-artifact-present-p
                  "mevedel-session-artifacts"
                  (session logical &optional committed-only))
(declare-function mevedel-session-artifacts-find-artifact-noselect
                  "mevedel-session-artifacts"
                  (session logical &optional inspection committed-only))
(autoload 'mevedel-session-artifacts-artifact-present-p
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-find-artifact-noselect
  "mevedel-session-artifacts")

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
(defvar mevedel--view-buffer)

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-segment-bounds
                  "mevedel-tool-render-data" (tool-use-id))

;; `mevedel-transcript'
(declare-function mevedel-transcript--tool-id-in-range
                  "mevedel-transcript" (start end))

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-properties
                  "mevedel-transcript-restore" (&optional only-if-missing))
(autoload 'mevedel-transcript-restore-properties "mevedel-transcript-restore")

;; `mevedel-utilities'
(declare-function mevedel--duration-label "mevedel-utilities" (seconds))
(autoload 'mevedel--duration-label "mevedel-utilities")
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))

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

;; `mevedel-view-disclosure'
(declare-function mevedel-view-disclosure-apply-rendering-state
                  "mevedel-view-disclosure" (rendering source))
(declare-function mevedel-view-disclosure-record-state
                  "mevedel-view-disclosure" (source vtype collapsed))
(declare-function mevedel-view-disclosure-section-bounds
                  "mevedel-view-disclosure" (&optional property))
(declare-function mevedel-view-disclosure-source-range
                  "mevedel-view-disclosure" (data-buffer start end))
(declare-function mevedel-view-disclosure-state-entry
                  "mevedel-view-disclosure" (source vtype))
(declare-function mevedel-view-toggle-section "mevedel-view-disclosure" ())

;; `mevedel-view-render'
(declare-function mevedel-view-render--full-now
                  "mevedel-view-render" (&optional transcript-buffer source-changed-p))
(declare-function mevedel-view-render-mutate
                  "mevedel-view-render" (key function &optional replacement cleanup))
(declare-function mevedel-view-render-terminal "mevedel-view-render" ())
(defvar mevedel-view-render--terminal-p)
(declare-function mevedel-view--debug-log "mevedel-view-render" (event &rest data))
(declare-function mevedel-view--full-rerender "mevedel-view-render" ())
(declare-function mevedel-view-render-batched-full "mevedel-view-render" ())
(declare-function mevedel-view--insert-rendered-tool "mevedel-view-render" (rendering source))
(declare-function mevedel-view--segment-rendering "mevedel-view-render" (data-buf seg-start seg-end &optional collapsed-only))
(declare-function mevedel-view--tool-block-bounds
                  "mevedel-view-render" (seg-start seg-end))
(declare-function mevedel-view-next-display "mevedel-view-render" ())
(declare-function mevedel-view-previous-display "mevedel-view-render" ())
(declare-function mevedel-view-render-add-display-properties
                  "mevedel-view-render"
                  (start end &optional default-vtype))
(declare-function mevedel-view-toggle-transcript "mevedel-view-render" ())

;; `mevedel-view-segments'
(declare-function mevedel-view-historical-segment-p "mevedel-view-segments" ())

;; `mevedel-view-stream'
(declare-function mevedel-view-stream--stop-now "mevedel-view-stream" ())
(declare-function mevedel-view-stream--release-turn "mevedel-view-stream" ())
(declare-function mevedel-view-stream-in-flight-turn-start-position
                  "mevedel-view-stream" ())
(declare-function mevedel-view-stream-post-tool
                  "mevedel-view-stream" (args))
(declare-function mevedel-view-stream-pre-tool
                  "mevedel-view-stream" (args))
(declare-function mevedel-view-stream-schedule "mevedel-view-stream" ())
(declare-function mevedel-view-stream-set-in-flight-turn-start
                  "mevedel-view-stream" (position))
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
  "Display action when opening an agent transcript inspection window.
Consulted by `mevedel-view-open-agent-transcript' so callers do not
need to set `display-buffer-overriding-action'.  Power users can
override globally or via `display-buffer-alist'.
Use `(display-buffer-same-window)' to replace the selected view;
\\[mevedel-view-close-agent-transcript] returns to the parent.
An existing inspection window is reused; close it before changing placement."
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

(defun mevedel-view-agent-model-label (metadata &optional full)
  "Return a compact model:effort label from METADATA, or nil.
FULL includes the provider.  Unknown effort is omitted.  METADATA is
resolved agent configuration, not the parent or summary-generation model."
  (when-let* ((model (plist-get metadata :model)))
    (replace-regexp-in-string
     "[\n\r\t]+" " "
     (concat (when-let* ((backend (and full (plist-get metadata :backend))))
               (concat backend " / "))
             model
             (when-let* ((effort (plist-get metadata :effort)))
               (concat ":" effort))))))

(defun mevedel-view-agent-model-details (metadata)
  "Return disclosure text for known provider/model/effort in METADATA."
  (when metadata
    (string-join
     (delq nil
           (list (when-let* ((backend (plist-get metadata :backend)))
                   (format "Provider  %s" backend))
                 (when-let* ((model (plist-get metadata :model)))
                   (format "Model     %s" model))
                 (when-let* ((effort (plist-get metadata :effort)))
                   (format "Effort    %s" effort))))
     "\n")))

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
                             (format " · %s" (mevedel--duration-label elapsed))
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

(defvar-local mevedel-view--agent-refresh-pending nil
  "Canonical agent paths waiting for a presentation refresh.")

(defvar-local mevedel-view--agent-refresh-timer nil
  "Timer draining the view's pending agent refreshes one at a time.")

(defvar mevedel-view--agent-parent-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                (lambda (event)
                  (interactive "e")
                  (when-let* ((window (posn-window (event-start event)))
                              ((window-live-p window)))
                    (select-window window)
                    (mevedel-view-close-agent-transcript))))
    map)
  "Header-line map for returning from agent inspection to its parent.")


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
  "TAB" #'mevedel-view-toggle-section
  "<tab>" #'mevedel-view-toggle-section
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
  (mevedel-view-agent-cancel-refresh)
  (add-hook 'kill-buffer-hook #'mevedel-view-agent-cancel-refresh nil t)
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
  "Return the header-line string for a transcript inspection view.
The header line evaluates this on every redisplay of the view.  Optional
fields therefore fit against a running width instead of remeasuring the
growing header, and hover details are built only when shown."
  (let* ((info (mevedel-view--agent-transcript-current-info))
         (agent-path (or mevedel-view--agent-path
                         (plist-get info :agent-path)))
         (badge (mevedel-view-agent--handle-badge
                 (list :status (plist-get info :status)
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
              "unknown"))
         (metadata (plist-get info :model-metadata))
         (parent (when (buffer-live-p mevedel-view--agent-transcript-parent-view)
                   (propertize "[← Parent]" 'face 'link 'mouse-face 'highlight
                               'help-echo "Return to parent view (q)"
                               'local-map mevedel-view--agent-parent-map)))
         (width (when-let* ((window (get-buffer-window (current-buffer))))
                  (window-body-width window)))
         (path (or agent-path "unknown"))
         (parent-width (if parent (string-width parent) 0)))
    ;; Keep the return action, agent identity and state ahead of optional
    ;; metadata.  Favor the agent's leaf name when truncating long paths.
    (when width
      (let ((limit (max 8 (- width parent-width 14))))
        (when (> (string-width badge) limit)
          (setq badge (truncate-string-to-width badge limit nil nil "…"))))
      (let ((budget (max 4 (- width parent-width (string-width badge)
                             (if parent 6 3))))
            (path-width (string-width path)))
        (when (> path-width budget)
          (setq path (concat "…" (truncate-string-to-width
                                  path path-width
                                  (- path-width (1- budget))))))))
    (let* ((parts (delq nil
                        (list parent
                              (propertize
                               path 'help-echo
                               (lambda (&rest _)
                                 (string-join
                                  (delq nil
                                        (list agent-path
                                              (format "Session %s" session-label)
                                              (mevedel-view-agent-model-details
                                               metadata)))
                                  "\n")))
                              (unless (string-empty-p badge) badge))))
           (used (+ (* 3 (1- (length parts)))
                    (apply #'+ (mapcar #'string-width parts))))
           extras)
      ;; Append each optional field that still fits after a separator.
      (cl-flet ((add (text)
                  (when text
                    (let ((text-width (string-width text)))
                      (when (or (null width) (<= (+ used 3 text-width) width))
                        (setq used (+ used 3 text-width))
                        (push (propertize text 'face 'shadow) extras))))))
        (or (add (mevedel-view-agent-model-label metadata t))
            (add (mevedel-view-agent-model-label metadata)))
        (when (integerp calls) (add (format "%d calls" calls)))
        (when (numberp elapsed) (add (mevedel--duration-label elapsed)))
        (add (format "session %s" session-label)))
      (string-join (nconc parts (nreverse extras)) " · "))))

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
    (when-let* ((session (plist-get info :session))
                (path (or mevedel-view--agent-path (plist-get info :agent-path))))
      (setq info (plist-put info :model-metadata
                            (mevedel-agent-control-model-metadata session path))))
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
  (when mevedel-view--agent-transcript-p
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
         mevedel-view--agent-transcript-p)))

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
            ;; Only this view's windows: with ALL, every streamed chunk
            ;; re-evaluated every mode, header and tab line in the frame.
            (force-mode-line-update))
        (error
         (mevedel--warn-once
          'view-agent-live-update
          "Live agent transcript update failed: %s"
          (error-message-string err))))))
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
        (when (and (consp source) (integer-or-marker-p (car source)))
          (mevedel-view-stream-set-in-flight-turn-start last-assistant)
          (setq mevedel-view--data-turn-start
                (with-current-buffer mevedel--data-buffer
                  (copy-marker (car source) nil))))))
    (unless (mevedel-view-stream-in-flight-turn-start-position)
      (mevedel-view-stream-set-in-flight-turn-start mevedel-view--input-marker)
      (setq mevedel-view--data-turn-start
            (with-current-buffer mevedel--data-buffer
              (copy-marker (point-max) nil))))))

(defun mevedel-view-agent-live-transcript-start (invocation)
  "Start live updates in open transcript views for INVOCATION."
  (when-let* (((mevedel-agent-invocation-p invocation))
              (agent-buf (mevedel-agent-invocation-buffer invocation))
              ((buffer-live-p agent-buf)))
    (let ((views (mevedel-view--agent-transcript-views-for-data agent-buf)))
      (dolist (view views)
        (when (buffer-live-p view)
          (with-current-buffer view
            (mevedel-view-render-mutate
             'agent-start
             (lambda ()
               ;; Guard execution, not enqueueing: a nested observer runs
               ;; only after the current projection writer has unwound.
               (condition-case err
                   (progn
                     (mevedel-view-stream--stop-now)
                     (setq mevedel-view-render--terminal-p nil)
                     (setq mevedel-view--agent-transcript-info
                           (plist-put
                            (copy-sequence mevedel-view--agent-transcript-info)
                            :live-buffer t))
                     (mevedel-view-render--full-now)
                     (mevedel-view--agent-transcript-start-streaming)
                     (force-mode-line-update t))
                 (error
                  (mevedel--warn-once
                   'view-agent-start-update
                   "Starting live agent transcript update failed: %s"
                   (error-message-string err)))))
             t))))
      views)))

(defun mevedel-view-agent-live-transcript-finalize (invocation)
  "Refresh live transcript views for finalized INVOCATION.
Returns non-nil when a live transcript view still references the
agent buffer and should keep that buffer alive.  The views become idle
retained projections that can follow another turn."
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
          (with-current-buffer view
            (mevedel-view-render-terminal)
            (mevedel-view-render-mutate
             'agent-finalize
             (lambda ()
               (condition-case err
                   (progn
                     (mevedel-view-stream--stop-now)
                     (let ((info (copy-sequence
                                  mevedel-view--agent-transcript-info)))
                       (setf (plist-get info :live-buffer) nil
                             (plist-get info :status) status
                             (plist-get info :calls) calls
                             (plist-get info :elapsed) elapsed
                             (plist-get info :reason) reason)
                       (setq mevedel-view--agent-transcript-info info))
                     (when (buffer-live-p mevedel--data-buffer)
                       (atomic-change-group
                         (mevedel-view-render--full-now))))
                 (error
                  (mevedel--warn-once
                   'view-agent-final-update
                   "Final live agent transcript update failed: %s"
                   (error-message-string err)))))
             nil #'mevedel-view-stream--release-turn))))
      views)))

(defun mevedel-view-close-agent-transcript ()
  "Close the selected transcript inspection and return to its parent view.
Saved transcript file buffers are killed with the view.  Retained
conversation buffers are left alone."
  (interactive)
  (unless mevedel-view--agent-transcript-p
    (user-error "Not an agent transcript view"))
  (let ((parent mevedel-view--agent-transcript-parent-view))
    (mevedel-view--kill-agent-transcript-view (current-buffer))
    (when (buffer-live-p parent)
      (if-let* ((window (get-buffer-window parent)))
          (select-window window)
        (switch-to-buffer parent)))))


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
      (let ((logical-path (expand-file-name rel-path save-path)))
        (unless (mevedel-session-artifacts-artifact-present-p
                 session rel-path)
          (user-error "Transcript artifact missing: %s" logical-path))
        (append (list :agent-path agent-path
                      :status status
                      :entry entry
                      :session session
                      :save-path save-path
                      :relative-path rel-path
                      :logical-path logical-path
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
                       (not (eq t (window-dedicated-p win)))
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
  (let* ((session (plist-get info :session))
         (session-key
          (or (mevedel-session-session-id session)
              (when (buffer-live-p parent-view)
                (when-let* ((parent-data
                             (buffer-local-value 'mevedel--data-buffer
                                                 parent-view))
                            ((buffer-live-p parent-data)))
                  (buffer-name parent-data)))
              (error "Agent transcript session identity is unavailable")))
         (live-p (plist-get info :live-buffer))
         (agent-data (or (plist-get info :buffer)
                         (mevedel-session-artifacts-find-artifact-noselect
                          session
                          (plist-get info :relative-path)
                          t)))
         (retained-p (mevedel-view--retained-agent-data-p agent-data info))
         (view-name (format "*mevedel-agent:%s:%s*"
                            session-key agent-path))
         (existing (get-buffer view-name))
         (view-name
          (if (and existing
                   (not (with-current-buffer existing
                          (and mevedel-view--agent-transcript-p
                               (eq mevedel--data-buffer agent-data)
                               (equal mevedel-view--agent-path agent-path)
                               (eq (plist-get
                                    mevedel-view--agent-transcript-info
                                    :session)
                                   session)))))
              (generate-new-buffer-name view-name)
            view-name))
         agent-view)
    ;; A cold transcript has no live session binding.  Bind its receiving
    ;; session before view setup so older agent archives remain navigable.
    (with-current-buffer agent-data
      (setq-local mevedel--session session))
    (setq agent-view
          (mevedel-view--ensure
           agent-data view-name
           (list :agent-transcript-p t
                 :agent-path agent-path
                 :parent-view parent-view
                 :preserve-data-view-buffer (or live-p retained-p)
                 :transcript-info info)))
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
      ;; A retained conversation buffer can run another turn while the
      ;; agent is idle now; freezing it makes the next gptel stream
      ;; insertion fail with buffer-read-only.  Only cold transcript
      ;; files are restored and frozen.
      (unless (or live-p retained-p)
        (mevedel-transcript-restore-properties)
        (unless buffer-read-only
          (read-only-mode +1))))
    (with-current-buffer agent-view
      ;; A live gptel stream needs complete render positions before it can
      ;; attach its tail.  Settled inspection needs only a display, so leave
      ;; the old projection visible while canonical preparation is scheduled.
      (if live-p
          (mevedel-view--full-rerender)
        (mevedel-view-render-batched-full))
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
cold transcripts.  Resident retained agents open from their conversation
buffer whether running or idle."
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
  (when-let* ((data mevedel--data-buffer)
              ((buffer-live-p data))
              (session (buffer-local-value 'mevedel--session data))
              (record (mevedel-view--agent-record agent-path)))
    (mevedel-agent-persistence-ensure-conversation
     session record data
     (with-current-buffer data (bound-and-true-p mevedel-session--read-only-mode))))
  (let* ((parent-view (or mevedel-view--agent-transcript-parent-view
                          (current-buffer)))
         (info (mevedel-view--resolve-agent-transcript agent-path))
         (agent-view (mevedel-view--ensure-agent-transcript-view
                      agent-path info parent-view)))
    (mevedel-view--display-agent-transcript-view agent-view)))

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

(defun mevedel-view--agent-status-collect ()
  "Collect status rows for every active agent of the current view's session."
  (mevedel-view-agent-active-rows (mevedel-view--session)))

(defun mevedel-view-agent-active-rows (session)
  "Return status rows for every active agent in SESSION, sorted by path.
Each row carries :path, :status, :activity, :model-metadata, :depth,
:parent-path, :started-at and the :tool-use-id of the Agent call that
started it, when known."
  (let (rows)
    (dolist (pair (and session (mevedel-session-agent-registry session)))
      (let* ((path (car pair))
             (record (cdr pair))
             (status (mevedel-view--agent-record-status record)))
        (when status
          (let ((invocation (mevedel-agent-record-invocation record)))
            (push (list :path path
                        :status status
                        :activity (mevedel-agent-record-activity record)
                        :model-metadata (mevedel-agent-control-record-model-metadata record)
                        :depth (mevedel-view--agent-path-depth path)
                        :parent-path (mevedel-agent-record-parent-path record)
                        :started-at
                        (and (mevedel-agent-invocation-p invocation)
                             (mevedel-agent-invocation-started-at invocation))
                        :tool-use-id
                        (and (mevedel-agent-invocation-p invocation)
                             (mevedel-agent-invocation-parent-tool-use-id invocation)))
                  rows)))))
    (sort rows (lambda (left right)
                 (string-lessp (plist-get left :path)
                               (plist-get right :path))))))



;;
;;; Status and targeted refresh

(defun mevedel-view--render-agent-status ()
  "Render the status zone after a visible agent state change."
  (mevedel-view--render-status)
  ;; An open background-work cockpit lists agents too.
  (when (fboundp 'mevedel-background-list-refresh-session)
    (mevedel-background-list-refresh-session (mevedel-view--session)))
  ;; Retained agents keep changing after the root request becomes idle.
  ;; This is the one funnel for their visible transitions; the room latches
  ;; the resulting roster, so an unchanged state is not broadcast.
  (when (fboundp 'mevedel-collaboration-notify-agents-changed)
    (mevedel-collaboration-notify-agents-changed
     (mevedel-view--session))))

(defun mevedel-view--agent-source-present-p (agent-path)
  "Return non-nil if the data buffer has an Agent source for AGENT-PATH.

The agent conversation owns markers around the render-data block it
patches for live updates.  Reuse those markers so this per-tick check
does not search the transcript."
  (when-let* ((record (mevedel-view--agent-record agent-path))
              (invocation (mevedel-agent-record-invocation record))
              ((mevedel-agent-invocation-p invocation))
              (start
               (mevedel-agent-invocation-render-data-start-marker
                invocation))
              (end
               (mevedel-agent-invocation-render-data-end-marker
                invocation))
              (data-buf (and (boundp 'mevedel--data-buffer)
                             mevedel--data-buffer))
              ((buffer-live-p data-buf))
              ((markerp start))
              ((markerp end)))
    (and (eq (marker-buffer start) data-buf)
         (eq (marker-buffer end) data-buf)
         (< start end))))

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
                   (equal path agent-path))
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
    (let* ((data-buf mevedel--data-buffer)
           (source (get-text-property pos 'mevedel-view-source))
           (tool-use-id
            (and source (buffer-live-p data-buf)
                 (with-current-buffer data-buf
                   (mevedel-transcript--tool-id-in-range
                    (car source) (cdr source)))))
           ;; Replacing metadata at a segment's end can collapse its end
           ;; marker into the deleted block.  Recover the owning call and
           ;; its wrapper: restored properties can separate its metadata.
           (source
            (if (and tool-use-id (buffer-live-p data-buf))
                (when-let* ((range
                            (with-current-buffer data-buf
                              (when-let* ((call
                                          (mevedel-tool-render-data-segment-bounds
                                           tool-use-id)))
                                (or (mevedel-view--tool-block-bounds
                                     (car call) (cdr call))
                                    call)))))
                  (mevedel-view-disclosure-source-range
                   data-buf (car range) (cdr range)))
              source))
           (bounds (mevedel-view-disclosure-section-bounds))
           (current-collapsed (and (get-text-property
                                    pos 'mevedel-view-collapsed)
                                   t))
           (state (mevedel-view-disclosure-state-entry
                   source 'agent-handle))
           (collapsed (if state (cdr state) current-collapsed))
           (turn-id (get-text-property pos 'mevedel-view-turn-id))
           (in-flight-after-section-p
            (and bounds
                 (when-let* ((start (mevedel-view-stream-in-flight-turn-start-position)))
                   (<= (car bounds) start (cdr bounds)))))
           (rendering
            (and bounds source
                 data-buf
                 (buffer-live-p data-buf)
                 (mevedel-view--segment-rendering
                  data-buf (car source) (cdr source) collapsed))))
      (when (and bounds rendering)
        (unless state
          (mevedel-view-disclosure-record-state
           source 'agent-handle collapsed))
        (let* ((view-start (car bounds))
               (view-end (cdr bounds))
               (rendering (mevedel-view-disclosure-apply-rendering-state
                           (plist-put (copy-sequence rendering)
                                      :initially-collapsed-p collapsed)
                           source)))
          ;; Audit disclosures have their own source spans outside BOUNDS.
          ;; Preserve those rows and their fold state when replacing the handle.
          (setq rendering (plist-put rendering :hook-audits nil))
          (goto-char view-start)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((ins-start (point)))
                (delete-region view-start view-end)
                (mevedel-view--insert-rendered-tool rendering source)
                (mevedel-view-render-add-display-properties
                 ins-start (point) (plist-get rendering :vtype))
                (when turn-id
                  (put-text-property ins-start (point)
                                     'mevedel-view-turn-id turn-id))
                (when in-flight-after-section-p
                  (set-marker mevedel-view--in-flight-turn-start ins-start)))
            (set-marker-insertion-type mevedel-view--input-marker nil))
          t)))))

(defun mevedel-view--refresh-agent-rendering-now (agent-path)
  "Refresh visible rendering for AGENT-PATH in the current view buffer."
  (mevedel-view-render-mutate
   (list 'agent agent-path)
   (lambda ()
     (if (mevedel-view-historical-segment-p)
         (mevedel-view--render-agent-status)
       (mevedel-view-agent--refresh-owned agent-path)))))

(defun mevedel-view-agent--refresh-owned (agent-path)
  "Rediscover and refresh AGENT-PATH's handles with projection ownership held."
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

(defun mevedel-view-agent-cancel-refresh ()
  "Cancel pending agent presentation work in the current view."
  (when (timerp mevedel-view--agent-refresh-timer)
    (cancel-timer mevedel-view--agent-refresh-timer))
  (setq mevedel-view--agent-refresh-timer nil
        mevedel-view--agent-refresh-pending nil))

(defun mevedel-view-agent--flush-refresh (view-buffer)
  "Refresh one queued agent in VIEW-BUFFER, then return control to Emacs."
  (when (buffer-live-p view-buffer)
    (with-current-buffer view-buffer
      (when (timerp mevedel-view--agent-refresh-timer)
        (cancel-timer mevedel-view--agent-refresh-timer))
      (setq mevedel-view--agent-refresh-timer nil)
      (when-let* ((path (pop mevedel-view--agent-refresh-pending)))
        (unwind-protect
            (mevedel-view--refresh-agent-rendering-now path)
          (when (and (buffer-live-p view-buffer)
                     mevedel-view--agent-refresh-pending
                     (not (timerp mevedel-view--agent-refresh-timer)))
            (setq mevedel-view--agent-refresh-timer
                  (run-at-time 0.001 nil #'mevedel-view-agent--flush-refresh
                               view-buffer))))))))

(defun mevedel-view-refresh-agent-rendering (view-buffer agent-path)
  "Queue a coalesced refresh of AGENT-PATH in VIEW-BUFFER.
One timer drains distinct agents in separate callbacks so a burst of completions
does not schedule many expensive refreshes to expire together."
  (when (and agent-path (buffer-live-p view-buffer))
    (with-current-buffer view-buffer
      (if (or (not (numberp mevedel-view-agent-refresh-delay))
              (<= mevedel-view-agent-refresh-delay 0))
          (progn
            (setq mevedel-view--agent-refresh-pending
                  (delete agent-path mevedel-view--agent-refresh-pending))
            (mevedel-view--refresh-agent-rendering-now agent-path))
        (unless (member agent-path mevedel-view--agent-refresh-pending)
          (setq mevedel-view--agent-refresh-pending
                (nconc mevedel-view--agent-refresh-pending (list agent-path))))
        (unless (timerp mevedel-view--agent-refresh-timer)
          (setq mevedel-view--agent-refresh-timer
                (run-at-time mevedel-view-agent-refresh-delay nil
                             #'mevedel-view-agent--flush-refresh view-buffer)))))))

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
                         (list 'font-lock-face 'mevedel-view-mailbox-header)
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
         font-lock-face link
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

(provide 'mevedel-view-agent)

;;; mevedel-view-agent.el ends here
