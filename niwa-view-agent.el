;;; niwa-view-agent.el --- Agent presentation and transcript views -*- lexical-binding: t -*-

;;; Commentary:

;; Owns agent transcript lookup and inspection, live-agent status rows for the
;; background-work panel, attribution controls, and targeted agent refreshes
;; in niwa view buffers.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-structs)
(require 'niwa-utilities)
(require 'niwa-view-zone)

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `cl-seq'
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `niwa-agent-control'
(declare-function niwa-agent-control-active-activity-p
                  "niwa-agent-control" (activity))
(declare-function niwa-agent-control-model-metadata
                  "niwa-agent-control" (session path))
(declare-function niwa-agent-control-record-model-metadata
                  "niwa-agent-control" (record))
(declare-function niwa-agent-control-retained-buffer-p
                  "niwa-agent-control" (session buffer))
(declare-function niwa-agent-record-activity
                  "niwa-agent-control" (cl-x) t)
(declare-function niwa-agent-record-conversation-buffer
                  "niwa-agent-control" (cl-x) t)
(declare-function niwa-agent-record-id
                  "niwa-agent-control" (cl-x) t)
(declare-function niwa-agent-record-invocation
                  "niwa-agent-control" (cl-x) t)
(declare-function niwa-agent-record-parent-path
                  "niwa-agent-control" (cl-x) t)
(declare-function niwa-agent-record-path
                  "niwa-agent-control" (cl-x) t)
(declare-function niwa-agent-record-role
                  "niwa-agent-control" (cl-x) t)
(autoload 'niwa-agent-control-model-metadata "niwa-agent-control")
(autoload 'niwa-agent-control-record-model-metadata "niwa-agent-control")

;; `niwa-agent-conversation'
(defvar niwa--agent-invocation)

;; `niwa-agent-persistence'
(declare-function niwa-agent-persistence-ensure-conversation
                  "niwa-agent-persistence"
                  (session record root-buffer &optional readonly-p))
(autoload 'niwa-agent-persistence-ensure-conversation "niwa-agent-persistence")
(declare-function niwa-agent-persistence-transcript-path-p
                  "niwa-agent-persistence" (path save-path))
(autoload 'niwa-agent-persistence-transcript-path-p
  "niwa-agent-persistence")

;; `niwa-agents'
(declare-function niwa-agent-invocation-agent "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-buffer "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-call-count "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-description "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-parent-tool-use-id
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-render-data-end-marker
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-render-data-start-marker
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-started-at "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-terminal-reason "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-transcript-status "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-verdict "niwa-agents" (cl-x) t)
(declare-function niwa-agent-name "niwa-agents" (cl-x) t)

;; `niwa-background-list'
(declare-function niwa-background-list-refresh-session
                  "niwa-background-list" (session &optional data-buffer))

;; `niwa-collaboration-agent'
(declare-function niwa-collaboration-notify-agents-changed
                  "niwa-collaboration-agent" (session))

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-artifact-present-p
                  "niwa-session-artifacts"
                  (session logical &optional committed-only))
(declare-function niwa-session-artifacts-find-artifact-noselect
                  "niwa-session-artifacts"
                  (session logical &optional inspection committed-only))
(autoload 'niwa-session-artifacts-artifact-present-p
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-find-artifact-noselect
  "niwa-session-artifacts")

;; `niwa-structs'
(declare-function niwa-session-agent-transcripts "niwa-structs" (cl-x) t)
(declare-function niwa-session-agent-registry "niwa-structs" (cl-x) t)
(declare-function niwa-session-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-queue
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(declare-function niwa-session-session-id "niwa-structs" (cl-x) t)
(defvar niwa--data-buffer)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-tool-render-data'
(declare-function niwa-tool-render-data-segment-bounds
                  "niwa-tool-render-data" (tool-use-id))

;; `niwa-transcript'
(declare-function niwa-transcript--tool-id-in-range
                  "niwa-transcript" (start end))

;; `niwa-transcript-restore'
(declare-function niwa-transcript-restore-properties
                  "niwa-transcript-restore" (&optional only-if-missing))
(autoload 'niwa-transcript-restore-properties "niwa-transcript-restore")

;; `niwa-utilities'
(declare-function niwa--duration-label "niwa-utilities" (seconds))
(declare-function niwa--warn-once
                  "niwa-utilities" (scope key format &rest args))
(declare-function niwa-diagnostic-message "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format &rest args))
(autoload 'niwa--duration-label "niwa-utilities")
(autoload 'niwa--warn-once "niwa-utilities")
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")


;; `niwa-view'
(declare-function niwa-view--display-fragment-keymap "niwa-view" (&rest maps))
(declare-function niwa-view--ensure "niwa-view" (data-buf &optional view-name options))
(declare-function niwa-view--indent-region-lines
                  "niwa-view" (start end prefix))
(declare-function niwa-view--render-status "niwa-view" (&optional data-buf))
(declare-function niwa-view--status-anchor "niwa-view" ())
(declare-function niwa-view--status-trailing-newline-suffix "niwa-view" (body))
(declare-function niwa-view--zone-separator "niwa-view" (label))
(declare-function niwa-view-activate-at-point "niwa-view" (&optional event))
(declare-function niwa-view-rerender "niwa-view" (&optional buffer))
(defvar niwa-view-mode-map)

;; `niwa-view-composer'
(declare-function niwa-view--transcript-gptel-send-blocked
                  "niwa-view-composer" (&optional arg))

;; `niwa-view-disclosure'
(declare-function niwa-view-disclosure-apply-rendering-state
                  "niwa-view-disclosure" (rendering source))
(declare-function niwa-view-disclosure-record-state
                  "niwa-view-disclosure" (source vtype collapsed))
(declare-function niwa-view-disclosure-section-bounds
                  "niwa-view-disclosure" (&optional property))
(declare-function niwa-view-disclosure-source-range
                  "niwa-view-disclosure" (data-buffer start end))
(declare-function niwa-view-disclosure-state-entry
                  "niwa-view-disclosure" (source vtype))
(declare-function niwa-view-toggle-section "niwa-view-disclosure" ())

;; `niwa-view-render'
(declare-function niwa-view--retire-tool-renderings
                  "niwa-view-render" (data-buf start end))
(declare-function niwa-view-render--full-now
                  "niwa-view-render" (&optional transcript-buffer source-changed-p))
(declare-function niwa-view-render-mutate
                  "niwa-view-render" (key function &optional replacement cleanup))
(declare-function niwa-view-render-terminal "niwa-view-render" ())
(defvar niwa-view-render--terminal-p)
(declare-function niwa-view--debug-log "niwa-view-render" (event &rest data))
(declare-function niwa-view--full-rerender "niwa-view-render" ())
(declare-function niwa-view-render-batched-full "niwa-view-render" ())
(declare-function niwa-view--insert-rendered-tool "niwa-view-render" (rendering source))
(declare-function niwa-view--segment-rendering "niwa-view-render" (data-buf seg-start seg-end &optional collapsed-only))
(declare-function niwa-view--tool-block-bounds
                  "niwa-view-render" (seg-start seg-end))
(declare-function niwa-view-next-display "niwa-view-render" ())
(declare-function niwa-view-previous-display "niwa-view-render" ())
(declare-function niwa-view-render-add-display-properties
                  "niwa-view-render"
                  (start end &optional default-vtype))
(declare-function niwa-view-toggle-transcript "niwa-view-render" ())

;; `niwa-view-segments'
(declare-function niwa-view-historical-segment-p "niwa-view-segments" ())

;; `niwa-view-stream'
(declare-function niwa-view-stream--stop-now "niwa-view-stream" ())
(declare-function niwa-view-stream--release-turn "niwa-view-stream" ())
(declare-function niwa-view-stream-in-flight-turn-start-position
                  "niwa-view-stream" ())
(declare-function niwa-view-stream-post-tool
                  "niwa-view-stream" (args))
(declare-function niwa-view-stream-pre-tool
                  "niwa-view-stream" (args))
(declare-function niwa-view-stream-schedule "niwa-view-stream" ())
(declare-function niwa-view-stream-set-in-flight-turn-start
                  "niwa-view-stream" (position))
(declare-function niwa-view-stream-stop "niwa-view-stream" ())
(defvar niwa-view--data-turn-start)
(defvar niwa-view--in-flight-turn-start)

;; `niwa-view-viewport'
(declare-function niwa-view-call-preserving-user-view-state
                  "niwa-view-viewport" (thunk &optional region follow-output))
(declare-function niwa-view-transcript-tail "niwa-view-viewport" ())
(declare-function niwa-view-transcript-tail-marker "niwa-view-viewport" ())

;; `niwa-view-zone'
(declare-function niwa-view-zone-collapse-state "niwa-view-zone" (key &optional default))
(declare-function niwa-view-zone-collapse-state-set-p "niwa-view-zone" (key))
(declare-function niwa-view-zone-set-collapse-state "niwa-view-zone" (key collapsed))


;;
;;; Customization

(defcustom niwa-agent-view-display-action
  '(display-buffer-in-side-window
    (side . right) (slot . 0) (window-width . 0.4))
  "Display action when opening an agent transcript inspection window.
Consulted by `niwa-view-open-agent-transcript' so callers do not
need to set `display-buffer-overriding-action'.  Power users can
override globally or via `display-buffer-alist'.
Use `(display-buffer-same-window)' to replace the selected view;
\\[niwa-view-close-agent-transcript] returns to the parent.
An existing inspection window is reused; close it before changing placement."
  :type 'sexp
  :group 'niwa)


(defcustom niwa-view-agent-refresh-delay 0.05
  "Seconds to coalesce live agent handle refreshes.
Agent tool start/finish hooks can arrive in bursts.  This delay keeps those
bursts from repainting the same handle repeatedly while still updating live
badges promptly."
  :type 'number
  :group 'niwa)


;;
;;; Presentation state


(defvar-local niwa-view--agent-path nil
  "Canonical agent path rendered by an agent transcript inspection view.")

(defvar-local niwa-view--agent-transcript-info nil
  "Resolved transcript metadata plist for an agent transcript inspection view.")

(defvar-local niwa-view--agent-transcript-parent-view nil
  "Parent session view buffer that opened this transcript inspection view.")

(defvar-local niwa-view--agent-transcript-window nil
  "Side window currently displaying an agent transcript for this view buffer.")

(defvar niwa-view--agent-transcript-data-kill-in-progress nil
  "Non-nil while transcript view/data kill hooks are already paired.")


;;;; Status helpers

(defun niwa-view-agent-model-label (metadata &optional full)
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

(defun niwa-view-agent-model-details (metadata)
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

(defun niwa-view-agent--handle-badge (render-data)
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
                             (format " · %s" (niwa--duration-label elapsed))
                           "")))
    (if blocked-reason
        (propertize (format "[blocked · awaiting %s]" blocked-reason)
                    'font-lock-face 'niwa-view-handle-blocked)
      (pcase status
        ('running
         (propertize (format "[running%s]" calls-suffix)
                     'font-lock-face 'niwa-view-handle-running))
        ('completed
         (pcase verdict
           ('fail
            (propertize (format "✗ verdict FAIL%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'niwa-view-handle-error))
           ('partial
            (propertize (format "○ verdict PARTIAL%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'niwa-view-handle-error))
           ('pass
            (propertize (format "✓ verdict PASS%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'niwa-view-handle-done))
           (_
            (propertize (format "✓ done%s%s" elapsed-suffix calls-suffix)
                        'font-lock-face 'niwa-view-handle-done))))
        ('error
         (propertize (format "✗ error%s"
                             (if reason (format " · %s" reason) ""))
                     'font-lock-face 'niwa-view-handle-error))
        ('aborted
         (propertize "✗ aborted"
                     'font-lock-face 'niwa-view-handle-error))
        ('incomplete
         (propertize "○ incomplete"
                     'font-lock-face 'niwa-view-handle-error))
        (_ "")))))

(defun niwa-view-agent--blocked-reason (path session)
  "Return the visible blocked reason for canonical PATH in SESSION, or nil."
  (when (and path session)
    (when (cl-some (lambda (entry)
                     (equal (plist-get entry :origin) path))
                   (niwa-session-permission-queue session))
      "permission")))


;;;; Aggregate state and keymaps

(defvar-local niwa-view--agent-refresh-pending nil
  "Canonical agent paths waiting for a presentation refresh.")

(defvar-local niwa-view--agent-refresh-timer nil
  "Timer draining the view's pending agent refreshes one at a time.")

(defvar niwa-view--agent-parent-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line mouse-1]
                (lambda (event)
                  (interactive "e")
                  (when-let* ((window (posn-window (event-start event)))
                              ((window-live-p window)))
                    (select-window window)
                    (niwa-view-close-agent-transcript))))
    map)
  "Tab-line map for returning from agent inspection to its parent.")


(defvar-keymap niwa-view--agent-handle-map
  :doc "Keymap active on non-label text in Agent handles.
The visible agent path carries its own transcript-opening
keymap; the rest of the handle remains navigable without opening the
transcript on click."
  "TAB" #'niwa-view-toggle-section
  "n" #'niwa-view-next-display
  "p" #'niwa-view-previous-display
  "t" #'niwa-view-toggle-transcript
  "q" #'niwa-view-close-agent-transcript)

(defvar-keymap niwa-view--agent-label-map
  :doc "Keymap active on the visible path in Agent handles."
  "TAB" #'niwa-view-toggle-section
  "<tab>" #'niwa-view-toggle-section
  "RET" #'niwa-view-activate-at-point
  "<mouse-1>" #'niwa-view-activate-at-point
  "<mouse-2>" #'niwa-view-activate-at-point)


(defun niwa-view-agent-initialize (options data-buffer)
  "Initialize agent presentation state for DATA-BUFFER from OPTIONS."
  (setq-local niwa-view--agent-transcript-p
              (and (plist-get options :agent-transcript-p) t))
  (setq-local niwa-view--agent-path (plist-get options :agent-path))
  (setq-local niwa-view--agent-transcript-info
              (plist-get options :transcript-info))
  (setq-local niwa-view--agent-transcript-parent-view
              (plist-get options :parent-view))
  (niwa-view-agent-cancel-refresh)
  (add-hook 'kill-buffer-hook #'niwa-view-agent-cancel-refresh nil t)
  (when niwa-view--agent-transcript-p
    (use-local-map (copy-keymap niwa-view-mode-map))
    (local-set-key (kbd "q") #'niwa-view-close-agent-transcript)
    (local-set-key [remap gptel-send]
                   #'niwa-view--transcript-gptel-send-blocked)
    ;; The agent header is this view's status strip.  The header line below
    ;; it stays free for the pinned prompt, as in the main view.
    (setq-local tab-line-format
                '(:eval (niwa-view--agent-transcript-status-strip)))
    (when (plist-get options :preserve-data-view-buffer)
      (with-current-buffer data-buffer
        (add-hook 'kill-buffer-hook
                  #'niwa-view--on-agent-transcript-data-killed nil t)))))


;;
;;; Transcript inspection

(defun niwa-view--agent-terminal-status-p (status)
  "Return non-nil when STATUS names a terminal agent transcript state."
  (memq status '(completed error aborted incomplete)))

(defun niwa-view--agent-effective-status (inv entry)
  "Return the effective visible status from live INV and sidecar ENTRY.
Terminal live invocation state wins over stale sidecar metadata.  If
there is no terminal live state, terminal sidecar metadata wins over a
non-terminal live state so a finalized transcript does not regress to
running in the UI."
  (let ((inv-status (and inv
                         (niwa-agent-invocation-transcript-status inv)))
        (entry-status (and entry (plist-get entry :status))))
    (cond
     ((niwa-view--agent-terminal-status-p inv-status) inv-status)
     ((niwa-view--agent-terminal-status-p entry-status) entry-status)
     (inv-status inv-status)
     (t entry-status))))

(defun niwa-view--agent-transcript-status-strip ()
  "Return the tab-line status strip for a transcript inspection view.
The tab line evaluates this on every redisplay of the view.  Optional
fields therefore fit against a running width instead of remeasuring the
growing header, and hover details are built only when shown."
  (let* ((info (niwa-view--agent-transcript-current-info))
         (agent-path (or niwa-view--agent-path
                         (plist-get info :agent-path)))
         (badge (niwa-view-agent--handle-badge
                 (list :status (plist-get info :status)
                       :reason (plist-get info :reason)
                       :blocked-reason
                       (plist-get info :blocked-reason))))
         (calls (plist-get info :calls))
         (elapsed (plist-get info :elapsed))
         (session-label
          (or (plist-get info :session-label)
              (when-let* ((session (plist-get info :session)))
                (or (niwa-session-session-id session)
                    (niwa-session-name session)))
              (when-let* ((parent niwa-view--agent-transcript-parent-view)
                          ((buffer-live-p parent))
                          (parent-data
                           (buffer-local-value 'niwa--data-buffer parent))
                          ((buffer-live-p parent-data))
                          (session (buffer-local-value 'niwa--session
                                                       parent-data)))
                (or (niwa-session-session-id session)
                    (niwa-session-name session)))
              "unknown"))
         (metadata (plist-get info :model-metadata))
         (parent (when (buffer-live-p niwa-view--agent-transcript-parent-view)
                   (propertize "[← Parent]" 'face 'link 'mouse-face 'highlight
                               'help-echo "Return to parent view (q)"
                               'local-map niwa-view--agent-parent-map)))
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
                                              (niwa-view-agent-model-details
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
        (or (add (niwa-view-agent-model-label metadata t))
            (add (niwa-view-agent-model-label metadata)))
        (when (integerp calls) (add (format "%d calls" calls)))
        (when (numberp elapsed) (add (niwa--duration-label elapsed)))
        (add (format "session %s" session-label)))
      (string-join (nconc parts (nreverse extras)) " · "))))

(defun niwa-view--agent-transcript-current-info ()
  "Return transcript metadata refreshed from the live invocation."
  (let* ((info (copy-sequence niwa-view--agent-transcript-info))
         (data-buf niwa--data-buffer)
         (inv (and (buffer-live-p data-buf)
                   (buffer-local-value 'niwa--agent-invocation data-buf))))
    (when (and (plist-get info :live-buffer)
               (niwa-agent-invocation-p inv))
      (let* ((started (niwa-agent-invocation-started-at inv))
             (session (and (buffer-live-p data-buf)
                           (buffer-local-value 'niwa--session data-buf)))
             (blocked-reason
              (niwa-view-agent--blocked-reason
               niwa-view--agent-path session)))
        (setq info
              (append
               (list :status
                     (niwa-agent-invocation-transcript-status inv)
                     :calls (niwa-agent-invocation-call-count inv)
                     :reason (niwa-agent-invocation-terminal-reason inv)
                     :blocked-reason blocked-reason)
               (when started
                 (list :elapsed
                       (float-time
                        (time-subtract (current-time) started))))
               info))))
    (when-let* ((session (plist-get info :session))
                (path (or niwa-view--agent-path (plist-get info :agent-path))))
      (setq info (plist-put info :model-metadata
                            (niwa-agent-control-model-metadata session path))))
    info))

(defun niwa-view--clear-parent-transcript-window ()
  "Clear this transcript view from its parent view's singleton slot."
  (when-let* ((parent niwa-view--agent-transcript-parent-view)
              ((buffer-live-p parent)))
    (with-current-buffer parent
      (setq niwa-view--agent-transcript-window nil))))

(defun niwa-view--retained-agent-data-p (data-buffer transcript-info)
  "Return non-nil when TRANSCRIPT-INFO's session retains DATA-BUFFER."
  (and (fboundp 'niwa-agent-control-retained-buffer-p)
       (niwa-agent-control-retained-buffer-p
        (plist-get transcript-info :session) data-buffer)))

(defun niwa-view-agent-handle-view-kill ()
  "Clean up the current transcript view and return non-nil when handled."
  (when niwa-view--agent-transcript-p
    (let ((data-buffer niwa--data-buffer)
          (view-buffer (current-buffer))
          (live-p (plist-get niwa-view--agent-transcript-info
                             :live-buffer)))
      (niwa-view--clear-parent-transcript-window)
      (when (buffer-live-p data-buffer)
        (with-current-buffer data-buffer
          (when (eq niwa--view-buffer view-buffer)
            (setq niwa--view-buffer nil)))
        (unless (or live-p
                    (niwa-view--retained-agent-data-p
                     data-buffer niwa-view--agent-transcript-info)
                    niwa-view--agent-transcript-data-kill-in-progress)
          (let ((niwa-view--agent-transcript-data-kill-in-progress t))
            (kill-buffer data-buffer)))))
    t))

(defun niwa-view--agent-transcript-view-p (buffer)
  "Return non-nil when BUFFER is an agent transcript inspection view."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         niwa-view--agent-transcript-p)))

(defun niwa-view--agent-transcript-views-for-parent (parent-view)
  "Return transcript inspection views opened from PARENT-VIEW."
  (let (views)
    (dolist (buf (buffer-list) (nreverse views))
      (when (and (niwa-view--agent-transcript-view-p buf)
                 (with-current-buffer buf
                   (eq niwa-view--agent-transcript-parent-view
                       parent-view)))
        (push buf views)))))

(defun niwa-view--agent-transcript-views-for-data (data-buffer)
  "Return transcript inspection views rendering DATA-BUFFER."
  (let (views)
    (dolist (buf (buffer-list) (nreverse views))
      (when (and (niwa-view--agent-transcript-view-p buf)
                 (with-current-buffer buf
                   (eq niwa--data-buffer data-buffer)))
        (push buf views)))))

(defun niwa-view--kill-agent-transcript-view
    (view-buffer &optional kill-retained)
  "Kill transcript inspection VIEW-BUFFER and disposable data.
Also kill retained conversation data when KILL-RETAINED is non-nil."
  (when (niwa-view--agent-transcript-view-p view-buffer)
    (let ((windows (niwa-view--windows 'visibility view-buffer))
          data-buffer live-p transcript-info)
      (with-current-buffer view-buffer
        (setq data-buffer niwa--data-buffer
              transcript-info niwa-view--agent-transcript-info
              live-p (plist-get niwa-view--agent-transcript-info
                                :live-buffer)))
      (dolist (win windows)
        (ignore-errors
          (quit-window nil win)))
      (when (buffer-live-p view-buffer)
        (kill-buffer view-buffer))
      (when (and (not live-p)
                 data-buffer
                 (buffer-live-p data-buffer)
                 (or kill-retained
                     (not (niwa-view--retained-agent-data-p
                           data-buffer transcript-info))))
        (let ((niwa-view--agent-transcript-data-kill-in-progress t))
          (kill-buffer data-buffer))))))

(defun niwa-view-agent-cleanup-parent (parent-view)
  "Kill every transcript inspection view opened from PARENT-VIEW."
  (dolist (view (niwa-view--agent-transcript-views-for-parent parent-view))
    (niwa-view--kill-agent-transcript-view view t)))

(defun niwa-view--on-agent-transcript-data-killed ()
  "Kill live transcript inspection views when their data buffer dies."
  (unless niwa-view--agent-transcript-data-kill-in-progress
    (let ((data-buffer (current-buffer))
          (niwa-view--agent-transcript-data-kill-in-progress t))
      (dolist (view (niwa-view--agent-transcript-views-for-data data-buffer))
        (when (buffer-live-p view)
          (kill-buffer view))))))

(defun niwa-view--agent-live-transcript-dispatch (function &rest args)
  "Call stream FUNCTION with ARGS for every view of the current agent buffer."
  (let* ((agent-buf (current-buffer))
         (scope (niwa-diagnostic-scope agent-buf)))
    (dolist (view (niwa-view--agent-transcript-views-for-data agent-buf))
      (condition-case err
          (with-current-buffer view
            (atomic-change-group
              (with-current-buffer agent-buf
                (let ((niwa--view-buffer view))
                  (apply function args))))
            ;; Only this view's windows: with ALL, every streamed chunk
            ;; re-evaluated every mode, header and tab line in the frame.
            (force-mode-line-update))
        (error
         (niwa--warn-once
          scope
          'view-agent-live-update
          "%s (Live agent transcript update failed)"
          (error-message-string err))))))
  nil)

(defun niwa-view-agent-live-transcript-stream ()
  "Fan out a streamed agent chunk to open transcript inspection views."
  (niwa-view--agent-live-transcript-dispatch
   #'niwa-view-stream-schedule))

(defun niwa-view-agent-live-transcript-pre-tool (args)
  "Fan out the agent pre-tool hook ARGS to open transcript views."
  (niwa-view--agent-live-transcript-dispatch
   #'niwa-view-stream-pre-tool args))

(defun niwa-view-agent-live-transcript-post-tool (args)
  "Fan out the agent post-tool hook ARGS to open transcript views."
  (niwa-view--agent-live-transcript-dispatch
   #'niwa-view-stream-post-tool args))

(defun niwa-view--agent-transcript-start-streaming ()
  "Anchor incremental rendering to the last rendered assistant turn."
  (let ((pos (point-min))
        last-assistant)
    (while (< pos (point-max))
      (when (eq (get-text-property pos 'niwa-view-turn-role) 'assistant)
        (setq last-assistant pos))
      (setq pos (or (next-single-property-change
                     pos 'niwa-view-turn-role nil (point-max))
                    (point-max))))
    (when last-assistant
      (let ((turn-id (get-text-property last-assistant
                                        'niwa-view-turn-id))
            (source-pos last-assistant)
            source)
        (while (and (< source-pos (point-max))
                    (eq (get-text-property source-pos
                                           'niwa-view-turn-id)
                        turn-id)
                    (not source))
          (setq source (get-text-property source-pos 'niwa-view-source))
          (setq source-pos (1+ source-pos)))
        (when (and (consp source) (integer-or-marker-p (car source)))
          (niwa-view-stream-set-in-flight-turn-start last-assistant)
          (setq niwa-view--data-turn-start
                (with-current-buffer niwa--data-buffer
                  (copy-marker (car source) nil))))))
    (unless (niwa-view-stream-in-flight-turn-start-position)
      (niwa-view-stream-set-in-flight-turn-start
       (niwa-view-transcript-tail-marker))
      (setq niwa-view--data-turn-start
            (with-current-buffer niwa--data-buffer
              (copy-marker (point-max) nil))))))

(defun niwa-view-agent-live-transcript-start (invocation)
  "Start live updates in open transcript views for INVOCATION."
  (when-let* (((niwa-agent-invocation-p invocation))
              (agent-buf (niwa-agent-invocation-buffer invocation))
              ((buffer-live-p agent-buf)))
    (let ((views (niwa-view--agent-transcript-views-for-data agent-buf))
          (scope (niwa-diagnostic-scope agent-buf)))
      (dolist (view views)
        (when (buffer-live-p view)
          (with-current-buffer view
            (niwa-view-render-mutate
             'agent-start
             (lambda ()
               ;; Guard execution, not enqueueing: a nested observer runs
               ;; only after the current projection writer has unwound.
               (condition-case err
                   (progn
                     (niwa-view-stream--stop-now)
                     (setq niwa-view-render--terminal-p nil)
                     (setq niwa-view--agent-transcript-info
                           (plist-put
                            (copy-sequence niwa-view--agent-transcript-info)
                            :live-buffer t))
                     (niwa-view-render--full-now)
                     (niwa-view--agent-transcript-start-streaming)
                     (force-mode-line-update t))
                 (error
                  (niwa--warn-once
                   scope
                   'view-agent-start-update
                   "%s (Starting live agent transcript update failed)"
                   (error-message-string err)))))
             t))))
      views)))

(defun niwa-view-agent-live-transcript-finalize (invocation)
  "Refresh live transcript views for finalized INVOCATION.
Returns non-nil when a live transcript view still references the
agent buffer and should keep that buffer alive.  The views become idle
retained projections that can follow another turn."
  (when-let* (((niwa-agent-invocation-p invocation))
              (agent-buf (niwa-agent-invocation-buffer invocation))
              ((buffer-live-p agent-buf)))
    (let* ((views (niwa-view--agent-transcript-views-for-data agent-buf))
           (scope (niwa-diagnostic-scope agent-buf))
           (status (niwa-agent-invocation-transcript-status invocation))
           (started (niwa-agent-invocation-started-at invocation))
           (elapsed (and started
                         (float-time
                          (time-subtract (current-time) started))))
           (reason (niwa-agent-invocation-terminal-reason invocation))
           (calls (niwa-agent-invocation-call-count invocation)))
      (dolist (view views)
        (when (buffer-live-p view)
          (with-current-buffer view
            (niwa-view-render-terminal)
            (niwa-view-render-mutate
             'agent-finalize
             (lambda ()
               (condition-case err
                   (progn
                     (niwa-view-stream--stop-now)
                     (let ((info (copy-sequence
                                  niwa-view--agent-transcript-info)))
                       (setf (plist-get info :live-buffer) nil
                             (plist-get info :status) status
                             (plist-get info :calls) calls
                             (plist-get info :elapsed) elapsed
                             (plist-get info :reason) reason)
                       (setq niwa-view--agent-transcript-info info))
                     (when (buffer-live-p niwa--data-buffer)
                       (atomic-change-group
                         (niwa-view-render--full-now))))
                 (error
                  (niwa--warn-once
                   scope
                   'view-agent-final-update
                   "%s (Final live agent transcript update failed)"
                   (error-message-string err)))))
             nil #'niwa-view-stream--release-turn))))
      views)))

(defun niwa-view-quit ()
  "Close an agent transcript inspection, or quit the selected view window."
  (interactive)
  (if niwa-view--agent-transcript-p
      (niwa-view-close-agent-transcript)
    (quit-window)))

(defun niwa-view-close-agent-transcript ()
  "Close the selected transcript inspection and return to its parent view.
Saved transcript file buffers are killed with the view.  Retained
conversation buffers are left alone."
  (interactive)
  (unless niwa-view--agent-transcript-p
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Not an agent transcript view"))
  (let ((parent niwa-view--agent-transcript-parent-view))
    (niwa-view--kill-agent-transcript-view (current-buffer))
    (when (buffer-live-p parent)
      (if-let* ((window (get-buffer-window parent)))
          (select-window window)
        (switch-to-buffer parent)))))


;;
;;; Transcript opening and status

(defun niwa-view-open-agent-transcript-at-point (&optional event)
  "Open the transcript referenced by the attribution at point or EVENT.

When EVENT is a mouse event outside an attribution, just move point as
usual.  Keyboard invocation outside an attribution signals a user error."
  (interactive (list last-nonmenu-event))
  (let* ((event-pos (and event
                         (eventp event)
                         (posn-point (event-end event))))
         (pos (if (integer-or-marker-p event-pos) event-pos (point)))
         (agent-path (get-text-property pos 'niwa-view-agent-path)))
    (cond
     (agent-path
     (when (and event (eventp event))
        (mouse-set-point event))
      (niwa-view-agent-handle-activate agent-path))
     ((and event (eventp event))
      (mouse-set-point event))
     (t
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No transcript at point")))))

(defun niwa-view--session ()
  "Return the root session displayed by the current view, or nil."
  (when-let* ((data-buf (and (boundp 'niwa--data-buffer)
                             niwa--data-buffer))
              ((buffer-live-p data-buf)))
    (buffer-local-value 'niwa--session data-buf)))

(defun niwa-view--agent-record (agent-path)
  "Return the retained record for canonical AGENT-PATH, or nil."
  (when-let* ((session (niwa-view--session)))
    (cdr (assoc agent-path (niwa-session-agent-registry session)))))

(defun niwa-view--lookup-transcript-pair (agent-path)
  "Return the transcript sidecar pair for canonical AGENT-PATH."
  (when-let* ((session (niwa-view--session)))
    (let ((entries (niwa-session-agent-transcripts session)))
      (if-let* ((record (niwa-view--agent-record agent-path)))
          (assoc (niwa-agent-record-id record) entries)
        (cl-find-if
         (lambda (entry)
           (equal agent-path (plist-get (cdr entry) :agent-path)))
         entries)))))

(defun niwa-view--resolve-agent-transcript (agent-path)
  "Return transcript info for canonical AGENT-PATH.
Retained agents resolve through their conversation buffer when it is resident;
otherwise their validated transcript file is opened.
Signals `user-error' when no transcript source can be opened."
  (let* ((data-buf (and (boundp 'niwa--data-buffer)
                        niwa--data-buffer))
         (session (and data-buf (buffer-live-p data-buf)
                       (buffer-local-value 'niwa--session data-buf)))
         (record (and session (niwa-view--agent-record agent-path)))
         (pair (niwa-view--lookup-transcript-pair agent-path))
         (entry (cdr pair))
         (inv (and record (niwa-view--agent-invocation agent-path)))
         (status (niwa-view--agent-effective-status inv entry))
         (conversation-buffer
          (and record (niwa-agent-record-conversation-buffer record)))
         (active-p
          (and record
               (niwa-agent-control-active-activity-p
                (niwa-agent-record-activity record))))
         (save-path (and session (niwa-session-save-path session)))
         (rel-path (and entry (plist-get entry :path))))
    (unless (or record pair)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Unknown agent path: %s" agent-path))
    (unless (or entry (buffer-live-p conversation-buffer))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No transcript entry for agent path: %s" agent-path))
    (if (buffer-live-p conversation-buffer)
        (append (list :agent-path agent-path
                      :status status
                      :entry entry
                      :session session
                      :buffer conversation-buffer
                      :live-buffer active-p
                      :calls (or (and inv
                                      (niwa-agent-invocation-call-count inv))
                                 (plist-get entry :calls))
                      :elapsed (niwa-view--agent-row-elapsed inv entry)
                      :reason (or (and inv
                                       (niwa-agent-invocation-terminal-reason
                                        inv))
                                  (plist-get entry :reason))
                      :session-label (or (and session
                                              (niwa-session-session-id
                                               session))
                                         (and session
                                              (niwa-session-name session))
                                         "unknown"))
                entry)
      (unless save-path
        (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Parent session has no save-path"))
      (unless (niwa-agent-persistence-transcript-path-p
               rel-path save-path)
        (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Transcript path failed validation: %s" rel-path))
      (let ((logical-path (expand-file-name rel-path save-path)))
        (unless (niwa-session-artifacts-artifact-present-p
                 session rel-path)
          (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Transcript artifact missing: %s" logical-path))
        (append (list :agent-path agent-path
                      :status status
                      :entry entry
                      :session session
                      :save-path save-path
                      :relative-path rel-path
                      :logical-path logical-path
                      :session-label (or (niwa-session-session-id session)
                                         (niwa-session-name session)
                                         "unknown"))
                entry)))))

(defun niwa-view--display-agent-transcript-view (view-buf)
  "Display transcript inspection VIEW-BUF in this view's singleton slot."
  (let ((parent-view (or (and (boundp 'niwa-view--agent-transcript-parent-view)
                              niwa-view--agent-transcript-parent-view)
                         (current-buffer))))
    (when (and (buffer-live-p parent-view)
               (not (eq parent-view view-buf)))
      (with-current-buffer parent-view
        (let ((win niwa-view--agent-transcript-window))
          (setq win
                (cond
                 ((and (window-live-p win)
                       (not (eq t (window-dedicated-p win)))
                       (buffer-live-p (window-buffer win))
                       (with-current-buffer (window-buffer win)
                         niwa-view--agent-transcript-p))
                  (condition-case nil
                      (let ((old-view (window-buffer win)))
                        (set-window-buffer win view-buf)
                        (unless (eq old-view view-buf)
                          (niwa-view--kill-agent-transcript-view
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
                      (display-buffer view-buf niwa-agent-view-display-action)
                    (error
                     (pop-to-buffer view-buf niwa-agent-view-display-action)
                     (selected-window)))))
          (setq niwa-view--agent-transcript-window
                (and (window-live-p win) win))
          (when (window-live-p win)
            (select-window win)
            (with-current-buffer view-buf
              (goto-char (point-max))
              (set-window-point win (point)))))))))

(defun niwa-view--ensure-agent-transcript-view (agent-path info parent-view)
  "Return a rendered transcript inspection view for AGENT-PATH and INFO.
PARENT-VIEW is the session view that opened the transcript."
  (let* ((session (plist-get info :session))
         (session-key
          (or (niwa-session-session-id session)
              (when (buffer-live-p parent-view)
                (when-let* ((parent-data
                             (buffer-local-value 'niwa--data-buffer
                                                 parent-view))
                            ((buffer-live-p parent-data)))
                  (buffer-name parent-data)))
              (error "Agent transcript session identity is unavailable")))
         (live-p (plist-get info :live-buffer))
         (agent-data (or (plist-get info :buffer)
                         (niwa-session-artifacts-find-artifact-noselect
                          session
                          (plist-get info :relative-path)
                          t)))
         (retained-p (niwa-view--retained-agent-data-p agent-data info))
         (view-name (format "*niwa-agent:%s:%s*"
                            session-key agent-path))
         (existing (get-buffer view-name))
         (view-name
          (if (and existing
                   (not (with-current-buffer existing
                          (and niwa-view--agent-transcript-p
                               (eq niwa--data-buffer agent-data)
                               (equal niwa-view--agent-path agent-path)
                               (eq (plist-get
                                    niwa-view--agent-transcript-info
                                    :session)
                                   session)))))
              (generate-new-buffer-name view-name)
            view-name))
         agent-view)
    ;; A cold transcript has no live session binding.  Bind its receiving
    ;; session before view setup so older agent archives remain navigable.
    (with-current-buffer agent-data
      (setq-local niwa--session session))
    (setq agent-view
          (niwa-view--ensure
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
        (niwa-transcript-restore-properties)
        (unless buffer-read-only
          (read-only-mode +1))))
    (with-current-buffer agent-view
      ;; A live gptel stream needs complete render positions before it can
      ;; attach its tail.  Settled inspection needs only a display, so leave
      ;; the old projection visible while canonical preparation is scheduled.
      (if live-p
          (niwa-view--full-rerender)
        (niwa-view-render-batched-full))
      (when live-p
        (niwa-view--agent-transcript-start-streaming))
      (setq buffer-read-only t))
    agent-view))

(defun niwa-view-agent-handle-activate (&optional agent-path)
  "Open the rendered agent handle at point or AGENT-PATH."
  (interactive)
  (let ((path (or agent-path
                  (get-text-property (point) 'niwa-view-agent-path)))
        (scope (niwa-diagnostic-scope (current-buffer))))
    (unless path
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No agent handle at point"))
    (condition-case err
        (niwa-view-open-agent-transcript path)
      (user-error
       (niwa-diagnostic-message scope "%s" (error-message-string err))))))

(defun niwa-view-open-agent-transcript (agent-path)
  "Open canonical AGENT-PATH's read-only transcript inspection view.

Looks up the entry in the parent session's `agent-transcripts'
slot and validates the path through
`niwa-agent-persistence-transcript-path-p' before opening
cold transcripts.  Resident retained agents open from their conversation
buffer whether running or idle."
  (interactive
   (list (completing-read
          "Agent transcript: "
          (when-let* ((data-buf (and (boundp 'niwa--data-buffer)
                                     niwa--data-buffer))
                      ((buffer-live-p data-buf))
                      (session (buffer-local-value
                                'niwa--session data-buf)))
            (delete-dups
             (append
              (mapcar #'car (niwa-session-agent-registry session))
              (delq nil
                    (mapcar (lambda (entry)
                              (plist-get (cdr entry) :agent-path))
                            (niwa-session-agent-transcripts session))))))
          nil t)))
  (when-let* ((data niwa--data-buffer)
              ((buffer-live-p data))
              (session (buffer-local-value 'niwa--session data))
              (record (niwa-view--agent-record agent-path)))
    (niwa-agent-persistence-ensure-conversation
     session record data
     (with-current-buffer data (bound-and-true-p niwa-session--read-only-mode))))
  (let* ((parent-view (or niwa-view--agent-transcript-parent-view
                          (current-buffer)))
         (info (niwa-view--resolve-agent-transcript agent-path))
         (agent-view (niwa-view--ensure-agent-transcript-view
                      agent-path info parent-view)))
    (niwa-view--display-agent-transcript-view agent-view)))

(defun niwa-view--agent-row-elapsed (inv entry)
  "Return elapsed seconds for INV or transcript ENTRY."
  (or (plist-get entry :elapsed)
      (and inv
           (niwa-agent-invocation-started-at inv)
           (float-time
            (time-subtract (current-time)
                           (niwa-agent-invocation-started-at inv))))))

(defun niwa-view--agent-record-status (record)
  "Return RECORD's visible active status, or nil when it is idle."
  (pcase (niwa-agent-record-activity record)
    ((or 'permission-blocked 'interaction-blocked) 'blocked)
    ('waiting 'waiting)
    ((or 'starting 'running) 'running)
    (_ nil)))

(defun niwa-view--agent-path-depth (path)
  "Return PATH's display depth below a direct child of root."
  (max 0 (- (length (split-string path "/" t)) 2)))

(defun niwa-view--agent-status-collect ()
  "Collect status rows for every active agent of the current view's session."
  (niwa-view-agent-active-rows (niwa-view--session)))

(defun niwa-view-agent-active-rows (session)
  "Return status rows for every active agent in SESSION, sorted by path.
Each row carries :path, :status, :activity, :model-metadata, :depth,
:parent-path, :started-at and the :tool-use-id of the Agent call that
started it, when known."
  (let (rows)
    (dolist (pair (and session (niwa-session-agent-registry session)))
      (let* ((path (car pair))
             (record (cdr pair))
             (status (niwa-view--agent-record-status record)))
        (when status
          (let ((invocation (niwa-agent-record-invocation record)))
            (push (list :path path
                        :status status
                        :activity (niwa-agent-record-activity record)
                        :model-metadata (niwa-agent-control-record-model-metadata record)
                        :depth (niwa-view--agent-path-depth path)
                        :parent-path (niwa-agent-record-parent-path record)
                        :started-at
                        (and (niwa-agent-invocation-p invocation)
                             (niwa-agent-invocation-started-at invocation))
                        :tool-use-id
                        (and (niwa-agent-invocation-p invocation)
                             (niwa-agent-invocation-parent-tool-use-id invocation)))
                  rows)))))
    (sort rows (lambda (left right)
                 (string-lessp (plist-get left :path)
                               (plist-get right :path))))))



;;
;;; Status and targeted refresh

(defun niwa-view--render-agent-status ()
  "Render the status zone after a visible agent state change."
  (niwa-view--render-status)
  ;; An open background-work cockpit lists agents too.
  (when (fboundp 'niwa-background-list-refresh-session)
    (niwa-background-list-refresh-session (niwa-view--session)))
  ;; Retained agents keep changing after the root request becomes idle.
  ;; This is the one funnel for their visible transitions; the room latches
  ;; the resulting roster, so an unchanged state is not broadcast.
  (when (fboundp 'niwa-collaboration-notify-agents-changed)
    (niwa-collaboration-notify-agents-changed
     (niwa-view--session))))

(defun niwa-view--agent-source-present-p (agent-path)
  "Return non-nil if the data buffer has an Agent source for AGENT-PATH.

The agent conversation owns markers around the render-data block it
patches for live updates.  Reuse those markers so this per-tick check
does not search the transcript."
  (when-let* ((record (niwa-view--agent-record agent-path))
              (invocation (niwa-agent-record-invocation record))
              ((niwa-agent-invocation-p invocation))
              (start
               (niwa-agent-invocation-render-data-start-marker
                invocation))
              (end
               (niwa-agent-invocation-render-data-end-marker
                invocation))
              (data-buf (and (boundp 'niwa--data-buffer)
                             niwa--data-buffer))
              ((buffer-live-p data-buf))
              ((markerp start))
              ((markerp end)))
    (and (eq (marker-buffer start) data-buf)
         (eq (marker-buffer end) data-buf)
         (< start end))))

(defvar-local niwa-view--agent-refresh-prefix nil
  "Last refresh's ((PATH TICK MIN) . START), with no matching handles before START.")

(defun niwa-view--agent-handle-refresh-points (agent-path)
  "Return source-backed visible handle positions for AGENT-PATH.
The return value is (POINTS . STALE-P), where STALE-P means a visible
non-status handle existed but lacked usable source metadata."
  (let ((pos (if (equal (car niwa-view--agent-refresh-prefix)
                        (list agent-path (buffer-modified-tick) (point-min)))
                 (cdr niwa-view--agent-refresh-prefix)
               (point-min)))
        (limit (min (point-max) (or (niwa-view-transcript-tail) (point-max))))
        points
        stale-p)
    (while (< pos limit)
      (let* ((path (get-text-property pos 'niwa-view-agent-path))
             (handle-p (get-text-property pos 'niwa-view-agent-handle-p))
             (source (get-text-property pos 'niwa-view-source)))
        (when (and handle-p
                   (equal path agent-path))
          (if (and (consp source)
                   (integer-or-marker-p (car source))
                   (integer-or-marker-p (cdr source)))
              (unless (cl-find-if
                       (lambda (point)
                         (eq (get-text-property point 'niwa-view-source)
                             source))
                       points)
                (push pos points))
            (setq stale-p t))))
      (setq pos (next-single-property-change
                 pos 'niwa-view-agent-path nil limit)))
    (cons (sort points #'>) stale-p)))

(defun niwa-view--refresh-agent-handle-at (pos _agent-path)
  "Refresh the rendered source-backed agent handle at POS.
Return non-nil on success."
  (save-excursion
    (goto-char pos)
    (let* ((data-buf niwa--data-buffer)
           (source (get-text-property pos 'niwa-view-source))
           (tool-use-id
            (and source (buffer-live-p data-buf)
                 (with-current-buffer data-buf
                   (niwa-transcript--tool-id-in-range
                    (car source) (cdr source)))))
           ;; Replacing metadata at a segment's end can collapse its end
           ;; marker into the deleted block.  Recover the owning call and
           ;; its wrapper: restored properties can separate its metadata.
           (source
            (if (and tool-use-id (buffer-live-p data-buf))
                (when-let* ((range
                            (with-current-buffer data-buf
                              (when-let* ((call
                                          (niwa-tool-render-data-segment-bounds
                                           tool-use-id)))
                                (or (niwa-view--tool-block-bounds
                                     (car call) (cdr call))
                                    call)))))
                  (niwa-view-disclosure-source-range
                   data-buf (car range) (cdr range)))
              source))
           (bounds (niwa-view-disclosure-section-bounds))
           (current-collapsed (and (get-text-property
                                    pos 'niwa-view-collapsed)
                                   t))
           (state (niwa-view-disclosure-state-entry
                   source 'agent-handle))
           (collapsed (if state (cdr state) current-collapsed))
           (turn-id (get-text-property pos 'niwa-view-turn-id))
           (in-flight-after-section-p
            (and bounds
                 (when-let* ((start (niwa-view-stream-in-flight-turn-start-position)))
                   (<= (car bounds) start (cdr bounds)))))
           (rendering
            (and bounds source
                 data-buf
                 (buffer-live-p data-buf)
                 (progn
                   (niwa-view--retire-tool-renderings data-buf (car source) (cdr source))
                   (niwa-view--segment-rendering
                    data-buf (car source) (cdr source) collapsed)))))
      (when (and bounds rendering)
        (unless state
          (niwa-view-disclosure-record-state
           source 'agent-handle collapsed))
        (let* ((view-start (car bounds))
               (view-end (cdr bounds))
               (rendering (niwa-view-disclosure-apply-rendering-state
                           (plist-put (copy-sequence rendering)
                                      :initially-collapsed-p collapsed)
                           source)))
          ;; Audit disclosures have their own source spans outside BOUNDS.
          ;; Preserve those rows and their fold state when replacing the handle.
          (setq rendering (plist-put rendering :hook-audits nil))
          (goto-char view-start)
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
          (unwind-protect
              (let ((ins-start (point)))
                (delete-region view-start view-end)
                (niwa-view--insert-rendered-tool rendering source)
                (niwa-view-render-add-display-properties
                 ins-start (point) (plist-get rendering :vtype))
                (when turn-id
                  (put-text-property ins-start (point)
                                     'niwa-view-turn-id turn-id))
                (when in-flight-after-section-p
                  (set-marker niwa-view--in-flight-turn-start ins-start)))
            (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil))
          t)))))

(defun niwa-view--refresh-agent-rendering-now (agent-path)
  "Refresh visible rendering for AGENT-PATH in the current view buffer."
  (niwa-view-render-mutate
   (list 'agent agent-path)
   (lambda ()
     (if (niwa-view-historical-segment-p)
         (niwa-view--render-agent-status)
       (niwa-view-agent--refresh-owned agent-path)))))

(defun niwa-view-agent--refresh-owned (agent-path)
  "Rediscover and refresh AGENT-PATH's handles with projection ownership held."
  (let* ((start-time (float-time))
         (found (niwa-view--agent-handle-refresh-points agent-path))
         ;; Only the handle sections and the status zone change; readers
         ;; elsewhere need no anchor scan through the whole transcript.
         (region (cons (apply #'min (niwa-view--status-anchor)
                              (mapcar (lambda (point)
                                        (save-excursion
                                          (goto-char point)
                                          (or (car (niwa-view-disclosure-section-bounds))
                                              point)))
                                      (car found)))
                       (point-max)))
         (prefix-end (min (point-max) (car region) (or (niwa-view-transcript-tail) (point-max))))
         stale-p)
    (niwa-view-call-preserving-user-view-state
     (lambda ()
       (let ((inhibit-read-only t)
             (inhibit-modification-hooks t))
         (pcase-let ((`(,points . ,stale) found))
           (setq stale-p (or stale
                             (and (null points)
                                  (niwa-view--agent-source-present-p
                                   agent-path))))
           (dolist (point points)
             (unless (niwa-view--refresh-agent-handle-at point agent-path)
               (setq stale-p t))))
         (niwa-view--render-agent-status)
         ;; REGION bounds this writer.  Reuse its unchanged prefix only
         ;; until another writer changes text or properties in the view.
         (unless stale-p
           (setq niwa-view--agent-refresh-prefix
                 (cons (list agent-path (buffer-modified-tick) (point-min)) prefix-end)))))
     region)
    (niwa-view--debug-log
     'agent-refresh
     :agent-path agent-path
     :elapsed (- (float-time) start-time)
     :fallback stale-p)
    (when stale-p
      (niwa-view-rerender (current-buffer)))
    (not stale-p)))

(defun niwa-view-agent-cancel-refresh ()
  "Cancel pending agent presentation work in the current view."
  (when (timerp niwa-view--agent-refresh-timer)
    (cancel-timer niwa-view--agent-refresh-timer))
  (setq niwa-view--agent-refresh-timer nil
        niwa-view--agent-refresh-pending nil))

(defun niwa-view-agent--flush-refresh (view-buffer)
  "Refresh one queued agent in VIEW-BUFFER, then return control to Emacs."
  (when (buffer-live-p view-buffer)
    (with-current-buffer view-buffer
      (when (timerp niwa-view--agent-refresh-timer)
        (cancel-timer niwa-view--agent-refresh-timer))
      (setq niwa-view--agent-refresh-timer nil)
      (when-let* ((path (pop niwa-view--agent-refresh-pending)))
        (unwind-protect
            (niwa-view--refresh-agent-rendering-now path)
          (when (and (buffer-live-p view-buffer)
                     niwa-view--agent-refresh-pending
                     (not (timerp niwa-view--agent-refresh-timer)))
            (setq niwa-view--agent-refresh-timer
                  (run-at-time 0.001 nil #'niwa-view-agent--flush-refresh
                               view-buffer))))))))

(defun niwa-view-refresh-agent-rendering (view-buffer agent-path)
  "Queue a coalesced refresh of AGENT-PATH in VIEW-BUFFER.
One timer drains distinct agents in separate callbacks so a burst of completions
does not schedule many expensive refreshes to expire together."
  (when (and agent-path (buffer-live-p view-buffer))
    (with-current-buffer view-buffer
      (if (or (not (numberp niwa-view-agent-refresh-delay))
              (<= niwa-view-agent-refresh-delay 0))
          (progn
            (setq niwa-view--agent-refresh-pending
                  (delete agent-path niwa-view--agent-refresh-pending))
            (niwa-view--refresh-agent-rendering-now agent-path))
        (unless (member agent-path niwa-view--agent-refresh-pending)
          (setq niwa-view--agent-refresh-pending
                (nconc niwa-view--agent-refresh-pending (list agent-path))))
        (unless (timerp niwa-view--agent-refresh-timer)
          (setq niwa-view--agent-refresh-timer
                (run-at-time niwa-view-agent-refresh-delay nil
                             #'niwa-view-agent--flush-refresh view-buffer)))))))

;;
;;; Attribution

(defun niwa-view--insert-attribution (agent-path)
  "Insert the `from PATH' attribution fragment for canonical AGENT-PATH.
Returns the propertized string (does not modify the buffer).
The path portion is always propertized as a click target.  Activation resolves
the current live or persisted transcript and reports any unavailable source."
  (let* ((header (concat "from " agent-path))
         (s (copy-sequence header)))
    (add-text-properties 0 (length s)
                         (list 'font-lock-face 'niwa-view-mailbox-header)
                         s)
    (let* ((from-prefix-len (length "from "))
           (id-end (length s))
           (open-fn
           (lambda ()
              (interactive)
              (niwa-view-agent-handle-activate agent-path)))
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
         niwa-view-agent-path ,agent-path
         help-echo "Open agent transcript")
       s))
    s))


;;;; Live invocation and blocked state

(defun niwa-view--agent-invocation (agent-path)
  "Return the retained invocation identity for canonical AGENT-PATH."
  (when-let* ((record (niwa-view--agent-record agent-path)))
    (or (niwa-agent-record-invocation record)
        (when-let* ((buffer (niwa-agent-record-conversation-buffer record))
                    ((buffer-live-p buffer)))
          (buffer-local-value 'niwa--agent-invocation buffer)))))

(provide 'niwa-view-agent)

;;; niwa-view-agent.el ends here
