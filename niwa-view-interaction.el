;;; niwa-view-interaction.el --- View interaction-zone UI -*- lexical-binding: t -*-

;;; Commentary:

;; Owns interaction descriptor registration, ordering, anchoring, and redraw.
;; Domain prompt modules retain callback settlement and outcome semantics.
;; Closing the last pending interaction of a root view re-offers its queued
;; idle work, since follow-up delivery and Goal continuation wait on it.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-structs)
(require 'niwa-telemetry)

;; `niwa-agent-control'
(declare-function niwa-agent-control-block-turn
		  "niwa-agent-control" (session path activity))
(autoload 'niwa-agent-control-block-turn "niwa-agent-control")

;; `niwa-agents'
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-parent-data-buffer
		  "niwa-agents" (cl-x) t)

;; `niwa-interaction-prompt'
(defvar niwa--prompt-overlays)

;; `niwa-pending-inputs'
(declare-function niwa-pending-inputs-offer-idle-work
                  "niwa-pending-inputs" (data-buffer))
(declare-function niwa-view--pending-inputs-render
                  "niwa-pending-inputs" (&optional session))
(autoload 'niwa-pending-inputs-offer-idle-work "niwa-pending-inputs")
(autoload 'niwa-view--pending-inputs-render "niwa-pending-inputs")

;; `niwa-permission-queue'
(declare-function niwa-permission-absence-hold
                  "niwa-permission-queue" (session))
(declare-function niwa-permission-queue--render-head
		  "niwa-permission-queue" (&optional session))
(autoload 'niwa-permission-queue--render-head "niwa-permission-queue")
(autoload 'niwa-permission-absence-hold "niwa-permission-queue")

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-effective
                  "niwa-permission-mode"
                  (&optional session data-buffer surface-buffer))
(autoload 'niwa-permission-mode-effective "niwa-permission-mode")

;; `niwa-plan-mode'
(declare-function niwa-plan-approval-render "niwa-plan-mode"
		  (&optional session))
(autoload 'niwa-plan-approval-render "niwa-plan-mode")

;; `niwa-structs'
(declare-function niwa-request-p "niwa-structs" (cl-x))
(declare-function niwa-request-set-active-work-paused
                  "niwa-structs" (request paused &optional now))
(declare-function niwa-session-pending-follow-ups
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-input-failure-paused
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-plan-approval
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-steering
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-mode "niwa-structs"
                  (cl-x) t)
(declare-function niwa-session-permission-queue "niwa-structs"
                  (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--data-buffer)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-telemetry'
;; `niwa-utilities'
(declare-function niwa--normalize-message-text "niwa-utilities"
		  (text))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format-string &rest args))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")

;; `niwa-view'
(declare-function niwa-view--display-fragment-keymap "niwa-view"
			  (&rest maps))
(declare-function niwa-view--status-anchor "niwa-view" nil)
(declare-function niwa-view--zone-separator "niwa-view" (label))
(defvar niwa-view--interaction-marker)

;; `niwa-view-composer'
(declare-function niwa-view--session "niwa-view-composer" nil)
(declare-function niwa-view-composer-draft-end "niwa-view-composer" (&optional view))
(declare-function niwa-view-composer-draft-start "niwa-view-composer" (&optional view))
(declare-function niwa-view-transcript-tail "niwa-view-composer" ())
(defvar niwa-composer--view)
(defvar niwa-view--prompt-hook-pending)

;; `niwa-view-pair'
(declare-function niwa-view-composer-window "niwa-view-pair" (&optional window))
(declare-function niwa-view-transcript-window "niwa-view-pair" (&optional window))

;; `niwa-view-control-transfer'
(declare-function niwa-view-control-transfer-current-descriptor
                  "niwa-view-control-transfer" nil)
(declare-function niwa-view-control-transfer-initialize
                  "niwa-view-control-transfer"
                  (rebuild-function drain-predicate))
(autoload 'niwa-view-control-transfer-initialize
  "niwa-view-control-transfer")

;; `niwa-view-render'
(declare-function niwa-view--debug-log "niwa-view-render"
                  (event &rest data))
(declare-function niwa-view--debug-state "niwa-view-render"
                  (&optional data-buf start end))
(declare-function niwa-view-render-mutate
                  "niwa-view-render" (key function &optional replacement cleanup))
(defvar niwa-view-render--owner)

;; `niwa-view-stream'
(declare-function niwa-view--start-spinner-timer "niwa-view-stream" (&optional resumed))
(declare-function niwa-view--render-request-progress
                  "niwa-view-stream" nil)

;; `niwa-view-zone'
(declare-function niwa-view-zone-fragment-bounds
		  "niwa-view-zone" (namespace id))
(declare-function niwa-view-zone-reconcile "niwa-view-zone"
		  (namespace start end fragments))
(declare-function niwa-view-zone-region "niwa-view-zone"
		  (namespace))
(defvar niwa-view-zone-width-change-functions)
(autoload 'niwa-view-zone-fragment-bounds "niwa-view-zone")
(autoload 'niwa-view-zone-reconcile "niwa-view-zone")
(autoload 'niwa-view-zone-region "niwa-view-zone")

;;
;;; State

(defvar-local niwa-view--interaction-descriptors nil
  "Hash table of live interaction-zone descriptors keyed by descriptor id.")

(defvar-local niwa-view--interaction-overlays nil
  "Hash table of live interaction-zone overlays keyed by descriptor id.")

(defvar-local niwa-view--interaction-lifecycle nil
  "Hash table of interaction lifecycle metadata retained across redraws.
Keyed by descriptor id from first registration until the interaction
closes.  Each value is a plist with `:opened-at', `:kind', `:origin', and
`:arrival', the sequence number that orders the zone.")

(defvar-local niwa-view--interaction-arrivals 0
  "Number of interactions that have arrived in this view.")

(defvar niwa-view--interaction-render-suppressed nil
  "Non-nil while a rebuild batches interaction re-registrations.
`niwa-view--interaction-render' becomes a no-op so the zone is
reconciled once by the rebuild's final render.  Rendering the
intermediate cleared state would delete and reinsert unchanged prompt
text on every rebuild tick, throwing point out of the fragment the user
is reading or typing in.")

(defvar-keymap niwa-view--pending-plan-map
  :doc "Keymap on the pending Plan segment in the interaction counter."
  "RET" #'niwa-view--show-pending-plan
  "<return>" #'niwa-view--show-pending-plan
  "<mouse-1>" #'niwa-view--show-pending-plan
  "<mouse-2>" #'niwa-view--show-pending-plan)


(defun niwa-view-interaction-initialize ()
  "Initialize interaction descriptor state in the current view buffer.
Overlays outlive the buffer-local registries that a mode change resets,
so release any interaction overlay left by an earlier setup of this
buffer; its retained-agent blocker would otherwise never be released."
  (save-restriction
    (widen)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'niwa-view-interaction-id)
        (niwa-view--interaction-delete-overlay overlay))))
  (setq-local niwa-view--interaction-descriptors
              (make-hash-table :test #'equal))
  (setq-local niwa-view--interaction-overlays
              (make-hash-table :test #'equal))
  (setq-local niwa-view--interaction-lifecycle
              (make-hash-table :test #'equal))
  (let ((view (current-buffer)))
    (niwa-view-control-transfer-initialize
     #'niwa-view--interaction-rebuild
     (lambda ()
       (niwa-view-interaction-transfer-drain-p view)))))

(defun niwa-view--interaction-lifecycle-close (id)
  "Record the closing of interaction ID and forget its lifecycle."
  (when-let* ((metadata
               (and (hash-table-p niwa-view--interaction-lifecycle)
                    (gethash id niwa-view--interaction-lifecycle))))
    (when-let* ((session (niwa-view--session)))
      (niwa-telemetry-record
       session 'interaction-closed
       :interaction-id id
       :kind (plist-get metadata :kind)
       :origin (plist-get metadata :origin)
       :permission-mode-base (niwa-session-permission-mode session)
       :permission-mode-effective
       (niwa-permission-mode-effective
        session niwa--data-buffer (current-buffer))
       :duration-ms
       (round (* 1000.0
                 (- (float-time) (plist-get metadata :opened-at))))))
    (remhash id niwa-view--interaction-lifecycle)))

(defun niwa-view-interaction-pending-p (&optional view-buffer)
  "Return non-nil when VIEW-BUFFER has a pending user interaction.
VIEW-BUFFER defaults to the current buffer."
  (let ((view (or view-buffer (current-buffer))))
    (and (buffer-live-p view)
         (with-current-buffer view
           (or (bound-and-true-p niwa-view--prompt-hook-pending)
               (and (hash-table-p niwa-view--interaction-descriptors)
                    (> (hash-table-count
                        niwa-view--interaction-descriptors)
                       0)))))))

(defun niwa-view-interaction-transfer-drain-p (&optional view-buffer)
  "Return non-nil when VIEW-BUFFER has a prompt that must drain.

The transfer control itself is deliberately excluded: it remains visible
while the owner waits for the other, settlement-blocking interactions."
  (let ((view (or view-buffer (current-buffer))))
    (and (buffer-live-p view)
         (with-current-buffer view
           (or (bound-and-true-p niwa-view--prompt-hook-pending)
               (and (hash-table-p niwa-view--interaction-descriptors)
                    (catch 'pending
                      (maphash
                       (lambda (_id descriptor)
                         (when (niwa-view--interaction-pauses-active-work-p
                                descriptor)
                           (throw 'pending t)))
                       niwa-view--interaction-descriptors)
                      nil)))))))

(defun niwa-view--interaction-pauses-active-work-p (descriptor)
  "Return non-nil when interaction DESCRIPTOR pauses active work."
  (if (plist-member descriptor :active-work-paused)
      (plist-get descriptor :active-work-paused)
    (memq (plist-get descriptor :kind)
          '(ask permission plan preview request))))

(defun niwa-view--interaction-active-work-paused-p ()
  "Return non-nil when the current view awaits actionable user input."
  (or
   (and (hash-table-p niwa-view--interaction-descriptors)
        (catch 'paused
          (maphash
           (lambda (_id descriptor)
             (when (niwa-view--interaction-pauses-active-work-p descriptor)
               (throw 'paused t)))
           niwa-view--interaction-descriptors)
          nil))
   (when-let* ((session (niwa-view--session))
               (entry (niwa-session-pending-plan-approval session)))
     (plist-get entry :hidden))))

(defun niwa-view-interaction-blocking-p (&optional view-buffer)
  "Return non-nil when VIEW-BUFFER awaits input outside pending-input UI."
  (let ((view (or view-buffer (current-buffer))))
    (and
     (buffer-live-p view)
     (with-current-buffer view
       (or
        (bound-and-true-p niwa-view--prompt-hook-pending)
        (niwa-view--interaction-active-work-paused-p))))))


;;
;;; Target view

(defun niwa-view--interaction-target-buffer (&optional data-buffer)
  "Return the live view buffer that should host queued interactions.
DATA-BUFFER, when non-nil, is the chat/data buffer whose
`niwa--view-buffer' binding should be consulted.  Signals when
there is no live non-transcript view.  Queue renderers catch this
as a render failure and abort the visible head rather than
silently placing controls in a data buffer."
  (cl-labels
      ((live-interaction-view-p (view)
         (and view
              (buffer-live-p view)
              (with-current-buffer view
                (and (not (bound-and-true-p
                           niwa-view--agent-transcript-p))
                     (boundp 'niwa-view--interaction-marker)
                     (markerp niwa-view--interaction-marker)
                     (eq (marker-buffer niwa-view--interaction-marker)
                         view)))))
       (view-for-data-buffer (buf &optional seen)
         (when (and buf
                    (buffer-live-p buf)
                    (not (memq buf seen)))
           (or (let ((view (buffer-local-value 'niwa--view-buffer
                                               buf)))
                 (and (live-interaction-view-p view) view))
               (when-let* ((inv (and (boundp 'niwa--agent-invocation)
                                     (buffer-local-value
                                      'niwa--agent-invocation buf)))
                           ((niwa-agent-invocation-p inv))
                           (parent (niwa-agent-invocation-parent-data-buffer
                                    inv)))
                 (view-for-data-buffer parent (cons buf seen)))))))
    ;; A named owner decides alone.  Interactions are often admitted from
    ;; timers and process callbacks, where the current buffer is whatever
    ;; the user is looking at, possibly another session's view.
    (or (if data-buffer
            (view-for-data-buffer data-buffer)
          (or (and (live-interaction-view-p (current-buffer))
                   (current-buffer))
              (view-for-data-buffer (current-buffer))
              (and (boundp 'niwa--view-buffer)
                   (live-interaction-view-p niwa--view-buffer)
                   niwa--view-buffer)))
        (error "No live view for queued prompt"))))



;;
;;; Rendering and lifecycle

(defun niwa-view--interaction-sync-active-work-pause ()
  "Synchronize request timing with visible blocking interactions."
  (when-let* (((buffer-live-p niwa--data-buffer))
              (request
               (buffer-local-value 'niwa--current-request
                                   niwa--data-buffer))
              ((niwa-request-p request)))
    (let ((paused (niwa-view--interaction-active-work-paused-p)))
      (niwa-request-set-active-work-paused request paused)
      (when (fboundp 'niwa-view--render-request-progress)
        (niwa-view--render-request-progress))
      ;; A pause holds the indicators still; resuming must move them again.
      (when (fboundp 'niwa-view--start-spinner-timer)
        (niwa-view--start-spinner-timer)))))

(defun niwa-view--interaction-plural (n singular plural)
  "Return N followed by SINGULAR or PLURAL."
  (format "%d %s" n (if (= n 1) singular plural)))

(defun niwa-view--interaction-count-label (&optional actionable)
  "Return the composite interaction-zone counter label, or nil.
With ACTIONABLE, omit queued inputs and the trailing pending label."
  (let ((previews 0)
        (plans 0)
        (requests 0)
        (asks 0)
        (permissions 0)
        (pending-inputs 0)
        parts)
    (when (hash-table-p niwa-view--interaction-descriptors)
      (maphash
       (lambda (_id descriptor)
         (let ((count (or (plist-get descriptor :count) 0)))
           (pcase (plist-get descriptor :kind)
             ('preview (cl-incf previews count))
             ('plan (cl-incf plans count))
             ('request (cl-incf requests (max 1 count)))
             ('ask (cl-incf asks (max 1 count)))
             ('permission (cl-incf permissions count))
             ('pending-input
              (cl-incf pending-inputs count)))))
       niwa-view--interaction-descriptors))
    (let ((session (niwa-view--session)))
      (when session
        (when (niwa-session-pending-plan-approval session)
          (setq plans (max plans 1)))
        (setq permissions
              (max permissions
                   (length (niwa-session-permission-queue session))))
        (setq pending-inputs
              (max
               pending-inputs
               (+ (length (niwa-session-pending-steering session))
                  (length
                   (niwa-session-pending-follow-ups session)))))))
    (setq parts
          (delq nil
                (list
                 (when (> previews 0)
                   (niwa-view--interaction-plural
                    previews "preview" "previews"))
                 (when (> plans 0)
                   (niwa-view--interaction-plural plans "plan" "plans"))
                 (when (> requests 0)
                   (niwa-view--interaction-plural
                    requests "request" "requests"))
                 (when (> asks 0)
                   (niwa-view--interaction-plural asks "question" "questions"))
                 (when (> permissions 0)
                   (niwa-view--interaction-plural
                    permissions "permission" "permissions"))
                 (when (and (not actionable) (> pending-inputs 0))
                   (niwa-view--interaction-plural
                    pending-inputs "input" "inputs")))))
    (when parts
      (concat (string-join parts " · ") (unless actionable " pending")))))

(defun niwa-view--interaction-rank (id descriptor)
  "Return the display rank of interaction ID with DESCRIPTOR.
Higher ranks render farther from the composer.  Cards rank by arrival,
newest farthest, so with the composer anchored on its screen row an
arriving card moves only the transcript above it, never a card the
user may be reading.  Pending input belongs to the composer that queued
it and stays pinned next to it."
  (if (eq (plist-get descriptor :kind) 'pending-input)
      0
    (or (plist-get (and (hash-table-p niwa-view--interaction-lifecycle)
                        (gethash id niwa-view--interaction-lifecycle))
                   :arrival)
        0)))

(defun niwa-view--interaction-preserve-on-rebuild-p (descriptor)
  "Return non-nil when DESCRIPTOR owns direct prompt state.
Direct request and preview prompts carry callbacks that are not represented
by a session queue.  Normal view rebuilds must keep them alive; explicit
clear/teardown paths still remove them."
  (memq (plist-get descriptor :kind) '(preview request ask)))

(defun niwa-view--interaction-body (descriptor overlay)
  "Return DESCRIPTOR's body with standard interaction text properties.
OVERLAY is stored on the text as the descriptor's callback handle.
A function `:body' is called with no arguments at every render so a
producer with live editable state can supply fresh text instead of a
snapshot taken at registration."
  (let* ((raw (plist-get descriptor :body))
         (body (copy-sequence
                (niwa--normalize-message-text
                 (or (if (functionp raw) (funcall raw) raw) ""))))
         (map (niwa-view--display-fragment-keymap
               (plist-get descriptor :keymap)))
         (help (plist-get descriptor :help-echo))
         (kind (plist-get descriptor :kind))
         (id (plist-get descriptor :id))
         (body-properties-owned
          (plist-get descriptor :body-properties-owned))
         (read-only (if (plist-member descriptor :read-only)
                        (plist-get descriptor :read-only)
                      t)))
    (add-text-properties
     0 (length body)
     `(niwa-view-interaction-kind ,kind
       niwa-view-interaction-id ,id
       niwa-view-interaction-overlay ,overlay)
     body)
    (unless body-properties-owned
      (add-text-properties
       0 (length body)
       `(read-only ,read-only front-sticky nil rear-nonsticky t)
       body))
    (when (and map (not body-properties-owned))
      (add-text-properties 0 (length body) `(keymap ,map) body))
    (when help
      (add-text-properties 0 (length body) `(help-echo ,help) body))
    body))

(defun niwa-view--interaction-region-end ()
  "Return the end boundary for fragment-managed interaction text."
  (or (niwa-view-transcript-tail) (point-max)))

(defun niwa-view--interaction-descriptor-pairs ()
  "Return live interaction descriptor pairs in display order.
The first pair renders farthest from the composer; see
`niwa-view--interaction-rank'."
  (let (pairs)
    (when (hash-table-p niwa-view--interaction-descriptors)
      (maphash
       (lambda (id descriptor)
         (push (cons id descriptor) pairs))
       niwa-view--interaction-descriptors))
    (sort pairs
          :key (lambda (pair) (niwa-view--interaction-rank (car pair) (cdr pair)))
          :lessp #'> :in-place t)))

(defun niwa-view--interaction-apply-overlay-properties
    (overlay descriptor)
  "Apply DESCRIPTOR metadata to interaction OVERLAY."
  (let ((kind (plist-get descriptor :kind))
        (id (plist-get descriptor :id))
        (origin (plist-get descriptor :origin)))
    (overlay-put overlay 'evaporate nil)
    (overlay-put overlay 'niwa-view-interaction-kind kind)
    (overlay-put overlay 'niwa-view-interaction-id id)
    (overlay-put overlay 'niwa-view-interaction-origin origin)
    (overlay-put overlay 'read-only
                 (if (plist-member descriptor :read-only)
                     (plist-get descriptor :read-only)
                   t))
    (overlay-put overlay 'keymap (plist-get descriptor :keymap))
    (overlay-put overlay 'help-echo (plist-get descriptor :help-echo))
    (overlay-put overlay 'niwa-view-interaction-entry
                 (plist-get descriptor :entry))
    (overlay-put overlay 'niwa-view-interaction-activate
                 (plist-get descriptor :activate))
    overlay))

(defun niwa-view--interaction-overlay-for (id descriptor)
  "Return live callback overlay for ID and DESCRIPTOR."
  (let ((overlay (and (hash-table-p niwa-view--interaction-overlays)
                      (gethash id niwa-view--interaction-overlays))))
    (unless (and (overlayp overlay) (overlay-buffer overlay))
      (let ((anchor (niwa-view--interaction-anchor)))
        (setq overlay (make-overlay anchor anchor (current-buffer) nil nil))))
    (when (hash-table-p niwa-view--interaction-overlays)
      (puthash id overlay niwa-view--interaction-overlays))
    (niwa-view--interaction-apply-overlay-properties overlay descriptor)
    overlay))

(defun niwa-view--show-pending-plan (&optional event)
  "Show and focus the current session's pending Plan approval from EVENT."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (when-let* ((session (niwa-view--session))
              (entry (niwa-session-pending-plan-approval session))
              (id (plist-get entry :interaction-id)))
    (let ((bounds (niwa-view-zone-fragment-bounds 'interaction id)))
      (unless bounds
        (plist-put entry :hidden nil)
        (niwa-plan-approval-render session)
        (setq bounds
              (niwa-view-zone-fragment-bounds 'interaction id)))
      (when bounds
        (goto-char (plist-get bounds :start))))))

(defun niwa-view--interaction-separator-fragment (label)
  "Return the non-navigatable interaction separator fragment for LABEL."
  (let ((body (niwa-view--zone-separator label)))
    (when (and (niwa-view--session)
               (niwa-session-pending-plan-approval
                (niwa-view--session))
               (string-match "\\b[0-9]+ plans?\\b" body))
      (add-text-properties
       (match-beginning 0) (match-end 0)
       `(face link
         keymap ,niwa-view--pending-plan-map
         mouse-face highlight
         follow-link t
         help-echo "Show pending plan"
         niwa-view-pending-plan t)
       body))
    (list :namespace 'interaction
          :id :separator
          :priority most-positive-fixnum
          :body body
          :navigatable nil)))

(defun niwa-view--interaction-fragment (id descriptor)
  "Return a fragment plist for interaction DESCRIPTOR ID."
  (let* ((overlay (niwa-view--interaction-overlay-for id descriptor))
         (body (niwa-view--interaction-body descriptor overlay))
         (fragment (list :namespace 'interaction
                         :id id
                         :priority (niwa-view--interaction-rank id descriptor)
                         :body body
                         :keymap (niwa-view--display-fragment-keymap
                                  (plist-get descriptor :keymap))
                         :help-echo (plist-get descriptor :help-echo)
                         :entry (plist-get descriptor :entry)
                         :activate (plist-get descriptor :activate)
                         :body-properties-owned
                         (plist-get descriptor :body-properties-owned)
                         :navigatable (and (or (plist-get descriptor :activate)
                                               (plist-get descriptor :keymap))
                                           t))))
    (when (plist-member descriptor :read-only)
      (setq fragment (plist-put fragment :read-only
                                (plist-get descriptor :read-only))))
    fragment))

(defun niwa-view--interaction-delete-overlay (overlay)
  "Release OVERLAY's retained-agent activity token and delete it."
  (when (overlayp overlay)
    (when-let* ((release
                 (overlay-get overlay 'niwa-agent-activity-release)))
      (overlay-put overlay 'niwa-agent-activity-release nil)
      (funcall release))
    (delete-overlay overlay)))

(defun niwa-view--interaction-delete-stale-overlays ()
  "Delete descriptor overlays whose descriptors are no longer live."
  (when (hash-table-p niwa-view--interaction-overlays)
    (maphash
     (lambda (id overlay)
       (unless (and (hash-table-p niwa-view--interaction-descriptors)
                    (gethash id niwa-view--interaction-descriptors))
         (niwa-view--interaction-delete-overlay overlay)
         (remhash id niwa-view--interaction-overlays)))
     niwa-view--interaction-overlays))
  (when (and (boundp 'niwa--prompt-overlays)
             (listp niwa--prompt-overlays))
    (setq niwa--prompt-overlays
          (seq-filter (lambda (overlay)
                        (and (overlayp overlay)
                             (overlay-buffer overlay)))
                      niwa--prompt-overlays))))

(defun niwa-view--interaction-sync-overlays (pairs)
  "Move descriptor callback overlays for PAIRS to fragment bounds."
  (dolist (pair pairs)
    (pcase-let* ((`(,id . ,descriptor) pair)
                 (overlay (and (hash-table-p niwa-view--interaction-overlays)
                               (gethash id niwa-view--interaction-overlays)))
                 (bounds (niwa-view-zone-fragment-bounds
                          'interaction id)))
      (when (and (overlayp overlay) bounds)
        (move-overlay overlay
                      (plist-get bounds :start)
                      (plist-get bounds :end)
                      (current-buffer))
        (niwa-view--interaction-apply-overlay-properties
         overlay descriptor)))))

(defvar-local niwa-view--interaction-focus-return nil
  "Explicit interaction visits as (WINDOW ID COMPOSER-OFFSET) entries.
Arrival never moves point.  Only a visit made from the composer is saved.")

(defun niwa-view-jump-to-interaction ()
  "Visit the pending question or approval nearest the composer.
That card has waited longest, since cards arrive farthest from the
composer.  Repeating the command cycles outward through the pending
cards and wraps.  Other windows do not move.
Remember this window's draft position and return there on settlement only
if the window is still visiting that interaction.  Reveal hidden Plan
approvals as needed."
  (interactive)
  (if-let* ((view (bound-and-true-p niwa-composer--view)))
      (let ((window (niwa-view-transcript-window))
            (offset (max 0 (- (point) (niwa-view-composer-draft-start)))))
        (unless window
          (niwa-diagnostic-user-error (niwa-diagnostic-scope view)
                                        "No paired transcript window"))
        (with-current-buffer view
          (niwa-view--jump-to-interaction window nil offset))
        (select-window window))
    (niwa-view--jump-to-interaction
     (selected-window) (get-text-property (point) 'niwa-view-interaction-id) nil)))

(defun niwa-view--jump-to-interaction (window current offset)
  "Visit the next card in WINDOW after CURRENT, retaining draft OFFSET."
  (when-let* ((session (niwa-view--session))
              (entry (niwa-session-pending-plan-approval session))
              ((plist-get entry :hidden)))
    (plist-put entry :hidden nil)
    (niwa-plan-approval-render session))
  (let* ((pairs (nreverse
                 (seq-filter
                  (lambda (pair)
                    (memq (plist-get (cdr pair) :kind)
                          '(preview plan request ask permission)))
                  (niwa-view--interaction-descriptor-pairs))))
         (pair (or (cadr (member (assoc current pairs) pairs)) (car pairs)))
         (bounds (and pair (niwa-view-zone-fragment-bounds
                            'interaction (car pair)))))
    (unless bounds (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No pending question or approval"))
    (let* ((saved (assq window niwa-view--interaction-focus-return))
           (offset (or offset (and saved (equal current (cadr saved)) (nth 2 saved))))
           (position (or (text-property-any
                          (plist-get bounds :start) (plist-get bounds :end)
                          'niwa-prompt-focus t)
                         (plist-get bounds :start))))
      (setq niwa-view--interaction-focus-return
            (assq-delete-all window niwa-view--interaction-focus-return))
      (when offset
        (push (list window (car pair) offset)
              niwa-view--interaction-focus-return))
      (set-window-point window position))))

(defun niwa-view--interaction-focused-visits ()
  "Return saved visits whose windows still point inside their interaction.
Inspect the old text before reconciliation; a settled callback overlay
may already have been deleted."
  (seq-filter
   (lambda (visit)
     (pcase-let ((`(,window ,id ,_) visit))
       (and (window-live-p window)
            (eq (window-buffer window) (current-buffer))
            (equal id (get-text-property
                       (window-point window) 'niwa-view-interaction-id)))))
   niwa-view--interaction-focus-return))

(defun niwa-view--interaction-sync-focus (pairs visits)
  "Restore settled explicit VISITS after rendering live PAIRS.
Only the selected visitor can return to its saved draft caret."
  (setq niwa-view--interaction-focus-return nil)
  (dolist (visit visits)
    (pcase-let ((`(,window ,id ,offset) visit))
      (if (assoc id pairs)
          (push visit niwa-view--interaction-focus-return)
        (when-let* (((eq window (selected-window)))
                    (composer (niwa-view-composer-window window))
                    (position (min (niwa-view-composer-draft-end)
                                   (+ (niwa-view-composer-draft-start) offset))))
          (set-window-point composer position)
          ;; The renderer still has work to do in the transcript buffer.
          (save-current-buffer (select-window composer)))))))

(defun niwa-view--interaction-render ()
  "Render interaction-zone fragments and descriptor callback overlays.
Coalesce materialization until an active projection writer finishes;
descriptor registration and callback ownership remain immediate."
  (unless niwa-view--interaction-render-suppressed
    (if (and (derived-mode-p 'niwa-view-mode)
             (bound-and-true-p niwa-view-render--owner))
        (niwa-view-render-mutate
         'interaction #'niwa-view--interaction-render-now)
      (niwa-view--interaction-render-now))))

(defun niwa-view--interaction-render-now (&optional visits)
  "Materialize interactions without reentering a projection writer.
VISITS preserves explicit visitors captured before a full view reset."
  (unless niwa-view--interaction-render-suppressed
    (let* ((visits (or visits (niwa-view--interaction-focused-visits)))
           (label (niwa-view--interaction-count-label))
           (pairs (niwa-view--interaction-descriptor-pairs))
           (render-p (or label pairs
                         (niwa-view-zone-region 'interaction))))
      (niwa-view--interaction-delete-stale-overlays)
      (when render-p
        (let* ((start (niwa-view--interaction-anchor))
               (end (max start (niwa-view--interaction-region-end)))
               (fragments
                (append
                 (when label
                   (list (niwa-view--interaction-separator-fragment label)))
                 (mapcar
                  (lambda (pair)
                    (pcase-let ((`(,id . ,descriptor) pair))
                      (niwa-view--interaction-fragment id descriptor)))
                  pairs))))
          (niwa-view-zone-reconcile 'interaction start end fragments)
          (niwa-view--interaction-sync-overlays pairs)
          (dolist (pair pairs)
            (when-let* ((after (plist-get (cdr pair) :after-render)))
              (funcall after)))
          (niwa-view--interaction-sync-focus pairs visits)))
      ;; Request progress exists only once the stream module has loaded.
      (when (fboundp 'niwa-view--render-request-progress)
        (niwa-view--render-request-progress)))))

;; Rebuilds, prompt settlement, and queue settlement all close interactions
;; here, so this is the one place that sees the last blocker go.  The offered
;; work rechecks every gate when its timer runs, so a permission sibling
;; rendered by the same settlement still holds it.
(defun niwa-view--interaction-offer-idle-work ()
  "Offer this root view's queued idle work when no interaction is pending.
Queued follow-ups and an active Goal wait while an interaction is pending,
and nothing else resumes them once the user settles it while the root
session is idle -- for example a child agent's permission card or a Plan
approval."
  (when (and (not niwa-view--agent-transcript-p)
             (boundp 'niwa--data-buffer)
             (buffer-live-p niwa--data-buffer)
             (eq (current-buffer)
                 (buffer-local-value 'niwa--view-buffer
                                     niwa--data-buffer))
             (not (niwa-view-interaction-pending-p)))
    (niwa-pending-inputs-offer-idle-work niwa--data-buffer)))

(defun niwa-view--permission-absence-render (hold)
  "Register the notice for HOLD, the session's expired-queue wait.
The notice only informs: it has no controls and does not pause active work."
  (let ((count (or (plist-get hold :count) 0)))
    (niwa-view--interaction-register
     (list :kind 'permission-absence
           :id 'permission-absence
           :active-work-paused nil
           :body
           (format "\n%s expired at %s without a response; nothing was approved.\nSend a message to resume permission prompts.\n"
                   (niwa-view--interaction-plural
                    count "permission request" "permission requests")
                   (format-time-string "%H:%M" (plist-get hold :expired-at)))))))

(defun niwa-view--interaction-rebuild (&optional owned visits)
  "Rebuild interaction-zone descriptors from live preview and queue state.
Descriptors are dropped and re-registered without rendering the
intermediate states; the single final render reconciles the zone once.
A descriptor that comes back under the same id reuses its overlay
object, so an unchanged rebuild leaves the zone text, point, and held
overlay references untouched.  Callbacks are never settled here.  A
rebuild that drops a descriptor closes that interaction, which can leave
queued idle work free to proceed.
With OWNED, the current projection writer materializes the zone now,
before restoring reader anchors into its rebuilt content.  VISITS holds
explicit visitors captured before that writer removed the old zone."
  (unless niwa-view--agent-transcript-p
    (let (closed)
      (unwind-protect
          (let ((niwa-view--interaction-render-suppressed t))
            (niwa-view--interaction-clear-for-rebuild)
            (when-let* ((session (niwa-view--session)))
              (when-let* ((descriptor
                           (niwa-view-control-transfer-current-descriptor)))
                (niwa-view--interaction-register descriptor))
              (when (niwa-session-pending-plan-approval session)
                (niwa-plan-approval-render session))
              (when (niwa-session-permission-queue session)
                (niwa-permission-queue--render-head session))
              (when-let* ((hold (niwa-permission-absence-hold session)))
                (niwa-view--permission-absence-render hold))
              (when (or (niwa-session-pending-steering session)
                        (niwa-session-pending-follow-ups session)
                        (niwa-session-pending-input-failure-paused session))
                (niwa-view--pending-inputs-render session)))
            (when (hash-table-p niwa-view--interaction-lifecycle)
              (maphash
               (lambda (id _metadata)
                 (unless
                     (and (hash-table-p
                           niwa-view--interaction-descriptors)
                          (gethash id niwa-view--interaction-descriptors))
                   (push id closed)))
               niwa-view--interaction-lifecycle)
              (dolist (id closed)
                (niwa-view--interaction-lifecycle-close id))))
        (unwind-protect
            (if owned
                (niwa-view--interaction-render-now visits)
              (niwa-view--interaction-render))
          (niwa-view--interaction-sync-active-work-pause)))
      (when closed
        (niwa-view--interaction-offer-idle-work)))))

(defun niwa-view--interaction-width-changed ()
  "Lay out the current view's interaction zone again for its new width.
The separator rule and width-fitted cards are sized when rendered."
  (when (and (derived-mode-p 'niwa-view-mode)
             (niwa-view-zone-region 'interaction))
    (niwa-view--interaction-rebuild)))

(add-hook 'niwa-view-zone-width-change-functions
          #'niwa-view--interaction-width-changed)

(defun niwa-view--interaction-register (descriptor)
  "Register DESCRIPTOR in the interaction zone and return its overlay."
  (unless (hash-table-p niwa-view--interaction-descriptors)
    (setq niwa-view--interaction-descriptors
          (make-hash-table :test #'equal)))
  (unless (hash-table-p niwa-view--interaction-overlays)
    (setq niwa-view--interaction-overlays
          (make-hash-table :test #'equal)))
  (let* ((id (plist-get descriptor :id))
         (anchor (niwa-view--interaction-anchor))
         (existing-overlay
          (and (hash-table-p niwa-view--interaction-overlays)
               (gethash id niwa-view--interaction-overlays)))
         (overlay (or existing-overlay
                      (make-overlay anchor anchor (current-buffer) nil nil))))
    (niwa-view--debug-log
     'interaction-register-begin
     :interaction-id id
     :kind (plist-get descriptor :kind)
     :state (niwa-view--debug-state niwa--data-buffer))
    (unless (hash-table-p niwa-view--interaction-lifecycle)
      (setq niwa-view--interaction-lifecycle
            (make-hash-table :test #'equal)))
    (unless (gethash id niwa-view--interaction-lifecycle)
      (puthash id
               (list :opened-at (float-time)
                     :kind (plist-get descriptor :kind)
                     :origin (plist-get descriptor :origin)
                     :arrival (cl-incf niwa-view--interaction-arrivals))
               niwa-view--interaction-lifecycle)
      (when-let* ((session (niwa-view--session)))
        (niwa-telemetry-record
         session 'interaction-opened
         :interaction-id id
         :kind (plist-get descriptor :kind)
         :origin (plist-get descriptor :origin)
         :permission-mode-base (niwa-session-permission-mode session)
         :permission-mode-effective
         (niwa-permission-mode-effective
          session niwa--data-buffer (current-buffer))
         :active-work-paused
         (and (niwa-view--interaction-pauses-active-work-p descriptor) t)
         :pending-count
         (and (hash-table-p niwa-view--interaction-descriptors)
              (1+ (hash-table-count
                   niwa-view--interaction-descriptors))))))
    (unless existing-overlay
      (let ((origin (plist-get descriptor :origin))
            (kind (plist-get descriptor :kind))
            (session (niwa-view--session)))
        (when (and session origin
                   (not (equal origin "/root"))
                   (not (eq kind 'permission)))
          (overlay-put
           overlay 'niwa-agent-activity-release
           (niwa-agent-control-block-turn
            session origin 'interaction-blocked)))))
    (puthash id descriptor niwa-view--interaction-descriptors)
    (puthash id overlay niwa-view--interaction-overlays)
    (niwa-view--interaction-apply-overlay-properties overlay descriptor)
    (unless niwa-view--interaction-render-suppressed
      (niwa-view--interaction-sync-active-work-pause))
    (niwa-view--interaction-render)
    (niwa-view--debug-log
     'interaction-register-end
     :interaction-id id
     :kind (plist-get descriptor :kind)
     :state (niwa-view--debug-state niwa--data-buffer))
    overlay))

(defun niwa-view--interaction-unregister (id)
  "Remove interaction-zone descriptor ID and its overlay.
Removal settles or hides an interaction, so queued idle work is offered
again once no other interaction remains pending."
  (niwa-view--interaction-lifecycle-close id)
  (when (hash-table-p niwa-view--interaction-descriptors)
    (remhash id niwa-view--interaction-descriptors))
  (niwa-view--interaction-sync-active-work-pause)
  (when (hash-table-p niwa-view--interaction-overlays)
    (when-let* ((overlay (gethash id niwa-view--interaction-overlays)))
      (niwa-view--interaction-delete-overlay overlay)
      (when (and (boundp 'niwa--prompt-overlays)
                 (listp niwa--prompt-overlays))
        (setq niwa--prompt-overlays
              (delq overlay niwa--prompt-overlays)))
      (remhash id niwa-view--interaction-overlays))
    (niwa-view--interaction-render))
  (niwa-view--interaction-offer-idle-work))

(defun niwa-view--interaction-clear-for-rebuild ()
  "Drop rebuild-owned interaction descriptors, preserving direct prompt UI.
Overlays stay registered so a producer that re-registers the same
interaction id reuses the identical overlay object; held references and
activity tokens survive an unchanged rebuild.  The next unsuppressed
render deletes the overlays whose descriptors did not come back and
prunes them from `niwa--prompt-overlays'."
  (when (hash-table-p niwa-view--interaction-descriptors)
    (let (remove-ids)
      (maphash
       (lambda (id descriptor)
         (unless (niwa-view--interaction-preserve-on-rebuild-p descriptor)
           (push id remove-ids)))
       niwa-view--interaction-descriptors)
      (dolist (id remove-ids)
        (remhash id niwa-view--interaction-descriptors))))
  (niwa-view--interaction-render))

(defun niwa-view--interaction-clear ()
  "Delete all interaction-zone overlays without firing callbacks."
  (when (hash-table-p niwa-view--interaction-descriptors)
    (clrhash niwa-view--interaction-descriptors))
  (niwa-view--interaction-sync-active-work-pause)
  (niwa-view--interaction-render)
  (when (hash-table-p niwa-view--interaction-overlays)
    (maphash (lambda (_id overlay)
               (niwa-view--interaction-delete-overlay overlay))
             niwa-view--interaction-overlays)
    (clrhash niwa-view--interaction-overlays))
  (when (and (boundp 'niwa--prompt-overlays)
             (listp niwa--prompt-overlays))
    (let (live)
      (dolist (ov niwa--prompt-overlays)
        (cond
         ((not (and (overlayp ov) (overlay-buffer ov))))
         ((and (eq (overlay-buffer ov) (current-buffer))
               (overlay-get ov 'niwa-view-interaction-id))
          (niwa-view--interaction-delete-overlay ov))
         (t
          (push ov live))))
      (setq niwa--prompt-overlays (nreverse live)))))


(defun niwa-view--interaction-anchor ()
  "Return the buffer position to anchor an interaction-zone overlay.
View buffers require a live `niwa-view--interaction-marker'.  If its
position has drifted outside the status/input boundaries, repair it to the
current status anchor.  Non-view buffers use `(point-max)' so tool rendering
can still build isolated fragments."
  (if (not (derived-mode-p 'niwa-view-mode))
      (point-max)
    (unless (and (markerp niwa-view--interaction-marker)
                 (eq (marker-buffer niwa-view--interaction-marker)
                     (current-buffer))
                 (marker-position niwa-view--interaction-marker))
      (error "View interaction marker is not live"))
    (let* ((input-pos (niwa-view-transcript-tail))
           (status-pos (niwa-view--status-anchor))
           (interaction-pos (marker-position
                             niwa-view--interaction-marker)))
      (if (and (>= interaction-pos status-pos)
               (or (not input-pos) (<= interaction-pos input-pos)))
          interaction-pos
        (let ((anchor (if input-pos
                          (min status-pos input-pos)
                        status-pos)))
          (set-marker niwa-view--interaction-marker anchor)
          anchor)))))


(provide 'niwa-view-interaction)

;;; niwa-view-interaction.el ends here
