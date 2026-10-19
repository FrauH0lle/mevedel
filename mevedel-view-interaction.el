;;; mevedel-view-interaction.el --- View interaction-zone UI -*- lexical-binding: t -*-

;;; Commentary:

;; Owns interaction descriptor registration, ordering, anchoring, and redraw.
;; Domain prompt modules retain callback settlement and outcome semantics.
;; Closing the last pending interaction of a root view re-offers its queued
;; idle work, since follow-up delivery and Goal continuation wait on it.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-structs)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-block-turn
		  "mevedel-agent-control" (session path activity))
(autoload 'mevedel-agent-control-block-turn "mevedel-agent-control")

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-data-buffer
		  "mevedel-agents" (cl-x) t)

;; `mevedel-interaction-prompt'
(defvar mevedel--prompt-overlays)

;; `mevedel-pending-inputs'
(declare-function mevedel-pending-inputs-offer-idle-work
                  "mevedel-pending-inputs" (data-buffer))
(declare-function mevedel-view--pending-inputs-render
                  "mevedel-pending-inputs" (&optional session))
(autoload 'mevedel-pending-inputs-offer-idle-work "mevedel-pending-inputs")
(autoload 'mevedel-view--pending-inputs-render "mevedel-pending-inputs")

;; `mevedel-permission-queue'
(declare-function mevedel-permission-absence-hold
                  "mevedel-permission-queue" (session))
(declare-function mevedel-permission-queue--render-head
		  "mevedel-permission-queue" (&optional session))
(autoload 'mevedel-permission-queue--render-head "mevedel-permission-queue")
(autoload 'mevedel-permission-absence-hold "mevedel-permission-queue")

;; `mevedel-permission-mode'
(declare-function mevedel-permission-mode-effective
                  "mevedel-permission-mode"
                  (&optional session data-buffer surface-buffer))
(autoload 'mevedel-permission-mode-effective "mevedel-permission-mode")

;; `mevedel-plan-mode'
(declare-function mevedel-plan-approval-render "mevedel-plan-mode"
		  (&optional session))
(autoload 'mevedel-plan-approval-render "mevedel-plan-mode")

;; `mevedel-structs'
(declare-function mevedel-request-p "mevedel-structs" (cl-x))
(declare-function mevedel-request-set-active-work-paused
                  "mevedel-structs" (request paused &optional now))
(declare-function mevedel-session-pending-follow-ups
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-input-failure-paused
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-plan-approval
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-steering
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs"
                  (cl-x) t)
(declare-function mevedel-session-permission-queue "mevedel-structs"
                  (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record "mevedel-telemetry"
		  (session event &rest props))

;; `mevedel-utilities'
(declare-function mevedel--normalize-message-text "mevedel-utilities"
		  (text))

;; `mevedel-view'
(declare-function mevedel-view--display-fragment-keymap "mevedel-view"
			  (&rest maps))
(declare-function mevedel-view--status-anchor "mevedel-view" nil)
(declare-function mevedel-view--zone-separator "mevedel-view" (label))
(defvar mevedel-view--interaction-marker)

;; `mevedel-view-composer'
(declare-function mevedel-view--input-marker-position
		  "mevedel-view-composer" nil)
(declare-function mevedel-view--input-start "mevedel-view-composer" ())
(declare-function mevedel-view--session "mevedel-view-composer" nil)
(defvar mevedel-view--prompt-hook-pending)

;; `mevedel-view-control-transfer'
(declare-function mevedel-view-control-transfer-current-descriptor
                  "mevedel-view-control-transfer" nil)
(declare-function mevedel-view-control-transfer-initialize
                  "mevedel-view-control-transfer"
                  (rebuild-function drain-predicate))
(autoload 'mevedel-view-control-transfer-initialize
  "mevedel-view-control-transfer")

;; `mevedel-view-render'
(declare-function mevedel-view--debug-log "mevedel-view-render"
                  (event &rest data))
(declare-function mevedel-view--debug-state "mevedel-view-render"
                  (&optional data-buf start end))
(declare-function mevedel-view-render-mutate
                  "mevedel-view-render" (key function &optional replacement cleanup))
(defvar mevedel-view-render--owner)

;; `mevedel-view-stream'
(declare-function mevedel-view--start-spinner-timer "mevedel-view-stream" (&optional resumed))
(declare-function mevedel-view--render-request-progress
                  "mevedel-view-stream" nil)
(declare-function mevedel-view--request-progress-region-start
		  "mevedel-view-stream" nil)

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-fragment-bounds
		  "mevedel-view-zone" (namespace id))
(declare-function mevedel-view-zone-reconcile "mevedel-view-zone"
		  (namespace start end fragments))
(declare-function mevedel-view-zone-region "mevedel-view-zone"
		  (namespace))
(defvar mevedel-view-zone-width-change-functions)
(autoload 'mevedel-view-zone-fragment-bounds "mevedel-view-zone")
(autoload 'mevedel-view-zone-reconcile "mevedel-view-zone")
(autoload 'mevedel-view-zone-region "mevedel-view-zone")

;;
;;; State

(defvar-local mevedel-view--interaction-descriptors nil
  "Hash table of live interaction-zone descriptors keyed by descriptor id.")

(defvar-local mevedel-view--interaction-overlays nil
  "Hash table of live interaction-zone overlays keyed by descriptor id.")

(defvar-local mevedel-view--interaction-lifecycle nil
  "Hash table of interaction lifecycle metadata retained across redraws.
Keyed by descriptor id from first registration until the interaction
closes.  Each value is a plist with `:opened-at', `:kind', `:origin', and
`:arrival', the sequence number that orders the zone.")

(defvar-local mevedel-view--interaction-arrivals 0
  "Number of interactions that have arrived in this view.")

(defvar mevedel-view--interaction-render-suppressed nil
  "Non-nil while a rebuild batches interaction re-registrations.
`mevedel-view--interaction-render' becomes a no-op so the zone is
reconciled once by the rebuild's final render.  Rendering the
intermediate cleared state would delete and reinsert unchanged prompt
text on every rebuild tick, throwing point out of the fragment the user
is reading or typing in.")

(defvar-keymap mevedel-view--pending-plan-map
  :doc "Keymap on the pending Plan segment in the interaction counter."
  "RET" #'mevedel-view--show-pending-plan
  "<return>" #'mevedel-view--show-pending-plan
  "<mouse-1>" #'mevedel-view--show-pending-plan
  "<mouse-2>" #'mevedel-view--show-pending-plan)


(defun mevedel-view-interaction-initialize ()
  "Initialize interaction descriptor state in the current view buffer."
  (setq-local mevedel-view--interaction-descriptors
              (make-hash-table :test #'equal))
  (setq-local mevedel-view--interaction-overlays
              (make-hash-table :test #'equal))
  (setq-local mevedel-view--interaction-lifecycle
              (make-hash-table :test #'equal))
  (let ((view (current-buffer)))
    (mevedel-view-control-transfer-initialize
     #'mevedel-view--interaction-rebuild
     (lambda ()
       (mevedel-view-interaction-transfer-drain-p view)))))

(defun mevedel-view--interaction-lifecycle-close (id)
  "Record the closing of interaction ID and forget its lifecycle."
  (when-let* ((metadata
               (and (hash-table-p mevedel-view--interaction-lifecycle)
                    (gethash id mevedel-view--interaction-lifecycle))))
    (when-let* ((session (mevedel-view--session))
                ((fboundp 'mevedel-telemetry-record)))
      (mevedel-telemetry-record
       session 'interaction-closed
       :interaction-id id
       :kind (plist-get metadata :kind)
       :origin (plist-get metadata :origin)
       :permission-mode-base (mevedel-session-permission-mode session)
       :permission-mode-effective
       (mevedel-permission-mode-effective
        session mevedel--data-buffer (current-buffer))
       :duration-ms
       (round (* 1000.0
                 (- (float-time) (plist-get metadata :opened-at))))))
    (remhash id mevedel-view--interaction-lifecycle)))

(defun mevedel-view-interaction-pending-p (&optional view-buffer)
  "Return non-nil when VIEW-BUFFER has a pending user interaction.
VIEW-BUFFER defaults to the current buffer."
  (let ((view (or view-buffer (current-buffer))))
    (and (buffer-live-p view)
         (with-current-buffer view
           (or (bound-and-true-p mevedel-view--prompt-hook-pending)
               (and (hash-table-p mevedel-view--interaction-descriptors)
                    (> (hash-table-count
                        mevedel-view--interaction-descriptors)
                       0)))))))

(defun mevedel-view-interaction-transfer-drain-p (&optional view-buffer)
  "Return non-nil when VIEW-BUFFER has a prompt that must drain.

The transfer control itself is deliberately excluded: it remains visible
while the owner waits for the other, settlement-blocking interactions."
  (let ((view (or view-buffer (current-buffer))))
    (and (buffer-live-p view)
         (with-current-buffer view
           (or (bound-and-true-p mevedel-view--prompt-hook-pending)
               (and (hash-table-p mevedel-view--interaction-descriptors)
                    (catch 'pending
                      (maphash
                       (lambda (_id descriptor)
                         (when (mevedel-view--interaction-pauses-active-work-p
                                descriptor)
                           (throw 'pending t)))
                       mevedel-view--interaction-descriptors)
                      nil)))))))

(defun mevedel-view--interaction-pauses-active-work-p (descriptor)
  "Return non-nil when interaction DESCRIPTOR pauses active work."
  (if (plist-member descriptor :active-work-paused)
      (plist-get descriptor :active-work-paused)
    (memq (plist-get descriptor :kind)
          '(ask permission plan preview request))))

(defun mevedel-view--interaction-active-work-paused-p ()
  "Return non-nil when the current view awaits actionable user input."
  (or
   (and (hash-table-p mevedel-view--interaction-descriptors)
        (catch 'paused
          (maphash
           (lambda (_id descriptor)
             (when (mevedel-view--interaction-pauses-active-work-p descriptor)
               (throw 'paused t)))
           mevedel-view--interaction-descriptors)
          nil))
   (when-let* ((session (mevedel-view--session))
               (entry (mevedel-session-pending-plan-approval session)))
     (plist-get entry :hidden))))

(defun mevedel-view-interaction-blocking-p (&optional view-buffer)
  "Return non-nil when VIEW-BUFFER awaits input outside pending-input UI."
  (let ((view (or view-buffer (current-buffer))))
    (and
     (buffer-live-p view)
     (with-current-buffer view
       (or
        (bound-and-true-p mevedel-view--prompt-hook-pending)
        (mevedel-view--interaction-active-work-paused-p))))))


;;
;;; Target view

(defun mevedel-view--interaction-target-buffer (&optional data-buffer)
  "Return the live view buffer that should host queued interactions.
DATA-BUFFER, when non-nil, is the chat/data buffer whose
`mevedel--view-buffer' binding should be consulted.  Signals when
there is no live non-transcript view.  Queue renderers catch this
as a render failure and abort the visible head rather than
silently placing controls in a data buffer."
  (cl-labels
      ((live-interaction-view-p (view)
         (and view
              (buffer-live-p view)
              (with-current-buffer view
                (and (not (bound-and-true-p
                           mevedel-view--agent-transcript-p))
                     (boundp 'mevedel-view--interaction-marker)
                     (markerp mevedel-view--interaction-marker)
                     (eq (marker-buffer mevedel-view--interaction-marker)
                         view)))))
       (view-for-data-buffer (buf &optional seen)
         (when (and buf
                    (buffer-live-p buf)
                    (not (memq buf seen)))
           (or (let ((view (buffer-local-value 'mevedel--view-buffer
                                               buf)))
                 (and (live-interaction-view-p view) view))
               (when-let* ((inv (and (boundp 'mevedel--agent-invocation)
                                     (buffer-local-value
                                      'mevedel--agent-invocation buf)))
                           ((mevedel-agent-invocation-p inv))
                           (parent (mevedel-agent-invocation-parent-data-buffer
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
              (and (boundp 'mevedel--view-buffer)
                   (live-interaction-view-p mevedel--view-buffer)
                   mevedel--view-buffer)))
        (error "No live view for queued prompt"))))



;;
;;; Rendering and lifecycle

(defun mevedel-view--interaction-sync-active-work-pause ()
  "Synchronize request timing with visible blocking interactions."
  (when-let* (((buffer-live-p mevedel--data-buffer))
              (request
               (buffer-local-value 'mevedel--current-request
                                   mevedel--data-buffer))
              ((mevedel-request-p request)))
    (let ((paused (mevedel-view--interaction-active-work-paused-p)))
      (mevedel-request-set-active-work-paused request paused)
      (when (fboundp 'mevedel-view--render-request-progress)
        (mevedel-view--render-request-progress))
      ;; A pause holds the indicators still; resuming must move them again.
      (when (fboundp 'mevedel-view--start-spinner-timer)
        (mevedel-view--start-spinner-timer)))))

(defun mevedel-view--interaction-plural (n singular plural)
  "Return N followed by SINGULAR or PLURAL."
  (format "%d %s" n (if (= n 1) singular plural)))

(defun mevedel-view--interaction-count-label (&optional actionable)
  "Return the composite interaction-zone counter label, or nil.
With ACTIONABLE, omit queued inputs and the trailing pending label."
  (let ((previews 0)
        (plans 0)
        (requests 0)
        (asks 0)
        (permissions 0)
        (pending-inputs 0)
        parts)
    (when (hash-table-p mevedel-view--interaction-descriptors)
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
       mevedel-view--interaction-descriptors))
    (let ((session (mevedel-view--session)))
      (when session
        (when (mevedel-session-pending-plan-approval session)
          (setq plans (max plans 1)))
        (setq permissions
              (max permissions
                   (length (mevedel-session-permission-queue session))))
        (setq pending-inputs
              (max
               pending-inputs
               (+ (length (mevedel-session-pending-steering session))
                  (length
                   (mevedel-session-pending-follow-ups session)))))))
    (setq parts
          (delq nil
                (list
                 (when (> previews 0)
                   (mevedel-view--interaction-plural
                    previews "preview" "previews"))
                 (when (> plans 0)
                   (mevedel-view--interaction-plural plans "plan" "plans"))
                 (when (> requests 0)
                   (mevedel-view--interaction-plural
                    requests "request" "requests"))
                 (when (> asks 0)
                   (mevedel-view--interaction-plural asks "question" "questions"))
                 (when (> permissions 0)
                   (mevedel-view--interaction-plural
                    permissions "permission" "permissions"))
                 (when (and (not actionable) (> pending-inputs 0))
                   (mevedel-view--interaction-plural
                    pending-inputs "input" "inputs")))))
    (when parts
      (concat (string-join parts " · ") (unless actionable " pending")))))

(defun mevedel-view--interaction-rank (id descriptor)
  "Return the display rank of interaction ID with DESCRIPTOR.
Higher ranks render farther from the composer.  Cards rank by arrival,
newest farthest, so with the composer anchored on its screen row an
arriving card moves only the transcript above it, never a card the
user may be reading.  Pending input belongs to the composer that queued
it and stays pinned next to it."
  (if (eq (plist-get descriptor :kind) 'pending-input)
      0
    (or (plist-get (and (hash-table-p mevedel-view--interaction-lifecycle)
                        (gethash id mevedel-view--interaction-lifecycle))
                   :arrival)
        0)))

(defun mevedel-view--interaction-preserve-on-rebuild-p (descriptor)
  "Return non-nil when DESCRIPTOR owns direct prompt state.
Direct request and preview prompts carry callbacks that are not represented
by a session queue.  Normal view rebuilds must keep them alive; explicit
clear/teardown paths still remove them."
  (memq (plist-get descriptor :kind) '(preview request ask)))

(defun mevedel-view--interaction-body (descriptor overlay)
  "Return DESCRIPTOR's body with standard interaction text properties.
OVERLAY is stored on the text as the descriptor's callback handle.
A function `:body' is called with no arguments at every render so a
producer with live editable state can supply fresh text instead of a
snapshot taken at registration."
  (let* ((raw (plist-get descriptor :body))
         (body (copy-sequence
                (mevedel--normalize-message-text
                 (or (if (functionp raw) (funcall raw) raw) ""))))
         (map (mevedel-view--display-fragment-keymap
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
     `(mevedel-view-interaction-kind ,kind
       mevedel-view-interaction-id ,id
       mevedel-view-interaction-overlay ,overlay)
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

(defun mevedel-view--interaction-region-end ()
  "Return the end boundary for fragment-managed interaction text."
  (let ((progress-start (mevedel-view--request-progress-region-start))
        (input-pos (mevedel-view--input-marker-position)))
    (or (and progress-start
             (or (not input-pos) (<= progress-start input-pos))
             progress-start)
        input-pos
        (point-max))))

(defun mevedel-view--interaction-descriptor-pairs ()
  "Return live interaction descriptor pairs in display order.
The first pair renders farthest from the composer; see
`mevedel-view--interaction-rank'."
  (let (pairs)
    (when (hash-table-p mevedel-view--interaction-descriptors)
      (maphash
       (lambda (id descriptor)
         (push (cons id descriptor) pairs))
       mevedel-view--interaction-descriptors))
    (sort pairs
          :key (lambda (pair) (mevedel-view--interaction-rank (car pair) (cdr pair)))
          :lessp #'> :in-place t)))

(defun mevedel-view--interaction-apply-overlay-properties
    (overlay descriptor)
  "Apply DESCRIPTOR metadata to interaction OVERLAY."
  (let ((kind (plist-get descriptor :kind))
        (id (plist-get descriptor :id))
        (origin (plist-get descriptor :origin)))
    (overlay-put overlay 'evaporate nil)
    (overlay-put overlay 'mevedel-view-interaction-kind kind)
    (overlay-put overlay 'mevedel-view-interaction-id id)
    (overlay-put overlay 'mevedel-view-interaction-origin origin)
    (overlay-put overlay 'read-only
                 (if (plist-member descriptor :read-only)
                     (plist-get descriptor :read-only)
                   t))
    (overlay-put overlay 'keymap (plist-get descriptor :keymap))
    (overlay-put overlay 'help-echo (plist-get descriptor :help-echo))
    (overlay-put overlay 'mevedel-view-interaction-entry
                 (plist-get descriptor :entry))
    (overlay-put overlay 'mevedel-view-interaction-activate
                 (plist-get descriptor :activate))
    overlay))

(defun mevedel-view--interaction-overlay-for (id descriptor)
  "Return live callback overlay for ID and DESCRIPTOR."
  (let ((overlay (and (hash-table-p mevedel-view--interaction-overlays)
                      (gethash id mevedel-view--interaction-overlays))))
    (unless (and (overlayp overlay) (overlay-buffer overlay))
      (let ((anchor (mevedel-view--interaction-anchor)))
        (setq overlay (make-overlay anchor anchor (current-buffer) nil nil))))
    (when (hash-table-p mevedel-view--interaction-overlays)
      (puthash id overlay mevedel-view--interaction-overlays))
    (mevedel-view--interaction-apply-overlay-properties overlay descriptor)
    overlay))

(defun mevedel-view--show-pending-plan (&optional event)
  "Show and focus the current session's pending Plan approval from EVENT."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (when-let* ((session (mevedel-view--session))
              (entry (mevedel-session-pending-plan-approval session))
              (id (plist-get entry :interaction-id)))
    (let ((bounds (mevedel-view-zone-fragment-bounds 'interaction id)))
      (unless bounds
        (plist-put entry :hidden nil)
        (mevedel-plan-approval-render session)
        (setq bounds
              (mevedel-view-zone-fragment-bounds 'interaction id)))
      (when bounds
        (goto-char (plist-get bounds :start))))))

(defun mevedel-view--interaction-separator-fragment (label)
  "Return the non-navigatable interaction separator fragment for LABEL."
  (let ((body (mevedel-view--zone-separator label)))
    (when (and (mevedel-view--session)
               (mevedel-session-pending-plan-approval
                (mevedel-view--session))
               (string-match "\\b[0-9]+ plans?\\b" body))
      (add-text-properties
       (match-beginning 0) (match-end 0)
       `(face link
         keymap ,mevedel-view--pending-plan-map
         mouse-face highlight
         follow-link t
         help-echo "Show pending plan"
         mevedel-view-pending-plan t)
       body))
    (list :namespace 'interaction
          :id :separator
          :priority most-positive-fixnum
          :body body
          :navigatable nil)))

(defun mevedel-view--interaction-fragment (id descriptor)
  "Return a fragment plist for interaction DESCRIPTOR ID."
  (let* ((overlay (mevedel-view--interaction-overlay-for id descriptor))
         (body (mevedel-view--interaction-body descriptor overlay))
         (fragment (list :namespace 'interaction
                         :id id
                         :priority (mevedel-view--interaction-rank id descriptor)
                         :body body
                         :keymap (mevedel-view--display-fragment-keymap
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

(defun mevedel-view--interaction-delete-overlay (overlay)
  "Release OVERLAY's retained-agent activity token and delete it."
  (when (overlayp overlay)
    (when-let* ((release
                 (overlay-get overlay 'mevedel-agent-activity-release)))
      (overlay-put overlay 'mevedel-agent-activity-release nil)
      (funcall release))
    (delete-overlay overlay)))

(defun mevedel-view--interaction-delete-stale-overlays ()
  "Delete descriptor overlays whose descriptors are no longer live."
  (when (hash-table-p mevedel-view--interaction-overlays)
    (maphash
     (lambda (id overlay)
       (unless (and (hash-table-p mevedel-view--interaction-descriptors)
                    (gethash id mevedel-view--interaction-descriptors))
         (mevedel-view--interaction-delete-overlay overlay)
         (remhash id mevedel-view--interaction-overlays)))
     mevedel-view--interaction-overlays))
  (when (and (boundp 'mevedel--prompt-overlays)
             (listp mevedel--prompt-overlays))
    (setq mevedel--prompt-overlays
          (seq-filter (lambda (overlay)
                        (and (overlayp overlay)
                             (overlay-buffer overlay)))
                      mevedel--prompt-overlays))))

(defun mevedel-view--interaction-sync-overlays (pairs)
  "Move descriptor callback overlays for PAIRS to fragment bounds."
  (dolist (pair pairs)
    (pcase-let* ((`(,id . ,descriptor) pair)
                 (overlay (and (hash-table-p mevedel-view--interaction-overlays)
                               (gethash id mevedel-view--interaction-overlays)))
                 (bounds (mevedel-view-zone-fragment-bounds
                          'interaction id)))
      (when (and (overlayp overlay) bounds)
        (move-overlay overlay
                      (plist-get bounds :start)
                      (plist-get bounds :end)
                      (current-buffer))
        (mevedel-view--interaction-apply-overlay-properties
         overlay descriptor)))))

(defvar-local mevedel-view--interaction-focus-return nil
  "Explicit interaction visits as (WINDOW ID COMPOSER-OFFSET) entries.
Arrival never moves point.  Only a visit made from the composer is saved.")

(defun mevedel-view-jump-to-interaction ()
  "Visit the pending question or approval nearest the composer.
That card has waited longest, since cards arrive farthest from the
composer.  Repeating the command cycles outward through the pending
cards and wraps.  Other windows do not move.
Remember this window's draft position and return there on settlement only
if the window is still visiting that interaction.  Reveal hidden Plan
approvals as needed."
  (interactive)
  (when-let* ((session (mevedel-view--session))
              (entry (mevedel-session-pending-plan-approval session))
              ((plist-get entry :hidden)))
    (plist-put entry :hidden nil)
    (mevedel-plan-approval-render session))
  (let* ((window (selected-window))
         (current (get-text-property (point) 'mevedel-view-interaction-id))
         (pairs (nreverse
                 (seq-filter
                  (lambda (pair)
                    (memq (plist-get (cdr pair) :kind)
                          '(preview plan request ask permission)))
                  (mevedel-view--interaction-descriptor-pairs))))
         (pair (or (cadr (member (assoc current pairs) pairs)) (car pairs)))
         (bounds (and pair (mevedel-view-zone-fragment-bounds
                            'interaction (car pair)))))
    (unless bounds (user-error "No pending question or approval"))
    (let* ((saved (assq window mevedel-view--interaction-focus-return))
           (input-start (mevedel-view--input-start))
           (offset (if (>= (point) input-start)
                       (- (point) input-start)
                     (and saved (equal current (cadr saved)) (nth 2 saved))))
           (position (or (text-property-any
                          (plist-get bounds :start) (plist-get bounds :end)
                          'mevedel-prompt-focus t)
                         (plist-get bounds :start))))
      (setq mevedel-view--interaction-focus-return
            (assq-delete-all window mevedel-view--interaction-focus-return))
      (when offset
        (push (list window (car pair) offset)
              mevedel-view--interaction-focus-return))
      (goto-char position)
      (set-window-point window position))))

(defun mevedel-view--interaction-focused-visits ()
  "Return saved visits whose windows still point inside their interaction.
Inspect the old text before reconciliation; a settled callback overlay
may already have been deleted."
  (seq-filter
   (lambda (visit)
     (pcase-let ((`(,window ,id ,_) visit))
       (and (window-live-p window)
            (eq (window-buffer window) (current-buffer))
            (equal id (get-text-property
                       (window-point window) 'mevedel-view-interaction-id)))))
   mevedel-view--interaction-focus-return))

(defun mevedel-view--interaction-sync-focus (pairs visits)
  "Restore settled explicit VISITS after rendering live PAIRS.
Never move a window that left its visited card or focus a newly arriving one."
  (setq mevedel-view--interaction-focus-return nil)
  (dolist (visit visits)
    (pcase-let ((`(,window ,id ,offset) visit))
      (if (assoc id pairs)
          (push visit mevedel-view--interaction-focus-return)
        (set-window-point
         window (min (point-max) (+ (mevedel-view--input-start) offset)))
        (when (eq window (selected-window))
          (goto-char (window-point window)))))))

(defun mevedel-view--interaction-render ()
  "Render interaction-zone fragments and descriptor callback overlays.
Coalesce materialization until an active projection writer finishes;
descriptor registration and callback ownership remain immediate."
  (unless mevedel-view--interaction-render-suppressed
    (if (and (derived-mode-p 'mevedel-view-mode)
             (bound-and-true-p mevedel-view-render--owner))
        (mevedel-view-render-mutate
         'interaction #'mevedel-view--interaction-render-now)
      (mevedel-view--interaction-render-now))))

(defun mevedel-view--interaction-render-now (&optional visits)
  "Materialize interactions without reentering a projection writer.
VISITS preserves explicit visitors captured before a full view reset."
  (unless mevedel-view--interaction-render-suppressed
    (let* ((visits (or visits (mevedel-view--interaction-focused-visits)))
           (label (mevedel-view--interaction-count-label))
           (pairs (mevedel-view--interaction-descriptor-pairs))
           (render-p (or label pairs
                         (mevedel-view-zone-region 'interaction))))
      (mevedel-view--interaction-delete-stale-overlays)
      (when render-p
        (let* ((start (mevedel-view--interaction-anchor))
               (end (max start (mevedel-view--interaction-region-end)))
               (fragments
                (append
                 (when label
                   (list (mevedel-view--interaction-separator-fragment label)))
                 (mapcar
                  (lambda (pair)
                    (pcase-let ((`(,id . ,descriptor) pair))
                      (mevedel-view--interaction-fragment id descriptor)))
                  pairs))))
          (mevedel-view-zone-reconcile 'interaction start end fragments)
          (mevedel-view--interaction-sync-overlays pairs)
          (dolist (pair pairs)
            (when-let* ((after (plist-get (cdr pair) :after-render)))
              (funcall after)))
          (mevedel-view--interaction-sync-focus pairs visits)))
      (mevedel-view--render-request-progress))))

;; Rebuilds, prompt settlement, and queue settlement all close interactions
;; here, so this is the one place that sees the last blocker go.  The offered
;; work rechecks every gate when its timer runs, so a permission sibling
;; rendered by the same settlement still holds it.
(defun mevedel-view--interaction-offer-idle-work ()
  "Offer this root view's queued idle work when no interaction is pending.
Queued follow-ups and an active Goal wait while an interaction is pending,
and nothing else resumes them once the user settles it while the root
session is idle -- for example a child agent's permission card or a Plan
approval."
  (when (and (not mevedel-view--agent-transcript-p)
             (boundp 'mevedel--data-buffer)
             (buffer-live-p mevedel--data-buffer)
             (eq (current-buffer)
                 (buffer-local-value 'mevedel--view-buffer
                                     mevedel--data-buffer))
             (not (mevedel-view-interaction-pending-p)))
    (mevedel-pending-inputs-offer-idle-work mevedel--data-buffer)))

(defun mevedel-view--permission-absence-render (hold)
  "Register the notice for HOLD, the session's expired-queue wait.
The notice only informs: it has no controls and does not pause active work."
  (let ((count (or (plist-get hold :count) 0)))
    (mevedel-view--interaction-register
     (list :kind 'permission-absence
           :id 'permission-absence
           :active-work-paused nil
           :body
           (format "\n%s expired at %s without a response; nothing was approved.\nSend a message to resume permission prompts.\n"
                   (mevedel-view--interaction-plural
                    count "permission request" "permission requests")
                   (format-time-string "%H:%M" (plist-get hold :expired-at)))))))

(defun mevedel-view--interaction-rebuild (&optional owned visits)
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
  (unless mevedel-view--agent-transcript-p
    (let (closed)
      (unwind-protect
          (let ((mevedel-view--interaction-render-suppressed t))
            (mevedel-view--interaction-clear-for-rebuild)
            (when-let* ((session (mevedel-view--session)))
              (when-let* ((descriptor
                           (mevedel-view-control-transfer-current-descriptor)))
                (mevedel-view--interaction-register descriptor))
              (when (mevedel-session-pending-plan-approval session)
                (mevedel-plan-approval-render session))
              (when (mevedel-session-permission-queue session)
                (mevedel-permission-queue--render-head session))
              (when-let* ((hold (mevedel-permission-absence-hold session)))
                (mevedel-view--permission-absence-render hold))
              (when (or (mevedel-session-pending-steering session)
                        (mevedel-session-pending-follow-ups session)
                        (mevedel-session-pending-input-failure-paused session))
                (mevedel-view--pending-inputs-render session)))
            (when (hash-table-p mevedel-view--interaction-lifecycle)
              (maphash
               (lambda (id _metadata)
                 (unless
                     (and (hash-table-p
                           mevedel-view--interaction-descriptors)
                          (gethash id mevedel-view--interaction-descriptors))
                   (push id closed)))
               mevedel-view--interaction-lifecycle)
              (dolist (id closed)
                (mevedel-view--interaction-lifecycle-close id))))
        (unwind-protect
            (if owned
                (mevedel-view--interaction-render-now visits)
              (mevedel-view--interaction-render))
          (mevedel-view--interaction-sync-active-work-pause)))
      (when closed
        (mevedel-view--interaction-offer-idle-work)))))

(defun mevedel-view--interaction-width-changed ()
  "Lay out the current view's interaction zone again for its new width.
The separator rule and width-fitted cards are sized when rendered."
  (when (and (derived-mode-p 'mevedel-view-mode)
             (mevedel-view-zone-region 'interaction))
    (mevedel-view--interaction-rebuild)))

(add-hook 'mevedel-view-zone-width-change-functions
          #'mevedel-view--interaction-width-changed)

(defun mevedel-view--interaction-register (descriptor)
  "Register DESCRIPTOR in the interaction zone and return its overlay."
  (unless (hash-table-p mevedel-view--interaction-descriptors)
    (setq mevedel-view--interaction-descriptors
          (make-hash-table :test #'equal)))
  (unless (hash-table-p mevedel-view--interaction-overlays)
    (setq mevedel-view--interaction-overlays
          (make-hash-table :test #'equal)))
  (let* ((id (plist-get descriptor :id))
         (anchor (mevedel-view--interaction-anchor))
         (existing-overlay
          (and (hash-table-p mevedel-view--interaction-overlays)
               (gethash id mevedel-view--interaction-overlays)))
         (overlay (or existing-overlay
                      (make-overlay anchor anchor (current-buffer) nil nil))))
    (mevedel-view--debug-log
     'interaction-register-begin
     :interaction-id id
     :kind (plist-get descriptor :kind)
     :state (mevedel-view--debug-state mevedel--data-buffer))
    (unless (hash-table-p mevedel-view--interaction-lifecycle)
      (setq mevedel-view--interaction-lifecycle
            (make-hash-table :test #'equal)))
    (unless (gethash id mevedel-view--interaction-lifecycle)
      (puthash id
               (list :opened-at (float-time)
                     :kind (plist-get descriptor :kind)
                     :origin (plist-get descriptor :origin)
                     :arrival (cl-incf mevedel-view--interaction-arrivals))
               mevedel-view--interaction-lifecycle)
      (when-let* ((session (mevedel-view--session))
                  ((fboundp 'mevedel-telemetry-record)))
        (mevedel-telemetry-record
         session 'interaction-opened
         :interaction-id id
         :kind (plist-get descriptor :kind)
         :origin (plist-get descriptor :origin)
         :permission-mode-base (mevedel-session-permission-mode session)
         :permission-mode-effective
         (mevedel-permission-mode-effective
          session mevedel--data-buffer (current-buffer))
         :active-work-paused
         (and (mevedel-view--interaction-pauses-active-work-p descriptor) t)
         :pending-count
         (and (hash-table-p mevedel-view--interaction-descriptors)
              (1+ (hash-table-count
                   mevedel-view--interaction-descriptors))))))
    (unless existing-overlay
      (let ((origin (plist-get descriptor :origin))
            (kind (plist-get descriptor :kind))
            (session (mevedel-view--session)))
        (when (and session origin
                   (not (equal origin "/root"))
                   (not (eq kind 'permission)))
          (overlay-put
           overlay 'mevedel-agent-activity-release
           (mevedel-agent-control-block-turn
            session origin 'interaction-blocked)))))
    (puthash id descriptor mevedel-view--interaction-descriptors)
    (puthash id overlay mevedel-view--interaction-overlays)
    (mevedel-view--interaction-apply-overlay-properties overlay descriptor)
    (unless mevedel-view--interaction-render-suppressed
      (mevedel-view--interaction-sync-active-work-pause))
    (mevedel-view--interaction-render)
    (mevedel-view--debug-log
     'interaction-register-end
     :interaction-id id
     :kind (plist-get descriptor :kind)
     :state (mevedel-view--debug-state mevedel--data-buffer))
    overlay))

(defun mevedel-view--interaction-unregister (id)
  "Remove interaction-zone descriptor ID and its overlay.
Removal settles or hides an interaction, so queued idle work is offered
again once no other interaction remains pending."
  (mevedel-view--interaction-lifecycle-close id)
  (when (hash-table-p mevedel-view--interaction-descriptors)
    (remhash id mevedel-view--interaction-descriptors))
  (mevedel-view--interaction-sync-active-work-pause)
  (when (hash-table-p mevedel-view--interaction-overlays)
    (when-let* ((overlay (gethash id mevedel-view--interaction-overlays)))
      (mevedel-view--interaction-delete-overlay overlay)
      (when (and (boundp 'mevedel--prompt-overlays)
                 (listp mevedel--prompt-overlays))
        (setq mevedel--prompt-overlays
              (delq overlay mevedel--prompt-overlays)))
      (remhash id mevedel-view--interaction-overlays))
    (mevedel-view--interaction-render))
  (mevedel-view--interaction-offer-idle-work))

(defun mevedel-view--interaction-clear-for-rebuild ()
  "Drop rebuild-owned interaction descriptors, preserving direct prompt UI.
Overlays stay registered so a producer that re-registers the same
interaction id reuses the identical overlay object; held references and
activity tokens survive an unchanged rebuild.  The next unsuppressed
render deletes the overlays whose descriptors did not come back and
prunes them from `mevedel--prompt-overlays'."
  (when (hash-table-p mevedel-view--interaction-descriptors)
    (let (remove-ids)
      (maphash
       (lambda (id descriptor)
         (unless (mevedel-view--interaction-preserve-on-rebuild-p descriptor)
           (push id remove-ids)))
       mevedel-view--interaction-descriptors)
      (dolist (id remove-ids)
        (remhash id mevedel-view--interaction-descriptors))))
  (mevedel-view--interaction-render))

(defun mevedel-view--interaction-clear ()
  "Delete all interaction-zone overlays without firing callbacks."
  (when (hash-table-p mevedel-view--interaction-descriptors)
    (clrhash mevedel-view--interaction-descriptors))
  (mevedel-view--interaction-sync-active-work-pause)
  (mevedel-view--interaction-render)
  (when (hash-table-p mevedel-view--interaction-overlays)
    (maphash (lambda (_id overlay)
               (mevedel-view--interaction-delete-overlay overlay))
             mevedel-view--interaction-overlays)
    (clrhash mevedel-view--interaction-overlays))
  (when (and (boundp 'mevedel--prompt-overlays)
             (listp mevedel--prompt-overlays))
    (let (live)
      (dolist (ov mevedel--prompt-overlays)
        (cond
         ((not (and (overlayp ov) (overlay-buffer ov))))
         ((and (eq (overlay-buffer ov) (current-buffer))
               (overlay-get ov 'mevedel-view-interaction-id))
          (mevedel-view--interaction-delete-overlay ov))
         (t
          (push ov live))))
      (setq mevedel--prompt-overlays (nreverse live)))))


(defun mevedel-view--interaction-anchor ()
  "Return the buffer position to anchor an interaction-zone overlay.
View buffers require a live `mevedel-view--interaction-marker'.  If its
position has drifted outside the status/input boundaries, repair it to the
current status anchor.  Non-view buffers use `(point-max)' so tool rendering
can still build isolated fragments."
  (if (not (derived-mode-p 'mevedel-view-mode))
      (point-max)
    (unless (and (markerp mevedel-view--interaction-marker)
                 (eq (marker-buffer mevedel-view--interaction-marker)
                     (current-buffer))
                 (marker-position mevedel-view--interaction-marker))
      (error "View interaction marker is not live"))
    (let* ((input-pos (mevedel-view--input-marker-position))
           (status-pos (mevedel-view--status-anchor))
           (interaction-pos (marker-position
                             mevedel-view--interaction-marker)))
      (if (and (>= interaction-pos status-pos)
               (or (not input-pos) (<= interaction-pos input-pos)))
          interaction-pos
        (let ((anchor (if input-pos
                          (min status-pos input-pos)
                        status-pos)))
          (set-marker mevedel-view--interaction-marker anchor)
          anchor)))))


(provide 'mevedel-view-interaction)

;;; mevedel-view-interaction.el ends here
