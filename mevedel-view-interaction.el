;;; mevedel-view-interaction.el --- View interaction-zone UI -*- lexical-binding: t -*-

;;; Commentary:

;; Owns interaction descriptor registration, ordering, anchoring, and redraw.
;; Domain prompt modules retain callback settlement and outcome semantics.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-block-turn
		  "mevedel-agent-control" (session path activity))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-data-buffer
		  "mevedel-agents" (cl-x) t)

;; `mevedel-interaction-prompt'
(defvar mevedel--prompt-overlays)

;; `mevedel-permission-queue'
(declare-function mevedel-permission-queue--render-head
		  "mevedel-permission-queue" (&optional session))

;; `mevedel-permissions'
(declare-function mevedel-permission-mode-effective
		  "mevedel-permissions"
		  (&optional session data-buffer surface-buffer))

;; `mevedel-plan-mode'
(declare-function mevedel-plan-approval-render "mevedel-plan-mode"
		  (&optional session))

;; `mevedel-structs'
(declare-function mevedel-session-pending-plan-approval
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-permission-queue "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-queued-user-messages
		  "mevedel-structs" (cl-x) t)
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

;; `mevedel-view-agent'
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-composer'
(declare-function mevedel-view--input-marker-position
		  "mevedel-view-composer" nil)
(declare-function mevedel-view--queued-user-messages-render
		  "mevedel-view-composer" (&optional session))
(declare-function mevedel-view--session "mevedel-view-composer" nil)
(defvar mevedel-view--prompt-hook-pending)

;; `mevedel-view-stream'
(declare-function mevedel-view--request-progress-region-start
		  "mevedel-view-stream" nil)

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-fragment-bounds
		  "mevedel-view-zone" (namespace id))
(declare-function mevedel-view-zone-reconcile "mevedel-view-zone"
		  (namespace start end fragments))
(declare-function mevedel-view-zone-region "mevedel-view-zone"
		  (namespace))

;;
;;; State

(defvar-local mevedel-view--interaction-descriptors nil
  "Hash table of live interaction-zone descriptors keyed by descriptor id.")

(defvar-local mevedel-view--interaction-overlays nil
  "Hash table of live interaction-zone overlays keyed by descriptor id.")

(defvar-local mevedel-view--interaction-telemetry-opened nil
  "Hash table of interaction lifecycle metadata retained across redraws.")


(defun mevedel-view-interaction-initialize ()
  "Initialize interaction descriptor state in the current view buffer."
  (setq-local mevedel-view--interaction-descriptors
              (make-hash-table :test #'equal))
  (setq-local mevedel-view--interaction-overlays
              (make-hash-table :test #'equal))
  (setq-local mevedel-view--interaction-telemetry-opened
              (make-hash-table :test #'equal)))

(defun mevedel-view--interaction-telemetry-close (id)
  "Record and forget telemetry lifetime ID."
  (when-let* ((metadata
               (and (hash-table-p mevedel-view--interaction-telemetry-opened)
                    (gethash id mevedel-view--interaction-telemetry-opened))))
    (when-let* ((session (mevedel-view--session))
                ((fboundp 'mevedel-telemetry-record)))
      (mevedel-telemetry-record
       session 'interaction-closed
       :interaction-id id
       :kind (plist-get metadata :kind)
       :origin (plist-get metadata :origin)
       :permission-mode-base (mevedel-session-permission-mode session)
       :permission-mode-effective
       (and (fboundp 'mevedel-permission-mode-effective)
            (mevedel-permission-mode-effective
             session mevedel--data-buffer (current-buffer)))
       :duration-ms
       (round (* 1000.0
                 (- (float-time) (plist-get metadata :opened-at))))))
    (remhash id mevedel-view--interaction-telemetry-opened)))

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
    (or (and (live-interaction-view-p (current-buffer))
             (current-buffer))
        (view-for-data-buffer data-buffer)
        (view-for-data-buffer (current-buffer))
        (and (boundp 'mevedel--view-buffer)
             (live-interaction-view-p mevedel--view-buffer)
             mevedel--view-buffer)
        (error "No live view for queued prompt"))))



;;
;;; Rendering and lifecycle

(defun mevedel-view--interaction-plural (n singular plural)
  "Return N followed by SINGULAR or PLURAL."
  (format "%d %s" n (if (= n 1) singular plural)))

(defun mevedel-view--interaction-count-label ()
  "Return the composite interaction-zone counter label, or nil."
  (let ((previews 0)
        (plans 0)
        (requests 0)
        (asks 0)
        (permissions 0)
        (queued-user-messages 0)
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
             ('queued-user-message
              (cl-incf queued-user-messages count)))))
       mevedel-view--interaction-descriptors))
    (let ((session (mevedel-view--session)))
      (when session
        (when (mevedel-session-pending-plan-approval session)
          (setq plans (max plans 1)))
        (setq permissions
              (max permissions
                   (length (mevedel-session-permission-queue session))))
        (setq queued-user-messages
              (max queued-user-messages
                   (length (mevedel-session-queued-user-messages session))))))
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
                 (when (> queued-user-messages 0)
                   (mevedel-view--interaction-plural
                    queued-user-messages "queued message"
                    "queued messages")))))
    (when parts
      (concat (string-join parts " · ") " pending"))))

(defun mevedel-view--interaction-kind-priority (kind)
  "Return the stable interaction overlay priority for KIND."
  (pcase kind
    ('preview 300)
    ('plan 200)
    ((or 'request 'ask) 150)
    ('permission 100)
    ('queued-user-message 80)
    (_ 50)))

(defun mevedel-view--interaction-preserve-on-rebuild-p (descriptor)
  "Return non-nil when DESCRIPTOR owns direct prompt state.
Direct request and preview prompts carry callbacks that are not represented
by a session queue.  Normal view rebuilds must keep them alive; explicit
clear/teardown paths still remove them."
  (memq (plist-get descriptor :kind) '(preview request ask)))

(defun mevedel-view--interaction-body (descriptor overlay)
  "Return DESCRIPTOR's body with standard interaction text properties.
OVERLAY is stored on the text as the descriptor's callback handle."
  (let* ((body (copy-sequence
                (mevedel--normalize-message-text
                 (or (plist-get descriptor :body) ""))))
         (map (mevedel-view--display-fragment-keymap
               (plist-get descriptor :keymap)))
         (help (plist-get descriptor :help-echo))
         (kind (plist-get descriptor :kind))
         (id (plist-get descriptor :id))
         (read-only (if (plist-member descriptor :read-only)
                        (plist-get descriptor :read-only)
                      t)))
    (add-text-properties
     0 (length body)
     `(mevedel-view-interaction-kind ,kind
       mevedel-view-interaction-id ,id
       mevedel-view-interaction-overlay ,overlay
       read-only ,read-only
       front-sticky nil
       rear-nonsticky t)
     body)
    (when map
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
  "Return live interaction descriptor pairs sorted by display priority."
  (let (pairs)
    (when (hash-table-p mevedel-view--interaction-descriptors)
      (maphash
       (lambda (id descriptor)
         (push (cons id descriptor) pairs))
       mevedel-view--interaction-descriptors))
    (sort pairs
          (lambda (a b)
            (> (or (plist-get (cdr a) :priority)
                   (mevedel-view--interaction-kind-priority
                    (plist-get (cdr a) :kind)))
               (or (plist-get (cdr b) :priority)
                   (mevedel-view--interaction-kind-priority
                    (plist-get (cdr b) :kind))))))))

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
    (overlay-put overlay 'priority
                 (or (plist-get descriptor :priority)
                     (mevedel-view--interaction-kind-priority kind)))
    (overlay-put overlay 'read-only
                 (if (plist-member descriptor :read-only)
                     (plist-get descriptor :read-only)
                   t))
    (if-let* ((map (plist-get descriptor :keymap)))
        (overlay-put overlay 'keymap map)
      (overlay-put overlay 'keymap nil))
    (if-let* ((help (plist-get descriptor :help-echo)))
        (overlay-put overlay 'help-echo help)
      (overlay-put overlay 'help-echo nil))
    (if (plist-member descriptor :entry)
        (overlay-put overlay 'mevedel-view-interaction-entry
                     (plist-get descriptor :entry))
      (overlay-put overlay 'mevedel-view-interaction-entry nil))
    (if-let* ((activate (plist-get descriptor :activate)))
        (overlay-put overlay 'mevedel-view-interaction-activate activate)
      (overlay-put overlay 'mevedel-view-interaction-activate nil))
    overlay))

(defun mevedel-view--interaction-overlay-for (id descriptor)
  "Return live callback overlay for ID and DESCRIPTOR."
  (let ((overlay (and (hash-table-p mevedel-view--interaction-overlays)
                      (gethash id mevedel-view--interaction-overlays))))
    (unless (and (overlayp overlay) (overlay-buffer overlay))
      (let ((anchor (mevedel-view--interaction-anchor)))
        (setq overlay (make-overlay anchor anchor (current-buffer) nil t))))
    (when (hash-table-p mevedel-view--interaction-overlays)
      (puthash id overlay mevedel-view--interaction-overlays))
    (mevedel-view--interaction-apply-overlay-properties overlay descriptor)
    overlay))

(defun mevedel-view--interaction-separator-fragment (label)
  "Return the non-navigatable interaction separator fragment for LABEL."
  (list :namespace 'interaction
        :id :separator
        :priority 1000
        :body (mevedel-view--zone-separator label)
        :navigatable nil))

(defun mevedel-view--interaction-fragment (id descriptor)
  "Return a fragment plist for interaction DESCRIPTOR ID."
  (let* ((overlay (mevedel-view--interaction-overlay-for id descriptor))
         (body (mevedel-view--interaction-body descriptor overlay))
         (fragment (list :namespace 'interaction
                         :id id
                         :priority (or (plist-get descriptor :priority)
                                       (mevedel-view--interaction-kind-priority
                                        (plist-get descriptor :kind)))
                         :body body
                         :keymap (mevedel-view--display-fragment-keymap
                                  (plist-get descriptor :keymap))
                         :help-echo (plist-get descriptor :help-echo)
                         :entry (plist-get descriptor :entry)
                         :activate (plist-get descriptor :activate)
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
     mevedel-view--interaction-overlays)))

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

(defun mevedel-view--interaction-render ()
  "Render interaction-zone fragments and descriptor callback overlays."
  (require 'mevedel-view-zone)
  (let* ((label (mevedel-view--interaction-count-label))
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
        (mevedel-view--interaction-sync-overlays pairs)))))

(defun mevedel-view--interaction-rebuild ()
  "Rebuild interaction-zone descriptors from live preview and queue state.
This deletes only interaction UI overlays and never settles callbacks."
  (unless mevedel-view--agent-transcript-p
    (mevedel-view--interaction-clear-for-rebuild)
    (when-let* ((session (mevedel-view--session)))
      (when (mevedel-session-pending-plan-approval session)
        (require 'mevedel-plan-mode)
        (mevedel-plan-approval-render session))
      (when (mevedel-session-permission-queue session)
        (require 'mevedel-permission-queue)
        (mevedel-permission-queue--render-head session))
      (when (mevedel-session-queued-user-messages session)
        (mevedel-view--queued-user-messages-render session)))
    (when (hash-table-p mevedel-view--interaction-telemetry-opened)
      (let (closed)
        (maphash
         (lambda (id _metadata)
           (unless (and (hash-table-p mevedel-view--interaction-descriptors)
                        (gethash id mevedel-view--interaction-descriptors))
             (push id closed)))
         mevedel-view--interaction-telemetry-opened)
        (dolist (id closed)
          (mevedel-view--interaction-telemetry-close id))))
    (mevedel-view--interaction-render)))

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
                      (make-overlay anchor anchor (current-buffer) nil t))))
    (unless (hash-table-p mevedel-view--interaction-telemetry-opened)
      (setq mevedel-view--interaction-telemetry-opened
            (make-hash-table :test #'equal)))
    (unless (gethash id mevedel-view--interaction-telemetry-opened)
      (puthash id
               (list :opened-at (float-time)
                     :kind (plist-get descriptor :kind)
                     :origin (plist-get descriptor :origin))
               mevedel-view--interaction-telemetry-opened)
      (when-let* ((session (mevedel-view--session))
                  ((fboundp 'mevedel-telemetry-record)))
        (mevedel-telemetry-record
         session 'interaction-opened
         :interaction-id id
         :kind (plist-get descriptor :kind)
         :origin (plist-get descriptor :origin)
         :permission-mode-base (mevedel-session-permission-mode session)
         :permission-mode-effective
         (and (fboundp 'mevedel-permission-mode-effective)
              (mevedel-permission-mode-effective
               session mevedel--data-buffer (current-buffer)))
         :active-work-paused
         (and (memq (plist-get descriptor :kind)
                    '(ask permission plan preview request))
              t)
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
          (require 'mevedel-agent-control)
          (overlay-put
           overlay 'mevedel-agent-activity-release
           (mevedel-agent-control-block-turn
            session origin 'interaction-blocked)))))
    (puthash id descriptor mevedel-view--interaction-descriptors)
    (puthash id overlay mevedel-view--interaction-overlays)
    (mevedel-view--interaction-apply-overlay-properties overlay descriptor)
    (mevedel-view--interaction-render)
    overlay))

(defun mevedel-view--interaction-unregister (id)
  "Remove interaction-zone descriptor ID and its overlay."
  (mevedel-view--interaction-telemetry-close id)
  (when (hash-table-p mevedel-view--interaction-descriptors)
    (remhash id mevedel-view--interaction-descriptors))
  (when (hash-table-p mevedel-view--interaction-overlays)
    (when-let* ((overlay (gethash id mevedel-view--interaction-overlays)))
      (mevedel-view--interaction-delete-overlay overlay)
      (when (and (boundp 'mevedel--prompt-overlays)
                 (listp mevedel--prompt-overlays))
        (setq mevedel--prompt-overlays
              (delq overlay mevedel--prompt-overlays)))
      (remhash id mevedel-view--interaction-overlays))
    (mevedel-view--interaction-render)))

(defun mevedel-view--interaction-clear-for-rebuild ()
  "Delete rebuild-owned interaction UI while preserving direct prompt UI."
  (let (remove-ids)
    (when (hash-table-p mevedel-view--interaction-descriptors)
      (maphash
       (lambda (id descriptor)
         (unless (mevedel-view--interaction-preserve-on-rebuild-p descriptor)
           (push id remove-ids)))
       mevedel-view--interaction-descriptors))
    (dolist (id remove-ids)
      (when (hash-table-p mevedel-view--interaction-overlays)
        (when-let* ((overlay (gethash id
                                      mevedel-view--interaction-overlays)))
          (mevedel-view--interaction-delete-overlay overlay))
        (remhash id mevedel-view--interaction-overlays))
      (when (hash-table-p mevedel-view--interaction-descriptors)
        (remhash id mevedel-view--interaction-descriptors))))
  (when (and (boundp 'mevedel--prompt-overlays)
             (listp mevedel--prompt-overlays))
    (let (live)
      (dolist (ov mevedel--prompt-overlays)
        (let* ((id (and (overlayp ov)
                        (overlay-get ov 'mevedel-view-interaction-id)))
               (descriptor
                (and id
                     (hash-table-p mevedel-view--interaction-descriptors)
                     (gethash id mevedel-view--interaction-descriptors))))
          (cond
           ((not (and (overlayp ov) (overlay-buffer ov))))
           ((and (eq (overlay-buffer ov) (current-buffer))
                 id
                 (not (mevedel-view--interaction-preserve-on-rebuild-p
                       descriptor)))
            (mevedel-view--interaction-delete-overlay ov))
           (t
            (push ov live)))))
      (setq mevedel--prompt-overlays (nreverse live))))
  (mevedel-view--interaction-render))

(defun mevedel-view--interaction-clear ()
  "Delete all interaction-zone overlays without firing callbacks."
  (when (hash-table-p mevedel-view--interaction-descriptors)
    (clrhash mevedel-view--interaction-descriptors))
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
