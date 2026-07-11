;;; mevedel-view-zone.el --- Managed view zones -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Owns the four fixed fragment-backed regions of view chrome: history-live,
;; status, interaction, and progress.  Producers supply complete fragment
;; sets; this module owns region identity, overlay lifetime, marker behavior,
;; preservation, reconciliation, recovery, collapse, and navigation.
;; Fragment text is a disposable cache.  Durable conversation state remains
;; in the data buffer and session structures.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `mevedel-view-composer'
(declare-function mevedel-view--input-start "mevedel-view-composer" ())
(defvar mevedel-view--input-marker)

;; `mevedel-view'
(defvar mevedel-view--interaction-marker)
(defvar mevedel-view--status-marker)

(defvar-local mevedel-view-zone--collapse-states nil
  "Buffer-local UI collapse state keyed by fragment collapse keys.")

(defvar-local mevedel-view-zone--regions nil
  "Managed region overlays keyed by fixed zone name.")

(defconst mevedel-view-zone--names
  '(history-live status interaction progress)
  "Fixed names of fragment-backed view zones.")


;;
;;; Region and key helpers

(defun mevedel-view-zone--region-id (region)
  "Return the identity object used for managed REGION keys."
  (unless (overlayp region)
    (error "Invalid managed zone region: %S" region))
  region)

(defun mevedel-view-zone--key (region fragment)
  "Return the container-scoped key for FRAGMENT in REGION."
  (let ((namespace (plist-get fragment :namespace))
        (id (plist-get fragment :id)))
    (unless namespace
      (error "Missing fragment namespace"))
    (unless id
      (error "Missing fragment id"))
    (list (mevedel-view-zone--region-id region) namespace id)))

(defun mevedel-view-zone--key-match-p (left right)
  "Return non-nil when LEFT and RIGHT are the same fragment key."
  (and (consp left) (consp right)
       (eq (car left) (car right))
       (eq (cadr left) (cadr right))
       (equal (caddr left) (caddr right))))

(defun mevedel-view-zone--collapse-key (region fragment)
  "Return the collapse-state key for FRAGMENT in REGION."
  (or (plist-get fragment :collapse-key)
      (mevedel-view-zone--key region fragment)))

(defun mevedel-view-zone--collapse-state-table ()
  "Return the current buffer's fragment collapse-state table."
  (unless (hash-table-p mevedel-view-zone--collapse-states)
    (setq mevedel-view-zone--collapse-states
          (make-hash-table :test #'equal)))
  mevedel-view-zone--collapse-states)

(defun mevedel-view-zone-collapse-state-set-p (key)
  "Return non-nil when KEY has explicit fragment collapse state."
  (let ((table (mevedel-view-zone--collapse-state-table)))
    (not (eq (gethash key table :mevedel-view-zone--missing)
             :mevedel-view-zone--missing))))

(defun mevedel-view-zone-collapse-state (key &optional default)
  "Return fragment collapse state for KEY, or DEFAULT when unset."
  (let ((table (mevedel-view-zone--collapse-state-table)))
    (gethash key table default)))

(defun mevedel-view-zone-set-collapse-state (key collapsed)
  "Set fragment collapse state for KEY to COLLAPSED."
  (puthash key (and collapsed t)
           (mevedel-view-zone--collapse-state-table)))

(defun mevedel-view-zone-toggle-collapse-state (key &optional default)
  "Toggle fragment collapse state for KEY, defaulting to DEFAULT."
  (let ((collapsed (not (mevedel-view-zone-collapse-state key default))))
    (mevedel-view-zone-set-collapse-state key collapsed)
    collapsed))

(defun mevedel-view-zone--fragment-collapsed-p (region fragment)
  "Return non-nil when FRAGMENT is currently collapsed in REGION."
  (and (plist-get fragment :collapsible)
       (mevedel-view-zone-collapse-state
        (mevedel-view-zone--collapse-key region fragment)
        (plist-get fragment :collapsed))))

(defun mevedel-view-zone-toggle-collapsed (&optional position)
  "Toggle the collapsible fragment at POSITION or point.
Return the new collapsed state, or signal when there is no collapsible
fragment at POSITION.  This updates UI state only; callers are
responsible for rerendering the producer."
  (interactive)
  (let* ((position (or position (point)))
         (key (get-text-property position 'mevedel-view-zone-collapse-key))
         (collapsible (get-text-property position
                                         'mevedel-view-zone-collapsible))
         (default (get-text-property position
                                     'mevedel-view-zone-collapsed)))
    (unless (and key collapsible)
      (user-error "No collapsible fragment at point"))
    (mevedel-view-zone-toggle-collapse-state key default)))

(defun mevedel-view-zone--normalize (fragment)
  "Return FRAGMENT with defaults applied."
  (unless (plist-get fragment :namespace)
    (error "Missing fragment namespace"))
  (unless (plist-get fragment :id)
    (error "Missing fragment id"))
  (let ((normalized (copy-sequence fragment)))
    (unless (plist-member normalized :priority)
      (setq normalized (plist-put normalized :priority 0)))
    (unless (plist-member normalized :read-only)
      (setq normalized (plist-put normalized :read-only t)))
    (unless (plist-member normalized :body)
      (setq normalized (plist-put normalized :body "")))
    normalized))

(defun mevedel-view-zone--region-bounds (region)
  "Return `(START . END)' bounds for managed REGION."
  (unless (and (overlayp region) (overlay-buffer region))
    (error "Managed zone region has no buffer"))
  (cons (overlay-start region) (overlay-end region)))

(defun mevedel-view-zone--set-region-bounds (region start end)
  "Set managed REGION bounds to START and END."
  (move-overlay region start end (current-buffer)))


;;
;;; Rendering

(defun mevedel-view-zone--section-properties (region fragment section)
  "Return text properties for FRAGMENT SECTION in REGION."
  (let* ((key (mevedel-view-zone--key region fragment))
         (read-only (plist-get fragment :read-only))
         (collapse-key (and (or (plist-get fragment :collapsible)
                                (plist-member fragment :collapse-key))
                            (mevedel-view-zone--collapse-key
                             region fragment)))
         (collapsed (mevedel-view-zone--fragment-collapsed-p
                     region fragment))
         (props `(mevedel-view-zone-key ,key
                  mevedel-view-zone-region ,(car key)
                  mevedel-view-zone-namespace ,(plist-get fragment :namespace)
                  mevedel-view-zone-id ,(plist-get fragment :id)
                  mevedel-view-zone-section ,section
                  mevedel-view-zone-navigatable
                  ,(plist-get fragment :navigatable)
                  mevedel-view-zone-activate ,(plist-get fragment :activate)
                  mevedel-view-zone-entry ,(plist-get fragment :entry)
                  mevedel-view-zone-collapsible
                  ,(plist-get fragment :collapsible)
                  mevedel-view-zone-collapse-key ,collapse-key
                  mevedel-view-zone-collapsed ,collapsed
                  rear-nonsticky (read-only font-lock-face face keymap help-echo)
                  front-sticky (read-only))))
    (when read-only
      (setq props (append `(read-only ,read-only) props)))
    props))

(defun mevedel-view-zone--add-default-property (text property value)
  "Return TEXT with PROPERTY set to VALUE only where absent."
  (if (or (null value) (not (stringp text)) (zerop (length text)))
      text
    (let ((copy (copy-sequence text))
          (pos 0)
          next)
      (while (< pos (length copy))
        (setq next (or (next-single-property-change pos property copy)
                       (length copy)))
        (when (null (get-text-property pos property copy))
          (put-text-property pos next property value copy))
        (setq pos next))
      copy)))

(defun mevedel-view-zone--single-trailing-newline (text &optional force)
  "Return TEXT with exactly one trailing newline when non-empty or FORCE."
  (let* ((copy (copy-sequence text))
         (length (length copy))
         (pos length))
    (cond
     ((zerop length)
      (if force "\n" copy))
     (t
      (while (and (> pos 0)
                  (eq (aref copy (1- pos)) ?\n))
        (setq pos (1- pos)))
      (if (= pos length)
          (concat copy "\n")
        (substring copy 0 (1+ pos)))))))

(defun mevedel-view-zone--body-text (fragment &optional force-newline)
  "Return FRAGMENT body as a string with default interactive properties.
When FORCE-NEWLINE is non-nil, return one tagged newline for an empty body."
  (let ((body (plist-get fragment :body)))
    (setq body
          (cond
           ((stringp body) (copy-sequence body))
           ((null body) "")
           (t (format "%s" body))))
    (setq body (mevedel-view-zone--single-trailing-newline
                body force-newline))
    (setq body (mevedel-view-zone--add-default-property
                body 'keymap (plist-get fragment :keymap)))
    (setq body (mevedel-view-zone--add-default-property
                body 'help-echo (plist-get fragment :help-echo)))
    body))

(defun mevedel-view-zone--label-text (fragment)
  "Return FRAGMENT label text, or nil when it has no label."
  (let ((left (plist-get fragment :label-left))
        (right (plist-get fragment :label-right)))
    (cond
     ((and left right) (format "%s %s" left right))
     (left (format "%s" left))
     (right (format "%s" right)))))

(defun mevedel-view-zone--render (region fragment)
  "Return propertized text for FRAGMENT in managed REGION."
  (let* ((fragment (mevedel-view-zone--normalize fragment))
         (label (mevedel-view-zone--label-text fragment))
         (collapsed (mevedel-view-zone--fragment-collapsed-p
                     region fragment))
         (body (if (and label collapsed)
                   ""
                 (mevedel-view-zone--body-text fragment (not label))))
         (body-suffix (plist-get fragment :body-suffix))
         (text ""))
    (when label
      (setq label (concat label "\n"))
      (add-text-properties
       0 (length label)
       (mevedel-view-zone--section-properties region fragment 'label)
       label)
      (setq text (concat text label)))
    (when (> (length body) 0)
      (add-text-properties
       0 (length body)
       (mevedel-view-zone--section-properties region fragment 'body)
       body)
      (setq text (concat text body))
      (when body-suffix
        (setq body-suffix (copy-sequence (format "%s" body-suffix)))
        (setq body-suffix
              (mevedel-view-zone--add-default-property
               body-suffix 'keymap (plist-get fragment :keymap)))
        (setq body-suffix
              (mevedel-view-zone--add-default-property
               body-suffix 'help-echo (plist-get fragment :help-echo)))
        (add-text-properties
         0 (length body-suffix)
         (mevedel-view-zone--section-properties region fragment 'body)
         body-suffix)
        (setq text (concat text body-suffix))))
    text))


;;
;;; Bounds and mutation

(defun mevedel-view-zone--bounds-at (&optional position)
  "Return fragment bounds at POSITION, or nil when no fragment is there."
  (let* ((position (or position (point)))
         (key (get-text-property position 'mevedel-view-zone-key)))
    (when key
      (let ((start position)
            (end position))
        (while (and (> start (point-min))
                    (mevedel-view-zone--key-match-p
                     (get-text-property (1- start)
                                        'mevedel-view-zone-key)
                     key))
          (setq start (1- start)))
        (while (and (< end (point-max))
                    (mevedel-view-zone--key-match-p
                     (get-text-property end
                                        'mevedel-view-zone-key)
                     key))
          (setq end (1+ end)))
        (list :start start :end end :key key
              :region (nth 0 key)
              :namespace (nth 1 key)
              :id (nth 2 key))))))

(defun mevedel-view-zone-next (&optional limit)
  "Move point to the next navigatable fragment before LIMIT.
Return point when movement succeeds, otherwise nil.  LIMIT defaults to
`point-max' and respects the current narrowing."
  (interactive)
  (let* ((limit (min (or limit (point-max)) (point-max)))
         (pos (point))
         (bounds (mevedel-view-zone--bounds-at pos)))
    (when bounds
      (setq pos (min limit (plist-get bounds :end))))
    (when-let* ((found (text-property-any
                        pos limit 'mevedel-view-zone-navigatable t)))
      (if-let* ((bounds (mevedel-view-zone--bounds-at found)))
          (goto-char (plist-get bounds :start))
        (goto-char found))
      (point))))

(defun mevedel-view-zone-previous (&optional limit)
  "Move point to the previous navigatable fragment after LIMIT.
Return point when movement succeeds, otherwise nil.  LIMIT defaults to
`point-min' and respects the current narrowing."
  (interactive)
  (let* ((limit (max (or limit (point-min)) (point-min)))
         (pos (point))
         (bounds (and (< pos (point-max))
                      (mevedel-view-zone--bounds-at pos)))
         found)
    (when (and bounds (> pos (plist-get bounds :start)))
      (setq pos (plist-get bounds :start)))
    (setq pos (1- pos))
    (while (and (>= pos limit) (not found))
      (if (get-text-property pos 'mevedel-view-zone-navigatable)
          (setq found pos)
        (setq pos (or (previous-single-property-change
                       pos 'mevedel-view-zone-navigatable nil limit)
                      limit))
        (unless (get-text-property pos 'mevedel-view-zone-navigatable)
          (setq pos (1- pos)))))
    (when found
      (if-let* ((bounds (mevedel-view-zone--bounds-at found)))
          (goto-char (plist-get bounds :start))
        (goto-char found))
      (point))))

(defun mevedel-view-zone--find-bounds (region namespace id)
  "Return bounds for fragment NAMESPACE ID inside REGION, or nil."
  (let* ((bounds (mevedel-view-zone--region-bounds region))
         (start (car bounds))
         (end (cdr bounds))
         (key (list (mevedel-view-zone--region-id region) namespace id))
         found-start found-end pos)
    (setq pos start)
    (while (and (< pos end) (not found-start))
      (if (mevedel-view-zone--key-match-p
           (get-text-property pos 'mevedel-view-zone-key) key)
          (setq found-start pos)
        (setq pos (or (next-single-property-change
                       pos 'mevedel-view-zone-key nil end)
                      end))))
    (when found-start
      (setq found-end found-start)
      (while (and (< found-end end)
                  (mevedel-view-zone--key-match-p
                   (get-text-property found-end
                                      'mevedel-view-zone-key)
                   key))
        (setq found-end (1+ found-end)))
      (list :start found-start :end found-end :key key
            :region (nth 0 key) :namespace namespace :id id))))

(defun mevedel-view-zone--call-mutating (thunk)
  "Call THUNK for a view-owned mutation with hooks suppressed."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (funcall thunk)))

(defun mevedel-view-zone--owned-spans (region)
  "Return text spans owned by managed REGION."
  (let* ((bounds (mevedel-view-zone--region-bounds region))
         (pos (car bounds))
         (end (cdr bounds))
         spans)
    (while (< pos end)
      (if (eq (get-text-property pos 'mevedel-view-zone-region) region)
          (let ((start pos))
            (while (and (< pos end)
                        (eq (get-text-property
                             pos 'mevedel-view-zone-region)
                            region))
              (setq pos (1+ pos)))
            (push (cons start pos) spans))
        (setq pos (or (next-single-property-change
                       pos 'mevedel-view-zone-region nil end)
                      end))))
    (nreverse spans)))

(defun mevedel-view-zone--fragment< (left right)
  "Return non-nil when LEFT should render before RIGHT."
  (let ((left-priority (or (plist-get left :priority) 0))
        (right-priority (or (plist-get right :priority) 0))
        (left-order (or (plist-get left :mevedel-view-zone--order) 0))
        (right-order (or (plist-get right :mevedel-view-zone--order) 0)))
    (if (= left-priority right-priority)
        (< left-order right-order)
      (> left-priority right-priority))))

(defun mevedel-view-zone--reconcile (region namespace fragments)
  "Reconcile FRAGMENTS for NAMESPACE inside managed REGION.
FRAGMENTS are sorted by descending priority and caller order for equal
priorities."
  (let* ((region-id (mevedel-view-zone--region-id region))
         (fragments
          (cl-loop for fragment in fragments
                   for order from 0
                   collect (plist-put (mevedel-view-zone--normalize fragment)
                                      :mevedel-view-zone--order order)))
         (fragments (sort fragments #'mevedel-view-zone--fragment<))
         (text (mapconcat (lambda (fragment)
                            (unless (eq (plist-get fragment :namespace) namespace)
                              (error "Fragment namespace mismatch: %S" fragment))
                            (mevedel-view-zone--render region fragment))
                          fragments ""))
         (old-bounds (mevedel-view-zone--region-bounds region))
         (region-start (car old-bounds))
         (spans (mevedel-view-zone--owned-spans region))
         (insert-marker (copy-marker (if spans (caar spans) region-start)))
         new-start new-end)
    (mevedel-view-zone--call-mutating
     (lambda ()
       (dolist (span (reverse spans))
         (delete-region (car span) (cdr span)))
       (goto-char insert-marker)
       (setq new-start (point))
       (insert text)
       (setq new-end (point))))
    (set-marker insert-marker nil)
    (mevedel-view-zone--set-region-bounds region new-start new-end)
    (list :start new-start :end new-end :region region-id :namespace namespace)))


;;
;;; Managed zones

(defun mevedel-view-zone--region-table ()
  "Return the current buffer's managed-zone region table."
  (unless (hash-table-p mevedel-view-zone--regions)
    (setq mevedel-view-zone--regions (make-hash-table :test #'eq)))
  mevedel-view-zone--regions)

(defun mevedel-view-zone--live-region (zone)
  "Return ZONE's live managed region, or nil."
  (when-let* ((region (gethash zone (mevedel-view-zone--region-table))))
    (if (and (overlayp region)
             (eq (overlay-buffer region) (current-buffer)))
        region
      (remhash zone mevedel-view-zone--regions)
      nil)))

(defun mevedel-view-zone--validate (zone start end fragments)
  "Validate ZONE, START, END, and FRAGMENTS."
  (unless (memq zone mevedel-view-zone--names)
    (error "Unknown managed zone: %S" zone))
  (unless (and (integer-or-marker-p start)
               (integer-or-marker-p end)
               (<= (point-min) start end (point-max)))
    (error "Invalid managed zone bounds: %S..%S" start end))
  (dolist (fragment fragments)
    (unless (eq (plist-get fragment :namespace) zone)
      (error "Fragment namespace mismatch: %S" fragment))))

(defun mevedel-view-zone--marker-types (zone)
  "Return insertion types for status, interaction, and input in ZONE."
  (pcase zone
    ('history-live '(t t t))
    ('status '(nil t t))
    ((or 'interaction 'progress) '(nil nil t))))

(defun mevedel-view-zone--call-with-marker-types (zone thunk)
  "Call THUNK with lower view markers advancing appropriately for ZONE."
  (let* ((markers (list mevedel-view--status-marker
                        mevedel-view--interaction-marker
                        mevedel-view--input-marker))
         (old-types (mapcar (lambda (marker)
                              (and (markerp marker)
                                   (marker-insertion-type marker)))
                            markers))
         (new-types (mevedel-view-zone--marker-types zone)))
    (unwind-protect
        (progn
          (cl-mapc (lambda (marker type)
                     (when (markerp marker)
                       (set-marker-insertion-type marker type)))
                   markers new-types)
          (funcall thunk))
      (cl-mapc (lambda (marker type)
                 (when (markerp marker)
                   (set-marker-insertion-type marker type)))
               markers old-types))))

(defun mevedel-view-zone--composer-live-p ()
  "Return non-nil when the current view has an editable composer."
  (and (not (bound-and-true-p mevedel-view--agent-transcript-p))
       (markerp mevedel-view--input-marker)
       (eq (marker-buffer mevedel-view--input-marker) (current-buffer))))

(defun mevedel-view-zone--input-start ()
  "Return the first editable composer position."
  (when (mevedel-view-zone--composer-live-p)
    (mevedel-view--input-start)))

(defun mevedel-view-zone--input-offset (position)
  "Return POSITION's composer-relative offset, or nil."
  (when-let* ((start (mevedel-view-zone--input-start)))
    (and (>= position start) (- position start))))

(defun mevedel-view-zone--capture-view-state ()
  "Capture composer text, point, and displayed-window state."
  (let ((input-start (mevedel-view-zone--input-start)))
    (list
     :input (and input-start
                 (buffer-substring-no-properties input-start (point-max)))
     :point (point)
     :point-offset (mevedel-view-zone--input-offset (point))
     :selected-window (selected-window)
     :windows
     (mapcar
      (lambda (window)
        (let ((window-point (window-point window)))
          (list window window-point (window-start window)
                (mevedel-view-zone--input-offset window-point))))
      (get-buffer-window-list (current-buffer) nil t)))))

(defun mevedel-view-zone--restore-view-state (state)
  "Restore composer text, point, and displayed windows from STATE."
  (when-let* ((input (plist-get state :input))
              (start (mevedel-view-zone--input-start)))
    (unless (equal input (buffer-substring-no-properties start (point-max)))
      (mevedel-view-zone--call-mutating
       (lambda ()
         (delete-region start (point-max))
         (goto-char start)
         (insert input)))))
  (let ((input-start (mevedel-view-zone--input-start)))
    (goto-char
     (min (point-max)
          (if (and input-start (plist-get state :point-offset))
              (+ input-start (plist-get state :point-offset))
            (plist-get state :point))))
    (dolist (entry (plist-get state :windows))
      (pcase-let ((`(,window ,window-point ,window-start ,input-offset) entry))
        (when (and (window-live-p window)
                   (eq (window-buffer window) (current-buffer)))
          (set-window-point
           window
           (min (point-max)
                (if (and input-start input-offset)
                    (+ input-start input-offset)
                  window-point)))
          (when (and window-start (<= window-start (point-max)))
            (set-window-start window window-start t))
          (when (eq window (plist-get state :selected-window))
            (goto-char (window-point window))))))))

(defun mevedel-view-zone--call-preserving (zone thunk)
  "Call THUNK with uniform view-state preservation for ZONE."
  (let ((state (mevedel-view-zone--capture-view-state))
        result)
    (unwind-protect
        (setq result
              (mevedel-view-zone--call-with-marker-types zone thunk))
      (mevedel-view-zone--restore-view-state state))
    result))

(defun mevedel-view-zone--capture-neighbor-regions (zone region)
  "Capture live managed regions other than ZONE REGION."
  (let ((zone-index (- (length mevedel-view-zone--names)
                       (length (memq zone mevedel-view-zone--names))))
        states)
    (maphash
     (lambda (other-zone other)
       (when (and (not (eq other region))
                  (overlayp other)
                  (eq (overlay-buffer other) (current-buffer)))
         (let* ((other-index
                 (- (length mevedel-view-zone--names)
                    (length (memq other-zone mevedel-view-zone--names))))
                (advance (> other-index zone-index)))
           (push (list other
                       (copy-marker (overlay-start other) advance)
                       (copy-marker (overlay-end other) advance))
                 states))))
     (mevedel-view-zone--region-table))
    states))

(defun mevedel-view-zone--restore-neighbor-regions (states)
  "Restore managed region overlays from captured STATES."
  (dolist (state states)
    (pcase-let ((`(,region ,start ,end) state))
      (when (and (overlayp region)
                 (eq (overlay-buffer region) (current-buffer)))
        (move-overlay region start end (current-buffer)))
      (set-marker start nil)
      (set-marker end nil))))

(defun mevedel-view-zone--delete-stale-text (zone region)
  "Delete ZONE text not owned by live REGION."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (pos (point-min)))
    (while (< pos (point-max))
      (if-let* ((start (text-property-any
                        pos (point-max)
                        'mevedel-view-zone-namespace zone)))
          (let* ((owner (get-text-property
                         start 'mevedel-view-zone-region))
                 (end (min (or (next-single-property-change
                                start 'mevedel-view-zone-namespace
                                nil (point-max))
                               (point-max))
                           (or (next-single-property-change
                                start 'mevedel-view-zone-region
                                nil (point-max))
                               (point-max)))))
            (if (eq owner region)
                (setq pos end)
              (delete-region start end)
              (setq pos start)))
        (setq pos (point-max))))))

(defun mevedel-view-zone--fragment-position-state (zone region position)
  "Return fragment-relative state for POSITION inside ZONE REGION."
  (when (and (integer-or-marker-p position)
             (< (overlay-start region) (overlay-end region))
             (<= (overlay-start region) position)
             (< position (overlay-end region)))
    (when-let* ((bounds (mevedel-view-zone--bounds-at position))
                ((eq (get-text-property
                      position 'mevedel-view-zone-namespace)
                     zone)))
      (list :id (get-text-property position 'mevedel-view-zone-id)
            :offset (- position (plist-get bounds :start))))))

(defun mevedel-view-zone--restore-fragment-position (zone state)
  "Return the restored buffer position for ZONE STATE, or nil."
  (when state
    (when-let* ((bounds (mevedel-view-zone-fragment-bounds
                         zone (plist-get state :id))))
      (min (1- (plist-get bounds :end))
           (+ (plist-get bounds :start)
              (plist-get state :offset))))))

(defun mevedel-view-zone--capture-logical-state (zone region)
  "Capture point and displayed-window positions inside ZONE REGION."
  (list
   :point (mevedel-view-zone--fragment-position-state zone region (point))
   :windows
   (mapcar
    (lambda (window)
      (list window
            (mevedel-view-zone--fragment-position-state
             zone region (window-point window))
            (mevedel-view-zone--fragment-position-state
             zone region (window-start window))))
    (get-buffer-window-list (current-buffer) nil t))))

(defun mevedel-view-zone--restore-logical-state (zone state)
  "Restore point and displayed-window positions for ZONE STATE."
  (when-let* ((position (mevedel-view-zone--restore-fragment-position
                         zone (plist-get state :point))))
    (goto-char position))
  (dolist (window-state (plist-get state :windows))
    (pcase-let ((`(,window ,point-state ,start-state) window-state))
      (when (window-live-p window)
        (when-let* ((position (mevedel-view-zone--restore-fragment-position
                               zone point-state)))
          (set-window-point window position))
        (when-let* ((position (mevedel-view-zone--restore-fragment-position
                               zone start-state)))
          (set-window-start window position t))))))

(defun mevedel-view-zone-reconcile (zone start end fragments)
  "Reconcile complete FRAGMENTS for fixed ZONE between START and END.
The zone owns its region identity, mutation preservation, marker
choreography, and cleanup.  START and END express view layout; existing
live zone text keeps its current bounds until reconciliation finishes."
  (mevedel-view-zone--validate zone start end fragments)
  (let* ((table (mevedel-view-zone--region-table))
         (region (mevedel-view-zone--live-region zone))
         (logical-state (and region
                             (mevedel-view-zone--capture-logical-state
                              zone region)))
         owned-p)
    (if (and (null fragments) (null region))
        (progn
          (mevedel-view-zone--call-preserving
           zone
           (lambda ()
             (mevedel-view-zone--delete-stale-text zone nil)))
          nil)
      (unless region
        (setq region (make-overlay start end (current-buffer) nil nil))
        (overlay-put region 'mevedel-view-zone zone)
        (overlay-put region 'evaporate nil)
        (puthash zone region table))
      (setq owned-p
            (and (< (overlay-start region) (overlay-end region))
                 (text-property-any
                  (overlay-start region) (overlay-end region)
                  'mevedel-view-zone-region region)))
      (let ((neighbors (mevedel-view-zone--capture-neighbor-regions
                        zone region)))
        (unwind-protect
            (progn
              (mevedel-view-zone--call-preserving
               zone
               (lambda ()
                 (mevedel-view-zone--delete-stale-text zone region)
                 (unless owned-p
                   (move-overlay region start end (current-buffer)))
                 (mevedel-view-zone--reconcile region zone fragments))))
          (mevedel-view-zone--restore-neighbor-regions neighbors)))
      (mevedel-view-zone--restore-logical-state zone logical-state)
      (if fragments
          region
        (delete-overlay region)
        (remhash zone table)
        nil))))

(defun mevedel-view-zone-region (zone)
  "Return ZONE's live managed region, or nil."
  (unless (memq zone mevedel-view-zone--names)
    (error "Unknown managed zone: %S" zone))
  (mevedel-view-zone--live-region zone))

(defun mevedel-view-zone-start (zone)
  "Return the start of live ZONE, or nil."
  (when-let* ((region (mevedel-view-zone-region zone)))
    (overlay-start region)))

(defun mevedel-view-zone-fragment-bounds (zone id)
  "Return bounds for fragment ID in live ZONE, or nil."
  (when-let* ((region (mevedel-view-zone-region zone)))
    (mevedel-view-zone--find-bounds region zone id)))

(defun mevedel-view-zone-bounds-at (&optional position)
  "Return managed fragment bounds at POSITION or point."
  (mevedel-view-zone--bounds-at position))

(defun mevedel-view-zone-clear (zone)
  "Remove all managed text and region state for ZONE."
  (let ((region (mevedel-view-zone-region zone)))
    (mevedel-view-zone-reconcile
     zone
     (if region (overlay-start region) (point-min))
     (if region (overlay-end region) (point-min))
     nil)))

(defun mevedel-view-zone-forget (&optional zone)
  "Forget disposable region state for ZONE, or every zone when nil.
Use after a larger redraw has already removed the managed text."
  (let ((table (mevedel-view-zone--region-table)))
    (if zone
        (progn
          (unless (memq zone mevedel-view-zone--names)
            (error "Unknown managed zone: %S" zone))
          (when-let* ((region (gethash zone table)))
            (when (overlayp region)
              (delete-overlay region)))
          (remhash zone table))
      (maphash (lambda (_name region)
                 (when (overlayp region)
                   (delete-overlay region)))
               table)
      (clrhash table))))

(provide 'mevedel-view-zone)
;;; mevedel-view-zone.el ends here
