;;; niwa-view-zone.el --- Managed view zones -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Owns the fixed fragment-backed regions of view chrome: status,
;; interaction, and progress.  Producers supply
;; complete fragment sets; this module owns region identity, overlay lifetime,
;; marker behavior,
;; preservation, reconciliation, recovery, collapse, and navigation.
;; Fragment text is a disposable cache.  Durable conversation state remains
;; in the data buffer and session structures.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'niwa-structs)
(require 'niwa-utilities)

;; `niwa-utilities'
(declare-function niwa-diagnostic-message "niwa-utilities" (scope format-string &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format-string &rest args))
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")

;; `niwa-view-render'
(declare-function niwa-view--debug-log "niwa-view-render"
                  (event &rest data))
(declare-function niwa-view--debug-state "niwa-view-render"
                  (&optional data-buf start end))
(defvar niwa-view-render-debug)


;; `niwa-view-viewport'
(declare-function niwa-view--call-with-render-boundaries-advancing "niwa-view-viewport"
                  (thunk &optional types))
(declare-function niwa-view--keep-native-vscroll "niwa-view-viewport" ())
(declare-function niwa-view-viewport-apply "niwa-view-viewport" (snapshot resolved))
(declare-function niwa-view-viewport-capture "niwa-view-viewport" (follow-output))
(autoload 'niwa-view--call-with-render-boundaries-advancing "niwa-view-viewport")
(autoload 'niwa-view--keep-native-vscroll "niwa-view-viewport")
(autoload 'niwa-view-viewport-apply "niwa-view-viewport")
(autoload 'niwa-view-viewport-capture "niwa-view-viewport")

(defvar-local niwa-view-zone--collapse-states nil
  "Buffer-local UI collapse state keyed by fragment collapse keys.")

(defvar-local niwa-view-zone--regions nil
  "Managed region overlays keyed by fixed zone name.")

(defvar-local niwa-view-zone--absent-zones nil
  "Zone names proven to have no text since the last region-table reset.")

(defconst niwa-view-zone--names
  '(status interaction progress)
  "Fixed names of fragment-backed view zones.")

(defconst niwa-view-zone-owned-properties
  '(niwa-view-zone-key niwa-view-zone-region niwa-view-zone-namespace
    niwa-view-zone-id niwa-view-zone-section niwa-view-zone-navigatable
    niwa-view-zone-entry niwa-view-zone-collapsible
    niwa-view-zone-collapse-key niwa-view-zone-collapsed
    niwa-view-zone-activate niwa-view-zone-anchor)
  "Text properties that tie managed zone text to its region.
Text yanked from a zone must not keep them: stale-text cleanup deletes
zone text its live region does not own, even inside the composer.")


;;
;;; Region and key helpers

(defun niwa-view-zone--region-id (region)
  "Return the identity object used for managed REGION keys."
  (unless (overlayp region)
    (error "Invalid managed zone region: %S" region))
  region)

(defun niwa-view-zone--key (region fragment)
  "Return the container-scoped key for FRAGMENT in REGION."
  (let ((namespace (plist-get fragment :namespace))
        (id (plist-get fragment :id)))
    (unless namespace
      (error "Missing fragment namespace"))
    (unless id
      (error "Missing fragment id"))
    (list (niwa-view-zone--region-id region) namespace id)))

(defun niwa-view-zone--key-match-p (left right)
  "Return non-nil when LEFT and RIGHT are the same fragment key."
  (and (consp left) (consp right)
       (eq (car left) (car right))
       (eq (cadr left) (cadr right))
       (equal (caddr left) (caddr right))))

(defun niwa-view-zone--collapse-key (region fragment)
  "Return the collapse-state key for FRAGMENT in REGION."
  (or (plist-get fragment :collapse-key)
      (niwa-view-zone--key region fragment)))

(defun niwa-view-zone--collapse-state-table ()
  "Return the current buffer's fragment collapse-state table."
  (unless (hash-table-p niwa-view-zone--collapse-states)
    (setq niwa-view-zone--collapse-states
          (make-hash-table :test #'equal)))
  niwa-view-zone--collapse-states)

(defun niwa-view-zone-collapse-state-set-p (key)
  "Return non-nil when KEY has explicit fragment collapse state."
  (let ((table (niwa-view-zone--collapse-state-table)))
    (not (eq (gethash key table :niwa-view-zone--missing)
             :niwa-view-zone--missing))))

(defun niwa-view-zone-collapse-state (key &optional default)
  "Return fragment collapse state for KEY, or DEFAULT when unset."
  (let ((table (niwa-view-zone--collapse-state-table)))
    (gethash key table default)))

(defun niwa-view-zone-set-collapse-state (key collapsed)
  "Set fragment collapse state for KEY to COLLAPSED."
  (puthash key (and collapsed t)
           (niwa-view-zone--collapse-state-table)))

(defun niwa-view-zone-toggle-collapse-state (key &optional default)
  "Toggle fragment collapse state for KEY, defaulting to DEFAULT."
  (let ((collapsed (not (niwa-view-zone-collapse-state key default))))
    (niwa-view-zone-set-collapse-state key collapsed)
    collapsed))

(defun niwa-view-zone--fragment-collapsed-p (region fragment)
  "Return non-nil when FRAGMENT is currently collapsed in REGION."
  (and (plist-get fragment :collapsible)
       (niwa-view-zone-collapse-state
        (niwa-view-zone--collapse-key region fragment)
        (plist-get fragment :collapsed))))

(defun niwa-view-zone-toggle-collapsed (&optional position)
  "Toggle the collapsible fragment at POSITION or point.
Return the new collapsed state, or signal when there is no collapsible
fragment at POSITION.  This updates UI state only; callers are
responsible for rerendering the producer."
  (interactive)
  (let* ((position (or position (point)))
         (key (get-text-property position 'niwa-view-zone-collapse-key))
         (collapsible (get-text-property position
                                         'niwa-view-zone-collapsible))
         (default (get-text-property position
                                     'niwa-view-zone-collapsed)))
    (unless (and key collapsible)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No collapsible fragment at point"))
    (niwa-view-zone-toggle-collapse-state key default)))

(defun niwa-view-zone--normalize (fragment)
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

(defun niwa-view-zone--region-bounds (region)
  "Return `(START . END)' bounds for managed REGION."
  (unless (and (overlayp region) (overlay-buffer region))
    (error "Managed zone region has no buffer"))
  (cons (overlay-start region) (overlay-end region)))

(defun niwa-view-zone--set-region-bounds (region start end)
  "Set managed REGION bounds to START and END."
  (move-overlay region start end (current-buffer)))


;;
;;; Width

(defvar niwa-view-zone-width-change-functions nil
  "Functions run with no arguments when a view's zone width changes.
Each runs with the view buffer current, once per change of
`niwa-view-zone-width'.  Chrome whose layout depends on that width
re-renders here, so a resize does not leave rows wrapped until an
unrelated redraw changes their height.")

(defvar-local niwa-view-zone--width nil
  "Zone width last reported to `niwa-view-zone-width-change-functions'.")

(defun niwa-view-zone-width ()
  "Return the narrowest body width of the windows showing this buffer.
Without such a window use the selected window's width, or the frame's
when that window is a composer."
  (let ((windows (niwa-view--windows 'geometry)))
    (cond
     (windows (apply #'min (mapcar #'window-body-width windows)))
     ((eq (buffer-local-value 'major-mode (window-buffer)) 'niwa-composer-mode)
      (frame-width))
     (t (window-body-width)))))

(defun niwa-view-zone--on-window-change (_window)
  "Run `niwa-view-zone-width-change-functions' if the zone width changed."
  (let ((width (niwa-view-zone-width)))
    (unless (eql width niwa-view-zone--width)
      (setq niwa-view-zone--width width)
      (let ((scope (niwa-diagnostic-scope
                    (or niwa--session niwa--data-buffer))))
        (condition-case-unless-debug err
            (run-hooks 'niwa-view-zone-width-change-functions)
          (error (niwa-diagnostic-message scope "%s" (error-message-string err))))))))

(defun niwa-view-zone--watch-width ()
  "Start reporting width changes of the windows showing this buffer."
  (setq niwa-view-zone--width (niwa-view-zone-width))
  (add-hook 'window-size-change-functions #'niwa-view-zone--on-window-change nil t)
  (add-hook 'window-buffer-change-functions #'niwa-view-zone--on-window-change nil t))


;;
;;; Rendering

(defun niwa-view-zone--section-properties (region fragment section)
  "Return text properties for FRAGMENT SECTION in REGION.
A body's activation is a default the body text may override line by line,
so it is applied by `niwa-view-zone--body-text' instead."
  (let* ((key (niwa-view-zone--key region fragment))
         (read-only (plist-get fragment :read-only))
         (collapse-key (and (or (plist-get fragment :collapsible)
                                (plist-member fragment :collapse-key))
                            (niwa-view-zone--collapse-key
                             region fragment)))
         (collapsed (niwa-view-zone--fragment-collapsed-p
                     region fragment))
         (body-properties-owned
          (plist-get fragment :body-properties-owned))
         (props `(niwa-view-zone-key ,key
                  niwa-view-zone-region ,(car key)
                  niwa-view-zone-namespace ,(plist-get fragment :namespace)
                  niwa-view-zone-id ,(plist-get fragment :id)
                  niwa-view-zone-section ,section
                  niwa-view-zone-navigatable
                  ,(plist-get fragment :navigatable)
                  niwa-view-zone-entry ,(plist-get fragment :entry)
                  niwa-view-zone-collapsible
                  ,(plist-get fragment :collapsible)
                  niwa-view-zone-collapse-key ,collapse-key
                  niwa-view-zone-collapsed ,collapsed)))
    (unless (eq section 'body)
      (setq props (append `(niwa-view-zone-activate
                            ,(plist-get fragment :activate))
                          props)))
    (unless body-properties-owned
      (setq props
            (append
             '(rear-nonsticky
               (read-only font-lock-face face keymap help-echo)
               front-sticky (read-only))
             props)))
    (when read-only
      (setq props (append `(read-only ,read-only) props)))
    props))

(defun niwa-view-zone--add-default-property (text property value)
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

(defun niwa-view-zone--single-trailing-newline (text &optional force)
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

(defun niwa-view-zone--add-body-defaults (text fragment)
  "Return TEXT with FRAGMENT's interactive properties where TEXT lacks them."
  (dolist (spec '((keymap . :keymap)
                  (help-echo . :help-echo)
                  (niwa-view-zone-activate . :activate))
                text)
    (setq text (niwa-view-zone--add-default-property
                text (car spec) (plist-get fragment (cdr spec))))))

(defun niwa-view-zone--body-text (fragment &optional force-newline)
  "Return FRAGMENT body as a string with default interactive properties.
Text already carrying its own keymap, help, or activation keeps it, so one
line of a fragment can act differently from the fragment as a whole.
When FORCE-NEWLINE is non-nil, return one tagged newline for an empty body."
  (let ((body (plist-get fragment :body)))
    (setq body
          (cond
           ((stringp body) (copy-sequence body))
           ((null body) "")
           (t (format "%s" body))))
    (setq body (niwa-view-zone--single-trailing-newline
                body force-newline))
    (niwa-view-zone--add-body-defaults body fragment)))

(defun niwa-view-zone--label-text (fragment)
  "Return FRAGMENT label text, or nil when it has no label."
  (let ((left (plist-get fragment :label-left))
        (right (plist-get fragment :label-right)))
    (cond
     ((and left right) (format "%s %s" left right))
     (left (format "%s" left))
     (right (format "%s" right)))))

(defun niwa-view-zone--render (region fragment)
  "Return propertized text for FRAGMENT in managed REGION."
  (let* ((fragment (niwa-view-zone--normalize fragment))
         (label (niwa-view-zone--label-text fragment))
         (collapsed (niwa-view-zone--fragment-collapsed-p
                     region fragment))
         (body (if (and label collapsed)
                   ""
                 (niwa-view-zone--body-text fragment (not label))))
         (body-suffix (plist-get fragment :body-suffix))
         (text ""))
    (when label
      (setq label (concat label "\n"))
      (add-text-properties
       0 (length label)
       (niwa-view-zone--section-properties region fragment 'label)
       label)
      (setq text (concat text label)))
    (when (> (length body) 0)
      (add-text-properties
       0 (length body)
       (niwa-view-zone--section-properties region fragment 'body)
       body)
      (setq text (concat text body))
      (when body-suffix
        (setq body-suffix (niwa-view-zone--add-body-defaults
                           (copy-sequence (format "%s" body-suffix))
                           fragment))
        (add-text-properties
         0 (length body-suffix)
         (niwa-view-zone--section-properties region fragment 'body)
         body-suffix)
        (setq text (concat text body-suffix))))
    text))


;;
;;; Bounds and mutation

(defun niwa-view-zone--bounds-at (&optional position)
  "Return fragment bounds at POSITION, or nil when no fragment is there."
  (let* ((position (or position (point)))
         (key (get-text-property position 'niwa-view-zone-key)))
    (when key
      (let ((start (if (or (= position (point-min))
                           (not (equal
                                 key
                                 (get-text-property
                                  (1- position) 'niwa-view-zone-key))))
                       position
                     (or (previous-single-property-change
                          position 'niwa-view-zone-key
                          nil (point-min))
                         (point-min))))
            (end (or (next-single-property-change
                      position 'niwa-view-zone-key nil (point-max))
                     (point-max))))
        (list :start start :end end :key key
              :region (nth 0 key)
              :namespace (nth 1 key)
              :id (nth 2 key))))))

(defun niwa-view-zone-next (&optional limit)
  "Move point to the next navigatable fragment before LIMIT.
Return point when movement succeeds, otherwise nil.  LIMIT defaults to
`point-max' and respects the current narrowing."
  (interactive)
  (let* ((limit (min (or limit (point-max)) (point-max)))
         (pos (point))
         (bounds (niwa-view-zone--bounds-at pos)))
    (when bounds
      (setq pos (min limit (plist-get bounds :end))))
    (when-let* ((found (text-property-any
                        pos limit 'niwa-view-zone-navigatable t)))
      (if-let* ((bounds (niwa-view-zone--bounds-at found)))
          (goto-char (plist-get bounds :start))
        (goto-char found))
      (point))))

(defun niwa-view-zone-previous (&optional limit)
  "Move point to the previous navigatable fragment after LIMIT.
Return point when movement succeeds, otherwise nil.  LIMIT defaults to
`point-min' and respects the current narrowing."
  (interactive)
  (let* ((limit (max (or limit (point-min)) (point-min)))
         (pos (point))
         (bounds (and (< pos (point-max))
                      (niwa-view-zone--bounds-at pos)))
         found)
    (when (and bounds (> pos (plist-get bounds :start)))
      (setq pos (plist-get bounds :start)))
    (setq pos (1- pos))
    (while (and (>= pos limit) (not found))
      (if (get-text-property pos 'niwa-view-zone-navigatable)
          (setq found pos)
        (setq pos (or (previous-single-property-change
                       pos 'niwa-view-zone-navigatable nil limit)
                      limit))
        (unless (get-text-property pos 'niwa-view-zone-navigatable)
          (setq pos (1- pos)))))
    (when found
      (if-let* ((bounds (niwa-view-zone--bounds-at found)))
          (goto-char (plist-get bounds :start))
        (goto-char found))
      (point))))

(defun niwa-view-zone--find-bounds (region namespace id)
  "Return bounds for fragment NAMESPACE ID inside REGION, or nil."
  (let* ((bounds (niwa-view-zone--region-bounds region))
         (start (car bounds))
         (end (cdr bounds))
         (key (list (niwa-view-zone--region-id region) namespace id))
         found-start found-end pos)
    (setq pos start)
    (while (and (< pos end) (not found-start))
      (if (niwa-view-zone--key-match-p
           (get-text-property pos 'niwa-view-zone-key) key)
          (setq found-start pos)
        (setq pos (or (next-single-property-change
                       pos 'niwa-view-zone-key nil end)
                      end))))
    (when found-start
      (setq found-end found-start)
      (while (and (< found-end end)
                  (niwa-view-zone--key-match-p
                   (get-text-property found-end
                                      'niwa-view-zone-key)
                   key))
        (setq found-end (1+ found-end)))
      (list :start found-start :end found-end :key key
            :region (nth 0 key) :namespace namespace :id id))))

(defun niwa-view-zone--call-mutating (thunk)
  "Call THUNK for a view-owned mutation with hooks suppressed."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (funcall thunk)))

(defun niwa-view-zone--owned-spans (region)
  "Return text spans owned by managed REGION."
  (let* ((bounds (niwa-view-zone--region-bounds region))
         (pos (car bounds))
         (end (cdr bounds))
         spans)
    (while (< pos end)
      (if (eq (get-text-property pos 'niwa-view-zone-region) region)
          (let ((start pos))
            (while (and (< pos end)
                        (eq (get-text-property
                             pos 'niwa-view-zone-region)
                            region))
              (setq pos (1+ pos)))
            (push (cons start pos) spans))
        (setq pos (or (next-single-property-change
                       pos 'niwa-view-zone-region nil end)
                      end))))
    (nreverse spans)))

(defun niwa-view-zone--render-fragments (region namespace fragments)
  "Render FRAGMENTS for NAMESPACE inside REGION in stable priority order."
  (let ((fragments
          (sort (mapcar #'niwa-view-zone--normalize fragments)
                :key (lambda (fragment) (or (plist-get fragment :priority) 0))
                :lessp #'> :in-place t)))
    (mapconcat (lambda (fragment)
                 (unless (eq (plist-get fragment :namespace) namespace)
                   (error "Fragment namespace mismatch: %S" fragment))
                 (niwa-view-zone--render region fragment))
               fragments "")))

(defun niwa-view-zone--unchanged-p (region namespace text)
  "Return non-nil if live REGION already contains TEXT with no stale NAMESPACE.
Compare properties too: equal characters alone can still change display.
Any namespace text outside REGION takes the ordinary cleanup path.
Compare the small region first: changed text, such as a ticking elapsed
label, takes that path anyway, without scanning the whole buffer here."
  (let ((start (overlay-start region))
        (end (overlay-end region)))
    (and (< start end)
         (not (text-property-not-all start end 'niwa-view-zone-region region))
         (equal-including-properties text (buffer-substring start end))
         (not (text-property-any (point-min) start
                                'niwa-view-zone-namespace namespace))
         (not (text-property-any end (point-max)
                                'niwa-view-zone-namespace namespace)))))

(defun niwa-view-zone--absent-p (zone region fragments)
  "Return non-nil if ZONE has no FRAGMENTS, live REGION or stale text.
Such a zone has nothing to reconcile, so it needs no viewport transaction.
Stale text, such as text that outlived lost local state, still needs one.
One whole-buffer scan proves absence until the region table entry changes:
only zone rendering writes zone text, and creating a region or losing the
table discards the proof."
  (and (null fragments)
       (null region)
       (or (memq zone niwa-view-zone--absent-zones)
           (unless (text-property-any (point-min) (point-max)
                                      'niwa-view-zone-namespace zone)
             (push zone niwa-view-zone--absent-zones)))))

(defun niwa-view-zone--reconcile (region namespace text)
  "Reconcile rendered TEXT for NAMESPACE inside managed REGION."
  (let* ((region-id (niwa-view-zone--region-id region))
         (old-bounds (niwa-view-zone--region-bounds region))
         (region-start (car old-bounds))
         (spans (niwa-view-zone--owned-spans region))
         (unchanged-p
          (and (= (length spans) 1)
               (equal (car spans) old-bounds)
               (equal-including-properties
                text
                (buffer-substring
                 (caar spans) (cdar spans)))))
         (insert-marker (copy-marker (if spans (caar spans) region-start)))
         (new-start region-start)
         (new-end (cdr old-bounds)))
    (unless unchanged-p
      (niwa-view-zone--call-mutating
       (lambda ()
         (dolist (span (reverse spans))
           (delete-region (car span) (cdr span)))
         (goto-char insert-marker)
         (setq new-start (point))
         (insert text)
         (setq new-end (point))))
      (niwa-view-zone--set-region-bounds region new-start new-end))
    (set-marker insert-marker nil)
    (list :start new-start :end new-end :region region-id :namespace namespace)))


;;
;;; Managed zones

(defun niwa-view-zone--region-table ()
  "Return the current buffer's managed-zone region table."
  (unless (hash-table-p niwa-view-zone--regions)
    (setq niwa-view-zone--regions (make-hash-table :test #'eq)
          niwa-view-zone--absent-zones nil)
    (niwa-view-zone--watch-width))
  niwa-view-zone--regions)

(defun niwa-view-zone--live-region (zone)
  "Return ZONE's live managed region, or nil."
  (let ((region (gethash zone (niwa-view-zone--region-table))))
    (cond ((not region) nil)
          ((eq (overlay-buffer region) (current-buffer)) region)
          (t (remhash zone niwa-view-zone--regions) nil))))

(defun niwa-view-zone--validate (zone start end fragments)
  "Validate ZONE, START, END, and FRAGMENTS."
  (unless (memq zone niwa-view-zone--names)
    (error "Unknown managed zone: %S" zone))
  (unless (and (integer-or-marker-p start)
               (integer-or-marker-p end)
               (<= (point-min) start end (point-max)))
    (error "Invalid managed zone bounds: %S..%S" start end))
  (dolist (fragment fragments)
    (unless (eq (plist-get fragment :namespace) zone)
      (error "Fragment namespace mismatch: %S" fragment))))

(defun niwa-view-zone--marker-types (zone)
  "Return insertion types for status, interaction, and tail in ZONE."
  (pcase zone
    ('status '(nil t t))
    ((or 'interaction 'progress) '(nil nil t))))

(defun niwa-view-zone--call-with-marker-types (zone thunk)
  "Call THUNK with the boundary insertion types for ZONE."
  (niwa-view--call-with-render-boundaries-advancing thunk (niwa-view-zone--marker-types zone)))

(defun niwa-view-zone--capture-view-state ()
  "Capture point, mark and displayed-window positions."
  (list
   :point (copy-marker (point) t)
   :mark (and (mark t) (copy-marker (mark t) t))
   :mark-active mark-active
   :deactivate-mark deactivate-mark
   :selected-window (selected-window)
   :windows
   (mapcar (lambda (window)
             (list window (copy-marker (window-point window) t)
                   (copy-marker (window-start window) t)))
           (niwa-view--windows 'geometry))))

(defun niwa-view-zone--restore-view-state (state)
  "Restore point, mark and displayed windows from STATE."
  (goto-char (min (point-max) (plist-get state :point)))
  (pcase-dolist (`(,window ,window-point ,window-start) (plist-get state :windows))
    (when (and (window-live-p window)
               (eq (window-buffer window) (current-buffer)))
      (set-window-point window (min (point-max) window-point))
      (when (and window-start (<= window-start (point-max)))
        (set-window-start window window-start t))
      (when (eq window (plist-get state :selected-window))
        (goto-char (window-point window)))))
  (when-let* ((mark (plist-get state :mark)))
    (set-marker (mark-marker) (min (point-max) mark)))
  (setq mark-active (plist-get state :mark-active)
        deactivate-mark (plist-get state :deactivate-mark))
  ;; Also on unwind after a failed mutation, which skips the reconcile's tail.
  (niwa-view--keep-native-vscroll))

(defun niwa-view-zone--call-preserving (zone thunk)
  "Call THUNK with uniform view-state preservation for ZONE."
  (let ((state (niwa-view-zone--capture-view-state))
        (inhibit-redisplay t)
        result)
    (unwind-protect
        (setq result (niwa-view-zone--call-with-marker-types zone thunk))
      (unwind-protect
          (niwa-view-zone--restore-view-state state)
        (set-marker (plist-get state :point) nil)
        (when-let* ((mark (plist-get state :mark)))
          (set-marker mark nil))
        (dolist (entry (plist-get state :windows))
          (set-marker (nth 1 entry) nil)
          (set-marker (nth 2 entry) nil))))
    result))

(defun niwa-view-zone--capture-neighbor-regions (zone region)
  "Capture live managed regions other than ZONE REGION."
  (let ((zone-index (- (length niwa-view-zone--names)
                       (length (memq zone niwa-view-zone--names))))
        states)
    (maphash
     (lambda (other-zone other)
       (when (and (not (eq other region))
                  (eq (overlay-buffer other) (current-buffer)))
         (let* ((other-index
                 (- (length niwa-view-zone--names)
                    (length (memq other-zone niwa-view-zone--names))))
                (advance (> other-index zone-index)))
           (push (list other
                       (copy-marker (overlay-start other) advance)
                       (copy-marker (overlay-end other) advance))
                 states))))
     (niwa-view-zone--region-table))
    states))

(defun niwa-view-zone--restore-neighbor-regions (states)
  "Restore managed region overlays from captured STATES."
  (dolist (state states)
    (pcase-let ((`(,region ,start ,end) state))
      (when (and (overlayp region)
                 (eq (overlay-buffer region) (current-buffer)))
        (move-overlay region start end (current-buffer)))
      (set-marker start nil)
      (set-marker end nil))))

(defun niwa-view-zone--capture-trailing-overlays (region)
  "Return foreign overlays starting exactly where REGION ends.
Such an overlay belongs to the text after the zone, such as `hl-line' on
the composer's line.  Re-rendering the zone deletes and reinserts text at
that boundary, which an overlay without front advance would swallow; the
rail directly above the composer re-renders every second."
  (let ((end (overlay-end region)))
    (seq-filter (lambda (overlay)
                  (and (= (overlay-start overlay) end)
                       (not (overlay-get overlay 'niwa-view-zone))))
                (overlays-in end (min (point-max) (1+ end))))))

(defun niwa-view-zone--restore-trailing-overlays (region overlays)
  "Start each of OVERLAYS at REGION's end again, as before reconciliation."
  (let ((end (overlay-end region)))
    (dolist (overlay overlays)
      (when (and (overlay-buffer overlay) (< (overlay-start overlay) end))
        (move-overlay overlay end (max end (overlay-end overlay)))))))

(defun niwa-view-zone--delete-stale-text (zone region)
  "Delete ZONE text not owned by live REGION."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (pos (point-min)))
    (while (< pos (point-max))
      (if-let* ((start (text-property-any
                        pos (point-max)
                        'niwa-view-zone-namespace zone)))
          (let* ((owner (get-text-property
                         start 'niwa-view-zone-region))
                 (end (min (or (next-single-property-change
                                start 'niwa-view-zone-namespace
                                nil (point-max))
                               (point-max))
                           (or (next-single-property-change
                                start 'niwa-view-zone-region
                                nil (point-max))
                               (point-max)))))
            (if (eq owner region)
                (setq pos end)
              (delete-region start end)
              (setq pos start)))
        (setq pos (point-max))))))

(defun niwa-view-zone--anchor-start (position anchor start)
  "Return the start of POSITION's ANCHOR run, not before START."
  (if (and (> position start)
           (eq anchor (get-text-property (1- position) 'niwa-view-zone-anchor)))
      (or (previous-single-property-change position 'niwa-view-zone-anchor nil start)
          start)
    position))

(defun niwa-view-zone--anchor-run (anchor start end)
  "Return (RUN-START . RUN-END) of the first text `equal' to ANCHOR.
Search the `niwa-view-zone-anchor' runs between START and END."
  (let ((pos start) found)
    (while (and (< pos end) (not found))
      (let ((next (or (next-single-property-change
                       pos 'niwa-view-zone-anchor nil end)
                      end)))
        (when (equal anchor (get-text-property pos 'niwa-view-zone-anchor))
          (setq found (cons pos next)))
        (setq pos next)))
    found))

(defun niwa-view-zone--parent-anchor-run (anchor start end)
  "Return the run of ANCHOR's nearest surviving parent between START and END.
An anchor (PARENT PART...) whose PARENT is itself a list names a part of
PARENT's row, such as an expanded agent's details; return nil when no
parent is left."
  (let (run)
    (while (and (not run) (consp anchor) (consp (car anchor)))
      (setq anchor (car anchor)
            run (niwa-view-zone--anchor-run anchor start end)))
    run))

(defun niwa-view-zone--fragment-position-state (zone region position
                                                        &optional window)
  "Return fragment-relative state for POSITION inside ZONE REGION.
Text a producer tagged with a `niwa-view-zone-anchor' value is
located by that value, so a row keeps its identity when text before it
changes length.  With WINDOW, POSITION is that window's start and the
offset counts its screen rows, so a re-wrapped line above it does not
shift the view by a row."
  (when (and (integer-or-marker-p position)
             (< (overlay-start region) (overlay-end region))
             (<= (overlay-start region) position)
             (< position (overlay-end region)))
    (when-let* ((bounds (niwa-view-zone--bounds-at position))
                ((eq (get-text-property
                      position 'niwa-view-zone-namespace)
                     zone)))
      (let* ((anchor (get-text-property position 'niwa-view-zone-anchor))
             (start (if anchor
                        (niwa-view-zone--anchor-start
                         position anchor (plist-get bounds :start))
                      (plist-get bounds :start))))
        (list :id (get-text-property position 'niwa-view-zone-id)
              :anchor anchor
              :window window
              :offset (if window
                          (count-screen-lines start position nil window)
                        (- position start)))))))

(defun niwa-view-zone--restore-fragment-position (zone state &optional strict)
  "Return the restored buffer position for ZONE STATE, or nil.
An anchored position whose anchor no longer exists returns to the start
of its nearest surviving parent anchor, or else to the fragment start,
rather than to an offset that now names other text.  With STRICT, return
nil instead when the anchor or its offset was lost."
  (when state
    (when-let* ((bounds (niwa-view-zone-fragment-bounds
                         zone (plist-get state :id))))
      (let* ((start (plist-get bounds :start))
             (end (plist-get bounds :end))
             (anchor (plist-get state :anchor))
             (run (if anchor
                      (niwa-view-zone--anchor-run anchor start end)
                    (cons start end)))
             (window (plist-get state :window)))
        (cond
         ((null run)
          (unless strict
            (or (car (niwa-view-zone--parent-anchor-run anchor start end))
                start)))
         ((and window (window-live-p window))
          (save-excursion
            (goto-char (car run))
            (let ((moved (vertical-motion (plist-get state :offset) window)))
              (when (or (not strict)
                        (and (= moved (plist-get state :offset))
                             (< (point) (cdr run))))
                (min (point) (1- end))))))
         (t (let ((position (+ (car run) (plist-get state :offset))))
              (when (or (not strict) (< position (cdr run)))
                (min (1- (cdr run)) position)))))))))

(defun niwa-view-zone-position-anchor (position &optional window)
  "Return an opaque identity for POSITION in managed zone text, or nil.
With WINDOW, POSITION is that window's start, kept by screen rows.
Pass the result to `niwa-view-zone-anchor-position' after the zone
was redrawn.  Producers make sub-parts of a fragment keep their identity
by tagging them with a `niwa-view-zone-anchor' text property whose
value is `equal' across redraws."
  (when-let* (((integer-or-marker-p position))
              ((< position (point-max)))
              (zone (get-text-property position 'niwa-view-zone-namespace))
              (region (niwa-view-zone--live-region zone))
              (state (niwa-view-zone--fragment-position-state
                      zone region position window)))
    (cons zone state)))

(defun niwa-view-zone-anchor-position (anchor &optional strict)
  "Return the current position of ANCHOR from `niwa-view-zone-position-anchor'.
Return nil when its fragment is gone.  With STRICT, also return nil when
its sub-anchor or offset no longer exists, rather than clamping it."
  (niwa-view-zone--restore-fragment-position (car anchor) (cdr anchor) strict))

(defun niwa-view-zone-visit-anchor (position)
  "Return an anchor for POSITION when it visits a managed row, or nil.
Interaction cards and status rows (background work, tasks, notices) are
rows a user visits deliberately: a redraw keeps such a row's screen
position instead of applying the reading-start policy."
  (and (memq (get-text-property position 'niwa-view-zone-namespace)
             '(interaction status))
       (niwa-view-zone-position-anchor position)))

(defun niwa-view-zone--capture-logical-state (zone region)
  "Capture logical positions in ZONE REGION for the buffer and its windows."
  (list
   :point (and region
               (niwa-view-zone--fragment-position-state zone region (point)))
   :mark (and region (mark t)
              (niwa-view-zone--fragment-position-state zone region (mark t)))
   :windows
   (mapcar
    (lambda (window)
      (list window
            (and region (niwa-view-zone--fragment-position-state
                         zone region (window-point window)))
            (and region (niwa-view-zone--fragment-position-state
                         zone region (window-start window) window))))
    (niwa-view--windows 'geometry))))

(defun niwa-view-zone--restore-logical-state (zone state snapshot)
  "Resolve positions in ZONE from STATE, then apply the viewport SNAPSHOT."
  (when-let* ((position (niwa-view-zone--restore-fragment-position
                         zone (plist-get state :point))))
    (goto-char position))
  (when-let* ((position (niwa-view-zone--restore-fragment-position
                         zone (plist-get state :mark))))
    (set-marker (mark-marker) position))
  (let (resolved)
    (dolist (entry (plist-get state :windows))
      (pcase-let ((`(,window ,point-state ,start-state) entry))
        (when (and (window-live-p window) (eq (window-buffer window) (current-buffer)))
          (when-let* ((position (niwa-view-zone--restore-fragment-position zone point-state)))
            (set-window-point window position))
          (let ((start (niwa-view-zone--restore-fragment-position zone start-state t)))
            (push (list window (or start (niwa-view-zone--restore-fragment-position
                                          zone start-state))
                        :lost-p (and start-state (not start)))
                  resolved)))))
    (niwa-view-viewport-apply snapshot (nreverse resolved))))

(defun niwa-view-zone-reconcile (zone start end fragments)
  "Reconcile complete FRAGMENTS for fixed ZONE between START and END.
The zone owns its region identity, mutation preservation, marker
choreography, and cleanup.  START and END express view layout; existing
live zone text keeps its current bounds until reconciliation finishes."
  (niwa-view-zone--validate zone start end fragments)
  (when (and (boundp 'niwa-view-render-debug)
             niwa-view-render-debug)
    (niwa-view--debug-log
     'zone-reconcile-begin
     :zone zone
     :start start
     :end end
     :fragment-ids (mapcar (lambda (fragment)
                             (plist-get fragment :id))
                           fragments)
     :state (niwa-view--debug-state)))
  (unwind-protect
      (let* ((table (niwa-view-zone--region-table))
             (region (niwa-view-zone--live-region zone))
             (text (and region (niwa-view-zone--render-fragments
                                region zone fragments))))
        (if (or (niwa-view-zone--absent-p zone region fragments)
                (and region (niwa-view-zone--unchanged-p region zone text)))
            region
          (let* ((snapshot (niwa-view-viewport-capture nil))
                 (logical-state (niwa-view-zone--capture-logical-state zone region))
                 owned-p)
            (if (and (null fragments) (null region))
		(niwa-view-zone--call-preserving
		 zone
		 (lambda ()
		   (niwa-view-zone--delete-stale-text zone nil)))
              (unless region
		;; History inserted at the boundary belongs before this zone.
		(setq region (make-overlay start end (current-buffer) t nil))
		(overlay-put region 'niwa-view-zone zone)
		(overlay-put region 'evaporate nil)
		(puthash zone region table)
                (setq niwa-view-zone--absent-zones
                      (delq zone niwa-view-zone--absent-zones)))
              (setq owned-p
                    (and (< (overlay-start region) (overlay-end region))
			 (text-property-any
			  (overlay-start region) (overlay-end region)
			  'niwa-view-zone-region region)))
              (let ((neighbors (niwa-view-zone--capture-neighbor-regions
				zone region))
                    (trailing (and owned-p
				   (niwa-view-zone--capture-trailing-overlays region))))
		(unwind-protect
                    (progn
                      (niwa-view-zone--call-preserving
                       zone
                       (lambda ()
			 (niwa-view-zone--delete-stale-text zone region)
			 (unless owned-p
			   (move-overlay region start end (current-buffer)))
			 (niwa-view-zone--reconcile
			  region zone (or text (niwa-view-zone--render-fragments
						region zone fragments))))))
		  (niwa-view-zone--restore-neighbor-regions neighbors)
		  (when (overlay-buffer region)
                    (niwa-view-zone--restore-trailing-overlays region trailing)))))
            (niwa-view-zone--restore-logical-state zone logical-state snapshot)
            (if fragments
		region
              (when region
		(delete-overlay region)
		(remhash zone table))
              nil))))
    (when (and (boundp 'niwa-view-render-debug)
               niwa-view-render-debug)
      (niwa-view--debug-log
       'zone-reconcile-end
       :zone zone
       :fragment-ids (mapcar (lambda (fragment)
                               (plist-get fragment :id))
                             fragments)
       :state (niwa-view--debug-state)))))

(defun niwa-view-zone-region (zone)
  "Return ZONE's live managed region, or nil."
  (unless (memq zone niwa-view-zone--names)
    (error "Unknown managed zone: %S" zone))
  (niwa-view-zone--live-region zone))

(defun niwa-view-zone-fragment-bounds (zone id)
  "Return bounds for fragment ID in live ZONE, or nil."
  (when-let* ((region (niwa-view-zone-region zone)))
    (niwa-view-zone--find-bounds region zone id)))

(defun niwa-view-zone-bounds-at (&optional position)
  "Return managed fragment bounds at POSITION or point."
  (niwa-view-zone--bounds-at position))

(defun niwa-view-zone-clear (zone)
  "Remove all managed text and region state for ZONE."
  (let ((region (niwa-view-zone-region zone)))
    (niwa-view-zone-reconcile
     zone
     (if region (overlay-start region) (point-min))
     (if region (overlay-end region) (point-min))
     nil)))

(provide 'niwa-view-zone)
;;; niwa-view-zone.el ends here
