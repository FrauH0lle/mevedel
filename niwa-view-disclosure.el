;;; niwa-view-disclosure.el -- Transcript disclosure state and actions -*- lexical-binding: t -*-

;;; Commentary:

;; Owns source-backed transcript disclosure identity, remembered state,
;; expansion/collapse actions, and the public section-toggle command.  The
;; renderer remains responsible for interpreting and projecting transcript
;; content; historical segment buffers belong to `niwa-view-segments'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `niwa-structs'
(defvar niwa--data-buffer)

;; `niwa-tool-task'
(declare-function niwa-toggle-tasks "niwa-tool-task" ())
(autoload 'niwa-toggle-tasks "niwa-tool-task")

;; `niwa-transcript'
(declare-function niwa-transcript-segments
                  "niwa-transcript" (start end))
(autoload 'niwa-transcript-segments "niwa-transcript")

;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format-string &rest args))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")

;; `niwa-view'
(defvar niwa-view--display-map)

;; `niwa-view-audit'
(declare-function niwa-view-audit-toggle-hook-audit
                  "niwa-view-audit" ())
(autoload 'niwa-view-audit-toggle-hook-audit "niwa-view-audit")

;; `niwa-view-composer'
(declare-function niwa-view-transcript-tail-marker "niwa-view-composer" ())

;; `niwa-view-render'
(declare-function niwa-view-render-mutate
                  "niwa-view-render" (key function &optional replacement cleanup))
(autoload 'niwa-view-render-mutate "niwa-view-render")
(defvar niwa-view-render--owner)
(declare-function niwa-view-render-add-display-properties
                  "niwa-view-render" (start end &optional default-vtype))
(declare-function niwa-view-render-child-calls-end
                  "niwa-view-render" (start limit source &optional parent-depth))
(declare-function niwa-view-render-collapsed-disclosure
                  "niwa-view-render" (data-buf source vtype))
(declare-function niwa-view-render-insert-expanded-disclosure
                  "niwa-view-render" (data-buf source vtype header))
(declare-function niwa-view-render-invalidate-live-tail
                  "niwa-view-render" ())
(declare-function niwa-view-render-live-update
                  "niwa-view-render" (data-buf))
(declare-function niwa-view-render-section-body-end
                  "niwa-view-render" (start limit))
(declare-function niwa-view-render-toggle-child-call
                  "niwa-view-render" ())
(declare-function niwa-view-render-toggle-hook-context
                  "niwa-view-render" ())
(declare-function niwa-view-render-toggle-turn
                  "niwa-view-render" (collapsed))
(declare-function niwa-view-render-toggle-user-input
                  "niwa-view-render" ())
(autoload 'niwa-view-render-add-display-properties "niwa-view-render")
(autoload 'niwa-view-render-child-calls-end "niwa-view-render")
(autoload 'niwa-view-render-collapsed-disclosure "niwa-view-render")
(autoload 'niwa-view-render-insert-expanded-disclosure
  "niwa-view-render")
(autoload 'niwa-view-render-invalidate-live-tail "niwa-view-render")
(autoload 'niwa-view-render-live-update "niwa-view-render")
(autoload 'niwa-view-render-section-body-end "niwa-view-render")
(autoload 'niwa-view-render-toggle-child-call "niwa-view-render")
(autoload 'niwa-view-render-toggle-hook-context "niwa-view-render")
(autoload 'niwa-view-render-toggle-turn "niwa-view-render")
(autoload 'niwa-view-render-toggle-user-input "niwa-view-render")

;; `niwa-view-segments'
(declare-function niwa-view-segments-display-buffer
                  "niwa-view-segments" ())
(autoload 'niwa-view-segments-display-buffer "niwa-view-segments")

;; `niwa-view-stream'
(declare-function niwa-view-stream-in-flight-turn-start-position
                  "niwa-view-stream" ())
(declare-function niwa-view-stream-set-in-flight-turn-start
                  "niwa-view-stream" (position))
(autoload 'niwa-view-stream-in-flight-turn-start-position
  "niwa-view-stream")
(autoload 'niwa-view-stream-set-in-flight-turn-start
  "niwa-view-stream")
(defvar niwa-view--data-turn-start)
(defvar niwa-view--pending-tool-calls)


;;
;;; State

(defvar-local niwa-view-disclosure--source-states nil
  "Hash table of source-backed disclosure states for this view.")

(defun niwa-view-disclosure-initialize ()
  "Initialize source-backed disclosure state in the current view."
  (setq-local niwa-view-disclosure--source-states
              (make-hash-table :test #'equal)))

(defun niwa-view-disclosure-rebase-state (delta)
  "Shift every remembered source identity by DELTA."
  (when (and (not (zerop delta))
             (hash-table-p niwa-view-disclosure--source-states))
    (let ((shifted
           (make-hash-table
            :test (hash-table-test niwa-view-disclosure--source-states)
            :size (hash-table-size niwa-view-disclosure--source-states))))
      (maphash
       (lambda (key value)
         (if (and (consp key)
                  (eq (car key) 'source)
                  (integerp (nth 2 key)))
             (let ((new-key (copy-tree key)))
               (setcar (nthcdr 2 new-key) (+ (nth 2 key) delta))
               (puthash new-key value shifted))
           (puthash key value shifted)))
       niwa-view-disclosure--source-states)
      (setq niwa-view-disclosure--source-states shifted))))

(defun niwa-view-disclosure-reset-state ()
  "Forget every remembered source-backed disclosure state."
  (setq niwa-view-disclosure--source-states
        (make-hash-table :test #'equal)))

(defun niwa-view-disclosure-compaction-tool-states ()
  "Capture tool disclosure states before the current transcript rotates.
Include remembered children hidden inside collapsed parent rows."
  (when-let* ((boundary (niwa-view-transcript-tail-marker)))
    (niwa-view-disclosure-capture-state (point-min) boundary))
  (let (states)
    (when (hash-table-p niwa-view-disclosure--source-states)
      (maphash
       (lambda (key value)
         (when (and (eq (car-safe key) 'source)
                    (eq (car-safe (nth 3 key)) 'tool)
                    (stringp (cadr (nth 3 key))))
           (push (cons key value) states)))
       niwa-view-disclosure--source-states))
    states))

(defun niwa-view-disclosure-rebase-compaction-tool-states (states data-buffer)
  "Rekey STATES for tool rows still present in DATA-BUFFER after rotation.
STATES came from `niwa-view-disclosure-compaction-tool-states'.
Never carry a removed tool's state into the successor transcript."
  (when (and states (buffer-live-p data-buffer))
    (let ((starts (make-hash-table :test #'equal)))
      (with-current-buffer data-buffer
        (dolist (segment (niwa-transcript-segments (point-min) (point-max)))
          (when (eq (car segment) 'tool)
            (let ((pos (cadr segment))
                  (end (caddr segment))
                  id)
              (while (and (< pos end) (not id))
                (let ((prop (get-text-property pos 'gptel)))
                  (when (and (consp prop) (eq (car prop) 'tool)
                             (stringp (cdr prop))
                             (not (equal (cdr prop) "")))
                    (setq id (cdr prop))))
                (setq pos (or (next-single-property-change
                               pos 'gptel nil end)
                              end)))
              (when id (puthash id (cadr segment) starts))))))
      (dolist (entry states)
        (remhash (car entry) niwa-view-disclosure--source-states))
      (dolist (entry states)
        (when-let* ((id (cadr (nth 3 (car entry))))
                    (start (gethash id starts)))
          (let ((key (copy-tree (car entry))))
            (setcar (nthcdr 2 key) start)
            (niwa-view-disclosure-record-state-for-key key (cdr entry))))))))

(defvar niwa-view-disclosure--settling-p nil
  "Non-nil while the terminal settle render is computing state keys.
The settle render runs before the stream clears the in-flight turn
markers, so without this flag every key it computes would still carry
the temporary `(in-flight)' anchor and be orphaned the moment those
markers clear.  Bound by `niwa-view-render-settle' so capture,
restore, and freshly stamped keys all use the durable post-settle
anchor instead.")

(defvar niwa-view-disclosure--collapsible-vtypes
  '(thinking-summary tool-summary tool-group response request-failure
    agent-handle prompt-summary hook-context hook-audit tool-child
    user-input-summary system-reminder-summary compaction-summary)
  "View types that `niwa-view-toggle-section' treats as section folds.
Turn-level folds (`turn-header', `turn-summary') are handled
separately.  Regions with other vtypes are navigable but not
toggleable.")

(defun niwa-view-disclosure-source-range (data-buffer start end)
  "Return marker-backed START..END coordinates in DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (cons (copy-marker start nil)
            (copy-marker end nil)))))

(defun niwa-view-disclosure--vtype-p (vtype)
  "Return non-nil when VTYPE can be restored from source coordinates."
  (or (memq vtype niwa-view-disclosure--collapsible-vtypes)
      (eq vtype 'turn-summary)))

(defconst niwa-view-disclosure--missing-state
  (make-symbol "niwa-view-missing-collapse-state")
  "Sentinel for absent source-backed collapse state entries.")

(defun niwa-view-disclosure-source-start (source)
  "Return SOURCE's numeric start position, or nil."
  (when (and source (consp source) (integer-or-marker-p (car source)))
    (if (markerp (car source))
        (marker-position (car source))
      (car source))))

(defun niwa-view-disclosure--in-flight-source-p (source)
  "Return non-nil when SOURCE belongs to the active in-flight turn."
  (when-let* (((not niwa-view-disclosure--settling-p))
              (start (niwa-view-disclosure-source-start source))
              (turn-start (cond
                           ((markerp niwa-view--data-turn-start)
                            (marker-position niwa-view--data-turn-start))
                           ((integerp niwa-view--data-turn-start)
                            niwa-view--data-turn-start))))
    (>= start turn-start)))

(defun niwa-view-disclosure--source-anchor (source)
  "Return a render-time identity anchor for SOURCE in the data buffer.
The anchor should remain stable when a source-backed segment extends in
place, but change when a later rewrite reuses the same numeric start."
  (when (and source
             (consp source)
             (integer-or-marker-p (car source))
             (integer-or-marker-p (cdr source))
             (buffer-live-p (niwa-view-segments-display-buffer)))
    (let ((data-buf (niwa-view-segments-display-buffer))
          (in-flight-p (niwa-view-disclosure--in-flight-source-p source)))
      (with-current-buffer data-buf
        (let* ((pmin (point-min))
               (pmax (point-max))
               (start (if (markerp (car source))
                          (marker-position (car source))
                        (car source)))
               (end (if (markerp (cdr source))
                        (marker-position (cdr source))
                      (cdr source)))
               (start (and start (max pmin (min start pmax))))
               (end (and end (max pmin (min end pmax)))))
          (when (and start end (< start end))
            (or
             (let ((pos start)
                   tool-id)
               (while (and (< pos end) (not tool-id))
                 (let ((prop (get-text-property pos 'gptel)))
                   ;; Restored transcripts stamp id-less tool blocks with
                   ;; an empty-string id where the live buffer had nil;
                   ;; treating "" as an id would give the same section
                   ;; two different anchors across a property restore.
                   (when (and (consp prop) (eq (car prop) 'tool)
                              (not (equal (cdr prop) "")))
                     (setq tool-id (cdr prop))))
                 (setq pos (or (next-single-property-change
                                pos 'gptel nil end)
                               end)))
               (and tool-id (list 'tool tool-id)))
             (and in-flight-p '(in-flight))
             (md5 (buffer-substring-no-properties start end)))))))))

(defun niwa-view-disclosure-state-key (source vtype &optional previous-key)
  "Return the source-backed collapse-state key for SOURCE and VTYPE.
Preserve any discriminator from PREVIOUS-KEY while rebasing its source."
  (when (and source
             (consp source)
             (niwa-view-disclosure-source-start source)
             (niwa-view-disclosure--vtype-p vtype))
    (append
     (list 'source vtype
           (niwa-view-disclosure-source-start source)
           (niwa-view-disclosure--source-anchor source))
     (and (eq (car-safe previous-key) 'source)
          (eq (cadr previous-key) vtype)
          (nthcdr 4 previous-key)))))

(defun niwa-view-disclosure--in-flight-key-p (key)
  "Return non-nil when KEY has the temporary in-flight anchor."
  (and (consp key)
       (eq (car key) 'source)
       (equal (nth 3 key) '(in-flight))))

(defun niwa-view-disclosure-state-snapshot ()
  "Return an alist snapshot of this view's remembered source fold choices.
Absent keys remain absent, distinct from an explicitly expanded nil value.
This reads the state table only, not the rendered view or reader anchors."
  (let (states)
    (when (hash-table-p niwa-view-disclosure--source-states)
      (maphash (lambda (key value) (push (cons key value) states))
               niwa-view-disclosure--source-states))
    states))

(defun niwa-view-disclosure--ensure-states ()
  "Return the view-local source-backed collapse-state table."
  (unless (hash-table-p niwa-view-disclosure--source-states)
    (setq niwa-view-disclosure--source-states
          (make-hash-table :test #'equal)))
  niwa-view-disclosure--source-states)

(defun niwa-view-disclosure-record-state-for-key (key collapsed)
  "Record COLLAPSED for disclosure KEY.
Callers that discriminate several sections sharing one source range,
such as the nested call rows of a compound tool, own their key."
  (when key
    (puthash key (and collapsed t)
             (niwa-view-disclosure--ensure-states))))

(defun niwa-view-disclosure-state-for-key (key)
  "Return (KEY . COLLAPSED) for remembered KEY, or nil when unremembered."
  (when (and key (hash-table-p niwa-view-disclosure--source-states))
    (let ((value (gethash key niwa-view-disclosure--source-states
                          niwa-view-disclosure--missing-state)))
      (unless (eq value niwa-view-disclosure--missing-state)
        (cons key value)))))

(defun niwa-view-disclosure-record-state (source vtype collapsed)
  "Record source-backed collapse state for SOURCE and VTYPE.
COLLAPSED is stored as t for collapsed and nil for expanded."
  (niwa-view-disclosure-record-state-for-key
   (niwa-view-disclosure-state-key source vtype) collapsed))

(defun niwa-view-disclosure-state-entry (source vtype)
  "Return saved collapse state entry for SOURCE and VTYPE, or nil.
The returned cons is (KEY . COLLAPSED), where COLLAPSED may be nil for
an explicitly expanded section."
  (niwa-view-disclosure-state-for-key
   (niwa-view-disclosure-state-key source vtype)))

(defun niwa-view-disclosure-apply-rendering-state (rendering source)
  "Return RENDERING with saved collapse state from SOURCE applied.
When no saved state exists, return RENDERING unchanged."
  (if (plist-get rendering :force-expanded-p)
      (plist-put (copy-sequence rendering) :initially-collapsed-p nil)
    (if-let* ((rendering rendering)
            (vtype (or (plist-get rendering :vtype) 'tool-summary))
            (entry (niwa-view-disclosure-state-entry source vtype)))
        (plist-put (copy-sequence rendering)
                   :initially-collapsed-p (cdr entry))
      rendering)))

(defun niwa-view-disclosure--next-state-change (pos limit boundaries)
  "Return the next fold-relevant property change after POS before LIMIT.
BOUNDARIES is a three-element vector, initially zeroed, retaining the next
source, type and mailbox boundary.  Advance POS monotonically with the same
LIMIT, and clear the vector after a toggle can change text or properties."
  (let ((next limit)
        (index 0))
    (dolist (prop '(niwa-view-source
                    niwa-view-type
                    niwa-view-mailbox-card))
      (when (<= (aref boundaries index) pos)
        (aset boundaries index
              (or (next-single-property-change pos prop nil limit) limit)))
      (setq next (min next (aref boundaries index))
            index (1+ index)))
    next))

(defun niwa-view-disclosure--mailbox-bounds-at (position)
  "Return bounds of the mailbox card at POSITION, or nil."
  (let ((card (get-text-property position 'niwa-view-mailbox-card)))
    (when card
      (let ((start (or (previous-single-property-change
                        position 'niwa-view-mailbox-card)
                       (point-min)))
            (end (or (next-single-property-change
                      position 'niwa-view-mailbox-card)
                     (point-max))))
        (when (and (< start position)
                   (not (eq (get-text-property
                             start 'niwa-view-mailbox-card)
                            card)))
          (setq start (or (next-single-property-change
                           start 'niwa-view-mailbox-card)
                          position)))
        (cons start end)))))

(defun niwa-view-disclosure--mailbox-body-text (start end)
  "Return the visible payload text for a mailbox card between START and END."
  (string-join
   (mapcar (lambda (range)
             (buffer-substring-no-properties (car range) (cdr range)))
           (niwa-view-disclosure--mailbox-body-ranges start end))
   "\n"))

(defun niwa-view-disclosure--mailbox-state-key (position counts)
  "Return a stable collapse-state key for the mailbox card at POSITION.
COUNTS is a hash table tracking repeated equivalent cards while the
caller scans the render span in display order."
  (when-let* ((bounds (niwa-view-disclosure--mailbox-bounds-at position)))
    (let* ((kind (or (get-text-property
                      (car bounds) 'niwa-view-mailbox-kind)
                     'agent-message))
           (agent-path (get-text-property
                        (car bounds) 'niwa-view-mailbox-agent-path))
           (body-hash (md5 (niwa-view-disclosure--mailbox-body-text
                            (car bounds) (cdr bounds))))
           (base (list 'mailbox-delivery kind agent-path body-hash))
           (index (1+ (or (gethash base counts) 0))))
      (puthash base index counts)
      (append base (list index)))))

(defun niwa-view-disclosure--state-key-at (position mailbox-counts)
  "Return the current disclosure identity at POSITION.
Rebase stamped keys after source movement or turn settlement while
retaining owner discriminators.  MAILBOX-COUNTS distinguishes repeated
mailbox cards as the caller scans in display order."
  (let* ((vtype (get-text-property position 'niwa-view-type))
         (source (get-text-property position 'niwa-view-source))
         (source-key (get-text-property position 'niwa-view-source-key))
         (state-type (cond
                      ((eq vtype 'tool-child) (or (cadr source-key) vtype))
                      ((memq vtype '(turn-header turn-summary)) 'turn-summary)
                      (t vtype))))
    (cond
     ((or (niwa-view-disclosure--in-flight-source-p source)
          (niwa-view-disclosure--in-flight-key-p source-key)
          (and (markerp (car-safe source))
               (not (equal (nth 2 source-key)
                           (niwa-view-disclosure-source-start source)))))
      (niwa-view-disclosure-state-key source state-type source-key))
     (source-key)
     ((niwa-view-disclosure-state-key source state-type))
     ((eq vtype 'mailbox-delivery)
      (niwa-view-disclosure--mailbox-state-key position mailbox-counts)))))

(defun niwa-view-disclosure-capture-state (from to)
  "Return an alist of collapse states for sections in FROM..TO.

Source-backed keys use the segment vtype, the car of
`niwa-view-source', and the render-time source anchor.  Values are t
when collapsed, nil when expanded.  Identity is keyed on the data-start
only (not the full source cons) plus that anchor and any owner discriminator.
Thus thinking-summary and tool-summary segments keep their saved state when
streaming extends the segment's end position, but rewritten data at the same
numeric start does not inherit stale state.  Locally decorated mailbox cards
use their rendered kind, agent id, body hash, and ordinal.

Source-backed states are also remembered beyond this span, so children of a
later-folded row keep them, but only states somebody chose: one already
remembered, or one that differs from its section type's default.  A section
shown in its default state is not a choice; remembering it would later
override a better default, such as a new group staying open over an open row."
  (let ((mailbox-counts (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (boundaries (make-vector 3 0))
        (states nil)
        (pos from))
    (while (< pos to)
      (let* ((vtype (get-text-property pos 'niwa-view-type))
             (collapsed (get-text-property pos 'niwa-view-collapsed))
             (mailbox-bounds
              (and (eq vtype 'mailbox-delivery)
                   (niwa-view-disclosure--mailbox-bounds-at pos)))
             (next (if mailbox-bounds
                       (min to (cdr mailbox-bounds))
                     (niwa-view-disclosure--next-state-change
                      pos to boundaries)))
             (key (niwa-view-disclosure--state-key-at pos mailbox-counts)))
        (when (and key (not (gethash key seen)))
          (let ((state (and collapsed t)))
            (puthash key t seen)
            (push (cons key state) states)
            (when (and (eq (car key) 'source)
                       (or (niwa-view-disclosure-state-for-key key)
                           (not (eq state (niwa-view-disclosure--default-collapsed-p
                                           (nth 1 key))))))
              (puthash key state
                       (niwa-view-disclosure--ensure-states)))))
        (setq pos next)))
    states))

(defun niwa-view-disclosure--default-collapsed-p (state-type)
  "Return non-nil when sections of STATE-TYPE render collapsed by default."
  (not (memq state-type '(response turn-summary thinking-summary))))

(defun niwa-view-disclosure-restore-state (from to states)
  "Toggle sections in FROM..TO so collapse state matches STATES.
STATES is an alist from `niwa-view-disclosure-capture-state'.
Sections whose current state already matches are left alone; only
mismatches are toggled, via `niwa-view-disclosure--expand-now' /
`--collapse-now' or the mailbox card toggle.  Upper bound is held as
a marker so toggles that change buffer length do not invalidate the walk.
Return non-nil when at least one section was toggled, so the caller
knows the freshly rendered span was rewritten after insertion."
  (when states
    (save-excursion
      (let ((mailbox-counts (make-hash-table :test 'equal))
            (to-marker (copy-marker to t))
            (wanted (make-hash-table :test 'equal))
            (boundaries (make-vector 3 0))
            (toggled nil))
        ;; A live turn holds hundreds of sections; look each one up once.
        (dolist (entry (reverse states))
          (puthash (car entry) entry wanted))
        (unwind-protect
            (progn
              (let ((pos from))
                (while (< pos (marker-position to-marker))
                  (let* ((vtype (get-text-property pos 'niwa-view-type))
                         (source (get-text-property pos 'niwa-view-source))
                         (collapsed (and (get-text-property
                                          pos 'niwa-view-collapsed)
                                         t))
                         (force-expanded
                          (get-text-property pos
                                             'niwa-view-force-expanded))
                         (mailbox-bounds
                          (and (eq vtype 'mailbox-delivery)
                               (niwa-view-disclosure--mailbox-bounds-at pos)))
                         (key (niwa-view-disclosure--state-key-at
                               pos mailbox-counts)))
                    (when-let* (((not force-expanded))
                                (entry (and key (gethash key wanted)))
                                ((not (eq collapsed (cdr entry)))))
                      (setq toggled t)
                      (goto-char pos)
                      (cond
                       ((memq vtype '(turn-header turn-summary))
                        (niwa-view-render-toggle-turn collapsed))
                       ((eq vtype 'mailbox-delivery)
                        (niwa-view-disclosure--toggle-mailbox))
                       ((eq vtype 'hook-context)
                        (niwa-view-render-toggle-hook-context))
                       ((eq vtype 'hook-audit)
                        (niwa-view-audit-toggle-hook-audit))
                       ((eq vtype 'tool-child)
                        (niwa-view-render-toggle-child-call))
                       ((eq vtype 'user-input-summary)
                        (niwa-view-render-toggle-user-input))
                       ((cdr entry)
                        (niwa-view-disclosure--collapse-now source vtype))
                       (t
                        (niwa-view-disclosure--expand-now source vtype)))
                      ;; Even a length-preserving replacement can move property
                      ;; boundaries.  Only unchanged runs may reuse the cursor.
                      (fillarray boundaries 0))
                    (setq pos
                          (if mailbox-bounds
                              (min (marker-position to-marker)
                                   (or (cdr (niwa-view-disclosure--mailbox-bounds-at
                                             pos))
                                       (cdr mailbox-bounds)))
                            (niwa-view-disclosure--next-state-change
                             pos (marker-position to-marker) boundaries))))))
              toggled)
          (set-marker to-marker nil))))))



;;
;;; Expand/collapse

(defun niwa-view-disclosure-indent-text (text width)
  "Return TEXT with a display-only WIDTH-column line and wrap inset."
  (let ((prefix (make-string width ?\s)))
    (propertize text
                'line-prefix prefix
                'wrap-prefix prefix
                'rear-nonsticky '(line-prefix wrap-prefix))))

(defun niwa-view-disclosure-truncate-line (text limit)
  "Return TEXT truncated to LIMIT characters with a trailing `...'."
  (if (> (length text) limit)
      (concat (substring text 0 (max 0 (- limit 3))) "...")
    text))

(defun niwa-view-disclosure--toggle-fragment ()
  "Toggle the migrated fragment-backed section at point.
Return non-nil when point was on a migrated fragment surface handled by
this helper.  Source-backed transcript/tool disclosure remains owned by
`niwa-view-toggle-section'."
  (let ((namespace (get-text-property (point)
                                      'niwa-view-zone-namespace))
        (id (get-text-property (point) 'niwa-view-zone-id)))
    (cond
     ((and (eq namespace 'status) (eq id 'tasks)
           (get-text-property (point) 'niwa-view-zone-collapsible))
      (niwa-toggle-tasks)
      t)
     ((and (eq namespace 'status) (eq id 'background)
           (get-text-property (point) 'niwa-view-zone-collapsible))
      (when-let* ((toggle (get-text-property (point) 'niwa-view-zone-toggle)))
        (funcall toggle))
      t))))

(defun niwa-view-disclosure--intent (source vtype collapsed function)
  "Apply desired COLLAPSED state for SOURCE and VTYPE through FUNCTION.
Rediscover the current row after an active writer finishes.  Source markers
and type/discriminator identity survive view replacement; view points do not."
  (if (not niwa-view-render--owner)
      (niwa-view-render-mutate
       'disclosure (lambda () (funcall function source vtype)))
    (let* ((data-buffer (niwa-view-segments-display-buffer))
         (start (and source (with-current-buffer data-buffer
                              (copy-marker (car source)))))
         (discriminator (nthcdr 4 (get-text-property
                                   (point) 'niwa-view-source-key)))
         (mailbox (get-text-property (point) 'niwa-view-mailbox-card))
         (type (if (memq vtype '(turn-header turn-summary)) 'turn vtype)))
    (niwa-view-render-mutate
     (list 'disclosure data-buffer (and start (marker-position start))
           type discriminator mailbox)
     (lambda ()
       (let ((pos (point-min)) found)
         (while (and (< pos (point-max)) (not found))
           (let* ((candidate (get-text-property pos 'niwa-view-source))
                  (candidate-type (get-text-property pos 'niwa-view-type)))
             (when (and (eq type (if (memq candidate-type '(turn-header turn-summary))
                                    'turn candidate-type))
                        (if start
                            (and (eq (marker-buffer start) data-buffer)
                                 candidate
                                 (equal (marker-position start)
                                        (niwa-view-disclosure-source-start candidate))
                                 (equal discriminator
                                        (nthcdr 4 (get-text-property
                                                   pos 'niwa-view-source-key))))
                          (and mailbox
                               (equal mailbox (get-text-property
                                               pos 'niwa-view-mailbox-card)))))
               (setq found pos)))
           (setq pos (1+ pos)))
         (when (and found
                    (not (eq collapsed (and (get-text-property
                                              found 'niwa-view-collapsed) t))))
           (save-excursion
             (goto-char found)
             (funcall function (get-text-property found 'niwa-view-source)
                      (get-text-property found 'niwa-view-type))))))))))

(defun niwa-view-toggle-section ()
  "Toggle the current disclosure, retaining its source and desired fold state."
  (interactive)
  (if (niwa-view-disclosure--toggle-fragment)
      t
    (niwa-view-disclosure--intent
     (get-text-property (point) 'niwa-view-source)
     (get-text-property (point) 'niwa-view-type)
     (not (get-text-property (point) 'niwa-view-collapsed))
     (lambda (_source _type) (niwa-view-disclosure--toggle-now)))))

(defun niwa-view-disclosure--toggle-now ()
  "Toggle expand/collapse of the section or turn at point.
On a turn header or collapsed-turn summary, toggles the whole turn.
On an inner section summary (thinking, tool, response), toggles that
section only."
  (interactive)
  (let ((collapsed (get-text-property (point) 'niwa-view-collapsed))
        (source (get-text-property (point) 'niwa-view-source))
        (vtype (get-text-property (point) 'niwa-view-type))
        ;; Calls in flight are drawn only by the live render, into the
        ;; turn's trailing run.  A toggle there rebuilds the section from
        ;; the transcript alone, so the live turn must be drawn again.
        (live-running
         (and (bound-and-true-p niwa-view--pending-tool-calls)
              ;; An archived segment shares the view but not the live turn.
              (eq niwa--data-buffer (niwa-view-segments-display-buffer))
              (when-let* ((start (niwa-view-stream-in-flight-turn-start-position)))
                (>= (point) start)))))
    (if (niwa-view-disclosure--toggle-fragment)
        t
      (prog1
          (cond
           ((memq vtype '(turn-header turn-summary))
            (niwa-view-render-toggle-turn collapsed))
           ((eq vtype 'mailbox-delivery)
            (niwa-view-disclosure--toggle-mailbox))
           ((eq vtype 'hook-context)
            (niwa-view-render-toggle-hook-context))
           ((eq vtype 'hook-audit)
            (niwa-view-audit-toggle-hook-audit))
           ((eq vtype 'tool-child)
            (niwa-view-render-toggle-child-call))
           ((eq vtype 'user-input-summary)
            (niwa-view-render-toggle-user-input))
           ((and source
                 (memq vtype niwa-view-disclosure--collapsible-vtypes))
            (if collapsed
                (niwa-view-disclosure--expand-now source vtype)
              (niwa-view-disclosure--collapse-now source vtype)))
           (t
            (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No collapsible section at point")))
        ;; A toggle deletes and re-inserts view text.  The retained
        ;; live-tail view marker can be dragged backwards by that
        ;; delete relative to its data-buffer twin, after which the
        ;; next retained render deletes rendered history its narrowed
        ;; reparse cannot regenerate -- the section visibly vanishes
        ;; until a later full render.  Dropping the retained pair
        ;; sends the next update down the non-retained path, whose
        ;; capture/restore preserves this toggle.
        (niwa-view-render-invalidate-live-tail)
        ;; Queued behind this toggle, so the reader never sees the
        ;; transcript-only section.
        (when (and live-running (buffer-live-p niwa--data-buffer))
          (niwa-view-render-live-update niwa--data-buffer))))))

(defun niwa-view-disclosure--mailbox-bounds ()
  "Return bounds of the mailbox card at point, or nil."
  (niwa-view-disclosure--mailbox-bounds-at (point)))

(defun niwa-view-disclosure--mailbox-body-ranges (start end)
  "Return mailbox body ranges between START and END."
  (let ((pos start)
        ranges)
    (while (< pos end)
      (let ((next (or (next-single-property-change
                       pos 'niwa-view-mailbox-body nil end)
                      end)))
        (if (get-text-property pos 'niwa-view-mailbox-body)
            (progn
              (push (cons pos next) ranges)
              (setq pos next))
          (setq pos next))))
    (nreverse ranges)))

(defun niwa-view-disclosure-mailbox-line-count (start end)
  "Return the number of non-empty mailbox body lines from START to END."
  (let ((count 0))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((line (buffer-substring-no-properties
                     (point) (min (line-end-position) end))))
          (when (string-match-p "\\S-" line)
            (setq count (1+ count))))
        (forward-line 1)))
    count))

(defun niwa-view-disclosure-mailbox-hint (line-count)
  "Return a mailbox collapse hint for LINE-COUNT body lines."
  (format " [%d %s collapsed]"
          line-count
          (if (= line-count 1) "line" "lines")))

(defun niwa-view-disclosure--delete-mailbox-hints (start end)
  "Delete mailbox collapse hints between START and END."
  (let ((end-marker (copy-marker end t)))
    (unwind-protect
        (save-excursion
          (goto-char start)
          (while (< (point) (marker-position end-marker))
            (let ((next (or (next-single-property-change
                             (point) 'niwa-view-mailbox-hint nil
                             (marker-position end-marker))
                            (marker-position end-marker))))
              (if (get-text-property (point) 'niwa-view-mailbox-hint)
                  (delete-region (point) next)
                (goto-char next)))))
      (set-marker end-marker nil))))

(defun niwa-view-disclosure-collapse-mailbox-body (start end)
  "Hide mailbox payload from START to END without joining following rows.
Keep the final payload newline visible, hiding the header separator instead.
The display iterator must encounter a visible newline before the next row
to apply that row's own line prefix.  Do not change payload ownership or bytes.

The hidden text is rear-nonsticky for `invisible', so the visible terminator
is not a position inside the hidden region.  Otherwise the command loop's
point adjustment treats a click past the header as entering hidden text and
moves point onto the following row."
  (let* ((terminator (and (< start end) (eq (char-before end) ?\n)))
         (hide-start (if (and terminator (eq (char-before start) ?\n))
                         (1- start)
                       start))
         (hide-end (if terminator (1- end) end)))
    (put-text-property hide-start hide-end
                       'invisible 'niwa-view-mailbox-collapsed)
    (when (< hide-start hide-end)
      (let ((nonsticky (get-text-property (1- hide-end) 'rear-nonsticky)))
        (unless (or (eq nonsticky t) (memq 'invisible nonsticky))
          (put-text-property (1- hide-end) hide-end
                             'rear-nonsticky (cons 'invisible nonsticky)))))
    (when terminator
      (remove-text-properties hide-end end '(invisible nil)))))

(defun niwa-view-disclosure--toggle-mailbox ()
  "Toggle a mailbox delivery card without consulting source text."
  (let* ((bounds (niwa-view-disclosure--mailbox-bounds))
         (collapsed (and bounds
                         (get-text-property
                          (car bounds) 'niwa-view-collapsed))))
    (unless bounds
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No collapsible section at point"))
    (let ((inhibit-read-only t)
          (start (car bounds))
          (end-marker (copy-marker (cdr bounds) t)))
      (unwind-protect
          (save-excursion
            (if collapsed
                (progn
                  (niwa-view-disclosure--delete-mailbox-hints
                   start (marker-position end-marker))
                  (remove-text-properties
                   start (marker-position end-marker)
                   '(invisible nil))
                  (put-text-property
                   start (marker-position end-marker)
                   'niwa-view-collapsed nil))
              (let* ((ranges (niwa-view-disclosure--mailbox-body-ranges
                              start (marker-position end-marker)))
                     (line-count
                      (apply #'+
                             (mapcar (lambda (range)
                                       (niwa-view-disclosure-mailbox-line-count
                                        (car range) (cdr range)))
                                     ranges))))
                (unless ranges
                  (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No collapsible section at point"))
                (niwa-view-disclosure--delete-mailbox-hints
                 start (marker-position end-marker))
                (setq ranges (niwa-view-disclosure--mailbox-body-ranges
                              start (marker-position end-marker)))
                (goto-char (caar ranges))
                (when (eq (char-before) ?\n)
                  (backward-char))
                (let ((hint
                       (propertize
                        (if (get-text-property start 'niwa-view-mailbox-notice)
                            ""
                          (niwa-view-disclosure-mailbox-hint line-count))
                        'font-lock-face 'niwa-view-attribution
                        'niwa-view-mailbox-hint t
                        'niwa-view-mailbox-card
                        (get-text-property start 'niwa-view-mailbox-card)
                        'niwa-view-type 'mailbox-delivery
                        'niwa-view-collapsed t
                        'read-only t
                        'keymap niwa-view--display-map
                        'front-sticky '(read-only keymap)
                        'rear-nonsticky '(read-only keymap))))
                  (insert hint)
                  (niwa-view-disclosure-collapse-mailbox-body
                   (+ (caar ranges) (length hint))
                   (+ (cdr (car (last ranges))) (length hint))))
                (put-text-property
                 start (marker-position end-marker)
                 'niwa-view-collapsed t))))
        (set-marker end-marker nil)))))

(defun niwa-view-disclosure-section-bounds (&optional property)
  "Return (START . END) of the current section at point.
A section is a contiguous region with the same non-nil PROPERTY value.
PROPERTY defaults to `niwa-view-source'.  Respects narrowing.
Compared with `eq' to match property-change scanning semantics -- two
conses with equal values but distinct identity are treated as a
boundary, which matters because the turn-level fallback source can
share a value with a nested section without being the same object."
  (let* ((property (or property 'niwa-view-source))
         (value (get-text-property (point) property)))
    (when value
      (let ((start (or (previous-single-property-change
                        (point) property)
                       (point-min)))
            (end (or (next-single-property-change
                      (point) property)
                     (point-max))))
        ;; `previous-single-property-change' returns the latest change
        ;; position before point -- which lands in the PREVIOUS run when
        ;; point is at the start of the current run.  Advance past any
        ;; such leading region whose value is not `eq' to point's.
        (when (and (< start (point))
                   (not (eq (get-text-property start property)
                            value)))
          (setq start (or (next-single-property-change
                           start property)
                          (point))))
        (cons start end)))))

(defun niwa-view-disclosure-data-substring (data-buf start end &optional properties)
  "Return text in DATA-BUF between START and END.
Widens DATA-BUF so narrowing does not hide valid coordinates, then
clamps START and END to the accessible range.  Returns the empty
string when the clamped range is empty, which keeps expand/collapse
from signalling `args-out-of-range' on stale source coordinates."
  (with-current-buffer data-buf
    (save-restriction
      (widen)
      (let* ((pmin (point-min))
             (pmax (point-max))
             (s (max pmin (min start pmax)))
             (e (max pmin (min end pmax))))
        (if (>= s e)
            ""
          (if properties
              (buffer-substring s e)
            (buffer-substring-no-properties s e)))))))

(defun niwa-view-disclosure--expand-section (source vtype)
  "Expand a collapsed section with SOURCE coordinates and VTYPE."
  (niwa-view-disclosure--intent
   source vtype nil #'niwa-view-disclosure--expand-now))

(defun niwa-view-disclosure--expand-now (source vtype)
  "Expand SOURCE of VTYPE while holding projection ownership."
  (let* ((bounds (niwa-view-disclosure-section-bounds))
         (data-buf (niwa-view-segments-display-buffer)))
    (when (and bounds data-buf (buffer-live-p data-buf))
      (let* ((inhibit-read-only t)
             (view-start (car bounds))
             (view-end (cdr bounds))
             (header (unless (eq vtype 'response)
                       (buffer-substring view-start view-end)))
             (source-key
              (get-text-property view-start 'niwa-view-source-key))
             (turn-id (get-text-property view-start 'niwa-view-turn-id))
             (in-flight-pos
              (niwa-view-stream-in-flight-turn-start-position)))
        (save-excursion
          (goto-char view-start)
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
          (unwind-protect
              (atomic-change-group
                (delete-region view-start view-end)
                (setq source
                      (niwa-view-render-insert-expanded-disclosure
                       data-buf source vtype header))
                (let ((body-end
                       (niwa-view-render-section-body-end
                        view-start (point))))
                  (add-text-properties
                   view-start body-end
                   `(niwa-view-source ,source
                     niwa-view-source-key
                     ,(niwa-view-disclosure-state-key
                       source vtype source-key)
                     niwa-view-type ,vtype
                     niwa-view-collapsed nil))
                  (niwa-view-disclosure-record-state source vtype nil)
                  (when turn-id
                    (put-text-property
                     view-start body-end 'niwa-view-turn-id turn-id)))
                ;; The delete above collapsed an in-flight marker that
                ;; sat inside the section to VIEW-START, which is where
                ;; the turn content still starts, so only a marker that
                ;; sat exactly at the old section end -- meaning the
                ;; turn begins after this section -- must be moved past
                ;; the re-inserted text.  Re-anchoring an interior
                ;; marker at the end instead would leave everything
                ;; above it stale for the next incremental render.
                (when (and in-flight-pos
                           (= in-flight-pos view-end))
                  (niwa-view-stream-set-in-flight-turn-start (point))))
            (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil)))))))

(defun niwa-view-disclosure--collapse-now (source vtype)
  "Collapse SOURCE of VTYPE while holding projection ownership."
  (let* ((bounds (niwa-view-disclosure-section-bounds))
         (data-buf (niwa-view-segments-display-buffer))
         (rendering
          (and data-buf
               (buffer-live-p data-buf)
               (niwa-view-render-collapsed-disclosure
                data-buf source vtype))))
    (when (and bounds rendering)
      (setq source (plist-get rendering :source))
      (let* ((inhibit-read-only t)
             (view-start (car bounds))
             (view-end (niwa-view-render-child-calls-end
                        (cdr bounds) (point-max) source))
             (turn-id (get-text-property view-start 'niwa-view-turn-id))
             (tool-use-id
              (get-text-property view-start 'niwa-view-tool-use-id))
             (source-key
              (get-text-property view-start 'niwa-view-source-key))
             (in-flight-pos
              (niwa-view-stream-in-flight-turn-start-position)))
        (save-excursion
          (goto-char view-start)
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
          (unwind-protect
              (progn
                (delete-region view-start view-end)
                (let ((ins-start (point)))
                  (insert (plist-get rendering :summary))
                  (unless (and (> (point) ins-start)
                               (eq (char-before) ?\n))
                    (insert "\n"))
                  (add-text-properties
                   ins-start (point)
                   `(niwa-view-type ,vtype
                     niwa-view-collapsed t
                     niwa-view-source ,source
                     niwa-view-source-key
                     ,(niwa-view-disclosure-state-key
                       source vtype source-key)))
                  (niwa-view-render-add-display-properties
                   ins-start (point) vtype)
                  (niwa-view-disclosure-record-state source vtype t)
                  (when tool-use-id
                    (put-text-property
                     ins-start (point) 'niwa-view-tool-use-id tool-use-id))
                  (when turn-id
                    (put-text-property
                     ins-start (point) 'niwa-view-turn-id turn-id))
                  ;; See the matching comment in `--expand-section': an
                  ;; interior in-flight marker already collapsed to
                  ;; VIEW-START; only a marker at the old section end
                  ;; belongs after the re-inserted summary.
                  (when (and in-flight-pos
                             (= in-flight-pos view-end))
                    (niwa-view-stream-set-in-flight-turn-start (point)))))
            (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil)))))))
(provide 'niwa-view-disclosure)
;;; niwa-view-disclosure.el ends here
