;;; mevedel-view-disclosure.el -- Transcript disclosure state and actions -*- lexical-binding: t -*-

;;; Commentary:

;; Owns source-backed transcript disclosure identity, remembered state,
;; expansion/collapse actions, and the public section-toggle command.  The
;; renderer remains responsible for interpreting and projecting transcript
;; content; historical segment buffers belong to `mevedel-view-segments'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `mevedel-tool-task'
(declare-function mevedel-toggle-tasks "mevedel-tool-task" ())

;; `mevedel-view'
(defvar mevedel-view--display-map)

;; `mevedel-view-agent'
(declare-function mevedel-view-agent-status-toggle
                  "mevedel-view-agent" ())

;; `mevedel-view-audit'
(declare-function mevedel-view-audit-toggle-hook-audit
                  "mevedel-view-audit" ())

;; `mevedel-view-composer'
(defvar mevedel-view--input-marker)

;; `mevedel-view-render'
(declare-function mevedel-view-render-add-display-properties
                  "mevedel-view-render" (start end &optional default-vtype))
(declare-function mevedel-view-render-child-calls-end
                  "mevedel-view-render" (start limit))
(declare-function mevedel-view-render-collapsed-disclosure
                  "mevedel-view-render" (data-buf source vtype))
(declare-function mevedel-view-render-insert-expanded-disclosure
                  "mevedel-view-render" (data-buf source vtype header))
(declare-function mevedel-view-render-section-body-end
                  "mevedel-view-render" (start limit))
(declare-function mevedel-view-render-toggle-child-call
                  "mevedel-view-render" ())
(declare-function mevedel-view-render-toggle-hook-context
                  "mevedel-view-render" ())
(declare-function mevedel-view-render-toggle-turn
                  "mevedel-view-render" (collapsed))

;; `mevedel-view-segments'
(declare-function mevedel-view-segments-display-buffer
                  "mevedel-view-segments" ())

;; `mevedel-view-stream'
(declare-function mevedel-view-stream-in-flight-turn-start-position
                  "mevedel-view-stream" ())
(declare-function mevedel-view-stream-set-in-flight-turn-start
                  "mevedel-view-stream" (position))
(defvar mevedel-view--data-turn-start)


;;
;;; State

(defvar-local mevedel-view-disclosure--source-states nil
  "Hash table of source-backed disclosure states for this view.")

(defun mevedel-view-disclosure-initialize ()
  "Initialize source-backed disclosure state in the current view."
  (setq-local mevedel-view-disclosure--source-states
              (make-hash-table :test #'equal)))

(defun mevedel-view-disclosure-rebase-state (delta)
  "Shift every remembered source identity by DELTA."
  (when (and (not (zerop delta))
             (hash-table-p mevedel-view-disclosure--source-states))
    (let ((shifted
           (make-hash-table
            :test (hash-table-test mevedel-view-disclosure--source-states)
            :size (hash-table-size mevedel-view-disclosure--source-states))))
      (maphash
       (lambda (key value)
         (if (and (consp key)
                  (eq (car key) 'source)
                  (integerp (nth 2 key)))
             (let ((new-key (copy-tree key)))
               (setcar (nthcdr 2 new-key) (+ (nth 2 key) delta))
               (puthash new-key value shifted))
           (puthash key value shifted)))
       mevedel-view-disclosure--source-states)
      (setq mevedel-view-disclosure--source-states shifted))))

(defun mevedel-view-disclosure-reset-state ()
  "Forget every remembered source-backed disclosure state."
  (setq mevedel-view-disclosure--source-states
        (make-hash-table :test #'equal)))

(defvar mevedel-view-disclosure--collapsible-vtypes
  '(thinking-summary tool-summary response request-failure agent-handle
    prompt-summary hook-context hook-audit tool-child
    system-reminder-summary)
  "View types that `mevedel-view-toggle-section' treats as section folds.
Turn-level folds (`turn-header', `turn-summary') are handled
separately.  Regions with other vtypes are navigable but not
toggleable.")

(defun mevedel-view-disclosure-source-range (data-buffer start end)
  "Return marker-backed START..END coordinates in DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (cons (copy-marker start nil)
            (copy-marker end nil)))))

(defun mevedel-view-disclosure--vtype-p (vtype)
  "Return non-nil when VTYPE can be restored from source coordinates."
  (or (memq vtype mevedel-view-disclosure--collapsible-vtypes)
      (eq vtype 'agent-handle)))

(defconst mevedel-view-disclosure--missing-state
  (make-symbol "mevedel-view-missing-collapse-state")
  "Sentinel for absent source-backed collapse state entries.")

(defun mevedel-view-disclosure-source-start (source)
  "Return SOURCE's numeric start position, or nil."
  (when (and source (consp source) (integer-or-marker-p (car source)))
    (if (markerp (car source))
        (marker-position (car source))
      (car source))))

(defun mevedel-view-disclosure--in-flight-source-p (source)
  "Return non-nil when SOURCE belongs to the active in-flight turn."
  (when-let* ((start (mevedel-view-disclosure-source-start source))
              (turn-start (cond
                           ((markerp mevedel-view--data-turn-start)
                            (marker-position mevedel-view--data-turn-start))
                           ((integerp mevedel-view--data-turn-start)
                            mevedel-view--data-turn-start))))
    (>= start turn-start)))

(defun mevedel-view-disclosure--source-anchor (source)
  "Return a render-time identity anchor for SOURCE in the data buffer.
The anchor should remain stable when a source-backed segment extends in
place, but change when a later rewrite reuses the same numeric start."
  (require 'mevedel-view-segments)
  (when (and source
             (consp source)
             (integer-or-marker-p (car source))
             (integer-or-marker-p (cdr source))
             (buffer-live-p (mevedel-view-segments-display-buffer)))
    (let ((data-buf (mevedel-view-segments-display-buffer))
          (in-flight-p (mevedel-view-disclosure--in-flight-source-p source)))
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
                   (when (and (consp prop) (eq (car prop) 'tool))
                     (setq tool-id (cdr prop))))
                 (setq pos (or (next-single-property-change
                                pos 'gptel nil end)
                               end)))
               (and tool-id (list 'tool tool-id)))
             (and in-flight-p '(in-flight))
             (md5 (buffer-substring-no-properties start end)))))))))

(defun mevedel-view-disclosure-state-key (source vtype &optional previous-key)
  "Return the source-backed collapse-state key for SOURCE and VTYPE.
Preserve any discriminator from PREVIOUS-KEY while rebasing its source."
  (when (and source
             (consp source)
             (mevedel-view-disclosure-source-start source)
             (mevedel-view-disclosure--vtype-p vtype))
    (append
     (list 'source vtype
           (mevedel-view-disclosure-source-start source)
           (mevedel-view-disclosure--source-anchor source))
     (and (eq (car-safe previous-key) 'source)
          (eq (cadr previous-key) vtype)
          (nthcdr 4 previous-key)))))

(defun mevedel-view-disclosure--in-flight-key-p (key)
  "Return non-nil when KEY has the temporary in-flight anchor."
  (and (consp key)
       (eq (car key) 'source)
       (equal (nth 3 key) '(in-flight))))

(defun mevedel-view-disclosure--ensure-states ()
  "Return the view-local source-backed collapse-state table."
  (unless (hash-table-p mevedel-view-disclosure--source-states)
    (setq mevedel-view-disclosure--source-states
          (make-hash-table :test #'equal)))
  mevedel-view-disclosure--source-states)

(defun mevedel-view-disclosure-record-state-for-key (key collapsed)
  "Record COLLAPSED for disclosure KEY.
Callers that discriminate several sections sharing one source range,
such as the nested call rows of a compound tool, own their key."
  (when key
    (puthash key (and collapsed t)
             (mevedel-view-disclosure--ensure-states))))

(defun mevedel-view-disclosure-state-for-key (key)
  "Return (KEY . COLLAPSED) for remembered KEY, or nil when unremembered."
  (when (and key (hash-table-p mevedel-view-disclosure--source-states))
    (let ((value (gethash key mevedel-view-disclosure--source-states
                          mevedel-view-disclosure--missing-state)))
      (unless (eq value mevedel-view-disclosure--missing-state)
        (cons key value)))))

(defun mevedel-view-disclosure-record-state (source vtype collapsed)
  "Record source-backed collapse state for SOURCE and VTYPE.
COLLAPSED is stored as t for collapsed and nil for expanded."
  (mevedel-view-disclosure-record-state-for-key
   (mevedel-view-disclosure-state-key source vtype) collapsed))

(defun mevedel-view-disclosure-state-entry (source vtype)
  "Return saved collapse state entry for SOURCE and VTYPE, or nil.
The returned cons is (KEY . COLLAPSED), where COLLAPSED may be nil for
an explicitly expanded section."
  (mevedel-view-disclosure-state-for-key
   (mevedel-view-disclosure-state-key source vtype)))

(defun mevedel-view-disclosure-apply-rendering-state (rendering source)
  "Return RENDERING with saved collapse state from SOURCE applied.
When no saved state exists, return RENDERING unchanged."
  (if (plist-get rendering :force-expanded-p)
      (plist-put (copy-sequence rendering) :initially-collapsed-p nil)
    (if-let* ((rendering rendering)
            (vtype (or (plist-get rendering :vtype) 'tool-summary))
            (entry (mevedel-view-disclosure-state-entry source vtype)))
        (plist-put (copy-sequence rendering)
                   :initially-collapsed-p (cdr entry))
      rendering)))

(defun mevedel-view-disclosure--next-state-change (pos limit)
  "Return the next fold-relevant property change after POS before LIMIT."
  (let ((next limit))
    (dolist (prop '(mevedel-view-source
                    mevedel-view-type
                    mevedel-view-mailbox-card))
      (when-let* ((change (next-single-property-change pos prop nil limit)))
        (setq next (min next change))))
    next))

(defun mevedel-view-disclosure--mailbox-bounds-at (position)
  "Return bounds of the mailbox card at POSITION, or nil."
  (let ((card (get-text-property position 'mevedel-view-mailbox-card)))
    (when card
      (let ((start (or (previous-single-property-change
                        position 'mevedel-view-mailbox-card)
                       (point-min)))
            (end (or (next-single-property-change
                      position 'mevedel-view-mailbox-card)
                     (point-max))))
        (when (and (< start position)
                   (not (eq (get-text-property
                             start 'mevedel-view-mailbox-card)
                            card)))
          (setq start (or (next-single-property-change
                           start 'mevedel-view-mailbox-card)
                          position)))
        (cons start end)))))

(defun mevedel-view-disclosure--mailbox-body-text (start end)
  "Return the visible payload text for a mailbox card between START and END."
  (string-join
   (mapcar (lambda (range)
             (buffer-substring-no-properties (car range) (cdr range)))
           (mevedel-view-disclosure--mailbox-body-ranges start end))
   "\n"))

(defun mevedel-view-disclosure--mailbox-state-key (position counts)
  "Return a stable collapse-state key for the mailbox card at POSITION.
COUNTS is a hash table tracking repeated equivalent cards while the
caller scans the render span in display order."
  (when-let* ((bounds (mevedel-view-disclosure--mailbox-bounds-at position)))
    (let* ((kind (or (get-text-property
                      (car bounds) 'mevedel-view-mailbox-kind)
                     'agent-message))
           (agent-path (get-text-property
                        (car bounds) 'mevedel-view-mailbox-agent-path))
           (body-hash (md5 (mevedel-view-disclosure--mailbox-body-text
                            (car bounds) (cdr bounds))))
           (base (list 'mailbox-delivery kind agent-path body-hash))
           (index (1+ (or (gethash base counts) 0))))
      (puthash base index counts)
      (append base (list index)))))

(defun mevedel-view-disclosure-capture-state (from to)
  "Return an alist of collapse states for sections in FROM..TO.

Source-backed keys use the segment vtype, the car of
`mevedel-view-source', and the render-time source anchor.  Values are t
when collapsed, nil when expanded.  Identity is keyed on the data-start
only (not the full source cons) plus that anchor and any owner discriminator.
Thus thinking-summary and tool-summary segments keep their saved state when
streaming extends the segment's end position, but rewritten data at the same
numeric start does not inherit stale state.  Locally decorated mailbox cards
use their rendered kind, agent id, body hash, and ordinal."
  (let ((mailbox-counts (make-hash-table :test 'equal))
        (states nil)
        (pos from))
    (while (< pos to)
      (let* ((vtype (get-text-property pos 'mevedel-view-type))
             (source (get-text-property pos 'mevedel-view-source))
             (collapsed (get-text-property pos 'mevedel-view-collapsed))
             (source-key (get-text-property pos 'mevedel-view-source-key))
             (mailbox-bounds
              (and (eq vtype 'mailbox-delivery)
                   (mevedel-view-disclosure--mailbox-bounds-at pos)))
             (next (if mailbox-bounds
                       (min to (cdr mailbox-bounds))
                     (mevedel-view-disclosure--next-state-change pos to)))
             (key
              (cond
               ((mevedel-view-disclosure--in-flight-source-p source)
                (mevedel-view-disclosure-state-key source vtype source-key))
               ((mevedel-view-disclosure--in-flight-key-p source-key)
                (mevedel-view-disclosure-state-key source vtype source-key))
               ((and (markerp (car-safe source))
                     (not (equal (nth 2 source-key)
                                 (mevedel-view-disclosure-source-start source))))
                (mevedel-view-disclosure-state-key source vtype source-key))
               (source-key)
               ((mevedel-view-disclosure-state-key source vtype))
               (mailbox-bounds
                (mevedel-view-disclosure--mailbox-state-key
                 pos mailbox-counts)))))
        (when (and key (not (assoc key states)))
          (let ((state (and collapsed t)))
            (push (cons key state) states)
            (when (eq (car key) 'source)
              (puthash key state
                       (mevedel-view-disclosure--ensure-states)))))
        (setq pos next)))
    states))

(defun mevedel-view-disclosure-restore-state (from to states)
  "Toggle sections in FROM..TO so collapse state matches STATES.
STATES is an alist from `mevedel-view-disclosure-capture-state'.
Sections whose current state already matches are left alone; only
mismatches are toggled, via `mevedel-view-disclosure--expand-section' /
`--collapse-section' or the mailbox card toggle.  Upper bound is held as
a marker so toggles that change buffer length do not invalidate the walk."
  (when states
    (save-excursion
      (let ((mailbox-counts (make-hash-table :test 'equal))
            (to-marker (copy-marker to t)))
        (unwind-protect
            (progn
              (let ((pos from))
                (while (< pos (marker-position to-marker))
                  (let* ((vtype (get-text-property pos 'mevedel-view-type))
                         (source (get-text-property pos 'mevedel-view-source))
                         (collapsed (and (get-text-property
                                          pos 'mevedel-view-collapsed)
                                         t))
                         (force-expanded
                          (get-text-property pos
                                             'mevedel-view-force-expanded))
                         (source-key (get-text-property
                                      pos 'mevedel-view-source-key))
                         (mailbox-bounds
                          (and (eq vtype 'mailbox-delivery)
                               (mevedel-view-disclosure--mailbox-bounds-at pos)))
                         (key
                          (cond
                           ((mevedel-view-disclosure--in-flight-source-p source)
                            (mevedel-view-disclosure-state-key
                             source vtype source-key))
                           ((mevedel-view-disclosure--in-flight-key-p source-key)
                            (mevedel-view-disclosure-state-key
                             source vtype source-key))
                           ((and (markerp (car-safe source))
                                 (not (equal
                                       (nth 2 source-key)
                                       (mevedel-view-disclosure-source-start
                                        source))))
                            (mevedel-view-disclosure-state-key
                             source vtype source-key))
                           (source-key)
                           ((mevedel-view-disclosure-state-key source vtype))
                           (mailbox-bounds
                            (mevedel-view-disclosure--mailbox-state-key
                             pos mailbox-counts)))))
                    (when-let* (((not force-expanded))
                                (entry (and key (assoc key states)))
                                ((not (eq collapsed (cdr entry)))))
                      (goto-char pos)
                      (cond
                       ((eq vtype 'mailbox-delivery)
                        (mevedel-view-disclosure--toggle-mailbox))
                       ((eq vtype 'tool-child)
                        (require 'mevedel-view-render)
                        (mevedel-view-render-toggle-child-call))
                       ((cdr entry)
                        (mevedel-view-disclosure--collapse-section source vtype))
                       (t
                        (mevedel-view-disclosure--expand-section source vtype))))
                    (setq pos
                          (if mailbox-bounds
                              (min (marker-position to-marker)
                                   (or (cdr (mevedel-view-disclosure--mailbox-bounds-at
                                             pos))
                                       (cdr mailbox-bounds)))
                            (mevedel-view-disclosure--next-state-change
                             pos (marker-position to-marker))))))))
          (set-marker to-marker nil))))))



;;
;;; Expand/collapse

(defun mevedel-view-disclosure-truncate-line (text limit)
  "Return TEXT truncated to LIMIT characters with a trailing `...'."
  (if (> (length text) limit)
      (concat (substring text 0 (max 0 (- limit 3))) "...")
    text))

(defun mevedel-view-disclosure--toggle-fragment ()
  "Toggle the migrated fragment-backed section at point.
Return non-nil when point was on a migrated fragment surface handled by
this helper.  Source-backed transcript/tool disclosure remains owned by
`mevedel-view-toggle-section'."
  (let ((namespace (get-text-property (point)
                                      'mevedel-view-zone-namespace))
        (id (get-text-property (point) 'mevedel-view-zone-id)))
    (cond
     ((and (eq namespace 'status) (eq id 'tasks)
           (get-text-property (point) 'mevedel-view-zone-collapsible))
      (require 'mevedel-tool-task)
      (mevedel-toggle-tasks)
      t)
     ((and (eq namespace 'status) (eq id 'agents)
           (get-text-property (point) 'mevedel-view-zone-collapsible))
      (require 'mevedel-view-agent)
      (mevedel-view-agent-status-toggle)
      t))))

(defun mevedel-view-toggle-section ()
  "Toggle expand/collapse of the section or turn at point.
On a turn header or collapsed-turn summary, toggles the whole turn.
On an inner section summary (thinking, tool, response), toggles that
section only."
  (interactive)
  (let ((collapsed (get-text-property (point) 'mevedel-view-collapsed))
        (source (get-text-property (point) 'mevedel-view-source))
        (vtype (get-text-property (point) 'mevedel-view-type)))
    (cond
     ((mevedel-view-disclosure--toggle-fragment)
      t)
     ((memq vtype '(turn-header turn-summary))
      (require 'mevedel-view-render)
      (mevedel-view-render-toggle-turn collapsed))
     ((eq vtype 'mailbox-delivery)
      (mevedel-view-disclosure--toggle-mailbox))
     ((eq vtype 'hook-context)
      (require 'mevedel-view-render)
      (mevedel-view-render-toggle-hook-context))
     ((eq vtype 'hook-audit)
      (require 'mevedel-view-audit)
      (mevedel-view-audit-toggle-hook-audit))
     ((eq vtype 'tool-child)
      (require 'mevedel-view-render)
      (mevedel-view-render-toggle-child-call))
     ((and source (memq vtype mevedel-view-disclosure--collapsible-vtypes))
      (if collapsed
          (mevedel-view-disclosure--expand-section source vtype)
        (mevedel-view-disclosure--collapse-section source vtype)))
     (t
      (user-error "No collapsible section at point")))))

(defun mevedel-view-disclosure--mailbox-bounds ()
  "Return bounds of the mailbox card at point, or nil."
  (mevedel-view-disclosure--mailbox-bounds-at (point)))

(defun mevedel-view-disclosure--mailbox-body-ranges (start end)
  "Return mailbox body ranges between START and END."
  (let ((pos start)
        ranges)
    (while (< pos end)
      (let ((next (or (next-single-property-change
                       pos 'mevedel-view-mailbox-body nil end)
                      end)))
        (if (get-text-property pos 'mevedel-view-mailbox-body)
            (progn
              (push (cons pos next) ranges)
              (setq pos next))
          (setq pos next))))
    (nreverse ranges)))

(defun mevedel-view-disclosure-mailbox-line-count (start end)
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

(defun mevedel-view-disclosure-mailbox-hint (line-count)
  "Return a mailbox collapse hint for LINE-COUNT body lines."
  (format " [%d %s collapsed]"
          line-count
          (if (= line-count 1) "line" "lines")))

(defun mevedel-view-disclosure--delete-mailbox-hints (start end)
  "Delete mailbox collapse hints between START and END."
  (let ((end-marker (copy-marker end t)))
    (unwind-protect
        (save-excursion
          (goto-char start)
          (while (< (point) (marker-position end-marker))
            (let ((next (or (next-single-property-change
                             (point) 'mevedel-view-mailbox-hint nil
                             (marker-position end-marker))
                            (marker-position end-marker))))
              (if (get-text-property (point) 'mevedel-view-mailbox-hint)
                  (delete-region (point) next)
                (goto-char next)))))
      (set-marker end-marker nil))))

(defun mevedel-view-disclosure--toggle-mailbox ()
  "Toggle a mailbox delivery card without consulting source text."
  (let* ((bounds (mevedel-view-disclosure--mailbox-bounds))
         (collapsed (and bounds
                         (get-text-property
                          (car bounds) 'mevedel-view-collapsed))))
    (unless bounds
      (user-error "No collapsible section at point"))
    (let ((inhibit-read-only t)
          (start (car bounds))
          (end-marker (copy-marker (cdr bounds) t)))
      (unwind-protect
          (save-excursion
            (if collapsed
                (progn
                  (mevedel-view-disclosure--delete-mailbox-hints
                   start (marker-position end-marker))
                  (remove-text-properties
                   start (marker-position end-marker)
                   '(invisible nil))
                  (put-text-property
                   start (marker-position end-marker)
                   'mevedel-view-collapsed nil))
              (let* ((ranges (mevedel-view-disclosure--mailbox-body-ranges
                              start (marker-position end-marker)))
                     (line-count
                      (apply #'+
                             (mapcar (lambda (range)
                                       (mevedel-view-disclosure-mailbox-line-count
                                        (car range) (cdr range)))
                                     ranges))))
                (unless ranges
                  (user-error "No collapsible section at point"))
                (mevedel-view-disclosure--delete-mailbox-hints
                 start (marker-position end-marker))
                (dolist (range (mevedel-view-disclosure--mailbox-body-ranges
                                start (marker-position end-marker)))
                  (add-text-properties
                   (car range) (cdr range)
                   '(invisible mevedel-view-mailbox-collapsed)))
                (goto-char (caar ranges))
                (when (eq (char-before) ?\n)
                  (backward-char))
                (insert
                 (propertize
                  (mevedel-view-disclosure-mailbox-hint line-count)
                  'font-lock-face 'mevedel-view-attribution
                  'mevedel-view-mailbox-hint t
                  'mevedel-view-mailbox-card
                  (get-text-property start 'mevedel-view-mailbox-card)
                  'mevedel-view-type 'mailbox-delivery
                  'mevedel-view-collapsed t
                  'read-only t
                  'keymap mevedel-view--display-map
                  'front-sticky '(read-only keymap)
                  'rear-nonsticky '(read-only keymap)))
                (put-text-property
                 start (marker-position end-marker)
                 'mevedel-view-collapsed t))))
        (set-marker end-marker nil)))))

(defun mevedel-view-disclosure-section-bounds ()
  "Return (START . END) of the current section at point.
A section is a contiguous region with the same `mevedel-view-source'.
Compared with `eq' to match property-change scanning semantics -- two
conses with equal values but distinct identity are treated as a
boundary, which matters because the turn-level fallback source can
share a value with a nested section without being the same object."
  (let ((source (get-text-property (point) 'mevedel-view-source)))
    (when source
      (let ((start (or (previous-single-property-change
                        (point) 'mevedel-view-source)
                       (point-min)))
            (end (or (next-single-property-change
                      (point) 'mevedel-view-source)
                     (point-max))))
        ;; `previous-single-property-change' returns the latest change
        ;; position before point -- which lands in the PREVIOUS run when
        ;; point is at the start of the current run.  Advance past any
        ;; such leading region whose source is not `eq' to point's.
        (when (and (< start (point))
                   (not (eq (get-text-property start 'mevedel-view-source)
                            source)))
          (setq start (or (next-single-property-change
                           start 'mevedel-view-source)
                          (point))))
        (cons start end)))))

(defun mevedel-view-disclosure-data-substring (data-buf start end &optional properties)
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

(defun mevedel-view-disclosure--expand-section (source vtype)
  "Expand a collapsed section with SOURCE coordinates and VTYPE."
  (require 'mevedel-view-render)
  (require 'mevedel-view-segments)
  (require 'mevedel-view-stream)
  (let* ((bounds (mevedel-view-disclosure-section-bounds))
         (data-buf (mevedel-view-segments-display-buffer)))
    (when (and bounds data-buf (buffer-live-p data-buf))
      (let* ((inhibit-read-only t)
             (view-start (car bounds))
             (view-end (cdr bounds))
             (header (unless (eq vtype 'response)
                       (buffer-substring view-start view-end)))
             (source-key
              (get-text-property view-start 'mevedel-view-source-key))
             (turn-id (get-text-property view-start 'mevedel-view-turn-id))
             (in-flight-after-section-p
              (when-let* ((pos
                           (mevedel-view-stream-in-flight-turn-start-position)))
                (<= view-start pos view-end))))
        (save-excursion
          (goto-char view-start)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (progn
                (delete-region view-start view-end)
                (setq source
                      (mevedel-view-render-insert-expanded-disclosure
                       data-buf source vtype header))
                (let ((body-end
                       (mevedel-view-render-section-body-end
                        view-start (point))))
                  (add-text-properties
                   view-start body-end
                   `(mevedel-view-source ,source
                     mevedel-view-source-key
                     ,(mevedel-view-disclosure-state-key
                       source vtype source-key)
                     mevedel-view-type ,vtype
                     mevedel-view-collapsed nil))
                  (mevedel-view-disclosure-record-state source vtype nil)
                  (when turn-id
                    (put-text-property
                     view-start body-end 'mevedel-view-turn-id turn-id)))
                (when in-flight-after-section-p
                  (mevedel-view-stream-set-in-flight-turn-start (point))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))))))

(defun mevedel-view-disclosure--collapse-section (source vtype)
  "Collapse an expanded SOURCE section of VTYPE to its rendered summary."
  (require 'mevedel-view-render)
  (require 'mevedel-view-segments)
  (require 'mevedel-view-stream)
  (let* ((bounds (mevedel-view-disclosure-section-bounds))
         (data-buf (mevedel-view-segments-display-buffer))
         (rendering
          (and data-buf
               (buffer-live-p data-buf)
               (mevedel-view-render-collapsed-disclosure
                data-buf source vtype))))
    (when (and bounds rendering)
      (setq source (plist-get rendering :source))
      (let* ((inhibit-read-only t)
             (view-start (car bounds))
             (view-end (mevedel-view-render-child-calls-end
                        (cdr bounds) (point-max)))
             (turn-id (get-text-property view-start 'mevedel-view-turn-id))
             (source-key
              (get-text-property view-start 'mevedel-view-source-key))
             (in-flight-after-section-p
              (when-let* ((pos
                           (mevedel-view-stream-in-flight-turn-start-position)))
                (<= view-start pos view-end))))
        (save-excursion
          (goto-char view-start)
          (set-marker-insertion-type mevedel-view--input-marker t)
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
                   `(mevedel-view-type ,vtype
                     mevedel-view-collapsed t
                     mevedel-view-source ,source
                     mevedel-view-source-key
                     ,(mevedel-view-disclosure-state-key
                       source vtype source-key)))
                  (mevedel-view-render-add-display-properties
                   ins-start (point) vtype)
                  (mevedel-view-disclosure-record-state source vtype t)
                  (when turn-id
                    (put-text-property
                     ins-start (point) 'mevedel-view-turn-id turn-id))
                  (when in-flight-after-section-p
                    (mevedel-view-stream-set-in-flight-turn-start (point)))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))))))
(provide 'mevedel-view-disclosure)
;;; mevedel-view-disclosure.el ends here
