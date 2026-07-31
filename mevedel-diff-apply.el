;;; mevedel-diff-apply.el --- Delete-and-recreate instructions after diff -*- lexical-binding: t; -*-

;;; Commentary:

;; `mevedel-diff-apply-buffer', a variant of `diff-apply-buffer' that tries to
;; preserve instruction overlays when applying diffs to buffers.
;;
;; ## How It Works
;;
;; The function operates in 4 phases to ensure correct overlay positioning:
;;
;; ### Phase 1: Analysis and Position Calculation
;; - Parses each diff hunk to find the minimal change region (trimming common
;;   prefix/suffix)
;; - Identifies overlays affected by the change
;; - Classifies the geometric relationship between each overlay and the change:
;;   * 'before: Change is before overlay (overlay shifts)
;;   * 'after: Change is after overlay (overlay unaffected)
;;   * 'within: Change is within overlay (overlay expands/shrinks)
;;   * 'encompasses: Change encompasses overlay (overlay expands or becomes
;;     stub)
;;   * 'complex: Overlapping (requires granular line-by-line analysis)
;; - Calculates new positions for each affected overlay
;; - Stores overlay data (original positions, properties, calculated positions)
;;
;; ### Phase 2: Deletion
;; - Deletes all affected overlays from the buffer
;; - This prevents overlays from being corrupted during text changes
;;
;; ### Phase 3: Text Application
;; - Applies all text changes from the diff
;; - Buffer content is now updated but overlays are gone
;;
;; ### Phase 4: Recreation
;; - Recreates overlays at their calculated positions
;; - Applies cumulative deltas from all previous changes in the buffer
;; - Handles special cases:
;;   * Stub creation: When overlay content is deleted, creates a minimal stub
;;     overlay to preserve access (line-based or single-char)
;;   * Line-span preservation: Overlays that spanned full lines are snapped
;;     back to line boundaries
;;   * Buffer-level overlays: Skipped entirely (changes are always within them)
;;   * Invalid positions: Detected and converted to stubs
;;
;; ## Overlay Adjustment Strategies
;;
;; ### Simple Cases (Fast Path)
;; - 'encompasses + replacement: Expand overlay to cover new content
;; - 'encompasses + deletion: Mark for stub creation
;;
;; ### Complex Cases (Granular Analysis)
;; - Parses changes line-by-line
;; - Tracks cumulative deltas for multi-line changes
;; - Adjusts overlay positions based on which lines were modified
;; - Fallback: If granular calculation fails (returns invalid positions):
;;   * For complex overlapping cases with replacement: Expand to cover
;;     replacement
;;   * For deletions: Return invalid positions to trigger stub creation
;;
;; ## Special Cases Handled
;;
;; 1. **Boundary Cases**: Overlays that extend slightly beyond change region
;;    (e.g., overlay includes final newline, but change doesn't)
;;    -> Detected when granular calculation fails, expanded to cover replacement
;;
;; 2. **Buffer-Level Overlays**: Overlays spanning entire buffer (point-min to
;;    point-max)
;;    -> Skipped entirely, as they always encompass all changes
;;
;; 3. **Nested Overlays**: Multiple overlays at same position or overlapping
;;    -> Each adjusted independently based on its own relationship to changes
;;
;; 4. **Line-Based Overlays**: Overlays that span complete lines (BOL to
;;    BOL/EOL)
;;    -> After adjustment, snapped back to line boundaries to maintain property
;;
;; 5. **Stub Overlays**: When overlay content is deleted
;;    -> Line-based: Stub at nearest line above change
;;    -> Partial-line: Single-char stub at deletion point
;;
;; 6. **File Operations**: Creates new files or removes deleted files as needed
;;    based on diff content

;;; Code:

(require 'diff-mode)

;; `mevedel-structs'
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

;; `mevedel'
(declare-function mevedel--instruction-activate-buffer "mevedel-overlays" (&optional buffer))
(declare-function mevedel--instruction-alist-value "mevedel-overlays" ())
(declare-function mevedel--instruction-bufferlevel-p "mevedel-overlays" (instruction))
(declare-function mevedel--set-instruction-alist-value
                  "mevedel-overlays" (value))


(defun mevedel--string-common-prefix (strings)
  "Return the common prefix of all STRINGS.
If STRINGS is empty or contains empty strings, return empty string."
  (if (or (null strings) (member "" strings))
      ""
    (let ((first (car strings))
          (rest (cdr strings))
          (prefix-len 0))
      (while (and (< prefix-len (length first))
                  (cl-every (lambda (s)
                              (and (< prefix-len (length s))
                                   (eq (aref first prefix-len)
                                       (aref s prefix-len))))
                            rest))
        (setq prefix-len (1+ prefix-len)))
      (substring first 0 prefix-len))))

(defun mevedel--safe-string-diff-regions (old-text new-text)
  "Calculate non-overlapping prefix and suffix for OLD-TEXT and NEW-TEXT.

Prioritizes suffix calculation since hunk-end is authoritative.
Returns (prefix-len suffix-len actual-old-middle actual-new-middle).

The algorithm works by:
1. Finding the maximum common suffix (reversed prefix) first
2. Finding the maximum common prefix that doesn't overlap with the
   suffix
3. Extracting the middle regions that differ between the strings

This ensures that prefix and suffix regions never overlap, even when one
string is significantly longer than the other."
  (let* ((old-len (length old-text))
         (new-len (length new-text))
         (min-len (min old-len new-len))
         (max-suffix-len (length (mevedel--string-common-prefix
                                  (list (reverse old-text) (reverse new-text)))))
         (max-suffix-len (min max-suffix-len min-len))
         (prefix-max-end (- min-len max-suffix-len))
         (max-prefix-len (if (> prefix-max-end 0)
                             (length (mevedel--string-common-prefix
                                      (list (substring old-text 0 prefix-max-end)
                                            (substring new-text 0 prefix-max-end))))
                           0))
         (old-middle (if (> old-len (+ max-prefix-len max-suffix-len))
                         (substring old-text max-prefix-len
                                    (- old-len max-suffix-len))
                       ""))
         (new-middle (if (> new-len (+ max-prefix-len max-suffix-len))
                         (substring new-text max-prefix-len
                                    (- new-len max-suffix-len))
                       "")))
    (list max-prefix-len max-suffix-len old-middle new-middle)))

(defun mevedel--parse-hunk-lines (old-text new-text hunk-start)
  "Parse OLD-TEXT and NEW-TEXT into line-by-line differences.
HUNK-START is the buffer position where the hunk begins.

Returns a list of (line-start line-end old-line new-line delta-so-far)."
  (let ((old-lines (split-string old-text "\n" t))
        (new-lines (split-string new-text "\n" t))
        (current-pos hunk-start)
        (cumulative-delta 0)
        (result nil))

    ;; Match lines using longest common subsequence approach (simplified). For
    ;; now, use a simple heuristic: pair lines by position
    (let ((max-lines (max (length old-lines) (length new-lines))))
      (dotimes (i max-lines)
        (let* ((old-line (and (< i (length old-lines))
                              (concat (nth i old-lines) "\n")))
               (new-line (and (< i (length new-lines))
                              (concat (nth i new-lines) "\n")))
               (old-len (if old-line (length old-line) 0))
               (new-len (if new-line (length new-line) 0))
               (line-delta (- new-len old-len))
               (line-start current-pos)
               (line-end (+ current-pos old-len)))

          (push (list :start line-start
                      :end line-end
                      :old old-line
                      :new new-line
                      :delta line-delta
                      :cumulative-delta cumulative-delta)
                result)

          (setq current-pos line-end)
          (setq cumulative-delta (+ cumulative-delta line-delta)))))

    (nreverse result)))

(defun mevedel--find-overlay-lines (overlay line-changes)
  "Find which line(s) in LINE-CHANGES the OVERLAY spans.
Returns a list of line-change entries that overlap with the overlay."
  (let ((ov-start (overlay-start overlay))
        (ov-end (overlay-end overlay))
        (affected-lines nil))
    (dolist (line line-changes)
      (let ((line-start (plist-get line :start))
            (line-end (plist-get line :end)))
        ;; Check if overlay overlaps with this line
        (when (not (or (>= ov-start line-end)
                       (<= ov-end line-start)))
          (push line affected-lines))))
    (nreverse affected-lines)))

(defun mevedel--calculate-overlay-adjustment-granular (overlay line-changes)
  "Calculate how OVERLAY should be adjusted based on LINE-CHANGES.
Returns (new-start new-end) or nil if overlay should not be adjusted."
  (let* ((ov-start (overlay-start overlay))
         (ov-end (overlay-end overlay))
         (affected-lines (mevedel--find-overlay-lines overlay line-changes)))

    (cond
     ;; No affected lines - overlay is outside change region
     ((null affected-lines)
      nil)

     ;; Overlay spans multiple lines or is within lines that change
     (t
      (let* ((first-line (car affected-lines))
             (last-line (car (last affected-lines)))
             (first-start (plist-get first-line :start))
             (first-end (plist-get first-line :end))
             (last-start (plist-get last-line :start))
             (last-end (plist-get last-line :end))
             (first-delta (plist-get first-line :cumulative-delta))
             (last-cumulative (plist-get last-line :cumulative-delta))
             (last-delta (plist-get last-line :delta)))

        ;; Calculate new positions
        (let ((new-start
               (cond
                ;; Overlay starts before first affected line
                ((< ov-start first-start)
                 ov-start)
                ;; Overlay starts in first affected line
                ((and (>= ov-start first-start) (< ov-start first-end))
                 ;; Keep relative position within the line
                 (let* ((offset-in-line (- ov-start first-start))
                        (new-line (plist-get first-line :new)))
                   ;; Map offset to new line, capped at new line length
                   (+ first-start
                      first-delta
                      (min offset-in-line (length (or new-line ""))))))
                ;; Overlay starts after first line
                (t (+ ov-start first-delta))))

              (new-end
               (cond
                ;; Overlay ends in last affected line
                ((and (> ov-end last-start) (<= ov-end last-end))
                 (let* ((offset-in-line (- ov-end last-start))
                        (new-line (plist-get last-line :new)))
                   ;; Map offset to new line, capped at new line length
                   (+ last-start
                      last-cumulative
                      (min offset-in-line (length (or new-line ""))))))
                ;; Overlay ends after last affected line
                ((> ov-end last-end)
                 (+ ov-end (+ last-cumulative last-delta)))
                ;; Overlay ends before last line start
                (t (+ ov-end last-cumulative)))))

          (list new-start new-end)))))))

(defun mevedel--replace-text (start end text)
  "Replace the text in the region from START to END with TEXT."
  (save-excursion
    (goto-char start)
    (insert text)
    (delete-region (point) (+ (point) (- end start)))))

(defun mevedel--overlay-is-line-based-p (start end buffer)
  "Check if positions START and END in BUFFER span full lines (BOL to EOL/BOL)."
  (with-current-buffer buffer
    (save-excursion
      (and (progn (goto-char start) (bolp))
           (progn (goto-char end) (or (bolp) (eolp)))))))

(defun mevedel--snap-to-full-lines (start end buffer)
  "Adjust START and END to span full lines in BUFFER.
Returns (new-start . new-end)."
  (with-current-buffer buffer
    (save-excursion
      (goto-char start)
      (let ((new-start (line-beginning-position)))
        (goto-char end)
        (let ((new-end (if (bolp) end (line-beginning-position 2))))
          (cons new-start new-end))))))

(defun mevedel--classify-change-relationship (ov-start ov-end change-start change-end)
  "Classify geometric relationship between overlay and change region.

OV-START is the start of the overlay, OV-END is the end of the overlay,
CHANGE-START is the start of the change region and CHANGE-END is the end
of the change region.

Returns one of: \\='before, \\='after, \\='within, \\='encompasses,
\\='complex."
  (cond
   ;; Change completely before overlay
   ((<= change-end ov-start) 'before)
   ;; Change completely after overlay
   ((>= change-start ov-end) 'after)
   ;; Change completely within overlay
   ((and (>= change-start ov-start) (<= change-end ov-end)) 'within)
   ;; Change encompasses overlay
   ((and (<= change-start ov-start) (>= change-end ov-end)) 'encompasses)
   ;; Complex case (overlapping)
   (t 'complex)))

(defun mevedel--find-stub-line (buffer change-start)
  "Find appropriate line for stub overlay in BUFFER near CHANGE-START.
Returns (line-start . line-end) for the line above the change, or
current line if none above."
  (with-current-buffer buffer
    (save-excursion
      (goto-char change-start)
      (let ((current-line (line-number-at-pos)))
        (if (> current-line 1)
            ;; Use line above
            (progn
              (forward-line -1)
              (cons (line-beginning-position) (line-beginning-position 2)))
          ;; Use current line if at top
          (cons (line-beginning-position) (line-beginning-position 2)))))))

(defun mevedel--path-has-suffix-p (path suffix)
  "Check if PATH ends with directory suffix SUFFIX."
  (let* ((path-parts (seq-filter (lambda (s) (not (string-empty-p s)))
                                 (file-name-split path)))
         (suffix-parts (seq-filter (lambda (s) (not (string-empty-p s)))
                                   (file-name-split suffix))))
    (and (>= (length path-parts) (length suffix-parts))
         (equal suffix-parts
                (last path-parts (length suffix-parts))))))

(defun mevedel--diff-find-file-operations ()
  "Determine if diff application requires the creation/deletion of files."
  (let ((ws-root (mevedel-workspace-root (mevedel-workspace)))
        files-to-create
        files-to-remove)
    (goto-char (point-min))
    (diff-beginning-of-hunk t)
    (while (pcase-let* ((`(,new ,old) (diff-hunk-file-names))
                        (new (expand-file-name (diff-filename-drop-dir new) ws-root))
                        (old (expand-file-name (diff-filename-drop-dir old) ws-root))
                        (create-p (mevedel--path-has-suffix-p old "dev/null"))
                        (delete-p (mevedel--path-has-suffix-p new "dev/null")))
             (cond (create-p
                    (push new files-to-create))
                   (delete-p
                    (push old files-to-remove)))
             (and (not (eq (prog1 (point) (ignore-errors (diff-hunk-next)))
                           (point)))
                  (looking-at-p diff-hunk-header-re))))
    (list files-to-create files-to-remove)))

(defun mevedel-diff-apply--change (edit)
  "Return the canonical minimal text change for EDIT in the current buffer."
  (let* ((pos (plist-get edit :pos))
         (src (plist-get edit :src))
         (dst (plist-get edit :dst))
         (hunk-start (car pos))
         (hunk-end (cdr pos))
         (buffer-text (buffer-substring-no-properties hunk-start hunk-end))
         (old-text (replace-regexp-in-string "\r" "" (car src)))
         (new-text (replace-regexp-in-string "\r" "" (car dst)))
         (text-offset
          (or (cl-loop for i from 0 to (length old-text)
                       when (and (<= (+ i (length buffer-text))
                                     (length old-text))
                                 (string= buffer-text
                                          (substring old-text i
                                                     (+ i
                                                        (length buffer-text)))))
                       return i)
              0))
         (old-text (if (> text-offset 0)
                       (substring old-text text-offset)
                     old-text))
         (new-text (if (> text-offset 0)
                       (substring new-text text-offset)
                     new-text))
         (regions (mevedel--safe-string-diff-regions old-text new-text))
         (prefix-length (nth 0 regions))
         (suffix-length (nth 1 regions))
         (old-middle (nth 2 regions))
         (new-middle (nth 3 regions))
         (start (+ hunk-start prefix-length))
         (end (- hunk-end suffix-length)))
    (list :hunk-start hunk-start
          :hunk-end hunk-end
          :start start
          :end end
          :old old-middle
          :new new-middle
          :delta (- (length new-middle) (- end start)))))

(defun mevedel-diff-apply--analyze-overlays (buffer changes)
  "Return overlay adjustments and ordered deltas for CHANGES in BUFFER."
  (let (saved-overlays ordered-deltas)
    (dolist (change changes)
      (let* ((hunk-start (plist-get change :hunk-start))
             (hunk-end (plist-get change :hunk-end))
             (old-middle (plist-get change :old))
             (new-middle (plist-get change :new))
             (change-start (plist-get change :start))
             (change-end (plist-get change :end))
             (line-changes
              (mevedel--parse-hunk-lines
               old-middle new-middle change-start)))
        (push (list change-start (plist-get change :delta)) ordered-deltas)
        (dolist (overlay
                 (seq-filter
                  (lambda (candidate)
                    (and (overlay-get candidate 'mevedel-instruction)
                         (overlay-start candidate)
                         (overlay-end candidate)
                         (not (or (>= (overlay-start candidate) change-end)
                                  (<= (overlay-end candidate) change-start)))))
                  (overlays-in hunk-start hunk-end)))
          (unless (mevedel--instruction-bufferlevel-p overlay)
            (let* ((start (overlay-start overlay))
                   (end (overlay-end overlay))
                   (line-based-p
                    (mevedel--overlay-is-line-based-p start end buffer))
                   (relationship
                    (mevedel--classify-change-relationship
                     start end change-start change-end))
                   (granular
                    (unless (and (eq relationship 'encompasses)
                                 (not (string-empty-p new-middle)))
                      (mevedel--calculate-overlay-adjustment-granular
                       overlay line-changes)))
                   (adjustment
                    (cond
                     ((and (eq relationship 'encompasses)
                           (not (string-empty-p new-middle)))
                      (list change-start
                            (+ change-start (length new-middle))))
                     ((and granular
                           (car granular)
                           (cadr granular)
                           (/= (car granular) (cadr granular)))
                      granular)
                     ((and (>= start change-start)
                           (< start change-end)
                           (not (string-empty-p new-middle))
                           (eq relationship 'complex))
                      (list change-start
                            (+ change-start (length new-middle))))
                     (t (list change-start change-start)))))
              (push (list overlay (car adjustment) (cadr adjustment)
                          change-start line-based-p start end)
                    saved-overlays))))))
    (list saved-overlays ordered-deltas)))

(defun mevedel-diff-apply--restore-overlays
    (buffer saved-overlays ordered-deltas)
  "Restore SAVED-OVERLAYS in BUFFER after ORDERED-DELTAS were applied.
Return a hash table of the final live overlays."
  (cl-labels
      ((stub-bounds (position line-based-p)
         (let ((position (max (point-min) (min position (point-max)))))
           (if line-based-p
               (let ((line (mevedel--find-stub-line buffer position)))
                 (list (car line) (cdr line)))
             (list position (min (1+ position) (point-max))))))
       (snap (start end line-based-p)
         (if (and line-based-p (< start end))
             (let ((bounds (mevedel--snap-to-full-lines start end buffer)))
               (list (car bounds) (cdr bounds)))
           (list start end))))
    (let ((final-overlays (make-hash-table :test 'eq)))
      (dolist (record saved-overlays)
        (let* ((overlay (nth 0 record))
               (calculated-start (nth 1 record))
               (calculated-end (nth 2 record))
               (hunk-position (nth 3 record))
               (line-based-p (nth 4 record))
               (original-start (nth 5 record))
               (original-end (nth 6 record))
               (cumulative-delta
                (cl-loop for (position delta) in ordered-deltas
                         when (< position hunk-position)
                         sum delta))
               (final-start (+ calculated-start cumulative-delta))
               (final-end (+ calculated-end cumulative-delta))
               (encompassing-change
                (cl-find-if
                 (lambda (change)
                   (let ((position (car change))
                         (delta (cadr change)))
                     (and (<= position original-start)
                          (>= (+ position (abs delta)) original-end)
                          (> delta 0))))
                 ordered-deltas)))
          (when (or (>= final-start final-end)
                    (< final-start (point-min))
                    (> final-end (point-max)))
            (if encompassing-change
                (setq final-start
                      (+ (car encompassing-change) cumulative-delta)
                      final-end
                      (+ (car encompassing-change)
                         (cadr encompassing-change)
                         cumulative-delta))
              (pcase-let ((`(,start ,end)
                           (stub-bounds hunk-position line-based-p)))
                (setq final-start start
                      final-end end))))
          (pcase-let ((`(,start ,end)
                       (snap final-start final-end line-based-p)))
            (setq final-start start
                  final-end end))
          (when (and (>= final-start (point-min))
                     (<= final-end (point-max))
                     (< final-start final-end))
            (if-let* ((existing (gethash overlay final-overlays)))
                (puthash overlay (plist-put existing :duplicate t)
                         final-overlays)
              (puthash overlay
                       (list :orig-start original-start
                             :orig-end original-end
                             :final-start final-start
                             :final-end final-end
                             :hunk-pos hunk-position
                             :was-line-based line-based-p
                             :duplicate nil)
                       final-overlays)))))
      (maphash
       (lambda (overlay state)
         (let ((original-start (plist-get state :orig-start))
               (original-end (plist-get state :orig-end))
               (final-start (plist-get state :final-start))
               (final-end (plist-get state :final-end)))
           (when (plist-get state :duplicate)
             (setq final-start original-start
                   final-end original-end)
             (dolist (change ordered-deltas)
               (when (< (car change) original-start)
                 (cl-incf final-start (cadr change)))
               (when (< (car change) original-end)
                 (cl-incf final-end (cadr change))))
             (pcase-let ((`(,start ,end)
                          (snap final-start final-end
                                (plist-get state :was-line-based))))
               (setq final-start start
                     final-end end)))
           (when (or (>= final-start final-end)
                     (< final-start (point-min))
                     (> final-end (point-max)))
             (pcase-let ((`(,start ,end)
                          (stub-bounds
                           (or (plist-get state :hunk-pos) original-start)
                           (plist-get state :was-line-based))))
               (setq final-start start
                     final-end end)))
           (move-overlay overlay final-start final-end (current-buffer))))
       final-overlays)
      final-overlays)))

(defun mevedel-diff-apply--apply-changes (changes)
  "Apply canonical CHANGES to the current buffer."
  (let ((inhibit-read-only t))
    (dolist (change changes)
      (mevedel--replace-text
       (plist-get change :start)
       (plist-get change :end)
       (plist-get change :new)))))

(defun mevedel-diff-apply--apply-buffer (buffer edits &optional created-p)
  "Apply EDITS and preserve instruction overlays in BUFFER.
CREATED-P means remove BUFFER's newly created file if saving fails."
  (with-current-buffer buffer
    (let* ((changes
            (mapcar #'mevedel-diff-apply--change (reverse edits)))
           (analysis
            (mevedel-diff-apply--analyze-overlays buffer changes))
           (saved-overlays (car analysis))
           (ordered-deltas (cadr analysis))
           (change-group (prepare-change-group))
           (file (buffer-file-name buffer))
           (was-modified-p (buffer-modified-p))
           (original-modtime (visited-file-modtime))
           (snapshot
            (and file (not created-p) (file-exists-p file)
                 (make-temp-file "mevedel-diff-snapshot-")))
           final-overlays
           accepted)
      (when snapshot
        (copy-file file snapshot t t t t))
      (unwind-protect
          (unwind-protect
              (progn
                (activate-change-group change-group)
                (dolist (record saved-overlays)
                  (delete-overlay (car record)))
                (mevedel-diff-apply--apply-changes changes)
                (setq final-overlays
                      (mevedel-diff-apply--restore-overlays
                       buffer saved-overlays ordered-deltas))
                (save-buffer)
                (accept-change-group change-group)
                (setq accepted t))
            (unless accepted
              (cancel-change-group change-group)
              (dolist (record saved-overlays)
                (move-overlay (nth 0 record) (nth 5 record) (nth 6 record)
                              buffer))
              (cond
               (created-p
                (when (and file (file-exists-p file))
                  (delete-file file)))
               (snapshot
                (copy-file snapshot file t t t t)))
              (if created-p
                  (set-visited-file-modtime original-modtime)
                (set-visited-file-modtime))
              (set-buffer-modified-p was-modified-p)))
        (when snapshot
          (delete-file snapshot)))
      (mevedel--instruction-activate-buffer buffer)
      (let ((instruction-alist (mevedel--instruction-alist-value)))
        (maphash
         (lambda (overlay _state)
           (cl-pushnew overlay (alist-get buffer instruction-alist)
                       :test #'eq))
         final-overlays)
        (setf (alist-get buffer instruction-alist)
              (cl-remove-if
               (lambda (overlay) (null (overlay-buffer overlay)))
               (alist-get buffer instruction-alist)))
        (mevedel--set-instruction-alist-value instruction-alist)))))

(defun mevedel-diff-apply-buffer (&optional no-prompt)
  "Apply diff using delete-and-recreate approach for overlay preservation.

Compared to `diff-apply-buffer', this variant adjusts overlays in
modified buffers and creates or removes files when required.

When NO-PROMPT is non-nil, reject hunks that Emacs would offer to
repair heuristically instead of prompting or modifying the diff.

This version first trims common prefixes/suffixes from each hunk to find
the minimal change region.  It then calculates overlay adjustments based
on this precise region, applies the change, and deletes and re-creates
the overlays."
  (interactive)
  (when no-prompt
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward diff-hunk-header-re nil t)
        (error "No diff hunks found"))
      (let ((hunk-start (match-beginning 0)))
        (while hunk-start
          (goto-char hunk-start)
          (let* ((start (line-beginning-position))
                 (end (min (point-max) (+ start 500)))
                 (diagnostic
                  (concat (buffer-substring-no-properties start end)
                          (when (< end (point-max)) "..."))))
            (condition-case err
                (cl-letf (((symbol-function 'y-or-n-p)
                           (lambda (prompt)
                             (error "Heuristic repair required: %s" prompt))))
                  (diff-sanity-check-hunk))
              (error
               (error "Rejected ambiguous diff hunk: %s\n%s"
                      (error-message-string err)
                      diagnostic))))
          (forward-line)
          (setq hunk-start
                (and (re-search-forward diff-hunk-header-re nil t)
                     (match-beginning 0)))))))
  (pcase-let ((buffer-edits nil)
              (failures 0)
              (created-files nil)
              (preexisting-buffers nil)
              (applied nil)
              (diff-refine nil)
              (`(,files-to-create ,files-to-remove) (mevedel--diff-find-file-operations)))
    (unwind-protect
        (progn
          (dolist (file files-to-create)
            (unless (file-exists-p file)
              (when-let* ((buffer (find-buffer-visiting file)))
                (push buffer preexisting-buffers))
              (make-empty-file file 'parents)
              (push file created-files)))
          (save-excursion
            (goto-char (point-min))
            (diff-beginning-of-hunk t)
            (while
                (pcase-let
                    ((`(,buf ,line-offset ,pos ,src ,dst ,switched)
                      (diff-find-source-location nil nil no-prompt)))
                  (cond ((and line-offset (not switched))
                         (push (list :buf buf :pos pos :src src :dst dst)
                               buffer-edits))
                        (t (setq failures (1+ failures))))
                  (and
                   (not
                    (eq (prog1 (point) (ignore-errors (diff-hunk-next)))
                        (point)))
                   (looking-at-p diff-hunk-header-re)))))

          (if (zerop failures)
              (let ((edits-by-buffer (make-hash-table :test 'eq)))
                (dolist (edit buffer-edits)
                  (let ((buf (plist-get edit :buf)))
                    (push edit (gethash buf edits-by-buffer))))

                (maphash
                 (lambda (buffer edits)
                   (mevedel-diff-apply--apply-buffer
                    buffer edits
                    (member (buffer-file-name buffer) created-files)))
                 edits-by-buffer)

                (dolist (file files-to-remove)
                  (when (file-exists-p file)
                    (when-let* ((buf (find-buffer-visiting file)))
                      (kill-buffer buf))
                    (delete-file file)))
                (setq applied t)
                (message "Saved %d buffers"
                         (hash-table-count edits-by-buffer)))
            (message "%d hunks failed; no buffers changed" failures)))
      (unless applied
        (dolist (file created-files)
          (when-let* ((buffer (find-buffer-visiting file))
                      ((not (memq buffer preexisting-buffers))))
            (kill-buffer buffer))
          (when (file-exists-p file)
            (delete-file file)))))))

(provide 'mevedel-diff-apply)

;;; mevedel-diff-apply.el ends here
