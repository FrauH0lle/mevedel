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
;;     overlay to preserve undo access (line-based or single-char)
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

;; `mevedel'
(declare-function mevedel--project-root "mevedel" ())

;; `mevedel'
(defvar mevedel--instructions)
(declare-function mevedel--instruction-bufferlevel-p "mevedel-instructions" (instruction))


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
  "Parse OLD-TEXT and NEW-TEXT into line-by-line changes.
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
  (let ((ws-root (mevedel--project-root))
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

(defun mevedel-diff-apply-buffer ()
  "Apply diff using delete-and-recreate approach for overlay preservation.

This variant of `diff-apply-buffer' tries to
 - correctly adjus overlays in modified buffers
 - creates/removes files if required

This version first trims common prefixes/suffixes from each hunk to find
the minimal change region. It then calculates overlay adjustments based
on this precise region, applies the change, and deletes and re-creates
the overlays."
  (interactive)
  (pcase-let ((buffer-edits nil)
              (failures 0)
              (diff-refine nil)
              (`(,files-to-create ,files-to-remove) (mevedel--diff-find-file-operations)))
    (when files-to-create
      (dolist (file files-to-create)
        (unless (file-exists-p file)
          (make-empty-file file 'parents))))
    (save-excursion
      (goto-char (point-min))
      (diff-beginning-of-hunk t)
      (while (pcase-let ((`(,buf ,line-offset ,pos ,src ,dst ,switched)
                          (diff-find-source-location nil nil)))
               (cond ((and line-offset (not switched))
                      (push (list :buf buf :pos pos :src src :dst dst)
                            buffer-edits))
                     (t (setq failures (1+ failures))))
               (and (not (eq (prog1 (point) (ignore-errors (diff-hunk-next)))
                             (point)))
                    (looking-at-p diff-hunk-header-re)))))

    (cond ((zerop failures)
           (let ((edits-by-buffer (make-hash-table :test 'eq)))
             (dolist (edit buffer-edits)
               (let ((buf (plist-get edit :buf)))
                 (push edit (gethash buf edits-by-buffer))))

             (maphash
              (lambda (buf edits)
                (with-current-buffer buf
                  (let ((saved-overlays nil)   ; (start end properties final-start final-end)
                        (all-changes nil))     ; (position delta)

                    ;; PHASE 1: Analyze all hunks and calculate final overlay positions
                    (message "\n=== PHASE 1: Analyzing hunks and calculating positions ===")
                    (dolist (edit (reverse edits))
                      (let* ((pos (plist-get edit :pos))
                             (src (plist-get edit :src))
                             (dst (plist-get edit :dst))
                             (inhibit-read-only t)
                             (hunk-start (car pos))
                             (hunk-end (cdr pos))
                             (old-text-full (car src))
                             (new-text-full (car dst))
                             (buffer-text (buffer-substring-no-properties hunk-start hunk-end))
                             (old-text-norm (replace-regexp-in-string "\r" "" old-text-full))
                             (new-text-norm (replace-regexp-in-string "\r" "" new-text-full))
                             (text-offset (or (cl-loop for i from 0 to (length old-text-norm)
                                                       when (and (<= (+ i (length buffer-text))
                                                                     (length old-text-norm))
                                                                 (string= buffer-text
                                                                          (substring old-text-norm i
                                                                                     (+ i (length buffer-text)))))
                                                       return i)
                                              0))
                             (old-text-aligned (if (> text-offset 0)
                                                   (substring old-text-norm text-offset)
                                                 old-text-norm))
                             (new-text-aligned (if (> text-offset 0)
                                                   (substring new-text-norm text-offset)
                                                 new-text-norm))
                             (diff-regions (mevedel--safe-string-diff-regions
                                            old-text-aligned new-text-aligned))
                             (prefix-len (nth 0 diff-regions))
                             (suffix-len (nth 1 diff-regions))
                             (old-middle (nth 2 diff-regions))
                             (new-middle (nth 3 diff-regions))
                             (change-start (+ hunk-start prefix-len))
                             (change-end (- hunk-end suffix-len)))

                        (message "Hunk [%d-%d] -> Change [%d-%d]" hunk-start hunk-end change-start change-end)

                        ;; Record this change
                        (push (list change-start (- (length new-middle) (- change-end change-start)))
                              all-changes)

                        ;; Find affected overlays
                        (let ((affected-ovs (seq-filter
                                             (lambda (ov)
                                               (and (overlay-get ov 'mevedel-instruction)
                                                    (overlay-start ov)
                                                    (overlay-end ov)
                                                    (not (or (>= (overlay-start ov) change-end)
                                                             (<= (overlay-end ov) change-start)))))
                                             (overlays-in hunk-start hunk-end))))

                          (when affected-ovs
                            (message "  ⚠ %d overlays affected" (length affected-ovs))

                            ;; Parse into lines
                            (let ((line-changes (mevedel--parse-hunk-lines
                                                 old-middle new-middle change-start)))

                              ;; Calculate final position for each overlay
                              (dolist (ov affected-ovs)
                                ;; Skip buffer-level overlays - they span the whole buffer
                                ;; and changes are always within them, so no adjustment needed
                                (unless (mevedel--instruction-bufferlevel-p ov)
                                  (let* ((ov-start (overlay-start ov))
                                         (ov-end (overlay-end ov))
                                         (ov-props (overlay-properties ov))
                                         (was-line-based (mevedel--overlay-is-line-based-p ov-start ov-end buf))
                                         (relationship (mevedel--classify-change-relationship
                                                        ov-start ov-end change-start change-end))
                                         (adjustment
                                          (if (and (eq relationship 'encompasses)
                                                   (> (length new-middle) 0))
                                              ;; Encompasses with replacement: expand to cover it
                                              (list change-start (+ change-start (length new-middle)))
                                            ;; Use granular calculation for other cases
                                            (let ((granular-result (mevedel--calculate-overlay-adjustment-granular
                                                                    ov line-changes)))
                                              ;; Check if granular result is invalid (empty or nil)
                                              (if (or (null granular-result)
                                                      (null (car granular-result))
                                                      (null (cadr granular-result))
                                                      (= (car granular-result) (cadr granular-result)))
                                                  ;; Granular failed - determine appropriate action
                                                  (if (and (>= ov-start change-start)
                                                           (< ov-start change-end)
                                                           (> (length new-middle) 0)
                                                           (eq relationship 'complex))  ; Only for complex overlapping cases
                                                      ;; Expand to cover replacement as fallback
                                                      (list change-start (+ change-start (length new-middle)))
                                                    ;; For deletions or other failures: return invalid positions to trigger stub creation
                                                    (list change-start change-start))
                                                ;; Granular succeeded - use it
                                                granular-result)))))
                                    (when adjustment
                                      (let ((calc-start (car adjustment))
                                            (calc-end (cadr adjustment)))
                                        (message "    Overlay [%d-%d] -> calculated [%d-%d]"
                                                 ov-start ov-end calc-start calc-end)
                                        ;; Save for later: original pos, props, calculated pos, hunk pos, was-line-based
                                        (push (list ov-start ov-end ov-props calc-start calc-end change-start was-line-based)
                                              saved-overlays)))))))))))

                    ;; PHASE 2: Delete only the affected overlays that were saved
                    (message "\n=== PHASE 2: Deleting %d overlays ===" (length saved-overlays))
                    ;; Build a set of overlays to delete by scanning saved-overlays
                    (let ((overlays-to-delete (make-hash-table :test 'eq)))
                      ;; Mark all overlays at saved positions for deletion
                      (dolist (ov-data saved-overlays)
                        (let* ((orig-start (nth 0 ov-data))
                               (orig-end (nth 1 ov-data)))
                          ;; Find the overlay at this position
                          (dolist (ov (overlays-at orig-start))
                            (when (and (overlay-get ov 'mevedel-instruction)
                                       (= (overlay-start ov) orig-start)
                                       (= (overlay-end ov) orig-end))
                              (puthash ov t overlays-to-delete)))))
                      ;; Now delete only the marked overlays
                      (maphash (lambda (ov _)
                                 (delete-overlay ov))
                               overlays-to-delete))

                    ;; PHASE 3: Apply all text changes
                    (message "\n=== PHASE 3: Applying text changes ===")
                    (let ((inhibit-read-only t))
                      (dolist (edit (reverse edits))
                        (let* ((pos (plist-get edit :pos))
                               (src (plist-get edit :src))
                               (dst (plist-get edit :dst))
                               (hunk-start (car pos))
                               (hunk-end (cdr pos))
                               (old-text-full (car src))
                               (new-text-full (car dst))
                               (buffer-text (buffer-substring-no-properties hunk-start hunk-end))
                               (old-text-norm (replace-regexp-in-string "\r" "" old-text-full))
                               (new-text-norm (replace-regexp-in-string "\r" "" new-text-full))
                               (text-offset (or (cl-loop for i from 0 to (length old-text-norm)
                                                         when (and (<= (+ i (length buffer-text))
                                                                       (length old-text-norm))
                                                                   (string= buffer-text
                                                                            (substring old-text-norm i
                                                                                       (+ i (length buffer-text)))))
                                                         return i)
                                                0))
                               (old-text-aligned (if (> text-offset 0)
                                                     (substring old-text-norm text-offset)
                                                   old-text-norm))
                               (new-text-aligned (if (> text-offset 0)
                                                     (substring new-text-norm text-offset)
                                                   new-text-norm))
                               (diff-regions (mevedel--safe-string-diff-regions
                                              old-text-aligned new-text-aligned))
                               (prefix-len (nth 0 diff-regions))
                               (suffix-len (nth 1 diff-regions))
                               ;; (old-middle (nth 2 diff-regions))  ; Not needed in Phase 3
                               (new-middle (nth 3 diff-regions))
                               (change-start (+ hunk-start prefix-len))
                               (change-end (- hunk-end suffix-len)))

                          (message "  Applying change at [%d-%d]" change-start change-end)
                          (mevedel--replace-text change-start change-end new-middle))))

                    ;; PHASE 4: Recreate overlays with cumulative delta adjustment
                    (message "\n=== PHASE 4: Recreating %d overlays ===" (length saved-overlays))
                    (message "All changes: %S" all-changes)
                    (dolist (ov-data saved-overlays)
                      (let* ((orig-start (nth 0 ov-data))
                             (orig-end (nth 1 ov-data))
                             (ov-props (nth 2 ov-data))
                             (calc-start (nth 3 ov-data))
                             (calc-end (nth 4 ov-data))
                             (hunk-pos (nth 5 ov-data))
                             (was-line-based (nth 6 ov-data)))

                        ;; Calculate cumulative delta from changes before this overlay's hunk
                        (let ((cumulative-delta 0))
                          (dolist (change all-changes)
                            (when (< (car change) hunk-pos)
                              (setq cumulative-delta (+ cumulative-delta (cadr change)))))

                          ;; Check if overlay was deleted (invalid calc positions)
                          (let ((final-start (+ calc-start cumulative-delta))
                                (final-end (+ calc-end cumulative-delta)))

                            ;; Check if this is an "encompasses" case - need to look at the actual change
                            ;; Find the corresponding change to see if there's replacement content
                            (let ((encompassing-change nil))
                              (dolist (change all-changes)
                                (let ((ch-pos (car change))
                                      (ch-delta (cadr change)))
                                  ;; If change encompasses original overlay and has positive delta (replacement)
                                  (when (and (<= ch-pos orig-start)
                                             (>= (+ ch-pos (abs ch-delta)) orig-end)
                                             (> ch-delta 0))
                                    (setq encompassing-change change))))

                              ;; Handle stub creation for deleted overlays
                              (when (or (>= final-start final-end)
                                        (< final-start (point-min))
                                        (> final-end (point-max)))
                                (if encompassing-change
                                    ;; Replacement case: expand to cover the new content
                                    (let ((ch-pos (car encompassing-change))
                                          (ch-delta (cadr encompassing-change)))
                                      (message "  Overlay [%d-%d] encompassed by replacement, expanding" orig-start orig-end)
                                      (setq final-start (+ ch-pos cumulative-delta))
                                      (setq final-end (+ ch-pos ch-delta cumulative-delta)))
                                  ;; Deletion case: create stub
                                  (message "  Overlay [%d-%d] was deleted, creating stub" orig-start orig-end)
                                  (let ((stub-line (mevedel--find-stub-line buf hunk-pos)))
                                    (if was-line-based
                                        ;; Full line stub
                                        (setq final-start (car stub-line)
                                              final-end (cdr stub-line))
                                      ;; Single char stub at deletion point
                                      (let ((stub-pos (max (point-min) (min hunk-pos (point-max)))))
                                        (setq final-start stub-pos
                                              final-end (min (1+ stub-pos) (point-max)))))))))

                            ;; Apply line-span snapping if needed
                            (when (and was-line-based
                                       (< final-start final-end))
                              (let ((snapped (mevedel--snap-to-full-lines final-start final-end buf)))
                                (setq final-start (car snapped))
                                (setq final-end (cdr snapped))))

                            (message "  Overlay [%d-%d] calculated [%d-%d] delta %d final [%d-%d]"
                                     orig-start orig-end calc-start calc-end
                                     cumulative-delta final-start final-end)

                            ;; Create new overlay
                            (when (and (>= final-start (point-min))
                                       (<= final-end (point-max))
                                       (< final-start final-end))
                              (let ((new-ov (make-overlay final-start final-end (current-buffer) t nil)))
                                ;; Restore properties
                                (while ov-props
                                  (overlay-put new-ov (car ov-props) (cadr ov-props))
                                  (setq ov-props (cddr ov-props)))
                                (push new-ov (alist-get (overlay-buffer new-ov) mevedel--instructions))
                                (message "    Recreated: %S"
                                         (let ((c (buffer-substring-no-properties final-start final-end)))
                                           (if (< (length c) 40) c
                                             (concat (substring c 0 37) "..."))))))))))
                    (setf (alist-get buf mevedel--instructions)
                          (cl-remove-if (lambda (ov) (null (overlay-buffer ov)))
                                        (alist-get buf mevedel--instructions)))

                    (save-buffer))))
              edits-by-buffer)

             (when files-to-remove
               (dolist (file files-to-remove)
                 (when (file-exists-p file)
                   (when-let* ((buf (find-buffer-visiting file)))
                     (kill-buffer buf))
                   (delete-file file))))
             (message "Saved %d buffers" (hash-table-count edits-by-buffer))))
          (t
           (message "%d hunks failed; no buffers changed" failures)))))

(provide 'mevedel-diff-apply)

;;; mevedel-diff-apply.el ends here
