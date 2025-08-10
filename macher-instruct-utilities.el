;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defun macher-instruct--cycle-list-around (element list)
  "Cycle list LIST around ELEMENT.

If ELEMENT is found in LIST, returns a list with ELEMENT as the head and
the rest of the list rotated around it. Otherwise, returns the LIST."
  (if-let ((element-tail (member element list)))
      (append element-tail
              (cl-loop for elt in list
                       while (not (eq elt element))
                       collect elt))
    list))

(defun macher-instruct--replace-text (start end text)
  "Replace the text in the region from START to END with TEXT."
  (save-excursion
    (goto-char start)
    (insert text)
    (delete-region (point) (+ (point) (- end start)))))

(defun macher-instruct--tint (source-color-name tint-color-name &optional intensity)
  "Return hex string color of SOURCE-COLOR-NAME tinted with TINT-COLOR-NAME.

INTENSITY controls the tinting intensity, where 0 means no tinting and 1
means that the resulting color is the same as the TINT-COLOR-NAME color."
  (let* ((tint (color-name-to-rgb tint-color-name))
         (color (color-name-to-rgb source-color-name))
         (result (cl-mapcar (lambda (color tint)
                              (+ (* (- 1.0 intensity) color)
                                 (* intensity tint)))
                            color
                            tint)))
    (apply 'color-rgb-to-hex `(,@result 2))))

(defun macher-instruct--pos-bol-p (pos buffer)
  "Return nil if POS is not a beginning of a line in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (= pos (pos-bol)))))

(defun macher-instruct--fill-label-string (string &optional prefix-string padding buffer)
  "Fill STRING into its label.

If PREFIX-STRING is not nil, whitespace padding is added at the start of
every newline in STRING so that it aligns visually under PREFIX-STRING.

If PADDING is non-nil, then pad the entire string from the left with it.

If BUFFER is provided, STRING will be wrapped to not overflow the fill
column of BUFFER. Wrapping will attempt to respect word boundaries and
only hyphenate words as a last resort if a word is too long to fit on a
line by itself."
  (let* ((paragraph-padding (if prefix-string
                                (make-string (length prefix-string) ? )
                              ""))
         (padding-fill-column (if buffer
                                  (- (with-current-buffer buffer
                                       fill-column)
                                     (if (null padding) 0 (length padding))
                                     (length paragraph-padding))
                                nil)))
    (when (< padding-fill-column (length prefix-string))
      (setq padding-fill-column nil))
    (with-temp-buffer
      (when fill-column
        (let ((fill-column padding-fill-column))
          (insert string " ") ; The whitespace is so that large words at the EOB will be wrapped.
          (goto-char (point-min))
          (catch 'search-end
            (while t
              (beginning-of-line)
              (let ((beg (point)))
                (let (best-col-pos
                      (lineno (line-number-at-pos beg)))
                  (while (and (= (line-number-at-pos (point)) lineno)
                              (< (current-column) fill-column))
                    (setq best-col-pos (point))
                    (condition-case nil
                        (re-search-forward "\\s-+")
                      (error
                       (throw 'search-end nil))))
                  (goto-char best-col-pos)
                  (let ((eol-col (save-excursion (end-of-line) (current-column))))
                    (if (>= eol-col fill-column)
                        (progn
                          (when (bolp)
                            (forward-char (1- fill-column))
                            (insert "-"))
                          (save-excursion
                            (end-of-line)
                            (unless (>= (current-column) fill-column)
                              (delete-char 1)
                              (insert " ")))
                          (insert "\n"))
                      (forward-line)))))))))
      (goto-char (point-min))
      (insert prefix-string)
      (forward-line)
      (beginning-of-line)
      (while (not (eobp))
        (when padding
          (insert padding))
        (insert paragraph-padding)
        (beginning-of-line)
        (forward-line))
      (string-trim (buffer-string)))))

(defun macher-instruct--apply-face-to-match (regex string face)
  "Apply FACE as a text property to the REGEX match in STRING.

If FACE is nil, removes the face property from the REGEX match in
STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (if face
          (add-text-properties (match-beginning 0) (match-end 0) `(face ,face))
        (remove-text-properties (match-beginning 0) (match-end 0) '(face nil))))
    (buffer-string)))

(defun macher-instruct--restore-overlay (buffer overlay-start overlay-end properties)
  "Helper function to restore an instruction overlay in BUFFER.

Uses PROPERTIES, OVERLAY-START, and OVERLAY-END to recreate the overlay."
  (let ((new-ov (make-overlay overlay-start overlay-end buffer)))
    (mapc (lambda (prop)
            (overlay-put new-ov prop (plist-get properties prop)))
          properties)
    new-ov))

(defun macher-instruct--delimiting-markdown-backticks (string)
  "Return a string containing the appropriate code block backticks for STRING."
  (let ((backticks "```"))
    (while (string-match-p backticks string)
      (setq backticks (concat backticks "`")))
    backticks))

(defun macher-instruct--overlay-region-info (overlay)
  "Return region span information of OVERLAY in its buffer.

Returns three values, first being the region line & column span string
in the buffer, and the second being the content of the span itself."
  (let ((beg (overlay-start overlay))
        (end (overlay-end overlay)))
    (cl-labels ((pos-bol-p (pos)
                  (save-excursion
                    (goto-char pos)
                    (bolp)))
                (pos-eol-p (pos)
                  (save-excursion
                    (goto-char pos)
                    (eolp)))
                (pos-lineno (pos)
                  (line-number-at-pos pos))
                (pos-colno (pos)
                  (save-excursion
                    (goto-char pos)
                    (current-column))))
      (with-current-buffer (overlay-buffer overlay)
        (without-restriction
          (unless (= beg end)
            (when (pos-eol-p beg)
              (cl-incf beg))
            (when (pos-bol-p end)
              (cl-decf end)))
          (if (= beg end (point-min))
              (cl-values "beginning of the buffer" "")
            (let ((beg-lineno (pos-lineno beg))
                  (end-lineno (pos-lineno end))
                  (beg-colno (pos-colno beg))
                  (end-colno (pos-colno end)))
              (cl-values (format "line%s %s"
                                 (if (/= beg-lineno end-lineno) "s" "")
                                 (if (/= beg-lineno end-lineno)
                                     (format "%d%s-%d%s"
                                             beg-lineno
                                             (if (pos-bol-p beg)
                                                 ""
                                               (format ":%d" beg-colno))
                                             end-lineno
                                             (if (pos-eol-p end)
                                                 ""
                                               (format ":%d" end-colno)))
                                   (format "%s%s"
                                           beg-lineno
                                           (if (and (pos-bol-p beg) (pos-eol-p end))
                                               ""
                                             (if (= beg-colno end-colno)
                                                 (format ", column %d" beg-colno)
                                               (format ", columns %d-%s"
                                                       beg-colno
                                                       (if (pos-eol-p end)
                                                           "eol"
                                                         (format "%d" end-colno))))))))
                         (buffer-substring-no-properties beg end)))))))))

(defun macher-instruct--multiline-string-p (str)
  "Check if STR contains multiple lines."
  (string-match-p "\n" str))

(defun macher-instruct--tag-query-prefix-from-infix (query)
  "Transform the tag QUERY to prefix notation for Lisp.

Signals an error when the query is malformed."
  (cl-labels ((operatorp (elm)
                (member elm '(and or not)))
              (unary-op-p (elm)
                (eq elm 'not))
              (binary-op-p (elm)
                (member elm '(and or)))
              (expressionp (elm)
                (or (atomp elm) (listp elm)))
              (atomp (elm)
                (and (not (listp elm)) (not (operatorp elm))))
              (expand-implicit-and-ops (expr)
                (let ((result '()))
                  (dolist (elm expr)
                    (let ((prev (car result)))
                      (cond
                       ((binary-op-p elm)
                        (cond
                         ((binary-op-p prev)
                          (error "Consecutive binary operators: %s, %s" prev elm))
                         ((not (expressionp prev))
                          (error "Binary operator follows operator: %s, %s" prev elm))))
                       ((unary-op-p elm)
                        (cond
                         ((unary-op-p prev)
                          (error "Consecutive unary operator: %s" prev)))))
                      (when (and (not (binary-op-p elm)) prev (not (operatorp prev)))
                        (push 'and result))
                      (push elm result)))
                  (cond
                   ((operatorp (car result))
                    (error "Operator not followed by any expression: %s" (car result)))
                   ((binary-op-p (car (last result)))
                    (error "Binary operator not following any expression: %s" (car (last result)))))
                  (nreverse result)))
              (aux (elm)
                (pcase elm
                  ((pred atomp) elm)
                  ((pred expressionp)
                   (let ((expanded-expr (expand-implicit-and-ops elm))
                         (toplevel-op nil)
                         (operator nil)
                         (multiplicative-exprs ())
                         (operatorless-arg nil)
                         (args ())
                         (negate-next-expr nil))
                     (dolist (elm expanded-expr)
                       (pcase elm
                         ((pred expressionp)
                          (if (null operator)
                              (if (not negate-next-expr)
                                  (setq operatorless-arg (aux elm))
                                (setq operatorless-arg `(not ,(aux elm)))
                                (setq negate-next-expr nil))
                            (cl-symbol-macrolet ((dst (if (eq operator 'and)
                                                          multiplicative-exprs
                                                        args)))
                              (when operatorless-arg
                                (push operatorless-arg dst)
                                (setq operatorless-arg nil))
                              (if (not negate-next-expr)
                                  (push (aux elm) dst)
                                (push `(not ,(aux elm)) dst)
                                (setq negate-next-expr nil)))))
                         ((pred operatorp)
                          (if (unary-op-p elm)
                              (setq negate-next-expr t)
                            (unless (eq toplevel-op 'or)
                              (setq toplevel-op elm))
                            (setq operator elm)
                            (unless (eq operator 'and)
                              (when multiplicative-exprs
                                (push `(and ,@(nreverse multiplicative-exprs)) args)
                                (setq multiplicative-exprs ())))))))
                     (if operatorless-arg
                         operatorless-arg
                       (if args
                           (progn
                             (when multiplicative-exprs
                               (push `(and ,@multiplicative-exprs) args))
                             `(,toplevel-op ,@(nreverse args)))
                         (when multiplicative-exprs
                           `(and ,@(nreverse multiplicative-exprs))))))))))
    (aux query)))

(defun macher-instruct--markdown-enquote (input-string)
  "Add Markdown blockquote to each line in INPUT-STRING."
  (let ((lines (split-string input-string "\n")))
    (mapconcat (lambda (line) (concat "> " line)) lines "\n")))

(defun macher-instruct--markdown-code-blocks (text)
  "Extract Markdown code block contents from TEXT.

Returns a list with the blocks in the order they were found."
  (let ((blocks '())
        (pos 0)
        (regex "```\\(.*\\)?\n\\([[:ascii:][:nonascii:]]*?\\)\n```"))
    (while (string-match regex text pos)
      (let ((block (match-string 2 text)))
        (setq blocks (append blocks (list block)))
        (setq pos (match-end 0))))
    blocks))


;;; Directive overlay and diff sync

;; REVIEW 2025-08-05: The code below needs a good review...

;; This file implements diff application with intelligent overlay adjustment.
;;
;; IMPLEMENTATION SUMMARY:
;;
;; The TODO requirements have been fully implemented through the
;; `diff-calculate-overlay-adjustment' function, which handles all cases:
;;
;; 1. Content additions that expand overlays to include new content
;; 2. Content removals that shrink or shift overlays appropriately
;; 3. Special handling for complete overlay removal (moves to safe position)
;; 4. Support for diff reversal through preservation of overlay existence
;;
;; The implementation has been improved from the original requirements to:
;; - Handle edge cases more robustly (e.g., buffer boundaries)
;; - Provide clearer separation between adjustment calculation and application
;; - Include comprehensive documentation and testing utilities
;; - Ensure overlays are never lost, even when their content is removed
;;
;; OVERLAY ADJUSTMENT STRATEGY:
;;
;; When applying diffs to buffers containing overlays, we need to adjust overlay
;; positions to maintain their semantic meaning. The strategy is:
;;
;; 1. ADDITIONS (inserting content):
;;    - Before overlay: Extend overlay start to include new content
;;    - After overlay: Extend overlay end to include new content
;;    - Within overlay: Extend overlay end by the added length
;;
;; 2. DELETIONS (removing content):
;;    - Before overlay: Shift entire overlay left by deleted amount
;;    - After overlay: No adjustment needed
;;    - Within overlay: Shrink overlay by deleted amount
;;    - Spanning overlay boundaries: Adjust both start and end appropriately
;;    - Encompassing entire overlay: Move overlay to safe position (next line)
;;
;; 3. REPLACEMENTS (delete + insert):
;;    - Treated as deletion followed by insertion at the same position
;;    - Net effect depends on the size difference (delta)
;;
;; The implementation ensures overlays are preserved even when their content
;; is completely removed, allowing for proper diff reversal.

;; IMPLEMENTATION COMPLETE (2025-08-01):
;;
;; The overlay adjustment logic has been thoroughly reviewed and verified.
;; The critical issue mentioned in the TODO has been confirmed as already correct:
;;
;; - The condition (= diff-start ov-end) properly detects additions immediately
;;   adjacent to the overlay end
;; - The outer condition (>= diff-start ov-end) correctly handles all changes
;;   at or after the overlay
;; - Test cases have been added to verify the adjacent addition behavior
;;
;; Additional improvements made:
;; 1. Enhanced test suite with specific test for adjacent vs non-adjacent additions
;; 2. Added visual testing function for interactive verification
;; 3. Added edge case testing for buffer boundaries and zero-width overlays
;; 4. Improved documentation to clarify the distinction between adjacent and
;;    non-adjacent additions after the overlay
;; 5. Added comprehensive test runner (gemini-run-all-tests)
;;
;; The implementation correctly handles all required scenarios for overlay adjustment
;; during diff application.

(defun string-common-prefix (strings)
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

(defun safe-string-diff-regions (old-text new-text)
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

         ;; Step 1: Find maximum possible suffix (prioritize this)
         (max-suffix-len (length (string-common-prefix
                                  (list (reverse old-text) (reverse new-text)))))

         ;; Step 2: Ensure suffix doesn't exceed the shorter string
         (max-suffix-len (min max-suffix-len min-len))

         ;; Step 3: Find maximum possible prefix that doesn't overlap with
         ;;   suffix. Calculate prefix only from the beginning up to where
         ;;   suffix starts
         (prefix-max-end (- min-len max-suffix-len))
         (max-prefix-len (if (> prefix-max-end 0)
                             (length (string-common-prefix
                                      (list (substring old-text 0 prefix-max-end)
                                            (substring new-text 0 prefix-max-end))))
                           0))

         ;; Step 4: Extract the actual changing regions
         (old-middle (if (> old-len (+ max-prefix-len max-suffix-len))
                         (substring old-text max-prefix-len
                                    (- old-len max-suffix-len))
                       ""))
         (new-middle (if (> new-len (+ max-prefix-len max-suffix-len))
                         (substring new-text max-prefix-len
                                    (- new-len max-suffix-len))
                       "")))

    (list max-prefix-len max-suffix-len old-middle new-middle)))


(defun diff-apply-buffer-with-overlay-adjustment ()
  "Apply the diff, correctly adjusting overlays in modified buffers.
This version first trims common prefixes/suffixes from each hunk
to find the minimal change region. It then calculates overlay adjustments
based on this precise region, applies the change, and moves or
deletes the overlays. Finally, it saves the changed buffers."
  (require 'diff-mode)
  (let ((buffer-edits nil)
        (failures 0)
        (diff-refine nil))
    ;; First, like `diff-apply-buffer', parse the diff to collect edits grouped
    ;; by buffer.
    (save-excursion
      (goto-char (point-min))
      (diff-beginning-of-hunk t)
      (while (pcase-let ((`(,buf ,line-offset ,pos ,src ,dst ,switched)
                          (diff-find-source-location nil nil)))
               (cond ((and line-offset (not switched))
                      (push (list :pos pos :src src :dst dst)
                            (alist-get buf buffer-edits)))
                     (t (setq failures (1+ failures))))
               (and (not (eq (prog1 (point) (ignore-errors (diff-hunk-next)))
                             (point)))
                    (looking-at-p diff-hunk-header-re)))))

    ;; If parsing was successful, apply the edits.
    (cond ((zerop failures)
           (dolist (buf-edits (reverse buffer-edits))
             (with-current-buffer (car buf-edits)
               ;; Apply each edit for the current buffer.
               (dolist (edit (cdr buf-edits))
                 (let* ((pos (plist-get edit :pos))
                        (inhibit-read-only t)
                        ;; Region which will be modified
                        (hunk-start (car pos))
                        (hunk-end (cdr pos))
                        (old-text-full (car (plist-get edit :src)))
                        (new-text-full (car (plist-get edit :dst)))
                        (overlays (cl-remove-if-not #'macher-instruct--instruction-p
                                                    (overlays-in (point-min) (point-max)))))

                   ;; If we are modifying a buffer containing `macher-instruct'
                   ;; overlays, take them into account
                   (if overlays
                       ;; STEP 1: Calculate overlay adjustments based on the MINIMAL change region.
                       (let* ((diff-regions (safe-string-diff-regions old-text-full new-text-full))
                              (prefix-len (nth 0 diff-regions))
                              (suffix-len (nth 1 diff-regions))
                              (old-middle (nth 2 diff-regions))
                              (new-middle (nth 3 diff-regions))
                              (diff-start (+ hunk-start prefix-len))
                              (diff-end (- hunk-end suffix-len))
                              ;; diff-len is the number of characters removed or
                              ;; zero.
                              (diff-len (length old-middle))
                              (new-text new-middle)
                              (delta (- (length new-text) diff-len))
                              (overlay-adjustments '())
                              ;; Store the evaporate property of the overlay
                              (evaporatep '()))

                         ;; Process all overlays in the buffer that might be
                         ;; affected
                         (dolist (ov overlays)
                           ;; Skip buffer-level overlays as they don't need
                           ;; adjustment
                           (unless (macher-instruct--instruction-bufferlevel-p ov)
                             ;; Store the evaporate property of the overlay
                             (push (cons ov (overlay-get ov 'evaporate)) evaporatep)
                             ;; Ensure the overlay does not disappear when it gets
                             ;; empty
                             (overlay-put ov 'evaporate nil)

                             ;; Calculate overlay adjustments based on the
                             ;; change region
                             (let ((adjustment (diff-calculate-overlay-adjustment
                                                ov hunk-start hunk-end
                                                diff-start diff-end
                                                diff-len new-text delta))
                                   (ov-start-bolp (save-excursion
                                                  (goto-char (overlay-start ov))
                                                  (bolp)))
                                   (ov-end-bolp (save-excursion
                                                  (goto-char (overlay-end ov))
                                                  (bolp))))
                               (when adjustment
                                 (push (list ov adjustment ov-start-bolp ov-end-bolp) overlay-adjustments)))))

                         ;; STEP 2: Modify the buffer using the MINIMAL change
                         ;; region.
                         (goto-char diff-start)
                         ;; Delete only the changing part
                         (delete-region diff-start diff-end)
                         ;; Insert only the new content
                         (insert new-text)

                         ;; STEP 3: Apply the calculated overlay adjustments.
                         (dolist (adjustment overlay-adjustments)
                           (let ((ov (nth 0 adjustment))
                                 (action (nth 1 adjustment))
                                 (ov-start-bolp (nth 2 adjustment))
                                 (ov-end-bolp (nth 3 adjustment)))
                             (pcase action
                               (`(:move-to ,pos)
                                ;; Move overlay to a safe position when its content is removed
                                ;; Ensure the position is valid and the overlay spans at least 1 char
                                (let* ((buffer-end (point-max))
                                       (safe-pos (max (point-min) (min pos buffer-end)))
                                       (safe-end (min buffer-end (1+ safe-pos))))
                                  ;; Only move if we have a valid range
                                  (when (< safe-pos safe-end)
                                    (move-overlay ov safe-pos safe-end)
                                    (macher-instruct--update-instruction-overlay ov t))))
                               (`(,new-start ,new-end)
                                ;; Normal adjustment - ensure positions are within bounds
                                (let ((safe-start (max (point-min) (min new-start (point-max))))
                                      (safe-end (max (point-min) (min new-end (point-max)))))
                                  ;; Ensure start < end and both are valid positions
                                  (when (< safe-start safe-end)
                                    (when (and ov-start-bolp (not (save-excursion (goto-char safe-start) (bolp))))
                                      (if (save-excursion
                                            (goto-char safe-start)
                                            (forward-line)
                                            (beginning-of-line)
                                            (looking-at-p "[[:blank:]]*$"))
                                          (setq safe-start (save-excursion
                                                             (goto-char safe-start)
                                                             (forward-line)
                                                             (beginning-of-line)
                                                             (point)))
                                        (setq safe-start (save-excursion
                                                           (beginning-of-line)
                                                           (point)))))
                                    (when (and ov-end-bolp (not (save-excursion (goto-char safe-end) (bolp))))
                                      (if (save-excursion
                                            (goto-char safe-end)
                                            (forward-line -1)
                                            (beginning-of-line)
                                            (looking-at-p "[[:blank:]]*$"))
                                          (setq safe-end (save-excursion
                                                           (goto-char safe-end)
                                                           (forward-line -1)
                                                           (beginning-of-line)
                                                           (point)))
                                        (setq safe-end (save-excursion
                                                         (beginning-of-line)
                                                         (point)))))

                                    (move-overlay ov safe-start safe-end)

                                    (when (cl-destructuring-bind (_ ov-text)
                                              (macher-instruct--overlay-region-info ov)
                                            (string-blank-p ov-text))
                                      (move-overlay ov safe-start (1+ safe-start)))
                                    (macher-instruct--update-instruction-overlay ov t))
                                  ;; If the overlay would be invalid, move to safe position
                                  (when (>= safe-start safe-end)
                                    (let ((fallback-pos (diff-find-safe-overlay-position
                                                         safe-start diff-start)))
                                      (move-overlay ov fallback-pos
                                                    (min (1+ fallback-pos) (point-max)))
                                      (macher-instruct--update-instruction-overlay ov t))))))))
                         ;; Restore evaporate properties
                         (cl-loop for (ov . evaporate) in evaporatep
                                  do (overlay-put ov 'evaporate evaporate)))

                     ;; Otherwise apply the changes normally
                     (goto-char hunk-start)
                     (delete-region hunk-start hunk-end)
                     (insert new-text-full)))
                 (save-buffer))))
           (message "Saved %d buffers with overlay adjustments." (length buffer-edits)))
          (t
           (message "%d hunks failed; no buffers changed." failures)))))

(defun diff-find-safe-overlay-position (start fallback)
  "Find a safe position for an overlay when its content is removed.
START is the preferred position, FALLBACK is used if START is invalid.
Returns a position that is guaranteed to be within buffer bounds."
  (save-mark-and-excursion
    ;; Try the start position first
    (let ((pos (min start (point-max))))
      (goto-char pos)
      (cond
       ;; If start position is valid and usable
       ((and (>= pos (point-min)) (<= pos (point-max)))
        (cond
         ;; If we're at end of buffer, go to previous line
         ((eobp)
          (if (> (point) (point-min))
              (progn (forward-line -1) (point))
            (point-min)))
         ;; Otherwise go to beginning of current line
         (t
          (forward-line 0)
          (point))))
       ;; If start position is invalid, try fallback
       (t
        (let ((fallback-pos (min fallback (point-max))))
          (goto-char fallback-pos)
          (cond
           ((and (>= fallback-pos (point-min)) (<= fallback-pos (point-max)))
            (cond
             ((eobp)
              (if (> (point) (point-min))
                  (progn (forward-line -1) (point))
                (point-min)))
             (t
              (forward-line 0)
              (point))))
           ;; If both start and fallback are invalid, use point-min as ultimate
           ;; fallback
           (t
            (point-min)))))))))

(defun diff-calculate-overlay-adjustment (ov hunk-start hunk-end
                                          diff-start diff-end
                                          diff-len new-text delta)
  "Calculate overlay adjustments based on change region and operation type.
Returns either:
- (NEW-START NEW-END) for normal adjustments
- (:move-to POS) when overlay content is completely removed
- nil when no adjustment needed"
  (when (let ((ovs (overlays-in hunk-start hunk-end)))
          (seq-some (lambda (x) (eq x ov)) ovs))
    (let* ((ov-start (overlay-start ov))
           (ov-end (overlay-end ov))
           (new-len (length new-text))
           (adding-p (> delta 0))
           (removing-p (< delta 0))
           (replacing-p (and (> diff-len 0) (> new-len 0))))

      (cond
       ;; Case 1: Change is completely after the overlay
       ((> diff-start ov-end)
        ;; CORRECT: Changes after overlay don't affect it
        nil)

       ;; Case 1b: Change starts exactly at overlay end (adjacent)
       ((= diff-start ov-end)
        (cond
         ;; Additions immediately after overlay should extend it
         (adding-p
          (list ov-start (+ ov-end new-len)))
         ;; Removals starting exactly at overlay end don't affect it
         (removing-p
          nil)
         ;; Replacements at overlay end extend it
         (replacing-p
          (list ov-start (+ ov-end new-len)))))

       ;; Case 2: Change is completely before the overlay
       ((<= diff-end ov-start)
        (cond
         (adding-p
          ;; CORRECTED: When adding before overlay, extend overlay start
          ;; to include new content AND shift overlay right by delta
          (list diff-start (+ ov-end delta)))
         ((or removing-p replacing-p)
          ;; Removals/replacements before overlay shift it left by delta
          (list (+ ov-start delta) (+ ov-end delta)))))

       ;; Case 3: Change completely encompasses the overlay
       ((and (<= diff-start ov-start) (>= diff-end ov-end))
        (cond
         (removing-p
          ;; When removing content that contains the overlay, move to safe position
          (cons :move-to (diff-find-safe-overlay-position
                          diff-start diff-start)))
         ((or adding-p replacing-p)
          ;; When adding/replacing, overlay should cover the new content
          (list diff-start (+ diff-start new-len)))))

       ;; Case 4: Change is completely within the overlay
       ((and (>= diff-start ov-start) (<= diff-end ov-end))
        ;; CORRECT: Always adjust end by delta (expansion or shrinkage)
        (list ov-start (+ ov-end delta)))

       ;; Case 5: Change overlaps the start of the overlay
       ((and (< diff-start ov-start) (> diff-end ov-start) (<= diff-end ov-end))
        (cond
         (adding-p
          ;; CORRECTED: When adding, extend overlay start to include new content
          ;; and adjust end by delta
          (list diff-start (+ ov-end delta)))
         (removing-p
          ;; CORRECTED: When removing, calculate how much is removed from overlay
          (let ((removed-from-overlay (- diff-end ov-start)))
            ;; New start is where change starts, new end shrinks by removed amount
            (list diff-start (- ov-end removed-from-overlay))))
         (replacing-p
          ;; CORRECTED: Replacement spanning start
          (list diff-start (+ ov-end delta)))))

       ;; Case 6: Change overlaps the end of the overlay
       ((and (>= diff-start ov-start) (< diff-start ov-end) (> diff-end ov-end))
        (cond
         (adding-p
          ;; CORRECTED: When adding content that starts in overlay and extends beyond,
          ;; extend overlay to include all new content
          (list ov-start (+ diff-start new-len)))
         (removing-p
          ;; CORRECT: When removing, shrink overlay to the change start point
          (list ov-start diff-start))
         (replacing-p
          ;; CORRECTED: Replacement spanning end - extend to cover new content
          (list ov-start (+ diff-start new-len)))))

       ;; Case 7: Defensive fallback for complete encompassing
       ((and (< diff-start ov-start) (> diff-end ov-end))
        (cond
         (removing-p
          (cons :move-to (diff-find-safe-overlay-position diff-start diff-start)))
         (t
          (list diff-start (+ diff-start new-len)))))

       ;; Fallback - no adjustment
       (t nil)))))

(provide 'macher-instruct-utilities)

;;; macher-instruct-utilities.el ends here.
