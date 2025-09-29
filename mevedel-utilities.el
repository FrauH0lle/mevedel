;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(eval-when-compile
  (require 'ediff-init))

(defun mevedel--cycle-list-around (element list)
  "Cycle list LIST around ELEMENT.

If ELEMENT is found in LIST, returns a list with ELEMENT as the head and
the rest of the list rotated around it. Otherwise, returns the LIST."
  (if-let* ((element-tail (member element list)))
      (append element-tail
              (cl-loop for elt in list
                       while (not (eq elt element))
                       collect elt))
    list))

(defun mevedel--replace-text (start end text)
  "Replace the text in the region from START to END with TEXT."
  (save-excursion
    (goto-char start)
    (insert text)
    (delete-region (point) (+ (point) (- end start)))))

(defun mevedel--tint (source-color-name tint-color-name &optional intensity)
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
    ;; HACK 2025-09-30: Otherwise tests fail as they are not interactive and I
    ;;   guess then there no colors
    (apply 'color-rgb-to-hex `(,@(if noninteractive
                                     (list 1.0 1.0 1.0)
                                   result)
                               2))))

(defun mevedel--pos-bol-p (pos buffer)
  "Return nil if POS is not a beginning of a line in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (= pos (pos-bol)))))

(defun mevedel--fill-label-string (string &optional prefix-string padding buffer)
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
      (when (and fill-column padding-fill-column)
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

(defun mevedel--apply-face-to-match (regex string face)
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

(defun mevedel--restore-overlay (buffer overlay-start overlay-end properties)
  "Helper function to restore an instruction overlay in BUFFER.

Uses PROPERTIES, OVERLAY-START, and OVERLAY-END to recreate the overlay."
  (let ((new-ov (make-overlay overlay-start overlay-end buffer)))
    (mapc (lambda (prop)
            (overlay-put new-ov prop (plist-get properties prop)))
          properties)
    new-ov))

(defun mevedel--delimiting-markdown-backticks (string)
  "Return a string containing the appropriate code block backticks for STRING."
  (let ((backticks "```"))
    (while (string-match-p backticks string)
      (setq backticks (concat backticks "`")))
    backticks))

(defun mevedel--overlay-region-info (overlay)
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

(defun mevedel--multiline-string-p (str)
  "Check if STR contains multiple lines."
  (string-match-p "\n" str))

(defun mevedel--tag-query-prefix-from-infix (query)
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
                          (user-error "Consecutive binary operators: %s, %s" prev elm))
                         ((not (expressionp prev))
                          (user-error "Binary operator follows operator: %s, %s" prev elm))))
                       ((unary-op-p elm)
                        (cond
                         ((unary-op-p prev)
                          (user-error "Consecutive unary operator: %s" prev)))))
                      (when (and (not (binary-op-p elm)) prev (not (operatorp prev)))
                        (push 'and result))
                      (push elm result)))
                  (cond
                   ((operatorp (car result))
                    (user-error "Operator not followed by any expression: %s" (car result)))
                   ((binary-op-p (car (last result)))
                    (user-error "Binary operator not following any expression: %s" (car (last result)))))
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

(defun mevedel--markdown-enquote (input-string)
  "Add Markdown blockquote to each line in INPUT-STRING."
  (let ((lines (split-string input-string "\n")))
    (mapconcat (lambda (line) (concat "> " line)) lines "\n")))

(defun mevedel--markdown-code-blocks (text)
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

         ;; Step 1: Find maximum possible suffix (prioritize this)
         (max-suffix-len (length (mevedel--string-common-prefix
                                  (list (reverse old-text) (reverse new-text)))))

         ;; Step 2: Ensure suffix doesn't exceed the shorter string
         (max-suffix-len (min max-suffix-len min-len))

         ;; Step 3: Find maximum possible prefix that doesn't overlap with
         ;;   suffix. Calculate prefix only from the beginning up to where
         ;;   suffix starts
         (prefix-max-end (- min-len max-suffix-len))
         (max-prefix-len (if (> prefix-max-end 0)
                             (length (mevedel--string-common-prefix
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


(defun mevedel--diff-apply-buffer-with-ov-adjustment ()
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
                        (overlays (cl-remove-if-not #'mevedel--instruction-p
                                                    (overlays-in (point-min) (point-max))))
                        ;; Get the actual buffer content at hunk positions for
                        ;; comparison
                        (buffer-text-at-hunk (buffer-substring-no-properties hunk-start hunk-end)))

                   ;; If we are modifying a buffer containing `mevedel'
                   ;; overlays, take them into account
                   (if overlays
                       ;; STEP 1: Calculate overlay adjustments based on the
                       ;;   MINIMAL change region.
                       ;; NOTE 2025-09-30: `diff-find-source-location' may
                       ;;   return text with extra context lines that don't
                       ;;   align with hunk-start/hunk-end. We need to trim this
                       ;;   context to work with the actual hunk content.
                       (let* (;; Normalize both strings to use LF only (remove CR)
                              (old-text-normalized (replace-regexp-in-string "\r" "" old-text-full))
                              (new-text-normalized (replace-regexp-in-string "\r" "" new-text-full))
                              ;; Find the correct offset by checking where buffer-text-at-hunk
                              ;; appears in the normalized old text
                              (text-offset (or (cl-loop for i from 0 to (length old-text-normalized)
                                                        when (and (<= (+ i (length buffer-text-at-hunk)) (length old-text-normalized))
                                                                  (string= buffer-text-at-hunk
                                                                           (substring old-text-normalized i (+ i (length buffer-text-at-hunk)))))
                                                        return i)
                                               0))
                              ;; Align both old and new text by removing the
                              ;; same offset (both should have the same leading
                              ;; context) - use normalized versions
                              (old-text-aligned (if (> text-offset 0)
                                                    (substring old-text-normalized text-offset)
                                                  old-text-normalized))
                              (new-text-aligned (if (> text-offset 0)
                                                    (substring new-text-normalized text-offset)
                                                  new-text-normalized))
                              (diff-regions (mevedel--safe-string-diff-regions old-text-aligned new-text-aligned))
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
                           ;; Skip buffer-level overlays and child overlays
                           ;; (children will be processed by their parents)
                           (unless (or (mevedel--instruction-bufferlevel-p ov)
                                       (mevedel--parent-instruction ov))
                             ;; Calculate overlay adjustments based on the
                             ;; change region (returns list of (ov . adjustment) pairs)
                             (let ((adjustments (mevedel--diff-calculate-ov-adjustment
                                                 ov hunk-start hunk-end
                                                 diff-start diff-end
                                                 diff-len new-text delta)))
                               ;; Process each overlay adjustment (parent and children)
                               (dolist (ov-adj adjustments)
                                 (let* ((target-ov (car ov-adj))
                                        (adjustment (cdr ov-adj)))
                                   ;; Store the evaporate property for this overlay
                                   (push (cons target-ov (overlay-get target-ov 'evaporate)) evaporatep)
                                   ;; Ensure the overlay does not disappear when it gets empty
                                   (overlay-put target-ov 'evaporate nil)
                                   ;; Store adjustment info
                                   (when adjustment
                                     (let ((ov-start-bolp (save-excursion
                                                            (goto-char (overlay-start target-ov))
                                                            (bolp)))
                                           (ov-end-bolp (save-excursion
                                                          (goto-char (overlay-end target-ov))
                                                          (bolp))))
                                       (push (list target-ov adjustment ov-start-bolp ov-end-bolp)
                                             overlay-adjustments))))))))

                         ;; STEP 2: Modify the buffer using the MINIMAL change
                         ;; region.
                         (mevedel--replace-text diff-start diff-end new-text)

                         ;; STEP 3: Apply the calculated overlay adjustments.
                         (dolist (adjustment overlay-adjustments)
                           (let ((ov (nth 0 adjustment))
                                 (action (nth 1 adjustment))
                                 (ov-start-bolp (nth 2 adjustment))
                                 (ov-end-bolp (nth 3 adjustment)))
                             (pcase action
                               (`(:delete . t)
                                ;; Delete child overlay that can't fit in parent
                                (mevedel--delete-instruction ov))
                               (`(:move-to . ,pos)
                                ;; Move overlay to a safe position when its content is removed
                                ;; Ensure the position is valid and the overlay spans at least 1 char
                                (let* ((buffer-end (point-max))
                                       (safe-pos (max (point-min) (min pos buffer-end)))
                                       (safe-end (min buffer-end (1+ safe-pos))))
                                  ;; Only move if we have a valid range
                                  (when (< safe-pos safe-end)
                                    (move-overlay ov safe-pos safe-end)
                                    (mevedel--update-instruction-overlay ov t))))
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
                                              (mevedel--overlay-region-info ov)
                                            (string-blank-p ov-text))
                                      (move-overlay ov safe-start (1+ safe-start)))
                                    (mevedel--update-instruction-overlay ov t))
                                  ;; If the overlay would be invalid, move to safe position
                                  (when (>= safe-start safe-end)
                                    (let ((fallback-pos (mevedel--diff-find-safe-ov-position
                                                         safe-start diff-start)))
                                      (move-overlay ov fallback-pos
                                                    (min (1+ fallback-pos) (point-max)))
                                      (mevedel--update-instruction-overlay ov t))))))))
                         ;; Restore evaporate properties
                         (cl-loop for (ov . evaporate) in evaporatep
                                  do (overlay-put ov 'evaporate evaporate)))

                     ;; Otherwise apply the changes normally
                     (mevedel--replace-text hunk-start hunk-end new-text-full)))
                 (save-buffer))))
           (message "Saved %d buffers with overlay adjustments." (length buffer-edits)))
          (t
           (message "%d hunks failed; no buffers changed." failures)))))

(defun mevedel--diff-find-safe-ov-position (start fallback)
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
          (beginning-of-line)
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
              (beginning-of-line)
              (point))))
           ;; If both start and fallback are invalid, use point-min as ultimate
           ;; fallback
           (t
            (point-min)))))))))

(defun mevedel--diff-calculate-ov-adjustment-single (ov hunk-start hunk-end
                                                    diff-start diff-end
                                                    diff-len new-text delta)
  "Calculate single overlay adjustment based on change region and operation type.
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
        ;; Changes after overlay don't affect it
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
          ;; When adding before overlay, extend overlay start to include new
          ;; content AND shift overlay right by delta
          (list diff-start (+ ov-end delta)))
         ((or removing-p replacing-p)
          ;; Removals/replacements before overlay shift it left by delta
          (list (+ ov-start delta) (+ ov-end delta)))))

       ;; Case 3: Change completely encompasses the overlay
       ((and (<= diff-start ov-start) (>= diff-end ov-end))
        (cond
         (replacing-p
          ;; When replacing (both old and new content exist), overlay should cover the new content
          (list diff-start (+ diff-start new-len)))
         (adding-p
          ;; When adding, overlay should cover the new content
          (list diff-start (+ diff-start new-len)))
         (removing-p
          ;; When removing content that contains the overlay, move to safe position
          (cons :move-to (mevedel--diff-find-safe-ov-position
                          diff-start diff-start)))))

       ;; Case 4: Change is completely within the overlay
       ((and (>= diff-start ov-start) (<= diff-end ov-end))
        ;; Always adjust end by delta (expansion or shrinkage)
        (list ov-start (+ ov-end delta)))

       ;; Case 5: Change overlaps the start of the overlay
       ((and (< diff-start ov-start) (> diff-end ov-start) (<= diff-end ov-end))
        (cond
         (adding-p
          ;; When adding, extend overlay start to include new content and adjust
          ;; end by delta
          (list diff-start (+ ov-end delta)))
         (removing-p
          ;; When removing, calculate how much is removed from overlay
          (let ((removed-from-overlay (- diff-end ov-start)))
            ;; New start is where change starts, new end shrinks by removed amount
            (list diff-start (- ov-end removed-from-overlay))))
         (replacing-p
          ;; Replacement spanning start
          (list diff-start (+ ov-end delta)))))

       ;; Case 6: Change overlaps the end of the overlay
       ((and (>= diff-start ov-start) (< diff-start ov-end) (> diff-end ov-end))
        (cond
         (adding-p
          ;; When adding content that starts in overlay and extends beyond,
          ;; extend overlay to include all new content
          (list ov-start (+ diff-start new-len)))
         (removing-p
          ;; When removing, shrink overlay to the change start point
          (list ov-start diff-start))
         (replacing-p
          ;; Replacement spanning end - extend to cover new content
          (list ov-start (+ diff-start new-len)))))

       ;; Fallback - no adjustment
       (t
        (warn "mevedel: Unexpected fallback case in `mevedel--diff-calculate-ov-adjustment reached'.\
 This may indicate a bug. ov=%s-%s, hunk=%s-%s, diff=%s-%s"
              ov-start ov-end hunk-start hunk-end diff-start diff-end)
        nil)))))

(defun mevedel--diff-calculate-ov-adjustment (ov hunk-start hunk-end
                                             diff-start diff-end
                                             diff-len new-text delta)
  "Calculate overlay adjustments for OV and its children.
Returns a list of adjustments: ((OV . ADJUSTMENT) (CHILD1 . ADJUSTMENT) ...)
Each ADJUSTMENT is either:
- (NEW-START NEW-END) for normal adjustments
- (:move-to POS) when overlay content is completely removed
- (:delete . t) when overlay should be deleted
- nil when no adjustment needed"
  (let* ((parent-adj (mevedel--diff-calculate-ov-adjustment-single
                      ov hunk-start hunk-end
                      diff-start diff-end
                      diff-len new-text delta))
         (children (mevedel--child-instructions ov))
         (result (list (cons ov parent-adj))))

    ;; Process children if parent has an adjustment and has children
    (when (and parent-adj children)
      (let ((new-parent-start nil)
            (new-parent-end nil))

        ;; Determine new parent bounds based on adjustment type
        (pcase parent-adj
          (`(:move-to ,pos)
           ;; Parent moved to safe position - children can't fit
           (setq new-parent-start pos)
           (setq new-parent-end (1+ pos)))
          (`(,start ,end)
           ;; Normal adjustment
           (setq new-parent-start start)
           (setq new-parent-end end))
          (_ nil))

        ;; Process each child
        (when (and new-parent-start new-parent-end)
          (let ((parent-size (- new-parent-end new-parent-start)))
            (dolist (child children)
              (let ((child-adj (mevedel--diff-calculate-ov-adjustment-single
                                child hunk-start hunk-end
                                diff-start diff-end
                                diff-len new-text delta)))
                (if child-adj
                    (pcase child-adj
                      (`(:move-to ,_)
                       ;; Child would be moved to safe position
                       ;; If parent is also at safe position, nest child within parent
                       (pcase parent-adj
                         (`(:move-to ,_)
                          ;; Both parent and child encompassed - nest child in parent
                          ;; Make child span within parent bounds (but smaller than parent)
                          (if (> parent-size 1)
                              ;; Parent has room - put child inside (1 char smaller)
                              (push (cons child (list new-parent-start (max new-parent-start (1- new-parent-end)))) result)
                            ;; Parent too small - delete child
                            (push (cons child '(:delete . t)) result)))
                         (_
                          ;; Parent not moved, but child is - delete child
                          (push (cons child '(:delete . t)) result))))
                      (`(,child-start ,child-end)
                       ;; Check if child fits within new parent bounds
                       (if (and (>= child-start new-parent-start)
                                (<= child-end new-parent-end)
                                (< child-start child-end)
                                ;; Child must not have exact same bounds as parent
                                (not (and (= child-start new-parent-start)
                                          (= child-end new-parent-end))))
                           ;; Child fits - include adjustment
                           (push (cons child child-adj) result)
                         ;; Child doesn't fit - adjust to fit within parent
                         ;; Make child smaller than parent by 1 char
                         (let ((adjusted-end (max new-parent-start (1- new-parent-end))))
                           (push (cons child (list new-parent-start adjusted-end)) result))))
                      (_
                       ;; Unknown adjustment type - keep child as is
                       nil))
                  ;; No adjustment needed for child - it stays as is
                  nil)))))))

    ;; Return list of adjustments (parent first, then children)
    (nreverse result)))


;;; Ediff

(defvar mevedel--old-patch-buffer-name " *mevedel-original-patch*"
  "Name of the buffer storing the original patch.")
(defvar mevedel--new-patch-buffer-name " *mevedel-modified-patch*"
  "Name of the buffer storing the modified patch.")
(defvar mevedel--ediff-custom-diff-buffer " *mevedel-ediff-custom-diff*"
  "Name of the buffer storing the current patch.")

(defvar mevedel--original-patch-string nil
  "String containing the original patch content before `ediff' session.")
(defvar mevedel--ediff-in-progress-p nil
  "Non-nil when a `ediff' patch editing session is in progress.")
(defvar mevedel--ediff-saved-wconf nil
  "Non-nil when a `ediff' patch editing session is in progress.")

(defun mevedel--cleanup-ediff-session ()
  "Clean up after an ediff patch editing session.

Resets state variables, restores window configuration, removes ediff
hooks, and kills temporary patch buffers."
  (setq mevedel--ediff-in-progress-p nil)
  (when (window-configuration-p mevedel--ediff-saved-wconf)
    (set-window-configuration mevedel--ediff-saved-wconf))
  (setq mevedel--ediff-saved-wconf nil)
  (remove-hook 'ediff-quit-session-group-hook #'mevedel--cleanup-ediff-session)
  (remove-hook 'ediff-quit-hook #'mevedel--cleanup-ediff-session)
  (remove-hook 'ediff-startup-hook #'mevedel--store-old-ediff-patch)
  (remove-hook 'ediff-startup-hook #'mevedel--setup-ediff-session)
  (remove-hook 'ediff-quit-hook #'mevedel--create-patch-from-ediff)
  (dolist (buf (list mevedel--old-patch-buffer-name
                     mevedel--new-patch-buffer-name
                     mevedel--ediff-custom-diff-buffer))
    (kill-buffer buf)))

(defun mevedel--setup-ediff-session ()
  "Set up the ediff session by moving to the first difference.

This function is called during ediff startup to ensure the session
begins with the cursor positioned at the first detected difference
between the files being compared."
  ;; Move to the first difference to start the ediff session
  (ediff-next-difference))

(defun mevedel-ediff-patch ()
  "Start an ediff session to review and modify the current patch.

This function retrieves the patch buffer from the current workspace,
saves the current window configuration, and launches an ediff session
for interactive patch editing. It sets up necessary hooks to handle
patch creation, cleanup, and session management."
  (interactive)
  (let ((patch-buf (macher-patch-buffer (macher-workspace))))
    ;; Ensure we have a patch buffer to work with
    (unless patch-buf
      (user-error "No patch buffer found"))

    ;; Save current window configuration for later restoration
    (setq mevedel--ediff-saved-wconf (current-window-configuration))

    (with-current-buffer patch-buf
      (goto-char (point-min))
      ;; From `ediff-patch-file'
      ;; Initialize patch processing based on ediff-patch-file logic
      (let (source-dir source-file)
        (require 'ediff-ptch)

        ;; Get the proper patch buffer for ediff processing
        (setq patch-buf
              (ediff-get-patch-buffer
               nil
               (and patch-buf (get-buffer patch-buf))))

        ;; Determine the source directory from the patch or fallback to
        ;; workspace root
        (setq source-dir (if-let* ((dir (file-name-directory
                                         (diff-filename-drop-dir (car (diff-hunk-file-names t))))))
                             (expand-file-name dir (macher--workspace-root (macher-workspace)))
                           (macher--workspace-root (macher-workspace))))

        ;; Construct the source file path
        (setq source-file
              (file-name-concat source-dir (file-name-nondirectory (diff-find-file-name t t))))

        (ediff-with-current-buffer patch-buf
          ;; Set up cleanup hooks based on whether we have single or multiple
          ;; patches
          (if (< (length ediff-patch-map) 2)
              (add-hook 'ediff-quit-hook #'mevedel--cleanup-ediff-session 99)
            (add-hook 'ediff-quit-session-group-hook #'mevedel--cleanup-ediff-session 99)))

        ;; Set up startup hooks for patch storage and session setup
        (add-hook 'ediff-startup-hook #'mevedel--store-old-ediff-patch)
        (add-hook 'ediff-startup-hook #'mevedel--setup-ediff-session)

        ;; Set up quit hook to create updated patch from ediff changes
        (add-hook 'ediff-quit-hook #'mevedel--create-patch-from-ediff)

        ;; Mark ediff session as in progress and start the patching job
        (setq mevedel--ediff-in-progress-p t)
        (ediff-dispatch-file-patching-job patch-buf source-file)))))

(defun mevedel--create-patch-from-ediff ()
  "Create and apply an updated patch from an ediff session.

This function is called as part of the ediff-quit-hook to generate a new
patch based on changes made during the ediff session and update the
original patch file with the new content."
  (when mevedel--ediff-in-progress-p
    (let* ((new-patch-buf (get-buffer-create mevedel--new-patch-buffer-name t))
           (file-a (buffer-file-name ediff-buffer-A))
           (file-b (buffer-file-name ediff-buffer-B))
           (patch-buffer (macher-patch-buffer (macher-workspace))))

      ;; Generate the new patch content based on ediff changes
      (mevedel--create-ediff-custom-patch new-patch-buf)

      ;; Update the main patch buffer by replacing the original patch content
      ;; with the newly generated patch from ediff
      (when (and patch-buffer
                 (buffer-live-p patch-buffer)
                 mevedel--original-patch-string)
        (with-current-buffer patch-buffer
          (let ((inhibit-read-only t)
                (new-content (with-current-buffer new-patch-buf
                               (string-trim
                                (buffer-substring-no-properties (point-min) (point-max))))))
            (save-excursion
              (goto-char (point-min))
              ;; Locate and replace the original patch string with new content
              (when (search-forward mevedel--original-patch-string nil t)
                (replace-match new-content t t)
                (message "Patch updated in %s" (buffer-name patch-buffer)))))))

      ;; Finalize the ediff session by removing read-only protection and
      ;; restoring the original file with the modified version
      (with-current-buffer ediff-buffer-A
        (read-only-mode -1)
        (rename-file file-a file-b t)
        (set-visited-file-name file-b t t))

      ;; Clean up buffer names: Ediff creates unique buffer names by suffixing
      ;; the original buffer (B) with <2>. We remove the duplicate buffer and
      ;; restore the original name
      (let ((orig-buffer-name (buffer-name ediff-buffer-B)))
        (kill-buffer ediff-buffer-B)
        (with-current-buffer ediff-buffer-A
          (rename-buffer orig-buffer-name))))))

(defun mevedel--store-old-ediff-patch ()
  "Store the original patch state before starting an ediff session.

This captures the current diff as a string to allow restoration later if
needed during the ediff process."
  (when mevedel--ediff-in-progress-p
    ;; Create or get the buffer for storing the old patch
    (let* ((old-patch-buf (get-buffer-create mevedel--old-patch-buffer-name t)))
      ;; Generate the custom patch content and store it as a trimmed string
      (setq mevedel--original-patch-string
            (with-current-buffer (mevedel--create-ediff-custom-patch old-patch-buf)
              (string-trim
               (buffer-substring-no-properties (point-min) (point-max))))))))

(defun mevedel--create-ediff-custom-patch (buffer)
  "Create a custom unified diff patch from an active ediff session.

The patch is generated in BUFFER and formatted to match git's diff
format with proper a/ and b/ path prefixes for the workspace root
directory."
  (let* (;; Get the workspace root directory for relative path calculations
         (base-dir (macher--workspace-root (macher-workspace)))
         ;; Get file paths for both ediff buffers
         (file-a (buffer-file-name ediff-buffer-A))
         (file-b (buffer-file-name ediff-buffer-B))
         ;; Remove backup extensions from file paths for clean diff display
         (file-a-no-backup-ext (string-remove-suffix ediff-backup-extension file-a))
         (file-b-no-backup-ext (string-remove-suffix ediff-backup-extension file-b))
         ;; Create buffer for storing custom diff output
         (ediff-custom-diff-buffer (get-buffer-create mevedel--ediff-custom-diff-buffer t))
         ;; Build diff options with proper labels and relative paths
         (ediff-custom-diff-options (concat "-c" " --label"
                                            ;; Use /dev/null for empty buffers,
                                            ;; otherwise use relative path
                                            (if (string-empty-p
                                                 (with-current-buffer ediff-buffer-A
                                                   (buffer-substring-no-properties (point-min) (point-max))))
                                                " /dev/null"
                                              (concat " a/" (file-relative-name file-a-no-backup-ext base-dir)))

                                            " --label"
                                            ;; Use /dev/null for empty buffers,
                                            ;; otherwise use relative path
                                            (if (string-empty-p
                                                 (with-current-buffer ediff-buffer-B
                                                   (buffer-substring-no-properties (point-min) (point-max))))
                                                " /dev/null"
                                              (concat " b/" (file-relative-name file-b-no-backup-ext base-dir))))))

    ;; Ensure we're operating within an ediff control buffer context
    (ediff-barf-if-not-control-buffer)
    ;; Ensure custom diffs are computed and available
    (ediff-compute-custom-diffs-maybe)

    (with-current-buffer buffer
      ;; Clear the buffer to prepare for new patch content
      (erase-buffer)
      ;; Insert standard git diff header with relative file paths
      (insert (format "diff --git a/%s b/%s\n"
                      (file-relative-name file-a-no-backup-ext base-dir)
                      (file-relative-name file-b-no-backup-ext base-dir)))
      ;; Insert and convert diff content from context format to unified format
      (insert (with-current-buffer ediff-custom-diff-buffer
                (diff-context->unified (point-min) (point-max))
                (buffer-substring-no-properties (point-min) (point-max))))
      ;; Normalize file paths in the diff output to ensure git-compatible
      ;; format. This step ensures consistency even if the diff command
      ;; generates different paths
      (goto-char (point-min))
      ;; Replace the --- line to use git's a/ prefix format
      (when (re-search-forward (concat "^--- " (regexp-quote file-a)) nil t)
        (replace-match (concat "--- a/" (file-relative-name file-a-no-backup-ext base-dir))))
      ;; Replace the +++ line to use git's b/ prefix format
      (goto-char (point-min))
      (when (re-search-forward (concat "^\\+\\+\\+ " (regexp-quote file-b)) nil t)
        (replace-match (concat "+++ b/" (file-relative-name file-b-no-backup-ext base-dir)))))
    ;; Return the buffer containing the formatted patch
    buffer))

(provide 'mevedel-utilities)

;;; mevedel-utilities.el ends here.
