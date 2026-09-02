;;; mevedel-view-table.el --- Rendered Markdown tables -*- lexical-binding: t -*-

;; Portions adapted from agent-shell-markdown.el,
;; Copyright (C) Alvaro Ramirez, https://github.com/xenodium/agent-shell,
;; distributed under GPL-3.0-or-later.

;;; Commentary:

;; Renders canonical Markdown pipe tables in mevedel views as aligned
;; box-drawing rows.  Columns wider than the usable window width are
;; shrunk proportionally toward their longest-word minima and their
;; cells wrapped.  Each rendered table retains its raw Markdown source
;; and the window pixel width it was laid out for as text properties,
;; so `mevedel-view-table-rerender' can rebuild only stale tables after
;; a window change.  The data buffer is never touched: everything here
;; rewrites the disposable view projection only.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-utilities)
(require 'text-property-search)

;; `mevedel-view-markdown'
(declare-function mevedel-view--markdown-source "mevedel-view-markdown"
                  (start end))
(declare-function mevedel-view--position-in-ranges-p "mevedel-view-markdown"
                  (position ranges))
(declare-function mevedel-view--render-markdown-url-links-in-range
                  "mevedel-view-markdown" (start end))
(declare-function mevedel-view--selected-text-properties
                  "mevedel-view-markdown" (position properties))

(defface mevedel-view-table-header
  '((t :weight bold))
  "Face for rendered Markdown table header rows."
  :group 'mevedel)

(defface mevedel-view-table-border
  '((t :inherit shadow))
  "Face for rendered Markdown table borders."
  :group 'mevedel)

(defcustom mevedel-view-table-zebra-intensity 0.09
  "How far the table stripe is blended from the background to the foreground.
Unlike a muted foreground this can be a constant: the blend direction
flips with the theme, so one value lands between 1.08:1 and 1.34:1
against the background on every theme measured, light or dark."
  :type 'float
  :group 'mevedel)

(defface mevedel-view-table-zebra
  '((t :extend t))
  "Face for alternating rendered Markdown table data rows.
The background is blended from the active theme by
`mevedel--derive-theme-faces'; a fixed grey is either invisible against
a near-black theme or wrong for a light one.  `:extend' keeps the stripe
running to the window edge instead of stopping at the last cell."
  :group 'mevedel)

(mevedel--derive-theme-face
 'mevedel-view-table-zebra
 (lambda (foreground background)
   (list :background
         (mevedel--tint background foreground
                        mevedel-view-table-zebra-intensity))))

(defconst mevedel-view-table--max-width-fraction 0.9
  "Fraction of the usable window width a rendered table may occupy.")

(defconst mevedel-view-table--line-regexp
  "^[ \t]*|[^\n]*|[ \t]*$"
  "Regexp matching one Markdown pipe-table row.")

(defconst mevedel-view-table--separator-regexp
  "^[ \t]*|[-:| \t]+|[ \t]*$"
  "Regexp matching a table separator row such as `|---|---|'.")

(defconst mevedel-view-table--carried-properties
  '(mevedel-view-source
    mevedel-view-source-key
    mevedel-view-type
    mevedel-view-collapsed
    mevedel-view-turn-id
    read-only
    keymap
    front-sticky
    rear-nonsticky
    line-prefix
    wrap-prefix)
  "Text properties carried from a table's source onto its rendering.")


;;
;;; Table discovery

(defun mevedel-view-table--row-blocked-p (pos avoid-ranges)
  "Return non-nil when the table row at POS must stay raw."
  (or (get-text-property pos 'mevedel-view-no-linkify)
      (mevedel-view--position-in-ranges-p pos avoid-ranges)))

(defun mevedel-view-table--find-tables (start end avoid-ranges)
  "Return pipe tables between START and END outside AVOID-RANGES.
Each element is a (START . END) range covering two or more consecutive
pipe rows, in reverse buffer order."
  (let ((tables nil)
        (inhibit-field-text-motion t))
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (while (< (point) end)
        (if (and (looking-at mevedel-view-table--line-regexp)
                 (not (mevedel-view-table--row-blocked-p
                       (point) avoid-ranges)))
            (let ((table-start (point))
                  (table-end nil)
                  (rows 0))
              (while (and (< (point) end)
                          (looking-at mevedel-view-table--line-regexp)
                          (not (mevedel-view-table--row-blocked-p
                                (point) avoid-ranges)))
                (setq table-end (line-end-position)
                      rows (1+ rows))
                (forward-line 1))
              (when (and (>= rows 2) (<= table-end end))
                (push (cons table-start table-end) tables)))
          (forward-line 1))))
    tables))


;;
;;; Row parsing

(defconst mevedel-view-table--raw-faces
  '(markdown-ts-table markdown-ts-table-header markdown-ts-table-cell
    markdown-ts-table-delimiter-cell)
  "Faces `markdown-ts-mode\=' puts on raw table source.
The renderer replaces pipe tables with its own box-drawing rows, so the
source fontification must not survive into the projection.")

(defun mevedel-view-table--raw-face-p (face)
  "Return non-nil when FACE is raw table fontification."
  (memq face mevedel-view-table--raw-faces))

(defun mevedel-view-table--strip-table-faces (start end &optional object)
  "Remove raw table fontification faces between START and END in OBJECT.
OBJECT is a string or nil for the current buffer."
  (dolist (prop '(face font-lock-face))
    (let ((pos start))
      (while (< pos end)
        (let* ((next (or (next-single-property-change pos prop object end)
                         end))
               (value (get-text-property pos prop object))
               (kept (cond
                      ((null value) value)
                      ((and (listp value) (not (keywordp (car value))))
                       (seq-remove #'mevedel-view-table--raw-face-p value))
                      ((mevedel-view-table--raw-face-p value) nil)
                      (t value))))
          (unless (equal kept value)
            (if kept
                (put-text-property pos next prop
                                   (if (and (listp kept) (null (cdr kept)))
                                       (car kept)
                                     kept)
                                   object)
              (remove-text-properties pos next (list prop nil) object)))
          (setq pos next))))))

(defun mevedel-view-table--unescape-cell (content)
  "Return CONTENT with escaped pipes outside code reduced to pipes.
Backslashes inside matched code spans and other backslash sequences
are left as written.  `display' properties are dropped so an inlined
image inside a cell measures as its text, and the image realignment
bookkeeping is dropped with them so the image pass never stamps a
display spec into a rendered row."
  (let ((result
         (with-temp-buffer
           (let ((inhibit-read-only t))
             (insert content)
             (goto-char (point-min))
             (while (re-search-forward "[\\\\`]" nil t)
               (if (eq (char-before) ?`)
                   (progn
                     (goto-char (match-beginning 0))
                     (mevedel-view-table--skip-code-span (point-max)))
                 (if (eq (char-after) ?|)
                     (delete-region (1- (point)) (point))
                   (when (not (eobp))
                     (forward-char 1))))))
           (buffer-string))))
    (when (> (length result) 0)
      (remove-text-properties 0 (length result)
                              '(display nil
                                mevedel-view-image-source nil
                                mevedel-view-image-ratio nil
                                mevedel-view-image-width nil)
                              result)
      (mevedel-view-table--strip-table-faces 0 (length result) result))
    result))

(defun mevedel-view-table--skip-code-span (end)
  "Move past a matching inline-code span starting at point before END.
Return non-nil when a matching backtick run was found.  On failure,
leave point after the opening run."
  (let ((start (point)))
    (skip-chars-forward "`" end)
    (let ((ticks (- (point) start))
          (after-open (point)))
      (catch 'matched
        (while (re-search-forward "`+" end t)
          (when (= (- (match-end 0) (match-beginning 0)) ticks)
            (throw 'matched t)))
        (goto-char after-open)
        nil))))

(defun mevedel-view-table--parse-row (start end)
  "Parse the table row between START and END into trimmed cell strings.
Cell strings keep the buffer text's properties.  A `|' preceded by a
backslash or inside a matched backtick span is literal cell content,
not a delimiter."
  (let ((cells nil))
    (save-excursion
      (goto-char start)
      (when (looking-at "[ \t]*|")
        (goto-char (match-end 0)))
      (let ((cell-start (point)))
        (while (< (point) end)
          (if (re-search-forward "[|\\\\`]" end t)
              (let ((ch (char-before))
                    (delim-pos (1- (point))))
                (cond
                 ((eq ch ?|)
                  (push (mevedel-view-table--unescape-cell
                         (string-trim
                          (buffer-substring cell-start delim-pos)))
                        cells)
                 (setq cell-start (point)))
                 ((eq ch ?\\)
                  (when (< (point) end)
                    (forward-char 1)))
                 ((eq ch ?`)
                  (goto-char delim-pos)
                  (mevedel-view-table--skip-code-span end))))
            (goto-char end)))))
    (nreverse cells)))

(defun mevedel-view-table--collect-rows ()
  "Collect table rows in the current (temporary) buffer.
Each row is a plist with :start, :end, :num, and :separator."
  (save-excursion
    (goto-char (point-min))
    (let ((rows nil)
          (row-num 0))
      (while (and (not (eobp))
                  (looking-at mevedel-view-table--line-regexp))
        (push (list :start (point)
                    :end (line-end-position)
                    :num row-num
                    :separator (and (looking-at
                                     mevedel-view-table--separator-regexp)
                                    t))
              rows)
        (setq row-num (1+ row-num))
        (forward-line 1))
      (nreverse rows))))


;;
;;; Width measurement

(defvar-local mevedel-view-table--char-pixel-cache nil
  "Cons of (FONT-WIDTH . SPACE-PIXELS) caching one space's pixel width.
Lives in the destination buffer; invalidated when the font width
changes, as under text scaling.")

(defun mevedel-view-table--pixel-capable-p (window)
  "Return non-nil when WINDOW supports pixel-accurate measurement."
  (and window
       (window-live-p window)
       (display-graphic-p (window-frame window))))

(defun mevedel-view-table--measure-string (str window)
  "Return the pixel width of STR as WINDOW renders it.
Measured in a temporary buffer shown briefly in WINDOW so its frame
font applies; `face-remapping-alist' is copied from WINDOW's buffer so
text scaling measures at its scaled width."
  ;; ponytail: one temp-buffer measurement per call, uncached; add a
  ;; string->pixels cache (equal-including-properties keyed) if faced
  ;; or non-ASCII tables stutter during streaming re-renders.
  (let ((remapping (buffer-local-value 'face-remapping-alist
                                       (window-buffer window))))
    (with-temp-buffer
      (setq-local display-line-numbers nil
                  line-prefix nil
                  wrap-prefix nil
                  face-remapping-alist remapping)
      (insert str)
      (remove-text-properties (point-min) (point-max)
                              '(line-prefix nil wrap-prefix nil display nil))
      (car (buffer-text-pixel-size nil window t)))))

(defun mevedel-view-table--char-pixel-width (window)
  "Return the pixel width of one space in WINDOW, cached."
  (with-current-buffer (window-buffer window)
    (let ((fw (window-font-width window)))
      (if (and mevedel-view-table--char-pixel-cache
               (= fw (car mevedel-view-table--char-pixel-cache)))
          (cdr mevedel-view-table--char-pixel-cache)
        (let ((sw (mevedel-view-table--measure-string " " window)))
          (setq mevedel-view-table--char-pixel-cache (cons fw sw))
          sw)))))

(defun mevedel-view-table--string-faced-p (str)
  "Return non-nil when STR carries any face or font-lock-face property."
  (or (text-property-not-all 0 (length str) 'face nil str)
      (text-property-not-all 0 (length str) 'font-lock-face nil str)))

(defun mevedel-view-table--pixel-width-needed-p (str window)
  "Return non-nil when STR needs pixel measurement in WINDOW."
  (and (mevedel-view-table--pixel-capable-p window)
       (or (assq 'default
                 (buffer-local-value 'face-remapping-alist
                                     (window-buffer window)))
           (not (string-match-p "\\`[[:ascii:]]*\\'" str))
           (mevedel-view-table--string-faced-p str))))

(defun mevedel-view-table--display-width (str window)
  "Return the display width of STR in character columns.
Plain ASCII uses `string-width'.  Non-ASCII or faced content is
pixel-measured against WINDOW when possible so columns line up under
variable-pitch and mixed-glyph content; without a graphic WINDOW the
`string-width' path is the complete fallback."
  (if (mevedel-view-table--pixel-width-needed-p str window)
      (condition-case nil
          (let ((char-px (mevedel-view-table--char-pixel-width window))
                (real-px (mevedel-view-table--measure-string str window)))
            (ceiling (/ (float real-px) char-px)))
        (error (string-width str)))
    (string-width str)))

(defun mevedel-view-table--longest-word (str window)
  "Return the display width of the longest unbreakable unit in STR.
Line-breakable characters (CJK ideographs, kana, Hangul) can wrap
anywhere, so each contributes only its own display width."
  (if (or (null str) (string-empty-p str))
      0
    (let ((len (length str))
          (longest 0)
          (word-start nil))
      (dotimes (i (1+ len))
        (let* ((ch (and (< i len) (aref str i)))
               (separator (or (null ch) (memq ch '(?\s ?\t ?\n))))
               (breakable (and (not separator)
                               (aref (char-category-set ch) ?|))))
          (when (and word-start (or separator breakable))
            (setq longest (max longest
                               (mevedel-view-table--display-width
                                (substring str word-start i) window)))
            (setq word-start nil))
          (cond
           (breakable
            (setq longest
                  (max longest
                       (mevedel-view-table--display-width
                        (substring str i (1+ i)) window))))
           ((and (not separator) (not word-start))
            (setq word-start i)))))
      longest)))

(defun mevedel-view-table--total-width (widths)
  "Return the total rendered width for column WIDTHS.
Each column adds two padding spaces and one border pipe, plus one
leading pipe."
  (+ 1 (seq-reduce (lambda (acc w) (+ acc w 3)) widths 0)))

(defun mevedel-view-table--allocate-widths (natural-widths min-widths target)
  "Shrink NATURAL-WIDTHS proportionally to fit TARGET, respecting MIN-WIDTHS.
MIN-WIDTHS holds each column's longest unbreakable word.  When even
those minima cannot fit TARGET, columns shrink down to one column each
and the cell wrapper hard-breaks long words."
  (let* ((total (mevedel-view-table--total-width natural-widths))
         (excess (- total target))
         (floors (if (> (mevedel-view-table--total-width min-widths) target)
                     (make-list (length min-widths) 1)
                   min-widths)))
    (if (<= excess 0)
        natural-widths
      (let* ((shrinkable (seq-mapn (lambda (w m) (max 0 (- w m)))
                                   natural-widths floors))
             (total-shrinkable (seq-reduce #'+ shrinkable 0)))
        (if (<= total-shrinkable 0)
            floors
          (let ((ratio (min 1.0 (/ (float excess) total-shrinkable))))
            (seq-mapn (lambda (w m s)
                        (max m (floor (- w (* s ratio)))))
                      natural-widths floors shrinkable)))))))


;;
;;; Cell wrapping

(defun mevedel-view-table--wrap-char-width
    (text pos &optional window char-px)
  "Return the display width contribution of the char at POS in TEXT.
U+FE0F VARIATION SELECTOR-16 counts as 1 so an emoji presentation
sequence totals its rendered two cells.  In a graphic WINDOW, faced
text and a remapped default face use their actual pixel width.
CHAR-PX is the window's space width when the caller already measured
it, which keeps that measurement out of a per-character loop."
  (let ((ch (aref text pos)))
    (cond
     ((= ch #xFE0F) 1)
     (t
      (let ((single (substring text pos (1+ pos))))
        (if (not (mevedel-view-table--pixel-width-needed-p single window))
            (char-width ch)
          (condition-case nil
              (/ (float (mevedel-view-table--measure-string single window))
                 (or char-px
                     (mevedel-view-table--char-pixel-width window)))
            (error (char-width ch)))))))))

(defun mevedel-view-table--char-widths (text &optional window)
  "Return a vector of per-character display widths for TEXT.

Pure ASCII text with no face and no remapped default face needs no
measurement at all, so it takes a path that allocates nothing per
character.  Anything else is measured once per character here rather
than on each visit: the wrapping loop asks for a character's width
twice, and measuring in place turned one table redraw into hundreds of
megabytes of substrings and `window-font-width' calls."
  (let* ((len (length text))
         (widths (make-vector len 0)))
    ;; Callers may hand over nil or empty text; asking the predicate about
    ;; it would fail where the old per-character loop simply did nothing.
    (if (or (zerop len)
            (not (mevedel-view-table--pixel-width-needed-p text window)))
        (dotimes (index len)
          (aset widths index (char-width (aref text index))))
      ;; Measuring can fail -- no graphic window, no live buffer -- and
      ;; the per-character path treated that as "use `char-width'".
      ;; Hoisting the measurement has to keep that tolerance, or one
      ;; failure escapes where it used to be absorbed per character.
      (let ((char-px (condition-case nil
                         (mevedel-view-table--char-pixel-width window)
                       (error nil))))
        (dotimes (index len)
          (aset widths index
                (if char-px
                    (mevedel-view-table--wrap-char-width
                     text index window char-px)
                  (char-width (aref text index)))))))
    widths))

(defun mevedel-view-table--break-after-p (text i)
  "Return non-nil when a wrapped line may break after index I in TEXT.
I + 1 must be a valid index.  Breaks are allowed after a line-breakable
character unless the next character is zero-width and must stay
attached."
  (and (aref (char-category-set (aref text i)) ?|)
       (> (char-width (aref text (1+ i))) 0)))

(defun mevedel-view-table--wrap-text (text width &optional window)
  "Wrap TEXT to fit WIDTH columns, returning a list of lines.
Text properties are preserved across wrapped lines.  Breaks happen
after whitespace or a line-breakable character; a run with no break
point splits at the width limit.  WINDOW supplies pixel metrics when
the text or the buffer's default face needs them."
  (cond
   ((or (null text) (string-empty-p text)) (list ""))
   (t
    ;; One width vector for both the fits-on-one-line test and the
    ;; wrapping loop: measuring in place asked for each character's width
    ;; three times, once for the test and twice per loop visit.
    (let* ((widths (mevedel-view-table--char-widths text window))
           (total (seq-reduce #'+ widths 0)))
      (if (<= total (- width (seq-count (lambda (c) (= c #xFE0F)) text)))
          (list text)
        (mevedel-view-table--wrap-with-widths text width widths))))))

(defun mevedel-view-table--wrap-with-widths (text width widths)
  "Wrap TEXT to WIDTH columns using precomputed per-character WIDTHS."
  (let ((lines nil)
        (pos 0)
      (len (length text)))
    (while (< pos len)
      (let ((end-pos pos)
            (line-width 0))
        (while (and (< end-pos len)
                    (<= (+ line-width (aref widths end-pos)) width))
          (setq line-width (+ line-width (aref widths end-pos)))
          (setq end-pos (1+ end-pos)))
        (when (= end-pos pos)
          (setq end-pos (1+ pos)))
        (let ((break-pos end-pos))
          (when (< end-pos len)
            (let ((scan (1- end-pos)))
              (while (and (>= scan pos)
                          (not (and (> scan pos)
                                    (memq (aref text scan) '(?\s ?\t))))
                          (not (mevedel-view-table--break-after-p
                                text scan)))
                (setq scan (1- scan)))
              (when (>= scan pos)
                (setq break-pos (1+ scan)))))
          (push (string-trim-right (substring text pos break-pos)) lines)
          (setq pos break-pos)
          (while (and (< pos len)
                      (memq (aref text pos) '(?\s ?\t)))
            (setq pos (1+ pos))))))
    (nreverse lines)))


;;
;;; Padding and row rendering

(defun mevedel-view-table--pad-string-ascii (str width)
  "Pad STR with plain spaces to reach WIDTH columns."
  (concat str (make-string (max 0 (- width (string-width str))) ?\s)))

(defun mevedel-view-table--pad-string (str width window &optional force-pixel)
  "Pad STR with spaces to reach WIDTH columns.
Non-ASCII or faced content is padded pixel-accurately against WINDOW
so right borders align across rows; the trailing partial space uses a
pixel `display' spec.  FORCE-PIXEL keeps all wrapped lines of one cell
on the same padding path."
  (if (or (and force-pixel
               (mevedel-view-table--pixel-capable-p window))
          (mevedel-view-table--pixel-width-needed-p str window))
      (condition-case nil
          (let* ((char-px (mevedel-view-table--char-pixel-width window))
                 (target-px (* width char-px))
                 (content-px (mevedel-view-table--measure-string str window))
                 (pad-px (- target-px content-px)))
            (if (<= pad-px 0)
                (mevedel-view-table--pad-string-ascii str width)
              (let* ((full-spaces (floor (/ (float pad-px) char-px)))
                     (remaining-px (- pad-px (* full-spaces char-px))))
                (concat str
                        (make-string full-spaces ?\s)
                        (if (> remaining-px 0)
                            (propertize " " 'display
                                        `(space :width (,remaining-px)))
                          "")))))
        (error (mevedel-view-table--pad-string-ascii str width)))
    (mevedel-view-table--pad-string-ascii str width)))

(defun mevedel-view-table--layer-face (string face)
  "Layer FACE under STRING's existing face and font-lock-face values."
  (let ((pos 0)
        (len (length string)))
    (while (< pos len)
      (let ((next (or (next-property-change pos string) len)))
        (dolist (prop '(face font-lock-face))
          (let ((existing (get-text-property pos prop string)))
            (put-text-property
             pos next prop
             (cond
              ((null existing) face)
              ((and (listp existing) (not (keywordp (car existing))))
               (append existing (list face)))
              (t (list existing face)))
             string)))
        (setq pos next)))
    string))

(defun mevedel-view-table--border (str)
  "Return border STR propertized with the table border face."
  (propertize str
              'face 'mevedel-view-table-border
              'font-lock-face 'mevedel-view-table-border))

(defun mevedel-view-table--render-separator-row (col-widths)
  "Build the rendered separator line for COL-WIDTHS."
  (mevedel-view-table--border
   (concat "├"
           (mapconcat (lambda (w) (make-string (+ w 2) ?─))
                      col-widths "┼")
           "┤")))

(defun mevedel-view-table--render-data-row (cells col-widths row-face window)
  "Build the rendered string for data row CELLS, possibly multi-line.
COL-WIDTHS is the allocated column width list.  ROW-FACE, when
non-nil, is layered under each cell's own faces.  WINDOW is used for
pixel-accurate padding."
  (let* ((pipe (mevedel-view-table--border "│"))
         (wrapped (seq-mapn (lambda (cell width)
                              (mevedel-view-table--wrap-text
                               cell width window))
                            cells col-widths))
         (force-pixel-flags
          (mapcar (lambda (cell)
                    (mevedel-view-table--pixel-width-needed-p cell window))
                  cells))
         (max-lines (apply #'max 1 (mapcar #'length wrapped)))
         (lines nil))
    (dotimes (line-idx max-lines)
      (let ((parts nil))
        (seq-mapn
         (lambda (cell-lines width force-pixel)
           (let* ((line (if (< line-idx (length cell-lines))
                            (nth line-idx cell-lines)
                          ""))
                  (padded (concat " "
                                  (mevedel-view-table--pad-string
                                   line width window
                                   (and force-pixel
                                        (not (string-empty-p line))))
                                  " ")))
             (when row-face
               (mevedel-view-table--layer-face padded row-face))
             (push padded parts)))
         wrapped col-widths force-pixel-flags)
        (push (concat pipe (string-join (nreverse parts) pipe) pipe)
              lines)))
    (mapconcat #'identity (nreverse lines) "\n")))


;;
;;; Source rendering

(defun mevedel-view-table--column-maxima (parsed-rows measure)
  "Return per-column maxima of MEASURE over the cells in PARSED-ROWS.
PARSED-ROWS is a list of (ROW . CELLS); MEASURE is called with one
cell string and returns its width in columns.  Ragged rows contribute
only to the columns they have."
  (let* ((columns (apply #'max 0 (mapcar (lambda (entry)
                                           (length (cdr entry)))
                                         parsed-rows)))
         (maxima (make-vector columns 0)))
    (dolist (entry parsed-rows)
      (let ((col 0))
        (dolist (cell (cdr entry))
          (aset maxima col (max (aref maxima col) (funcall measure cell)))
          (setq col (1+ col)))))
    (append maxima nil)))

(defun mevedel-view-table--usable-columns (window inset)
  "Return the usable rendering width in columns for WINDOW minus INSET.
Falls back to 80 columns when WINDOW is not usable."
  (max 10 (- (or (ignore-errors (window-body-width window)) 80) inset)))

(defun mevedel-view-table--render-source (source window inset)
  "Render Markdown table SOURCE into an aligned box-drawing string.
WINDOW supplies font metrics for pixel measurement and the width
target; INSET is the column width consumed by line prefixes at the
table's position."
  (with-temp-buffer
    (insert source)
    (setq-local inhibit-field-text-motion t)
    (mevedel-view--render-markdown-url-links-in-range
     (point-min) (point-max))
    (let* ((rows (mevedel-view-table--collect-rows))
           (separator-row-num
            (seq-position rows 'separator
                          (lambda (row _) (plist-get row :separator))))
           (parsed-rows
            (mapcar (lambda (row)
                      (cons row
                            (unless (plist-get row :separator)
                              (mevedel-view-table--parse-row
                               (plist-get row :start)
                               (plist-get row :end)))))
                    rows))
           (natural-widths
            (mevedel-view-table--column-maxima
             parsed-rows
             (lambda (cell)
               (mevedel-view-table--display-width cell window)))))
      (let* ((target (floor (* (mevedel-view-table--usable-columns
                                window inset)
                               mevedel-view-table--max-width-fraction)))
             (col-widths
              (if (> (mevedel-view-table--total-width natural-widths) target)
                  (mevedel-view-table--allocate-widths
                   natural-widths
                   (mevedel-view-table--column-maxima
                    parsed-rows
                    (lambda (cell)
                      (mevedel-view-table--longest-word cell window)))
                   target)
                natural-widths))
             (data-row-num 0)
             (rendered-rows nil))
        (dolist (entry parsed-rows)
          (let* ((row (car entry))
                 (cells (cdr entry))
                 (row-num (plist-get row :num))
                 (is-separator (plist-get row :separator))
                 (is-header (and separator-row-num
                                 (< row-num separator-row-num)))
                 (is-zebra (and (not is-header)
                                (not is-separator)
                                (= (mod data-row-num 2) 1)))
                 (row-face (cond (is-header 'mevedel-view-table-header)
                                 (is-zebra 'mevedel-view-table-zebra))))
            (unless (or is-header is-separator)
              (setq data-row-num (1+ data-row-num)))
            (push (if is-separator
                      (mevedel-view-table--render-separator-row col-widths)
                    ;; Ragged rows can have fewer cells than columns;
                    ;; pad with empty cells so borders stay aligned.
                    (mevedel-view-table--render-data-row
                     (append cells
                             (make-list (max 0 (- (length col-widths)
                                                  (length cells)))
                                        ""))
                     col-widths row-face window))
                  rendered-rows)))
        (string-join (nreverse rendered-rows) "\n")))))


;;
;;; Buffer rewriting

(defun mevedel-view-table--inset-at (pos)
  "Return the display columns consumed by line prefixes at POS."
  (let ((line (or (get-text-property pos 'line-prefix)
                  (and (stringp line-prefix) line-prefix)))
        (wrap (or (get-text-property pos 'wrap-prefix)
                  (and (stringp wrap-prefix) wrap-prefix))))
    (max (if (stringp line) (string-width line) 0)
         (if (stringp wrap) (string-width wrap) 0))))

(defun mevedel-view-table--rear-nonsticky (carried)
  "Return the rear-nonsticky value for a rendered table.
Merges this module's properties with any CARRIED rear-nonsticky."
  (let ((existing (plist-get carried 'rear-nonsticky))
        (ours '(mevedel-view-table-source
                mevedel-view-table-width
                mevedel-view-no-linkify)))
    (if (eq existing t)
        t
      (delete-dups (append ours (copy-sequence existing))))))

(defun mevedel-view-table--fill-carried-properties (start end carried)
  "Set each property in plist CARRIED between START and END where absent.
Cell content keeps its own values -- a button rendered inside a cell
must not have its keymap or stickiness clobbered by the table's
surrounding transcript properties."
  (while carried
    (let ((prop (car carried))
          (value (cadr carried))
          (pos start))
      (while (< pos end)
        (let ((next (or (next-single-property-change pos prop nil end) end)))
          (unless (get-text-property pos prop)
            (put-text-property pos next prop value))
          (setq pos next))))
    (setq carried (cddr carried))))

(defun mevedel-view-table--render-region (start end source &optional window)
  "Replace START..END with SOURCE rendered as an aligned table.
The rendered text retains SOURCE and the displaying window's pixel
width as text properties, and carries the view's own properties from
START across the rewrite.  WINDOW, when live, is the window the
layout targets; otherwise a window showing the buffer is used."
  (let* ((window (if (and window (window-live-p window))
                     window
                   (get-buffer-window (current-buffer) t)))
         (width (and window (window-body-width window t)))
         (inset (mevedel-view-table--inset-at start))
         (rendered (mevedel-view-table--render-source source window inset))
         (carried (mevedel-view--selected-text-properties
                   start mevedel-view-table--carried-properties))
         (inhibit-read-only t))
    (save-excursion
      (delete-region start end)
      (goto-char start)
      (insert rendered)
      (let ((rend (point)))
        (when carried
          (mevedel-view-table--fill-carried-properties start rend carried))
        (add-text-properties
         start rend
         (list 'mevedel-view-table-source source
               'mevedel-view-table-width width
               'mevedel-view-no-linkify t
               'rear-nonsticky (mevedel-view-table--rear-nonsticky
                                carried)))
        ;; The newline after the raw table kept its pipe-table
        ;; fontification; a background face there paints a stray band
        ;; to the window edge.
        (when (eq (char-after rend) ?\n)
          (mevedel-view-table--strip-table-faces rend (1+ rend)))))))

(defun mevedel-view-table-decorate (start end avoid-ranges)
  "Render Markdown pipe tables between START and END.
Tables overlapping AVOID-RANGES or linkify-exempt text stay raw;
already rendered regions are ignored."
  ;; Render back to front so earlier bounds stay valid as each
  ;; replacement shifts everything after it.
  (dolist (table (mevedel-view-table--find-tables start end avoid-ranges))
    (mevedel-view-table--render-region
     (car table)
     (cdr table)
     (mevedel-view--markdown-source
      (car table) (cdr table)))))

(defun mevedel-view-table-rerender (&optional window)
  "Re-render tables whose stored width no longer matches WINDOW.
WINDOW defaults to a window showing the buffer.  Each rendered table
is rebuilt from its retained Markdown source at the window's width,
only when the width it was laid out for differs.  A no-op when the
buffer is undisplayed or nothing is stale.  Callers own undo and
modified-flag discipline."
  ;; ponytail: every stale table in the buffer re-renders, on-screen or
  ;; not; restrict to window-start..window-end first if long transcripts
  ;; make a resize visibly stall.
  (when-let* ((window (if (and window (window-live-p window))
                          window
                        (get-buffer-window (current-buffer) t)))
              (width (window-body-width window t)))
    (save-excursion
      (goto-char (point-max))
      (let (match)
        (while (setq match (text-property-search-backward
                            'mevedel-view-table-source))
          (let ((beg (prop-match-beginning match)))
            (unless (eql width (get-text-property
                                beg 'mevedel-view-table-width))
              (mevedel-view-table--render-region
               beg (prop-match-end match) (prop-match-value match)
               window))))))))

(provide 'mevedel-view-table)

;;; mevedel-view-table.el ends here
