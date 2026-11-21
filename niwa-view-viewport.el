;;; niwa-view-viewport.el -- Transcript window preservation -*- lexical-binding: t -*-

;;; Commentary:

;; Transcript writes and managed zone reconciles resolve positions in
;; their own text.  Both capture and apply the same per-window viewport
;; policy around those writes (ADR 0126).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'niwa-utilities)
(require 'niwa-view-pair)
(require 'niwa-view-zone)

;; `niwa-transcript'
(declare-function niwa-transcript--tool-id-in-range
                  "niwa-transcript" (start end))
(autoload 'niwa-transcript--tool-id-in-range "niwa-transcript")

;; `niwa-view'
(defvar niwa-view--interaction-marker)
(defvar niwa-view--status-marker)

;; `niwa-view-disclosure'
(declare-function niwa-view-disclosure-source-start
                  "niwa-view-disclosure" (source))
(autoload 'niwa-view-disclosure-source-start "niwa-view-disclosure")

;; `niwa-view-stream'
(declare-function niwa-view-stream-in-flight-turn-start-position
                  "niwa-view-stream" ())
(autoload 'niwa-view-stream-in-flight-turn-start-position "niwa-view-stream")

(defvar-local niwa-view--transcript-tail nil
  "Boundary after the managed transcript zones.")

(defun niwa-view-transcript-tail-marker ()
  "Return the view's transcript tail marker, or nil before setup."
  (when (and (markerp niwa-view--transcript-tail)
             (eq (marker-buffer niwa-view--transcript-tail) (current-buffer)))
    niwa-view--transcript-tail))

(defun niwa-view-transcript-tail ()
  "Return the view's transcript tail position, or nil before setup."
  (when-let* ((tail (niwa-view-transcript-tail-marker)))
    (marker-position tail)))

(defun niwa-view-transcript-tail-initialize ()
  "Initialize the transcript tail at point and return its position."
  (setq niwa-view--transcript-tail (point-marker))
  (set-marker-insertion-type niwa-view--transcript-tail t)
  (point))

(defun niwa-view--render-anchor-identity (pos)
  "Return POS's source start, section role and child discriminator.
Disclosure keys keep grouped rows' standalone roles and distinguish
compound children sharing source coordinates.  Their content hash or
in-flight token is not part of reader identity: it can change as the
section grows or settles."
  (when-let* ((source (get-text-property pos 'niwa-view-source))
              (start (niwa-view-disclosure-source-start source)))
    (let ((key (get-text-property pos 'niwa-view-source-key)))
      (if (eq (car-safe key) 'source)
          (list start (cadr key) (nthcdr 4 key))
        (list start (get-text-property pos 'niwa-view-type) nil)))))

(defun niwa-view--render-anchor-run-end (pos limit)
  "Return the next source or section boundary after POS, up to LIMIT."
  (let ((span 64) (scan pos) boundary)
    ;; Unbounded independent searches revisit the remaining suffix for a
    ;; constant property at every changing source row.  Probe nearby first;
    ;; keep interval traversal in C even across dense unrelated formatting.
    (while (not boundary)
      (let* ((probe (min limit (+ pos span)))
             (end (next-single-property-change scan 'niwa-view-source nil probe)))
        (setq end (next-single-property-change scan 'niwa-view-type nil end)
              end (next-single-property-change scan 'niwa-view-source-key nil end))
        (if (or (< end probe) (= probe limit))
            (setq boundary end)
          ;; The prefix is constant.  Overlap its last character so the
          ;; next pass includes a boundary exactly at the old probe.
          (setq scan (1- probe) span (* 2 span)))))
    boundary))

(defun niwa-view--position-render-anchor (pos &optional window region)
  "Return a semantic anchor for POS that survives a re-render, or nil.

Redraws delete and re-insert view text, so a raw buffer position saved
across one lands in different content whenever lengths shift above or
around it.  A managed fragment's semantic anchor or the transcript source
map (section identity
plus offset into its rendered run) identifies the same content after
the redraw.  With WINDOW, POS is its start; managed zone offsets then
count screen rows rather than characters.

REGION, a cons (START . END), says that the redraw replaces START..END.
A POS outside it is not part of the redraw, so a marker follows it: it
advances over text inserted at END.  Only a POS inside the region needs a
semantic anchor, counted from START instead of `point-min'.  Make END an
advancing marker so the anchor finds the region's new end.  Release the
returned anchor with `niwa-view--release-render-anchor'."
  (when (and (integer-or-marker-p pos)
             (>= pos (point-min))
             (< pos (point-max)))
    (cond
     ((and region (< pos (car region))) (list 'marker (copy-marker pos)))
     ((and region (>= pos (cdr region))) (list 'marker (copy-marker pos t)))
     (t
      (or
       (when-let* ((anchor (niwa-view-zone-position-anchor pos window)))
         (list 'fragment anchor))
       (when-let* ((key (get-text-property pos 'niwa-view-pending-key)))
         (list 'pending key
               (get-text-property pos 'niwa-view-pending-tool-use-id)
               (- pos (save-excursion (goto-char pos) (line-beginning-position)))))
       (when-let* ((identity (niwa-view--render-anchor-identity pos)))
         (let ((nth 0)
               (scan (if region (car region) (point-min)))
               (limit (min (point-max) (1+ pos)))
               anchor)
           ;; Count only runs of this section.  A retained-tail render can
           ;; give a neighboring separator the same source start; it must
           ;; not shift a group header or its first child's ordinal.
           (while (<= scan pos)
             (let ((next (niwa-view--render-anchor-run-end scan limit)))
               (if (< pos next)
                   (setq anchor
                         (append (list 'source identity nth (- pos scan))
                                 (and region (list (car region) (cdr region)))))
                 (when (equal identity (niwa-view--render-anchor-identity scan))
                   (setq nth (1+ nth))))
               (setq scan next)))
           anchor)))))))

(defun niwa-view--release-render-anchor (anchor)
  "Detach any marker ANCHOR holds, after its final resolution."
  (when (eq (car-safe anchor) 'marker)
    (set-marker (cadr anchor) nil)))

(defun niwa-view--render-anchor-position (anchor &optional strict)
  "Return the buffer position ANCHOR identifies after a redraw, or nil.
ANCHOR comes from `niwa-view--position-render-anchor'.  The result
is clamped into the re-rendered fragment or source run, so a position
whose content shrank stays inside the same content instead of drifting
into a neighbor.  With STRICT, return nil instead of clamping a lost offset."
  (pcase anchor
    (`(marker ,marker) (marker-position marker))
    (`(fragment ,anchor)
     (niwa-view-zone-anchor-position anchor strict))
    (`(pending ,key ,id ,offset)
     (let ((pos (text-property-any (point-min) (point-max)
                                   'niwa-view-pending-key key)))
       ;; Once the result lands, the pipeline id is gptel's transcript id.
       ;; Search only on that transition; ordinary pending redraws use the key.
       (when (and (not pos) id)
         (let ((scan (or (niwa-view-stream-in-flight-turn-start-position)
                         (point-min)))
               (limit (point-max)))
           (while (and (not pos) (< scan limit))
             (let ((source (get-text-property scan 'niwa-view-source)))
               (when (and (eq (get-text-property scan 'niwa-view-type)
                              'tool-summary)
                          (consp source) (markerp (car source))
                          (equal id (with-current-buffer (marker-buffer (car source))
                                      (niwa-transcript--tool-id-in-range
                                       (car source) (cdr source)))))
                 (setq pos scan)))
             (setq scan (niwa-view--render-anchor-run-end scan limit)))))
       (when pos
         (save-excursion
           (goto-char pos)
           (let ((position (+ (line-beginning-position) offset)))
             (when (or (not strict) (<= position (line-end-position)))
               (min (line-end-position) position)))))))
    (`(source ,identity ,nth ,offset . ,region)
     ;; A region anchor counts runs from the replaced region's start and
     ;; ends where the text after it now begins.
     (let ((limit (if region (min (point-max) (nth 1 region)) (point-max)))
           (pos (if region (min (car region) (point-max)) (point-min)))
           (count 0)
           found done)
       (while (and (not done) (< pos limit))
         (let ((next (niwa-view--render-anchor-run-end pos limit))
               (current (niwa-view--render-anchor-identity pos)))
           (cond
            ((equal identity current)
             (if (= count nth)
                 (setq done t
                       found (and (or (not strict) (< (+ pos offset) next))
                                  (min (max pos (1- next)) (+ pos offset))))
               (setq count (1+ count))))
            ;; A completed response can absorb its formerly separate live
            ;; paragraph.  Retain the character, not the container's header.
            ((and (eq (cadr identity) 'response)
                  (eq (cadr current) 'response)
                  (not (get-text-property pos 'niwa-view-collapsed))
                  (< (car current) (car identity))
                  (< (car identity)
                     (cdr (get-text-property pos 'niwa-view-source))))
             (let ((position (+ pos (- (car identity) (car current)) offset)))
               (setq done t
                     found (and (or (not strict) (< position next))
                                (min (1- next) position))))))
           (setq pos next)))
       (or found
           (and (not strict)
               (niwa-view--render-anchor-container
                (car identity) limit (car region))))))))

(defun niwa-view--render-anchor-container (source-start limit &optional origin)
  "Return the start of the narrowest rendered run containing SOURCE-START.
A redraw can fold a section into an enclosing one, such as a tool row into
its group's single row; the reader then belongs on what now shows it.
Search rendered runs from ORIGIN, default `point-min', before LIMIT."
  (let ((pos (or origin (point-min)))
        best best-size)
    (while (< pos limit)
      (let ((source (get-text-property pos 'niwa-view-source))
            (next (or (next-single-property-change
                       pos 'niwa-view-source nil limit)
                      limit)))
        (when-let* (((consp source))
                    (start (niwa-view-disclosure-source-start source))
                    (end (and (integer-or-marker-p (cdr source))
                              (if (markerp (cdr source))
                                  (marker-position (cdr source))
                                (cdr source))))
                    ((<= start source-start))
                    ((< source-start end))
                    ((or (null best-size) (< (- end start) best-size))))
          (setq best pos
                best-size (- end start)))
        (setq pos next)))
    best))

(defun niwa-view--window-body-geometry (window)
  "Return WINDOW's fresh (TOP . HEIGHT) in pixels before or after redisplay.
TOP includes header and tab lines.  HEIGHT excludes those lines, the mode
line, horizontal scrollbar and bottom divider.  Native visibility queries
measure fresh chrome, so cached line heights cannot supply either value."
  (let* ((buffer (window-buffer window))
         (top (or (window-parameter window 'header-line-format)
                  (buffer-local-value 'header-line-format buffer)
                  (window-parameter window 'tab-line-format)
                  (buffer-local-value 'tab-line-format buffer)))
         (mode (or (window-parameter window 'mode-line-format)
                   (buffer-local-value 'mode-line-format buffer)))
         (start (window-start window))
         (chrome 0)
         (bottom 0))
    (when (or top mode)
      ;; Zero caps positive text height, but pixel vscroll can make it
      ;; negative.  Subtract the same text-only baseline to isolate chrome.
      (let ((text-height (cdr (window-text-pixel-size
                               window start start nil 0 nil t))))
        (setq chrome (- (cdr (window-text-pixel-size
                              window start start nil 0 t t))
                        text-height)
              bottom (cond
                      ((not mode) 0)
                      ((not top) chrome)
                      (t (- (cdr (window-text-pixel-size
                                  window start start nil 0 'mode-line t))
                            text-height))))))
    (cons (- chrome bottom)
          (max 0 (- (window-pixel-height window)
                    (window-scroll-bar-height window)
                    (window-bottom-divider-width window)
                    chrome)))))

(defun niwa-view--position-screen-y (position window &optional fully)
  "Return POSITION's body-relative pixel Y in WINDOW, or nil if hidden.
When FULLY is non-nil, reject partially visible lines too.
The native visibility query traverses the current window, even before
redisplay after a mutation; unlike `window-end', it is not a cached extent."
  (when-let* ((visible (pos-visible-in-window-p position window t))
              ((or (not fully) (null (cddr visible)))))
    (- (cadr visible) (car (niwa-view--window-body-geometry window)))))

(defun niwa-view--viewport-at-y (window position y)
  "Return (START . VSCROLL) placing POSITION at body pixel Y in WINDOW.
Native backward traversal handles wrapping and variable-height lines.
On terminals the same API uses character cells as pixels."
  (if (<= y 0)
      (cons (save-excursion
              (goto-char position)
              (vertical-motion 0 window)
              (point))
            (- y))
    (let ((size (window-text-pixel-size
                 window (cons position (- y)) position nil nil nil t)))
      (cons (nth 2 size) (max 0 (- (nth 1 size) y))))))

(defun niwa-view--set-viewport (window viewport)
  "Set WINDOW to VIEWPORT's (START . VSCROLL), avoiding redundant setters.
A window `niwa-view--geometry-excluded-p' accepts keeps its own viewport."
  (unless (niwa-view--geometry-excluded-p window)
    (unless (= (window-start window) (car viewport))
      (set-window-start window (car viewport) t))
    ;; Setting the start can reset vertical scrolling, so check it afterwards.
    (unless (= (window-vscroll window t) (cdr viewport))
      (set-window-vscroll window (cdr viewport) t t))))

(defun niwa-view--keep-native-vscroll ()
  "Ask the next redisplay to keep each window's pixel scrolling.
Run after restoring window starts.  `set-window-start' lets redisplay
recompute a start inside a wrapped line when text around it changed;
that clears pixel scrolling unless preservation was requested since the
previous redisplay, and then moves point off a bottom row the reset
clips.  The request lasts for one redisplay, so renew it after every
restoration, including numeric no-ops."
  (dolist (window (niwa-view--windows 'geometry))
    (let ((vscroll (window-vscroll window t)))
      (unless (zerop vscroll)
        (set-window-vscroll window vscroll t t)))))

(defun niwa-view--history-reader-windows (&optional windows)
  "Return displayed windows whose caret is in the current transcript.
WINDOWS defaults to the current buffer's geometry windows.
Capture this before mutation: a disappearing interaction must not become
a history reader just because its fallback point lands in the transcript."
  (when (and (markerp (bound-and-true-p niwa-view--status-marker))
             (eq (marker-buffer niwa-view--status-marker) (current-buffer)))
    (cl-loop for window in (or windows (niwa-view--windows 'geometry))
             when (< (window-point window) niwa-view--status-marker) collect window)))

(defun niwa-view--window-tail-lengths (&optional windows)
  "Return (WINDOW . LENGTH) entries for displayed windows.
WINDOWS defaults to the current buffer's geometry windows.
LENGTH counts characters from the window start to `point-max'."
  (mapcar (lambda (window)
            (cons window (- (point-max) (window-start window))))
          (or windows (niwa-view--windows 'geometry))))

(defun niwa-view--last-page (window)
  "Return the viewport that fits the last page of WINDOW."
  (niwa-view--viewport-at-y
   window (point-max)
   (max 0 (- (cdr (niwa-view--window-body-geometry window))
             (with-selected-window window
               (save-excursion
                 (goto-char (point-max))
                 (line-pixel-height)))))))

(defun niwa-view-transcript-end-visible-p (window)
  "Return non-nil if WINDOW fully shows its buffer end, even before redisplay."
  (pos-visible-in-window-p (with-current-buffer (window-buffer window) (point-max))
                           window))

(defun niwa-view-transcript-show-end (window)
  "Show WINDOW's transcript end without changing focus.
Keep its caret if it remains visible on the last page."
  (with-current-buffer (window-buffer window)
    (unless (niwa-view-transcript-end-visible-p window)
      (niwa-view--set-viewport window (niwa-view--last-page window))
      (unless (pos-visible-in-window-p (window-point window) window t)
        (set-window-point window (point-max))))))

(defun niwa-view--clamp-window-starts (tails &optional force)
  "Fill the last page when text below a restored start shrank.
TAILS contains (WINDOW . LENGTH) entries captured before mutation by
`niwa-view--window-tail-lengths'.  Compare against restored starts,
after semantic anchoring.  Unchanged or growing tails keep
their reading position, including deliberate space below the buffer.
FORCE lists windows whose logical start was lost; fill their last page
regardless of character-tail length after anchoring.
Compute the last page in pixels, including wrapped and variable-height lines."
  (pcase-dolist (`(,window . ,length) tails)
    (when (and (window-live-p window)
               (eq (window-buffer window) (current-buffer))
               (or (> (window-start window) (point-min))
                   (> (window-vscroll window t) 0))
               (or (memq window force)
                   (< (- (point-max) (window-start window)) length)))
      (let* ((last-page (niwa-view--last-page window))
             (current (window-start window)))
        (when (or (> current (car last-page))
                  (and (= current (car last-page))
                       (> (window-vscroll window t) (cdr last-page))))
          (niwa-view--set-viewport window last-page))))))

(defun niwa-view--window-end-row (window)
  "Return (Y . HEIGHT) of WINDOW's final buffer row, or nil if hidden.
Measure fresh native rows, not glyph Y: text beside a tall image can sit
below the row top.  Limit traversal to the visible window, and measure the
last row separately because an empty EOF has no text pixel size."
  (when (niwa-view--position-screen-y (point-max) window t)
    (cons (cdr (window-text-pixel-size
                window (window-start window) (point-max) nil nil nil t))
          (with-selected-window window
            (save-excursion
              (goto-char (point-max))
              (line-pixel-height))))))

(defun niwa-view--window-blank-pixels (window)
  "Return blank body pixels below the buffer end in WINDOW, or nil if hidden."
  (when-let* ((row (niwa-view--window-end-row window)))
    (max 0 (- (cdr (niwa-view--window-body-geometry window))
              (car row) (cdr row)))))

(defun niwa-view--restore-window-blank-pixels (window blank)
  "Fill only blank pixels in WINDOW exceeding the previous amount BLANK."
  (when-let* ((row (niwa-view--window-end-row window))
              (current (max 0 (- (cdr (niwa-view--window-body-geometry window))
                                 (car row) (cdr row))))
              ((> current blank)))
    (niwa-view--set-viewport
     window (niwa-view--viewport-at-y
             window (point-max)
             (+ (car row) (- current blank))))))

(defun niwa-view-viewport-capture (follow-output)
  "Capture viewport policy before a write to the current transcript.
With FOLLOW-OUTPUT, remember windows showing the end.  Return an opaque
snapshot for `niwa-view-viewport-apply'."
  (when-let* ((windows (niwa-view--windows 'geometry)))
    (let ((readers (niwa-view--history-reader-windows windows))
          (tails (niwa-view--window-tail-lengths windows))
          placements)
      (dolist (tail tails)
        (let* ((window (car tail))
               (point (window-point window))
               (visit (niwa-view-zone-visit-anchor point))
               (y (and visit (niwa-view--position-screen-y point window)))
               (following (and follow-output (pos-visible-in-window-p (point-max) window)
                               (if (and (memq (get-text-property point 'niwa-view-type)
                                              '(tool-summary tool-child))
                                        (pos-visible-in-window-p point window t))
                                   'tool t))))
          (when (or y following)
            (push (list window (and y visit) y
                        (and y (eq (car visit) 'interaction)
                             (or (niwa-view--window-blank-pixels window) 0))
                        following)
                  placements))))
      (list readers tails placements))))

(defun niwa-view-viewport-apply (snapshot resolved)
  "Restore the captured viewport SNAPSHOT using RESOLVED window starts.
Each entry is (WINDOW START :lost-p LOST [NO-FOLLOW]).  NO-FOLLOW means
an anchored tool row survived; it cancels tool-row following only."
  (pcase-let ((`(,readers ,tails ,placements) snapshot)
              (lost nil) (placed nil))
    (dolist (entry resolved)
      (pcase-let ((`(,window ,start :lost-p ,lost-p . ,rest) entry))
        (when (and (window-live-p window) (eq (window-buffer window) (current-buffer)))
          (when lost-p (push window lost))
          (let* ((placement (assq window placements))
                 (visit (nth 1 placement))
                 (position (and visit (niwa-view-zone-anchor-position visit)))
                 (following (nth 4 placement)))
            (if position
                (progn
                  (set-window-point window position)
                  (niwa-view--set-viewport window (niwa-view--viewport-at-y
                                                   window position (nth 2 placement)))
                  (when-let* ((blank (nth 3 placement)))
                    (niwa-view--restore-window-blank-pixels window blank))
                  (push window placed))
              ;; Check end visibility from the restored start, not the raw marker.
              (when (and start (<= start (point-max)))
                (set-window-start window start t))
              (when (and following (not (and (eq following 'tool) (car rest))))
                (niwa-view-transcript-show-end window)
                (push window placed)))))))
    (niwa-view--clamp-window-starts
     (seq-remove (lambda (tail)
                   (or (memq (car tail) placed)
                       (and (memq (car tail) readers) (not (memq (car tail) lost)))))
                 tails)
     lost)
    (niwa-view--keep-native-vscroll)))

(defun niwa-view--call-preserving-window-state
    (thunk &optional positions-preserved region follow-output)
  "Call THUNK while preserving each displayed window's browsing state.
Preserves those values for every window displaying the current buffer.
With FOLLOW-OUTPUT, windows showing the end continue to show it unless
they have a surviving visit to a visible tool row or managed row.
Other windows retain their reading viewport.

Used to wrap delete-and-re-render operations so the user's scroll
position and caret do not jump back to the edit site on every
progress tick.  Point, window points, and window starts are restored
through semantic render anchors (`niwa-view--position-render-anchor')
so a position inside re-rendered text returns to the same content
rather than to the same numeric offset; a position whose anchor cannot
be resolved after BODY is quietly clamped to the buffer.  The buffer
mark, active-region state, and selection direction are preserved with
point.
With POSITIONS-PRESERVED, THUNK owns exact point, mark and `window-start'
relocation (for example, table cell splicing).  Keep viewport policy but
do not override that relocation or classify its old offsets as lost.
REGION, a cons (START . END), says that THUNK replaces that transcript
text.  Markers then follow positions outside it, and anchors inside it are
found without scanning the rest of the buffer."
  (let* ((region (and region (cons (copy-marker (car region))
                                   (copy-marker (cdr region) t))))
         (old-end (point-max))
         (snapshot (niwa-view-viewport-capture follow-output))
         (selected (selected-window))
         (buffer (current-buffer))
         (old-point (point))
         (point-anchor (niwa-view--position-render-anchor old-point nil region))
         (old-mark (mark t))
         (mark-anchor (and old-mark (niwa-view--position-render-anchor old-mark nil region)))
         (active mark-active) (deactivate deactivate-mark)
         (saved (mapcar (lambda (tail)
                          (let* ((window (car tail))
                                 (wp (window-point window)) (ws (window-start window)))
                            (list window wp ws (niwa-view--position-render-anchor wp nil region)
                                  (niwa-view--position-render-anchor ws window region))))
                        (nth 1 snapshot))))
    (unwind-protect (funcall thunk)
      ;; Restoration can fail too; detach this call's markers anyway.
      (unwind-protect
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (niwa-view-pair-refresh-turn-rule)
              (unless positions-preserved
                (goto-char (min (point-max) (or (niwa-view--render-anchor-position point-anchor)
                                                old-point))))
              (let (resolved)
                (dolist (entry saved)
                  (pcase-let ((`(,window ,wp ,ws ,wp-anchor ,ws-anchor) entry))
                    (when (and (window-live-p window) (eq (window-buffer window) buffer))
                      (if positions-preserved
                          (push (list window nil :lost-p nil) resolved)
                        (let ((point (niwa-view--render-anchor-position wp-anchor))
                              (start (niwa-view--render-anchor-position ws-anchor t)))
                          (set-window-point window (if (= wp old-end) (point-max)
                                                     (min (point-max) (or point wp))))
                          (when (eq window selected) (goto-char (window-point window)))
                          (push (list window (or start ws) :lost-p (and ws-anchor (not start))
                                      (and follow-output point
                                           (memq (get-text-property point 'niwa-view-type)
                                                 '(tool-summary tool-child))))
                                resolved))))))
                (when (and (not positions-preserved) old-mark)
                  (set-marker (mark-marker)
                              (min (point-max) (or (niwa-view--render-anchor-position mark-anchor)
                                                   old-mark)))
                  (setq mark-active active deactivate-mark deactivate))
                (niwa-view-viewport-apply snapshot (nreverse resolved)))))
        (when region
          (niwa-view--release-render-anchor point-anchor)
          (niwa-view--release-render-anchor mark-anchor)
          (dolist (entry saved)
            (niwa-view--release-render-anchor (nth 3 entry))
            (niwa-view--release-render-anchor (nth 4 entry)))
          (set-marker (car region) nil)
          (set-marker (cdr region) nil))))))

(defun niwa-view-call-preserving-window-configuration (thunk)
  "Call THUNK, restoring the frame layout and semantic view positions.
Unlike a raw window configuration, view anchors survive text replaced by
redraws during THUNK.  Buffer edits are not reverted.  THUNK runs in the
caller's buffer, not in a displayed view."
  (let ((configuration (current-window-configuration))
        (caller (current-buffer))
        (buffers (delete-dups
                  (mapcar #'window-buffer (window-list nil 'no-minibuffer)))))
    (cl-labels
        ((preserve (remaining)
           (if (null remaining)
               (unwind-protect
                   (with-current-buffer caller (funcall thunk))
                 (set-window-configuration configuration))
             (let ((buffer (car remaining)))
               (if (with-current-buffer buffer
                     (derived-mode-p 'niwa-view-mode))
                   (with-current-buffer buffer
                     (niwa-view--call-preserving-window-state
                      (lambda () (preserve (cdr remaining)))))
                 (preserve (cdr remaining)))))))
      (preserve buffers))))

(defun niwa-view--call-with-render-boundaries-advancing (thunk &optional types)
  "Call THUNK with advancing zone boundaries, then restore their types.
Explicit TYPES sets the status, interaction and tail insertion types."
  (let* ((types (or types '(t t t)))
         (status (bound-and-true-p niwa-view--status-marker))
         (interaction (bound-and-true-p niwa-view--interaction-marker))
         (tail (niwa-view-transcript-tail-marker))
         (s (and (markerp status) (marker-insertion-type status)))
         (i (and (markerp interaction) (marker-insertion-type interaction)))
         (e (and tail (marker-insertion-type tail))))
    (unwind-protect
        (progn
          (when (markerp status) (set-marker-insertion-type status (car types)))
          (when (markerp interaction) (set-marker-insertion-type interaction (nth 1 types)))
          (when tail (set-marker-insertion-type tail (nth 2 types)))
          (funcall thunk))
      (when (markerp status) (set-marker-insertion-type status s))
      (when (markerp interaction) (set-marker-insertion-type interaction i))
      (when tail (set-marker-insertion-type tail e)))))

(defmacro niwa-view--with-render-boundaries-advancing (&rest body)
  "Execute BODY while zone boundary markers advance across insertions."
  (declare (indent 0) (debug t))
  `(niwa-view--call-with-render-boundaries-advancing
    (lambda () ,@body)))

(defun niwa-view-call-preserving-user-view-state (thunk &optional region follow-output)
  "Call THUNK, a view write the user did not command; return its value.
This is the entry every asynchronous view writer uses: timers, process
output, stream chunks, deferred and batched renders.  Around THUNK it keeps
transcript point and mark, and each window's reading or visited-row
viewport.  A lost start fills the last page.  With FOLLOW-OUTPUT, a window
already showing the end keeps it visible, unless a row visit survives.
The separate composer is outside this write.

REGION, a cons (START . END), promises that THUNK replaces only that text.
Anchors inside it are then resolved locally instead of scanning the
buffer, so frequent small writers stay cheap."
  (niwa-view--call-preserving-window-state
   (lambda () (let ((inhibit-modification-hooks t)) (funcall thunk)))
   nil region follow-output))

(provide 'niwa-view-viewport)
;;; niwa-view-viewport.el ends here
