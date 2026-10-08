;;; mevedel-view-animation.el --- Prepared working-status frames -*- lexical-binding: t -*-

;;; Commentary:

;; Pure time-based animation samples for the view's request and tool indicators.
;; Scheduling, status ownership, and buffer writes belong to the view stream.
;; Shimmer is cadenced: a faded band sweeps the label for one second every
;; four.  Between sweeps the label keeps its normal foreground without wakeups.
;; Color banks hold only a bounded animated prefix; the entire label remains
;; readable.  Six shared banks reuse frames between views; each live view can
;; pin their active banks so another view cannot evict its active sample.  Theme changes
;; discard both caches before the next sample.

;;; Code:

(require 'cl-lib)
(require 'color)

(defconst mevedel-view-animation--cycle 3.6
  "Duration in seconds of a color animation cycle.")

(defconst mevedel-view-animation--frame-count 216
  "Number of prepared color samples in one cycle.")

(defconst mevedel-view-animation--sweep-delay 0.6
  "Seconds after an animation starts before its first shimmer sweep.")

(defconst mevedel-view-animation--sweep-duration 1.0
  "Seconds one shimmer sweep takes to cross its label.")

(defconst mevedel-view-animation--sweep-interval 4.0
  "Seconds from one shimmer sweep's start to the next.
Every wakeup repaints a pgtk frame's whole surface.  A continuous shimmer
held the editor at 34% CPU at 12 fps and 70% at 60; resting three seconds
in four lets each sweep run smoothly at a fraction of that.")

(defconst mevedel-view-animation--sweep-frames 60
  "Prepared shimmer samples across one sweep.")

(defconst mevedel-view-animation--palette-size 64
  "Number of theme-derived shades in a color bank.")

(defconst mevedel-view-animation--prefix-limit 48
  "Maximum number of characters animated in a label.")

(defconst mevedel-view-animation--cache-limit 6
  "Maximum number of prepared color banks retained across labels and frames.")

(defvar-local mevedel-view-animation--view-cache-limit 4
  "Maximum prepared banks retained by this view.
The scheduler sizes this to cover the bounded set of registered labels.")

(defconst mevedel-view-animation-prefixes
  (append (mapcar (lambda (char) (concat (string char) " "))
                  (string-to-list "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏-\\|/"))
          (mapcar (lambda (dots) (concat dots " "))
                  '("●···" "·●··" "··●·" "···●"
                    "*..." ".*.." "..*." "...*")))
  "Visible compact indicator prefixes, for identifying live tool rows.
Static mode has no prefix and is deliberately absent: an empty prefix
would match every row.")

(defvar mevedel-view-animation--cache nil
  "Recent banks or fallback markers (KEY . DATA), newest first.")

(defvar-local mevedel-view-animation--view-cache nil
  "Prepared banks pinned by this view while it remains alive.
Unlike the shared reuse cache, these banks cannot be evicted by other views.")

(defvar mevedel-view-animation--dots-cache nil
  "Recent dots glyph support readings (FRAME . SUPPORTED).")

(defun mevedel-view-animation-reserve (labels)
  "Reserve this view's prepared banks for LABELS active indicator spans.
Keep at least four reusable banks; shrink obsolete capacity immediately.
Call at semantic scheduling boundaries, never on each decorative frame."
  (setq mevedel-view-animation--view-cache-limit (max 4 labels))
  (when (> (length mevedel-view-animation--view-cache)
           mevedel-view-animation--view-cache-limit)
    (setcdr (nthcdr (1- mevedel-view-animation--view-cache-limit)
                    mevedel-view-animation--view-cache)
            nil)))

(defun mevedel-view-animation-reset-glyph-support ()
  "Retry dots glyph support after a display or font change."
  (setq mevedel-view-animation--dots-cache nil))

(defun mevedel-view-animation-invalidate (&rest _ignored)
  "Discard color banks after theme or face changes.
Call this after changing a face outside of the theme system.  The view
may redisplay its active indicators immediately after invalidation."
  (setq mevedel-view-animation--cache nil
        mevedel-view-animation--dots-cache nil)
  (dolist (buffer (buffer-list))
    (when (local-variable-p 'mevedel-view-animation--view-cache buffer)
      (with-current-buffer buffer
        (setq mevedel-view-animation--view-cache nil)))))

(defun mevedel-view-animation-check-colors ()
  "Discard prepared banks if their resolved display colors changed.
Call from semantic maintenance, never on each animation frame.  This
also retries a fallback bank when a frame gains color support."
  ;; A changed display font need not change resolved face colors.  Retry dots
  ;; support at semantic cadence rather than probing on every visual sample.
  (mevedel-view-animation-reset-glyph-support)
  (when (cl-some
         (lambda (entry)
           (let ((key (car entry)) (bank (cdr entry)))
             (not (equal (mevedel-view-animation--colors
                          (nth 2 key) (nth 3 key))
                         (and (consp bank) (nth 2 bank))))))
         (append mevedel-view-animation--view-cache
                 mevedel-view-animation--cache))
    (mevedel-view-animation-invalidate)))

(add-hook 'enable-theme-functions #'mevedel-view-animation-invalidate)
(add-hook 'disable-theme-functions #'mevedel-view-animation-invalidate)

(defun mevedel-view-animation-sweep-phase (seconds)
  "Return seconds into the shimmer sweep at animation SECONDS, or nil at rest."
  (let ((since (- seconds mevedel-view-animation--sweep-delay)))
    (when (>= since 0)
      (let ((into (mod since mevedel-view-animation--sweep-interval)))
        (and (< into mevedel-view-animation--sweep-duration) into)))))

(defun mevedel-view-animation-next-delay (style seconds period)
  "Return seconds until STYLE next changes after animation SECONDS.
PERIOD is the frame period to use while STYLE moves.  Shimmer moves only
during its sweep: from rest, wait for the next sweep; within one, step
by PERIOD but never past its end, so the label settles at rest."
  (if (not (eq style 'shimmer))
      period
    (let ((since (- seconds mevedel-view-animation--sweep-delay)))
      (if (< since 0)
          (- since)
        (let ((into (mod since mevedel-view-animation--sweep-interval)))
          (cond
           ((< into mevedel-view-animation--sweep-duration)
            (min period (- mevedel-view-animation--sweep-duration into)))
           ;; Rounding can land a hair before the next sweep: start it.
           ((< (- mevedel-view-animation--sweep-interval into) 0.001) period)
           (t (- mevedel-view-animation--sweep-interval into))))))))

(defun mevedel-view-animation-period (style)
  "Return the natural update period in seconds for STYLE, or nil if static.
The caller may choose a slower rendering ceiling without changing the
time-based animation phase.  Shimmer moves at this period only during
its sweep; see `mevedel-view-animation-next-delay'."
  (pcase style
    ('shimmer (/ 1.0 60))
    ((or 'breathe 'bounce) (/ 1.0 8))
    ((or 'braille 'ascii) 0.24)
    ('dots 0.48)
    ('ellipsis 0.96)
    (_ nil)))

(defun mevedel-view-animation--colors (face frame)
  "Return (FOREGROUND . BACKGROUND) for FACE on FRAME, or nil.
Only called when preparing a color bank, never to sample a cached bank.
Small terminal palettes cannot resolve the 64 shades into a useful color
animation; use the glyph fallback instead of scheduling invisible frames."
  (condition-case nil
      (when (and (display-color-p frame)
                 (or (display-graphic-p frame)
                     (>= (or (display-color-cells frame) 0) 256)))
        (let* ((foreground (face-foreground face frame t))
               (background (face-background 'default frame t)))
          (when (and (stringp foreground) (stringp background)
                     (color-name-to-rgb foreground frame)
                     (color-name-to-rgb background frame))
            (cons foreground background))))
    (error nil)))

(defun mevedel-view-animation--palette (foreground background &optional frame)
  "Prepare 64 shades between BACKGROUND and FOREGROUND for FRAME."
  (let ((fg (color-name-to-rgb foreground frame))
        (bg (color-name-to-rgb background frame)))
    (when (and fg bg)
      (let ((palette (make-vector mevedel-view-animation--palette-size nil)))
        (dotimes (i mevedel-view-animation--palette-size)
          (let ((weight (+ 0.52 (* 0.48 (/ i 63.0)))))
            (aset palette i
                  (apply #'color-rgb-to-hex
                         (cl-mapcar (lambda (front back)
                                      (+ (* weight front)
                                         (* (- 1 weight) back)))
                                    fg bg)))))
        palette))))

(defun mevedel-view-animation--prefix-end (label)
  "Return a bounded animation endpoint in LABEL, without cutting a mark.
If the endpoint would separate combining characters from their base,
leave that entire final cluster unanimated."
  (let ((end (min (length label) mevedel-view-animation--prefix-limit)))
    (when (< end (length label))
      (while (and (> end 0)
                  (memq (get-char-code-property (aref label end)
                                                'general-category)
                        '(Mn Mc Me)))
        (setq end (1- end))))
    end))

(defun mevedel-view-animation--sweep-sample (head palette tick)
  "Return HEAD shaded from PALETTE at shimmer sweep TICK, 0 being at rest.
A cosine band at least three columns wide fades toward the background as
it crosses HEAD; outside it and at rest the text keeps its normal foreground."
  (let* ((width (float (string-width head)))
         (half (max 3.0 (* 0.1 width)))
         (position (and (> tick 0)
                        (- (* (/ (1- tick)
                                 (float mevedel-view-animation--sweep-frames))
                              (+ width (* 2 half)))
                           half)))
         (sample (copy-sequence head))
         (column 0.0))
    (dotimes (i (length head))
      (let* ((glyph (float (char-width (aref head i))))
             (distance (if position
                           (min 1.0 (/ (abs (- (+ column (/ glyph 2)) position))
                                       half))
                         1.0))
             (intensity (* 0.5 (+ 1 (cos (* float-pi distance)))))
             (shade (aref palette (min 63 (max 0 (round (* 63 (- 1 intensity))))))))
        (setq column (+ column glyph))
        (put-text-property i (1+ i) 'face `(:foreground ,shade) sample)))
    sample))

(defun mevedel-view-animation--prepare (style label colors frame)
  "Build a bank of color frames for STYLE and LABEL using COLORS on FRAME."
  (let* ((palette (mevedel-view-animation--palette
                   (car colors) (cdr colors) frame))
         (end (mevedel-view-animation--prefix-end label))
         (suffix (substring label end))
         (head (substring label 0 end))
         (frames (make-vector (if (eq style 'shimmer)
                                  (1+ mevedel-view-animation--sweep-frames)
                                mevedel-view-animation--frame-count)
                              nil)))
    (when palette
      (dotimes (tick (length frames))
        (aset frames tick
              (if (eq style 'shimmer)
                  (mevedel-view-animation--sweep-sample head palette tick)
                (let* ((seconds (/ tick 60.0))
                       (center (- 3.5 (* 3.5 (cos (/ (* seconds float-pi) 1.8)))))
                       (breath (when (eq style 'breathe)
                                 (/ (- 1 (cos (/ (* seconds float-pi) 1.8))) 2)))
                       (sample (copy-sequence head)))
                  (dotimes (i end)
                    (let* ((intensity
                            (or breath
                                (max 0.0 (- 1 (/ (abs (- i center)) 2.5)))))
                           (shade (aref palette
                                        (min 63 (max 0 (round (* 63 (- 1 intensity))))))))
                      (put-text-property i (1+ i) 'face
                                         `(:foreground ,shade) sample)))
                  sample))))
      (list frames suffix colors))))

(defun mevedel-view-animation--remember-view-bank (key bank)
  "Pin BANK for KEY within the current live view, up to its local limit."
  (when (derived-mode-p 'mevedel-view-mode)
    (setq mevedel-view-animation--view-cache
          (cons (cons key bank)
                (assoc-delete-all key mevedel-view-animation--view-cache)))
    (when (> (length mevedel-view-animation--view-cache)
             mevedel-view-animation--view-cache-limit)
      (setcdr (nthcdr (1- mevedel-view-animation--view-cache-limit)
                      mevedel-view-animation--view-cache)
              nil))))

(defun mevedel-view-animation--color-frame (style label seconds face frame)
  "Return prepared color STYLE for LABEL at SECONDS with FACE on FRAME."
  (let* ((frame (or frame (selected-frame)))
         (key (list style label face frame))
         (local (assoc key mevedel-view-animation--view-cache))
         (entry (or local (assoc key mevedel-view-animation--cache)))
         (bank (if entry (cdr entry)
                 (let* ((colors (mevedel-view-animation--colors face frame))
                        (prepared (and colors (mevedel-view-animation--prepare
                                               style label colors frame))))
                   (push (cons key (or prepared :fallback))
                         mevedel-view-animation--cache)
                   (when (> (length mevedel-view-animation--cache)
                            mevedel-view-animation--cache-limit)
                     (setcdr (nthcdr (1- mevedel-view-animation--cache-limit)
                                     mevedel-view-animation--cache)
                             nil))
                   prepared))))
    (unless local
      (mevedel-view-animation--remember-view-bank key (or bank :fallback)))
    (when (consp bank)
      (let* ((tick
              (if (eq style 'shimmer)
                  (if-let* ((phase (mevedel-view-animation-sweep-phase
                                    (max 0.0 seconds))))
                      (min mevedel-view-animation--sweep-frames
                           (1+ (floor (* (/ phase mevedel-view-animation--sweep-duration)
                                         mevedel-view-animation--sweep-frames))))
                    0)
                (mod (floor (* (mod (max 0.0 seconds)
                                    mevedel-view-animation--cycle)
                               60))
                     mevedel-view-animation--frame-count)))
             (sample (aref (car bank) tick)))
        (if (equal (cadr bank) "") sample
          (concat sample (cadr bank)))))))

(defun mevedel-view-animation-color-available-p (style label face frame)
  "Return non-nil when color STYLE has a prepared bank on FRAME.
Call when (re)arming a view, not on every animation frame.  Reuse the
bounded bank prepared for the first visible frame when available."
  (when (memq style '(shimmer breathe bounce))
    (let* ((frame (or frame (selected-frame)))
           (sample (mevedel-view-animation--color-frame style label 0 face frame))
           (entry (assoc (list style label face frame)
                         mevedel-view-animation--view-cache)))
      ;; Promote only during semantic preparation.  Otherwise old-frame banks
      ;; can evict an active label already visited by this preparation pass.
      ;; Decorative cache hits stay allocation-free at the retention boundary.
      (when entry
        (mevedel-view-animation--remember-view-bank (car entry) (cdr entry)))
      sample)))

(defun mevedel-view-animation-color-ready-p (style label face frame)
  "Return non-nil if a current color or glyph-fallback bank is cached.
Unlike `mevedel-view-animation-color-available-p', this never resolves
colors or constructs frames; visual callbacks can use it after a theme
invalidation to defer bank preparation until semantic maintenance."
  (let ((key (list style label face (or frame (selected-frame)))))
    (or (assoc key mevedel-view-animation--view-cache)
        (assoc key mevedel-view-animation--cache))))

(defun mevedel-view-animation--braille-supported-p (frame)
  "Return non-nil when the display FRAME can draw Braille.
A status visible in multiple frames uses ASCII, which works on all of
them.  Selecting a frame here does not move input focus; it is restored
before returning."
  (and (not (eq frame :multiple))
       (if (and (framep frame) (not (eq frame (selected-frame))))
           (with-selected-frame frame (char-displayable-p ?⠋))
         (char-displayable-p ?⠋))))

(defun mevedel-view-animation--dots-frame-supported-p (frame)
  "Return non-nil when both dots glyphs render on FRAME."
  (if (and (framep frame) (not (eq frame (selected-frame))))
      (with-selected-frame frame
        (and (char-displayable-p ?●) (char-displayable-p ?·)))
    (and (char-displayable-p ?●) (char-displayable-p ?·))))

(defun mevedel-view-animation--dots-supported-p (frame)
  "Return non-nil when dots glyphs render on every target FRAME.
FRAME may be a list of frames actually displaying the indicator.  The
undisplayed or ambiguous `:multiple' case uses portable ASCII dots."
  (unless (eq frame :multiple)
    (cl-every
     (lambda (target)
       (let ((entry (assq target mevedel-view-animation--dots-cache)))
         (unless entry
           (setq entry (cons target
                             (mevedel-view-animation--dots-frame-supported-p
                              target)))
           (push entry mevedel-view-animation--dots-cache)
           (when (> (length mevedel-view-animation--dots-cache) 12)
             (setcdr (nthcdr 11 mevedel-view-animation--dots-cache) nil)))
         (cdr entry)))
     (if (consp frame) frame (list (or frame (selected-frame)))))))

(defun mevedel-view-animation-frame (style label seconds face &optional frame)
  "Return a fixed-width display sample for STYLE and LABEL at SECONDS.
FACE supplies the foreground of color styles on optional display FRAME.
Styles are shimmer, breathe, bounce, dots, ellipsis, braille, ascii, and
static.  For compact tool indicators pass an empty LABEL to a glyph style;
the result is only its prefix, including a separating space.  Static with
an empty LABEL returns an empty string.  Colorless displays use the braille
glyph fallback.  SECONDS is elapsed animation time, not a frame counter."
  (let ((label (or label ""))
        (tick (floor (+ 1e-8 (/ (max 0.0 seconds) 0.24)))))
    (when (and (eq style 'braille)
               (not (mevedel-view-animation--braille-supported-p frame)))
      (setq style 'ascii))
    (pcase style
      ((or 'shimmer 'breathe 'bounce)
       (or (mevedel-view-animation--color-frame style label seconds face frame)
           (mevedel-view-animation-frame
            (if (mevedel-view-animation--braille-supported-p frame)
                'braille 'ascii)
            label seconds face frame)))
      ('braille
       (concat (string (aref "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" (mod tick 10))) " " label))
      ('ascii
       (concat (string (aref "-\\|/" (mod tick 4))) " " label))
      ('dots
       (concat (aref (if (mevedel-view-animation--dots-supported-p frame)
                         ["●···" "·●··" "··●·" "···●"]
                       ["*..." ".*.." "..*." "...*"])
                     (mod (/ tick 2) 4)) " " label))
      ('ellipsis
       (let ((n (mod (/ tick 4) 4)))
         (concat label (make-string n ?.) (make-string (- 3 n) ?\s))))
      (_ label))))

(provide 'mevedel-view-animation)
;;; mevedel-view-animation.el ends here
