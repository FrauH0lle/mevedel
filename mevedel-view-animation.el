;;; mevedel-view-animation.el --- Prepared working-status frames -*- lexical-binding: t -*-

;;; Commentary:

;; Pure time-based animation samples for the view's request and tool indicators.
;; Scheduling, status ownership, and buffer writes belong to the view stream.
;; Color banks hold only a bounded animated prefix; the entire label remains
;; readable.  Theme changes discard prepared banks before the next sample.

;;; Code:

(require 'cl-lib)
(require 'color)

(defconst mevedel-view-animation--cycle 3.6
  "Duration in seconds of a color animation cycle.")

(defconst mevedel-view-animation--frame-count 216
  "Number of prepared color samples in one cycle.")

(defconst mevedel-view-animation--palette-size 64
  "Number of theme-derived shades in a color bank.")

(defconst mevedel-view-animation--prefix-limit 48
  "Maximum number of characters animated in a label.")

(defconst mevedel-view-animation--cache-limit 6
  "Maximum number of prepared color banks retained across labels and frames.")

(defconst mevedel-view-animation-prefixes
  (append (mapcar (lambda (char) (concat (string char) " "))
                  (string-to-list "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏-\\|/"))
          (mapcar (lambda (dots) (concat dots " "))
                  '("●···" "·●··" "··●·" "···●")))
  "Visible compact indicator prefixes, for identifying live tool rows.
Static mode has no prefix and is deliberately absent: an empty prefix
would match every row.")

(defvar mevedel-view-animation--cache nil
  "Recent banks or fallback markers (KEY . DATA), newest first.")

(defun mevedel-view-animation-invalidate (&rest _ignored)
  "Discard color banks after theme or face changes.
Call this after changing a face outside of the theme system.  The view
may redisplay its active indicators immediately after invalidation."
  (setq mevedel-view-animation--cache nil))

(defun mevedel-view-animation-check-colors ()
  "Discard prepared banks if their resolved display colors changed.
Call from semantic maintenance, never on each animation frame.  This
also retries a fallback bank when a frame gains color support."
  (when (cl-some
         (lambda (entry)
           (let ((key (car entry)) (bank (cdr entry)))
             (not (equal (mevedel-view-animation--colors
                          (nth 2 key) (nth 3 key))
                         (and (consp bank) (nth 2 bank))))))
         mevedel-view-animation--cache)
    (mevedel-view-animation-invalidate)))

(add-hook 'enable-theme-functions #'mevedel-view-animation-invalidate)
(add-hook 'disable-theme-functions #'mevedel-view-animation-invalidate)

(defun mevedel-view-animation-period (style)
  "Return the natural update period in seconds for STYLE, or nil if static.
The caller may choose a slower rendering ceiling without changing the
time-based animation phase."
  (pcase style
    ((or 'shimmer 'breathe 'bounce) (/ 1.0 60))
    ((or 'braille 'ascii) 0.12)
    ('dots 0.24)
    ('ellipsis 0.48)
    (_ nil)))

(defun mevedel-view-animation--colors (face frame)
  "Return (FOREGROUND . BACKGROUND) for FACE on FRAME, or nil.
Only called when preparing a color bank, never to sample a cached bank."
  (condition-case nil
      (when (display-color-p frame)
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

(defun mevedel-view-animation--prepare (style label colors frame)
  "Build a bank of color frames for STYLE and LABEL using COLORS on FRAME."
  (let* ((palette (mevedel-view-animation--palette
                   (car colors) (cdr colors) frame))
         (end (mevedel-view-animation--prefix-end label))
         (suffix (substring label end))
         (head (substring label 0 end))
         (frames (make-vector mevedel-view-animation--frame-count nil)))
    (when palette
      (dotimes (tick mevedel-view-animation--frame-count)
        (let* ((seconds (/ tick 60.0))
               (center (if (eq style 'bounce)
                           (- 3.5 (* 3.5 (cos (/ (* seconds float-pi) 1.8))))
                         (- (* (/ seconds mevedel-view-animation--cycle) 16) 4)))
               (breath (when (eq style 'breathe)
                         (/ (- 1 (cos (/ (* seconds float-pi) 1.8))) 2)))
               (sample (copy-sequence head)))
          (dotimes (i end)
            (let* ((intensity
                    (or breath
                        (max 0.0 (- 1 (/ (abs (- i center)) 2.5)))))
                   (shade (aref palette
                                (min 63 (max 0 (round (* 63 intensity)))))))
              (put-text-property i (1+ i) 'face
                                 `(:foreground ,shade) sample)))
          (aset frames tick sample)))
      (list frames suffix colors))))

(defun mevedel-view-animation--color-frame (style label seconds face frame)
  "Return prepared color STYLE for LABEL at SECONDS with FACE on FRAME."
  (let* ((frame (or frame (selected-frame)))
         (key (list style label face frame))
         (entry (assoc key mevedel-view-animation--cache))
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
    (when (consp bank)
      (let* ((tick (mod (floor (* (mod (max 0.0 seconds)
                                       mevedel-view-animation--cycle)
                                  60))
                        mevedel-view-animation--frame-count))
             (sample (aref (car bank) tick)))
        (if (equal (cadr bank) "") sample
          (concat sample (cadr bank)))))))

(defun mevedel-view-animation-color-available-p (style label face frame)
  "Return non-nil when color STYLE has a prepared bank on FRAME.
Call when (re)arming a view, not on every animation frame.  Reuse the
bounded bank prepared for the first visible frame when available."
  (and (memq style '(shimmer breathe bounce))
       (mevedel-view-animation--color-frame style label 0 face frame)))

(defun mevedel-view-animation-color-ready-p (style label face frame)
  "Return non-nil if a current color or glyph-fallback bank is cached.
Unlike `mevedel-view-animation-color-available-p', this never resolves
colors or constructs frames; visual callbacks can use it after a theme
invalidation to defer bank preparation until semantic maintenance."
  (assoc (list style label face (or frame (selected-frame)))
         mevedel-view-animation--cache))

(defun mevedel-view-animation--braille-supported-p (frame)
  "Return non-nil when the display FRAME can draw Braille.
A status visible in multiple frames uses ASCII, which works on all of
them.  Selecting a frame here does not move input focus; it is restored
before returning."
  (and (not (eq frame :multiple))
       (if (and (framep frame) (not (eq frame (selected-frame))))
           (with-selected-frame frame (char-displayable-p ?⠋))
         (char-displayable-p ?⠋))))

(defun mevedel-view-animation-frame (style label seconds face &optional frame)
  "Return a fixed-width display sample for STYLE and LABEL at SECONDS.
FACE supplies the foreground of color styles on optional display FRAME.
Styles are shimmer, breathe, bounce, dots, ellipsis, braille, ascii, and
static.  For compact tool indicators pass an empty LABEL to a glyph style;
the result is only its prefix, including a separating space.  Static with
an empty LABEL returns an empty string.  Colorless displays use the braille
glyph fallback.  SECONDS is elapsed animation time, not a frame counter."
  (let ((label (or label ""))
        (tick (floor (+ 1e-8 (/ (max 0.0 seconds) 0.12)))))
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
       (concat (aref ["●···" "·●··" "··●·" "···●"]
                     (mod (/ tick 2) 4)) " " label))
      ('ellipsis
       (let ((n (mod (/ tick 4) 4)))
         (concat label (make-string n ?.) (make-string (- 3 n) ?\s))))
      (_ label))))

(provide 'mevedel-view-animation)
;;; mevedel-view-animation.el ends here
