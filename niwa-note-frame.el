;;; niwa-note-frame.el -- Note frame surface -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Displays a note conversation in a floating child
;; frame anchored at the note's source position.  The frame shows the
;; real view buffer, so permissions, Ask, patch review, streaming, and the
;; composer work in it unmodified and no second renderer exists.
;;
;; At most one frame exists at a time.  Reuse keeps the exact
;; note identity or retargets every source field before display.
;; The frame is dismissed explicitly; teardown runs from
;; `delete-frame-functions' so every deletion path restores point.

;;; Code:

(require 'niwa-utilities)
(require 'niwa-theme-faces)

(eval-when-compile (require 'cl-lib))

;; `niwa-chat'

;; `niwa-instruction-registry'

;; `niwa-models'
(declare-function niwa-model-current-label
                  "niwa-models" (&optional buffer))
(autoload 'niwa-model-current-label "niwa-models")

;; `niwa-note-source'

;; `niwa-overlays'
(declare-function niwa--topmost-instruction
                  "niwa-overlays" (instruction &optional of-type pred))
(autoload 'niwa--topmost-instruction "niwa-overlays")

;; `niwa-structs'
;; `niwa-tools'
(declare-function niwa-tools-active-count "niwa-tools" (&optional buffer))
(autoload 'niwa-tools-active-count "niwa-tools")

;; `niwa-turn'
(declare-function niwa-request-state-label "niwa-turn" (&optional buffer))
(autoload 'niwa-request-state-label "niwa-turn")

;; `niwa-view'
(defvar niwa--data-buffer)
(defvar niwa--view-buffer)

;; `niwa-view-composer'
(declare-function niwa-view-composer-buffer "niwa-view-composer" (&optional view))

;; `niwa-view-pair'
(declare-function niwa-view-composer-display "niwa-view-pair" (view-window))
(declare-function niwa-view-composer-window "niwa-view-pair" (&optional window))
(declare-function niwa-view-transcript-window "niwa-view-pair" (&optional window))

;; `niwa-view-render'
(declare-function niwa-view--rendered-turn-starts "niwa-view-render" ())


;;
;;; Customization

(defcustom niwa-note-frame-width 0.6
  "Width of the note frame as a fraction of its parent frame."
  :type 'float
  :group 'niwa)

(defcustom niwa-note-frame-height 20
  "Height of the note frame in lines."
  :type 'integer
  :group 'niwa)

(defcustom niwa-note-frame-border-width 3
  "Border width of the note frame in pixels."
  :type 'integer
  :group 'niwa)

(defcustom niwa-note-frame-min-height 8
  "Smallest height of the note frame in lines.
The frame fits itself to its content between this and
`niwa-note-frame-height'."
  :type 'integer
  :group 'niwa)

(defface niwa-note-frame-border
  '((((background dark)) :background "#9a9a9a")
    (((background light)) :background "#5a5a5a"))
  "Border of the note frame while it has focus.
The background is blended from the active theme by
`niwa--derive-theme-faces'; the grey here is only the fallback for a
display whose colors cannot be measured.  The background attribute is
what paints; a face that inherits an unset background makes the border
invisible against the frame."
  :group 'niwa)

(defface niwa-note-frame-border-inactive
  '((((background dark)) :background "#555555")
    (((background light)) :background "#c0c0c0"))
  "Border of the note frame while another frame has focus.
Derived from the theme like `niwa-note-frame-border', but lighter."
  :group 'niwa)

(defun niwa-note-frame--register-border-faces ()
  "Derive both border faces from the active theme."
  (pcase-dolist (`(,face . ,intensity)
                 '((niwa-note-frame-border . 0.65)
                   (niwa-note-frame-border-inactive . 0.25)))
    (niwa--derive-theme-face
     face
     (lambda (foreground background)
       (list :background (niwa--tint background foreground intensity))))))

(niwa-note-frame--register-border-faces)

(defvar niwa-collaboration--share-frame)
(declare-function niwa-collaboration--paint-share-border
                  "niwa-collaboration-share" (frame))

(defun niwa-note-frame--repaint-borders (&rest _)
  "Repaint the shown note frame and share panel after a theme change."
  (when (frame-live-p niwa-note-frame--frame)
    (niwa-note-frame--paint-border niwa-note-frame--frame))
  (when (and (boundp 'niwa-collaboration--share-frame)
             (frame-live-p niwa-collaboration--share-frame))
    (niwa-collaboration--paint-share-border
     niwa-collaboration--share-frame)))

;; Appended after the derivation hooks, so the faces are current first.
(add-hook 'enable-theme-functions #'niwa-note-frame--repaint-borders)
(add-hook 'disable-theme-functions #'niwa-note-frame--repaint-borders)


;;
;;; State

(defvar niwa-note-frame--frame nil
  "The single live note frame, or nil.")

(defvar niwa-note-frame--note-id nil
  "Note id whose turns the live frame displays.")

(defvar niwa-note-frame--view-buffer nil
  "View buffer displayed by the live frame.")

(defvar niwa-note-frame--origin nil
  "Marker for the source position the live frame was opened from.")

(defvar niwa-note-frame--note nil
  "Note overlay the live frame is anchored to.")

(defvar niwa-note-frame--source-buffer nil
  "Source buffer carrying the live frame's follow hooks.")

(defvar niwa-note-frame--following nil
  "Non-nil while repositioning, so redisplay hooks cannot recurse.")

(defvar niwa-note-frame--pending-restore nil
  "Focus and point to restore once the deleted frame is actually gone.
A plist of :frame, :window, and :origin handed from
`delete-frame-functions' to `after-delete-frame-functions'.")

(defvar niwa-note-frame--origin-window nil
  "Window that was selected when the live frame was opened.")


(defconst niwa--child-frame-parameters
  '((min-width . t)
    (min-height . t)
    (border-width . 0)
    (outer-border-width . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (tab-bar-lines-keep-state . t)
    (no-other-frame . t)
    (unsplittable . t)
    (undecorated . t)
    (fullscreen . nil)
    (no-special-glyphs . t)
    (desktop-dont-save . t)
    (inhibit-double-buffering . t))
  "Parameters shared by niwa child frames.
Adapted from `corfu--frame-parameters'.  Unlike a completion popup this
frame accepts focus and shows a cursor, so `no-accept-focus',
`no-focus-on-map', and a nil `cursor-type' are deliberately absent.")


;;
;;; Availability

(defun niwa-note-frame--available-p ()
  "Return non-nil when a child frame can be created in this session.
Emacs 31 supports child frames on terminal displays as well as graphical
ones, so the Emacs version no longer decides this.  A batch session
still does: it has no terminal at all and `make-frame' signals there."
  (not noninteractive))


;;
;;; Frame refresh


(defun niwa-note-frame-refresh ()
  "Refit the note frame after a re-render.
Safe to call from any buffer and when no note frame is live.  Runs
on the view's throttled render cadence, not per streamed token, so the
frame grows with the answer instead of resizing on every chunk."
  (when (frame-live-p niwa-note-frame--frame)
    (niwa-note-frame--fit-height)
    ;; A frame flipped above its note grows upward, so refitting moves
    ;; where its top belongs.
    (niwa-note-frame--follow)))


;;
;;; Header line

(defun niwa-note-frame--header ()
  "Return the condensed header line for the note frame.
Leads with note identity, then the state that changes while the
conversation runs.  Session facts the parent frame's status strip
already shows -- session name, workspace root, execution target, preset
-- are deliberately absent: this header has a fraction of the width."
  (when (and (boundp 'niwa--data-buffer)
             (buffer-live-p niwa--data-buffer))
    (let* ((data-buffer niwa--data-buffer)
           (id niwa-note-frame--note-id)
           (state (niwa-request-state-label data-buffer))
           (model (niwa-model-current-label data-buffer))
           (tools (niwa-tools-active-count data-buffer)))
      (concat
       (propertize "◆ " 'face 'niwa-view-note-scope)
       (propertize (or id "note")
                   'face 'niwa-view-note-scope)
       (propertize " · " 'face 'shadow)
       (propertize (or state "idle") 'face 'shadow)
       (propertize " · " 'face 'shadow)
       (propertize (or model "model none") 'face 'shadow)
       (propertize (format " · %d tool%s · C-c C-z close"
                           tools (if (= tools 1) "" "s"))
                   'face 'shadow)))))


;;
;;; Frame geometry

(defun niwa-note-frame--source-window (note)
  "Return the window showing NOTE's source buffer, or nil.
Never returns a window inside the note frame: once that frame has
focus it would otherwise become its own parent, and re-displaying would
reparent the frame to the frame being replaced."
  (when (and (overlayp note)
             (buffer-live-p (overlay-buffer note)))
    (let* ((buffer (overlay-buffer note))
           (candidates
            (cl-loop for window in (get-buffer-window-list buffer nil t)
                     unless (and (frame-live-p niwa-note-frame--frame)
                                 (eq (window-frame window)
                                     niwa-note-frame--frame))
                     collect window)))
      ;; The same buffer can be on screen more than once.  Prefer the window
      ;; the user is actually in, then any window on the selected frame, and
      ;; only then whatever is left -- otherwise the frame anchors itself to
      ;; a copy of the note in a window nobody is looking at.
      (or (car (memq (selected-window) candidates))
          (cl-find-if (lambda (window)
                        (eq (window-frame window) (selected-frame)))
                      candidates)
          (car candidates)))))

(defun niwa-note-frame--place
    (anchor-x start-y end-y line-height width height parent-width parent-height)
  "Return the position (X . Y) for a WIDTH by HEIGHT note frame.
ANCHOR-X is the note's end column, START-Y the top of its first line,
and END-Y the top of its last line, all in pixels relative to a parent
of PARENT-WIDTH by PARENT-HEIGHT; LINE-HEIGHT is one line's height.

The frame goes below the note, else flush above its first line, so
the note stays readable.  Only when neither side has room does it
take the larger side and overlap the note."
  (let ((x (max 0 (min anchor-x (- parent-width width))))
        (below (+ end-y line-height))
        (above (- start-y height)))
    (cons x
          (cond
           ((<= (+ below height) parent-height) below)
           ((>= above 0) above)
           ((> (- parent-height below) start-y)
            (max 0 (- parent-height height)))
           (t 0)))))

(defun niwa-note-frame--anchor (note window &optional height)
  "Return the pixel position (X . Y) to place NOTE's frame in WINDOW.
Returns nil when NOTE has no visible source position, in which case
the caller centers the frame on its parent.  HEIGHT defaults to the live
frame's actual height, so a frame fitted shorter than the maximum still
sits flush against its note."
  (when (and (window-live-p window)
             (overlayp note)
             (buffer-live-p (overlay-buffer note)))
    (when-let* ((position (window-absolute-pixel-position
                           (overlay-end note) window)))
      (let* ((frame (window-frame window))
             ;; `window-absolute-pixel-position' answers in display
             ;; coordinates while a child frame's `left' and `top' are
             ;; relative to its parent's native frame.  Without this
             ;; translation the anchor carries the parent's own screen
             ;; position, and the clamp below pins the frame to the right
             ;; edge of any Emacs frame not sitting at the display origin.
             ;; Terminal frames report a native origin of (0, 0), so the
             ;; subtraction is inert there.
             (native (frame-edges frame 'native-edges))
             (start (window-absolute-pixel-position
                     (overlay-start note) window))
             (parent-width (frame-pixel-width frame))
             (child niwa-note-frame--frame))
        (niwa-note-frame--place
         (- (car position) (nth 0 native))
         ;; A first line scrolled above the window leaves the window's top
         ;; as the note's highest visible edge; body edges are already
         ;; relative to the native frame.
         (if start
             (- (cdr start) (nth 1 native))
           (nth 1 (window-body-pixel-edges window)))
         (- (cdr position) (nth 1 native))
         (frame-char-height frame)
         (round (* niwa-note-frame-width parent-width))
         (cond (height)
               ((frame-live-p child) (frame-pixel-height child))
               (t (* niwa-note-frame-height
                     (frame-char-height frame))))
         parent-width
         (frame-pixel-height frame))))))

(defun niwa-note-frame--size (parent)
  "Return the pixel size (WIDTH . HEIGHT) of a frame parented to PARENT."
  (cons (round (* niwa-note-frame-width (frame-pixel-width parent)))
        (* niwa-note-frame-height (frame-char-height parent))))

(defun niwa-note-frame--make (buffer parent)
  "Return a child frame of PARENT displaying BUFFER.
Reuses the live note frame when it still matches PARENT and the
display type, and recreates it otherwise."
  (let* ((graphic (display-graphic-p parent))
         (frame niwa-note-frame--frame)
         (params
          `((parent-frame . ,parent)
            (minibuffer . ,(minibuffer-window parent))
            (font . ,(frame-parameter parent 'font))
            (internal-border-width . ,niwa-note-frame-border-width)
            (child-frame-border-width
             . ,niwa-note-frame-border-width)
            ,@niwa--child-frame-parameters)))
    (unless (and (frame-live-p frame)
                 (eq (frame-parent frame) parent)
                 (eq graphic (display-graphic-p frame))
                 (window-live-p (frame-first-window frame)))
      (when (frame-live-p frame) (delete-frame frame))
      (setq frame
            (make-frame
             `((name . "niwa-note")
               (width . 0) (height . 0) (visibility . nil)
               ,@params))))
    (let ((window (frame-first-window frame)))
      (set-window-dedicated-p window nil)
      (set-window-buffer window buffer)
      (set-window-parameter window 'no-delete-other-windows t)
      (set-window-dedicated-p window t)
      ;; Window parameters rather than buffer-local settings: the view buffer
      ;; is shared with the main view, which keeps its own mode line and its
      ;; full-width status strip.  The condensed header takes the strip's tab
      ;; line; the buffer's pinned prompt row stays below it, as in the view.
      (set-window-parameter window 'mode-line-format 'none)
      (set-window-parameter window 'tab-line-format
                            '(:eval (niwa-note-frame--header)))
      (set-window-parameter window 'header-line-format nil)
      (set-window-fringes window 0 0))
    (niwa-note-frame--paint-border frame)
    frame))

(defun niwa-note-frame--paint-border (&optional frame focused)
  "Paint FRAME's border to show whether it holds focus.
FOCUSED, a function of FRAME, decides that; it defaults to
`frame-focus-state'.  An unchanged color is not set again: that would
refresh FRAME's faces for nothing.
The border width alone draws nothing: without an explicit background on
`internal-border' and `child-frame-border' the border takes the default
background and is invisible against the frame.  A terminal draws a child
frame's border as box characters in that frame's own `border' face, so
there the color is its foreground.  A shown terminal child's box changes
only when its parent is redrawn; that waits for a timer, outside
redisplay, and happens only when the color changed."
  (let ((frame (or frame niwa-note-frame--frame)))
    (when (frame-live-p frame)
      (let ((color (face-attribute
                    (if (funcall (or focused #'frame-focus-state) frame)
                        'niwa-note-frame-border
                      'niwa-note-frame-border-inactive)
                    :background nil 'default)))
        (cond
         ((not (display-graphic-p frame))
          (unless (equal color (face-foreground 'border frame))
            (set-face-foreground 'border color frame)
            (when-let* ((parent (frame-parent frame)))
              (run-at-time 0 nil (lambda ()
                                   (when (frame-live-p parent)
                                     (redraw-frame parent)))))))
         ((not (and (equal color (face-background 'child-frame-border frame))
                    (equal color (face-background 'internal-border frame))))
          (set-face-background 'internal-border color frame)
          (set-face-background 'child-frame-border color frame)))))))

(add-function :after after-focus-change-function
              #'niwa-note-frame--paint-border)

(defun niwa-note-frame--follow (&optional window _start)
  "Keep the note frame aligned with its note as the source scrolls.
Runs from `window-scroll-functions' and `window-configuration-change-hook'
in the note's source buffer.  The frame hides once the note
leaves the window and returns when it scrolls back, so the note and
its frame scroll as one thing.

WINDOW is supplied by `window-scroll-functions'."
  (unless niwa-note-frame--following
    (let ((niwa-note-frame--following t)
          (frame niwa-note-frame--frame)
          (note niwa-note-frame--note))
      (cond
       ((not (frame-live-p frame))
        (niwa-note-frame--unfollow))
       ((not (and (overlayp note)
                  (buffer-live-p (overlay-buffer note))))
        (when (frame-visible-p frame) (make-frame-invisible frame)))
       (t
        ;; Stay with the window the frame anchored to.  When the note's
        ;; buffer is on screen twice, scrolling the other copy must not drag
        ;; the frame across to it.
        (let ((source
               (if (and (window-live-p niwa-note-frame--origin-window)
                        (eq (window-buffer
                             niwa-note-frame--origin-window)
                            (overlay-buffer note)))
                   niwa-note-frame--origin-window
                 (setq niwa-note-frame--origin-window
                       (niwa-note-frame--source-window note)))))
          (if (not (and (window-live-p source)
                        ;; nil WINDOW is the configuration-change hook, which
                        ;; is about the layout rather than one window.
                        (or (null window) (eq window source))
                        (pos-visible-in-window-p
                         (overlay-end note) source)))
              (when (and (frame-visible-p frame)
                         (or (null window) (eq window source)))
                (make-frame-invisible frame))
            (when-let* ((anchor (niwa-note-frame--anchor
                                 note source)))
              ;; Only move on a real change: setting the position from a
              ;; redisplay hook triggers redisplay again.  Compare against
              ;; the frame's own parameters, which `set-frame-position'
              ;; wrote and which share the anchor's parent-relative space;
              ;; `frame-position' answers in display coordinates and would
              ;; report a difference on every single scroll event.
              (let ((left (frame-parameter frame 'left))
                    (top (frame-parameter frame 'top)))
                (unless (and (numberp left)
                             (numberp top)
                             (= (car anchor) left)
                             (= (cdr anchor) top))
                  (set-frame-position frame (car anchor) (cdr anchor)))))
            (unless (frame-visible-p frame)
              (make-frame-visible frame)))))))))

(defun niwa-note-frame--follow-setup (note)
  "Track NOTE's source buffer so the frame scrolls with it."
  (when (and (overlayp note)
             (buffer-live-p (overlay-buffer note)))
    (niwa-note-frame--unfollow)
    (setq niwa-note-frame--note note
          niwa-note-frame--source-buffer (overlay-buffer note))
    (with-current-buffer (overlay-buffer note)
      (add-hook 'window-scroll-functions
                #'niwa-note-frame--follow nil t)
      (add-hook 'window-configuration-change-hook
                #'niwa-note-frame--follow nil t))))

(defun niwa-note-frame--unfollow ()
  "Stop tracking the source buffer and forget the anchored note."
  (when (buffer-live-p niwa-note-frame--source-buffer)
    (with-current-buffer niwa-note-frame--source-buffer
      (remove-hook 'window-scroll-functions
                   #'niwa-note-frame--follow t)
      (remove-hook 'window-configuration-change-hook
                   #'niwa-note-frame--follow t)))
  (setq niwa-note-frame--source-buffer nil
        niwa-note-frame--note nil))

(defun niwa-note-frame--fit-height ()
  "Fit the note frame's height to its content.
Bounded by `niwa-note-frame-min-height' and
`niwa-note-frame-height' so a one-line answer does not collapse
the frame and a long one does not cover the buffer behind it."
  (when-let* ((frame niwa-note-frame--frame)
              ((frame-live-p frame)))
    (let* ((window (or (niwa-view-transcript-window (frame-first-window frame))
                       (frame-first-window frame)))
           (composer (niwa-view-composer-window window)))
      (if (not composer)
          (fit-frame-to-buffer frame niwa-note-frame-height
                               niwa-note-frame-min-height nil nil 'vertically)
        ;; `fit-frame-to-buffer' only fits a frame with a single window.
        (let* ((row (frame-char-height frame))
               (limit (* row niwa-note-frame-height))
               (content (cdr (window-text-pixel-size window t t nil limit t)))
               (height (max (* row niwa-note-frame-min-height)
                            (min limit (+ content (window-pixel-height composer))))))
          (unless (= height (frame-text-height frame))
            (set-frame-height frame height nil t)))))))


;;
;;; Display

;;;###autoload
(defun niwa-note-frame-display (note view-buffer &optional focus)
  "Display VIEW-BUFFER in a note frame anchored at NOTE.
With FOCUS non-nil the frame is selected, which is what an explicit user
action wants; a request dispatch leaves focus where it is.  Falls back to
an ordinary right-hand split when child frames are unavailable."
  (unless (and (overlayp note)
               (eq (overlay-get note 'niwa-instruction-type) 'note))
    (user-error "Select a note to open its conversation"))
  (let ((note-id (format "Note %s" (overlay-get note 'niwa-id))))
    (unless (eq niwa-note-frame--view-buffer view-buffer)
      (niwa-note-frame--set-mode niwa-note-frame--view-buffer -1))
    (cond
     ((or (not (niwa-note-frame--available-p))
          (not (display-graphic-p)))
      ;; A native side window cannot contain the two-window dock.
      (when-let* ((window (display-buffer
                           view-buffer
                           '((display-buffer-reuse-window display-buffer-in-direction)
                             (direction . right) (window . main) (window-width . 0.5)))))
        (niwa-view-composer-display window)
        (when focus (select-window window))))
     ;; Reopening the same conversation must not move the frame under its reader.
     ((and (frame-live-p niwa-note-frame--frame)
           (eq niwa-note-frame--view-buffer view-buffer)
           (equal niwa-note-frame--note-id note-id))
      (make-frame-visible niwa-note-frame--frame)
      (when focus
        (select-frame-set-input-focus niwa-note-frame--frame))
      niwa-note-frame--frame)
     (t
      (let* ((source (niwa-note-frame--source-window note))
             (parent (if (window-live-p source)
                         (window-frame source)
                       (window-frame (selected-window))))
             (size (niwa-note-frame--size parent))
             (anchor (niwa-note-frame--anchor
                      note source (cdr size)))
             (frame (niwa-note-frame--make view-buffer parent)))
        (unless (eq frame niwa-note-frame--frame)
          (setq niwa-note-frame--frame frame))
        (setq niwa-note-frame--view-buffer view-buffer
              niwa-note-frame--note-id note-id
              niwa-note-frame--origin-window
              (if (window-live-p source) source (selected-window)))
        (when (and (overlayp note)
                   (buffer-live-p (overlay-buffer note)))
          (setq niwa-note-frame--origin
                (copy-marker (overlay-start note))))
        (set-frame-size frame (car size) (cdr size) t)
        (niwa-view-composer-display (frame-first-window frame))
        (set-frame-position frame
                            (or (car anchor)
                                (/ (- (frame-pixel-width parent)
                                      (car size))
                                   2))
                            (or (cdr anchor)
                                (/ (- (frame-pixel-height parent)
                                      (cdr size))
                                   2)))
        (make-frame-visible frame)
        (niwa-note-frame--set-mode view-buffer 1)
        (niwa-note-frame--follow-setup note)
        (when focus (select-frame-set-input-focus frame))
        frame)))))

(defun niwa-note-frame-close ()
  "Dismiss the note frame."
  (interactive)
  (unless (frame-live-p niwa-note-frame--frame)
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No note frame is open"))
  (delete-frame niwa-note-frame--frame))


;;
;;; Teardown

(defun niwa-note-frame--on-delete (frame)
  "Release note frame state when FRAME is deleted.
Runs from `delete-frame-functions' so that every deletion path -- the
dismiss command, \\[delete-frame], or Emacs exit -- restores point."
  (when (eq frame niwa-note-frame--frame)
    (let* ((view-buffer niwa-note-frame--view-buffer)
           (origin niwa-note-frame--origin)
           (window niwa-note-frame--origin-window)
           (parent (or (frame-parent frame)
                       (and (window-live-p window) (window-frame window)))))
      ;; Hand input focus back before the frame dies.  This hook runs while
      ;; FRAME is still live and still holds focus; deleting a focused child
      ;; frame without moving focus first leaves no frame focused, and Emacs
      ;; stops responding to the keyboard.
      (when (and (frame-live-p parent) (not (eq parent frame)))
        (select-frame-set-input-focus parent))
      (niwa-note-frame--unfollow)
      (niwa-note-frame--set-mode view-buffer -1)
      (setq niwa-note-frame--frame nil
            niwa-note-frame--view-buffer nil
            niwa-note-frame--note-id nil
            niwa-note-frame--origin nil
            niwa-note-frame--origin-window nil)
      ;; Point and the cursor are settled after the frame is actually gone.
      ;; This hook runs before deletion, so anything done here about focus is
      ;; provisional.
      (setq niwa-note-frame--pending-restore
            (list :frame parent :window window :origin origin)))))

(defun niwa-note-frame--after-delete (&optional _frame)
  "Restore point and the cursor once the note frame is gone.
Runs from `after-delete-frame-functions'.  Re-asserting focus before
deletion is not enough: the window system reports focus back to the
parent asynchronously, so until Emacs processes that event the parent
draws no cursor -- it reappears only when the next key arrives.  Focusing
again after deletion and forcing one redisplay settles it immediately."
  (when-let* ((pending niwa-note-frame--pending-restore))
    (setq niwa-note-frame--pending-restore nil)
    (let ((parent (plist-get pending :frame))
          (window (plist-get pending :window))
          (origin (plist-get pending :origin)))
      (when (frame-live-p parent)
        (select-frame-set-input-focus parent))
      (when (window-live-p window)
        (select-window window)
        (when (and (markerp origin)
                   (eq (marker-buffer origin) (window-buffer window)))
          (goto-char origin)))
      (when (markerp origin) (set-marker origin nil))
      (when (frame-live-p parent)
        (redisplay t)))))

(add-hook 'after-delete-frame-functions
          #'niwa-note-frame--after-delete)

(add-hook 'delete-frame-functions #'niwa-note-frame--on-delete)


;;
;;; Keys

(defvar-keymap niwa-note-frame-mode-map
  :doc "Keymap active in the transcript and composer of the note frame."
  "C-c C-z" #'niwa-note-frame-close)

(defun niwa-note-frame--display-in-parent (buffer alist)
  "Display BUFFER in the note frame's parent frame honoring ALIST.
The note frame is too short for menus, cockpits and other popups.  Redirect
at the display layer so every popup gets the same treatment."
  (when-let* ((frame niwa-note-frame--frame)
              ((frame-live-p frame))
              ((eq frame (selected-frame)))
              (parent (frame-parent frame))
              ((frame-live-p parent)))
    (let ((window
           (with-selected-frame parent
             ;; Clear the override inside, or display-buffer recurses into
             ;; this same function.  ALIST must be re-wrapped as an action:
             ;; display-buffer's second argument is (FUNCTIONS . ALIST) and
             ;; passing a bare alist makes Emacs read its first entry as an
             ;; action function.
             (let ((display-buffer-overriding-action nil))
               (display-buffer buffer (cons nil alist))))))
      (when (window-live-p window)
        ;; Hand input focus over with the buffer.  Callers like
        ;; `pop-to-buffer' select the window this returns; without moving
        ;; focus too, the selected window and the focused frame disagree and
        ;; typing still goes to the note frame.
        (select-frame-set-input-focus parent)
        (select-window window))
      window)))

(define-minor-mode niwa-note-frame-mode
  "Minor mode for buffers displayed by the note frame."
  :lighter ""
  :keymap niwa-note-frame-mode-map
  :interactive nil
  (if niwa-note-frame-mode
      (setq-local display-buffer-overriding-action
                  '(niwa-note-frame--display-in-parent . nil))
    (kill-local-variable 'display-buffer-overriding-action)))

(defun niwa-note-frame--set-mode (view arg)
  "Set the note-frame mode of VIEW and its composer according to ARG."
  (when (buffer-live-p view)
    (with-current-buffer view (niwa-note-frame-mode arg))
    (when-let* ((composer (niwa-view-composer-buffer view)))
      (with-current-buffer composer (niwa-note-frame-mode arg)))))

(provide 'niwa-note-frame)

;;; niwa-note-frame.el ends here
