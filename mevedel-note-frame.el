;;; mevedel-note-frame.el -- Note frame surface -*- lexical-binding: t -*-

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

(require 'mevedel-utilities)

(eval-when-compile (require 'cl-lib))

;; `mevedel-chat'

;; `mevedel-instruction-registry'

;; `mevedel-models'
(declare-function mevedel-model-current-label
                  "mevedel-models" (&optional buffer))
(autoload 'mevedel-model-current-label "mevedel-models")

;; `mevedel-note-source'

;; `mevedel-overlays'
(declare-function mevedel--topmost-instruction
                  "mevedel-overlays" (instruction &optional of-type pred))
(autoload 'mevedel--topmost-instruction "mevedel-overlays")

;; `mevedel-structs'
;; `mevedel-tools'
(declare-function mevedel-tools-active-count "mevedel-tools" (&optional buffer))
(autoload 'mevedel-tools-active-count "mevedel-tools")

;; `mevedel-turn'
(declare-function mevedel-request-state-label "mevedel-turn" (&optional buffer))
(autoload 'mevedel-request-state-label "mevedel-turn")

;; `mevedel-view'
(defvar mevedel--data-buffer)
(defvar mevedel--view-buffer)

;; `mevedel-view-composer'
(declare-function mevedel-view--input-marker-position "mevedel-view-composer" ())


;; `mevedel-view-render'
(declare-function mevedel-view--rendered-turn-starts "mevedel-view-render" ())


;;
;;; Customization

(defcustom mevedel-note-frame-width 0.6
  "Width of the note frame as a fraction of its parent frame."
  :type 'float
  :group 'mevedel)

(defcustom mevedel-note-frame-height 20
  "Height of the note frame in lines."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-note-frame-border-width 2
  "Border width of the note frame in pixels."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-note-frame-min-height 8
  "Smallest height of the note frame in lines.
The frame fits itself to its content between this and
`mevedel-note-frame-height'."
  :type 'integer
  :group 'mevedel)

(defface mevedel-note-frame-border
  '((((background dark)) :background "#5c7a80")
    (((background light)) :background "#7d979d"))
  "Border of the note frame while it has focus.
The background attribute is what paints; a face that inherits an unset
background makes the border invisible against the frame."
  :group 'mevedel)

(defface mevedel-note-frame-border-inactive
  '((((background dark)) :background "#2d383b")
    (((background light)) :background "#c6d2d5"))
  "Border of the note frame while another frame has focus."
  :group 'mevedel)


;;
;;; State

(defvar mevedel-note-frame--frame nil
  "The single live note frame, or nil.")

(defvar mevedel-note-frame--note-id nil
  "Note id whose turns the live frame displays.")

(defvar mevedel-note-frame--view-buffer nil
  "View buffer displayed by the live frame.")

(defvar mevedel-note-frame--origin nil
  "Marker for the source position the live frame was opened from.")

(defvar mevedel-note-frame--note nil
  "Note overlay the live frame is anchored to.")

(defvar mevedel-note-frame--source-buffer nil
  "Source buffer carrying the live frame's follow hooks.")

(defvar mevedel-note-frame--following nil
  "Non-nil while repositioning, so redisplay hooks cannot recurse.")

(defvar mevedel-note-frame--pending-restore nil
  "Focus and point to restore once the deleted frame is actually gone.
A plist of :frame, :window, and :origin handed from
`delete-frame-functions' to `after-delete-frame-functions'.")

(defvar mevedel-note-frame--origin-window nil
  "Window that was selected when the live frame was opened.")


(defconst mevedel--child-frame-parameters
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
  "Parameters shared by mevedel child frames.
Adapted from `corfu--frame-parameters'.  Unlike a completion popup this
frame accepts focus and shows a cursor, so `no-accept-focus',
`no-focus-on-map', and a nil `cursor-type' are deliberately absent.")


;;
;;; Availability

(defun mevedel-note-frame--available-p ()
  "Return non-nil when a child frame can be created in this session.
Emacs 31 supports child frames on terminal displays as well as graphical
ones, so the Emacs version no longer decides this.  A batch session
still does: it has no terminal at all and `make-frame' signals there."
  (not noninteractive))


;;
;;; Frame refresh


(defun mevedel-note-frame-refresh ()
  "Refit the note frame after a re-render.
Safe to call from any buffer and when no note frame is live.  Runs
on the view's throttled render cadence, not per streamed token, so the
frame grows with the answer instead of resizing on every chunk."
  (when (frame-live-p mevedel-note-frame--frame)
    (mevedel-note-frame--fit-height)
    ;; A frame flipped above its note grows upward, so refitting moves
    ;; where its top belongs.
    (mevedel-note-frame--follow)))


;;
;;; Header line

(defun mevedel-note-frame--header ()
  "Return the condensed header line for the note frame.
Leads with note identity, then the state that changes while the
conversation runs.  Session facts the parent frame's status strip
already shows -- session name, workspace root, execution target, preset
-- are deliberately absent: this header has a fraction of the width."
  (when (and (boundp 'mevedel--data-buffer)
             (buffer-live-p mevedel--data-buffer))
    (let* ((data-buffer mevedel--data-buffer)
           (id mevedel-note-frame--note-id)
           (state (mevedel-request-state-label data-buffer))
           (model (mevedel-model-current-label data-buffer))
           (tools (mevedel-tools-active-count data-buffer)))
      (concat
       (propertize "◆ " 'face 'mevedel-view-note-scope)
       (propertize (or id "note")
                   'face 'mevedel-view-note-scope)
       (propertize " · " 'face 'shadow)
       (propertize (or state "idle") 'face 'shadow)
       (propertize " · " 'face 'shadow)
       (propertize (or model "model none") 'face 'shadow)
       (propertize (format " · %d tool%s" tools (if (= tools 1) "" "s"))
                   'face 'shadow)))))


;;
;;; Frame geometry

(defun mevedel-note-frame--source-window (note)
  "Return the window showing NOTE's source buffer, or nil.
Never returns a window inside the note frame: once that frame has
focus it would otherwise become its own parent, and re-displaying would
reparent the frame to the frame being replaced."
  (when (and (overlayp note)
             (buffer-live-p (overlay-buffer note)))
    (let* ((buffer (overlay-buffer note))
           (candidates
            (cl-loop for window in (get-buffer-window-list buffer nil t)
                     unless (and (frame-live-p mevedel-note-frame--frame)
                                 (eq (window-frame window)
                                     mevedel-note-frame--frame))
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

(defun mevedel-note-frame--place
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

(defun mevedel-note-frame--anchor (note window &optional height)
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
             (child mevedel-note-frame--frame))
        (mevedel-note-frame--place
         (- (car position) (nth 0 native))
         ;; A first line scrolled above the window leaves the window's top
         ;; as the note's highest visible edge; body edges are already
         ;; relative to the native frame.
         (if start
             (- (cdr start) (nth 1 native))
           (nth 1 (window-body-pixel-edges window)))
         (- (cdr position) (nth 1 native))
         (frame-char-height frame)
         (round (* mevedel-note-frame-width parent-width))
         (cond (height)
               ((frame-live-p child) (frame-pixel-height child))
               (t (* mevedel-note-frame-height
                     (frame-char-height frame))))
         parent-width
         (frame-pixel-height frame))))))

(defun mevedel-note-frame--size (parent)
  "Return the pixel size (WIDTH . HEIGHT) of a frame parented to PARENT."
  (cons (round (* mevedel-note-frame-width (frame-pixel-width parent)))
        (* mevedel-note-frame-height (frame-char-height parent))))

(defun mevedel-note-frame--make (buffer parent)
  "Return a child frame of PARENT displaying BUFFER.
Reuses the live note frame when it still matches PARENT and the
display type, and recreates it otherwise."
  (let* ((graphic (display-graphic-p parent))
         (frame mevedel-note-frame--frame)
         (params
          `((parent-frame . ,parent)
            (minibuffer . ,(minibuffer-window parent))
            (font . ,(frame-parameter parent 'font))
            (internal-border-width . ,mevedel-note-frame-border-width)
            (child-frame-border-width
             . ,mevedel-note-frame-border-width)
            ,@mevedel--child-frame-parameters)))
    (unless (and (frame-live-p frame)
                 (eq (frame-parent frame) parent)
                 (eq graphic (display-graphic-p frame))
                 (window-live-p (frame-root-window frame)))
      (when (frame-live-p frame) (delete-frame frame))
      (setq frame
            (make-frame
             `((name . "mevedel-note")
               (width . 0) (height . 0) (visibility . nil)
               ,@params))))
    (let ((window (frame-root-window frame)))
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
                            '(:eval (mevedel-note-frame--header)))
      (set-window-parameter window 'header-line-format nil)
      (set-window-fringes window 0 0))
    (mevedel-note-frame--paint-border frame)
    frame))

(defun mevedel-note-frame--paint-border (&optional frame)
  "Paint FRAME's border to show whether it holds focus.
The border width alone draws nothing: without an explicit background on
`internal-border' and `child-frame-border' the border takes the default
background and is invisible against the frame."
  (let ((frame (or frame mevedel-note-frame--frame)))
    (when (frame-live-p frame)
      (let ((color (face-attribute
                    (if (frame-focus-state frame)
                        'mevedel-note-frame-border
                      'mevedel-note-frame-border-inactive)
                    :background nil 'default)))
        (set-face-background 'internal-border color frame)
        (set-face-background 'child-frame-border color frame)))))

(add-function :after after-focus-change-function
              #'mevedel-note-frame--paint-border)

(defun mevedel-note-frame--follow (&optional window _start)
  "Keep the note frame aligned with its note as the source scrolls.
Runs from `window-scroll-functions' and `window-configuration-change-hook'
in the note's source buffer.  The frame hides once the note
leaves the window and returns when it scrolls back, so the note and
its frame scroll as one thing.

WINDOW is supplied by `window-scroll-functions'."
  (unless mevedel-note-frame--following
    (let ((mevedel-note-frame--following t)
          (frame mevedel-note-frame--frame)
          (note mevedel-note-frame--note))
      (cond
       ((not (frame-live-p frame))
        (mevedel-note-frame--unfollow))
       ((not (and (overlayp note)
                  (buffer-live-p (overlay-buffer note))))
        (when (frame-visible-p frame) (make-frame-invisible frame)))
       (t
        ;; Stay with the window the frame anchored to.  When the note's
        ;; buffer is on screen twice, scrolling the other copy must not drag
        ;; the frame across to it.
        (let ((source
               (if (and (window-live-p mevedel-note-frame--origin-window)
                        (eq (window-buffer
                             mevedel-note-frame--origin-window)
                            (overlay-buffer note)))
                   mevedel-note-frame--origin-window
                 (setq mevedel-note-frame--origin-window
                       (mevedel-note-frame--source-window note)))))
          (if (not (and (window-live-p source)
                        ;; nil WINDOW is the configuration-change hook, which
                        ;; is about the layout rather than one window.
                        (or (null window) (eq window source))
                        (pos-visible-in-window-p
                         (overlay-end note) source)))
              (when (and (frame-visible-p frame)
                         (or (null window) (eq window source)))
                (make-frame-invisible frame))
            (when-let* ((anchor (mevedel-note-frame--anchor
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

(defun mevedel-note-frame--follow-setup (note)
  "Track NOTE's source buffer so the frame scrolls with it."
  (when (and (overlayp note)
             (buffer-live-p (overlay-buffer note)))
    (mevedel-note-frame--unfollow)
    (setq mevedel-note-frame--note note
          mevedel-note-frame--source-buffer (overlay-buffer note))
    (with-current-buffer (overlay-buffer note)
      (add-hook 'window-scroll-functions
                #'mevedel-note-frame--follow nil t)
      (add-hook 'window-configuration-change-hook
                #'mevedel-note-frame--follow nil t))))

(defun mevedel-note-frame--unfollow ()
  "Stop tracking the source buffer and forget the anchored note."
  (when (buffer-live-p mevedel-note-frame--source-buffer)
    (with-current-buffer mevedel-note-frame--source-buffer
      (remove-hook 'window-scroll-functions
                   #'mevedel-note-frame--follow t)
      (remove-hook 'window-configuration-change-hook
                   #'mevedel-note-frame--follow t)))
  (setq mevedel-note-frame--source-buffer nil
        mevedel-note-frame--note nil))

(defun mevedel-note-frame--fit-height ()
  "Fit the note frame's height to its content.
Bounded by `mevedel-note-frame-min-height' and
`mevedel-note-frame-height' so a one-line answer does not collapse
the frame and a long one does not cover the buffer behind it."
  (when (frame-live-p mevedel-note-frame--frame)
    ;; `vertically' matters: fitting both dimensions widens the frame to the
    ;; longest unwrapped line in the transcript, which readily exceeds the
    ;; parent frame.  Width stays as computed from the parent at open.
    (fit-frame-to-buffer mevedel-note-frame--frame
                         mevedel-note-frame-height
                         mevedel-note-frame-min-height
                         nil nil 'vertically)))


;;
;;; Display

;;;###autoload
(defun mevedel-note-frame-display (note view-buffer &optional focus)
  "Display VIEW-BUFFER in a note frame anchored at NOTE.
With FOCUS non-nil the frame is selected, which is what an explicit user
action wants; a request dispatch leaves focus where it is.  Falls back to
an ordinary window when child frames are unavailable."
  (unless (and (overlayp note)
               (eq (overlay-get note 'mevedel-instruction-type) 'note))
    (user-error "Select a note to open its conversation"))
  (let ((note-id (format "Note %s" (overlay-get note 'mevedel-id))))
    (unless (eq mevedel-note-frame--view-buffer view-buffer)
      (when (buffer-live-p mevedel-note-frame--view-buffer)
        (with-current-buffer mevedel-note-frame--view-buffer
          (mevedel-note-frame-mode -1))))
    (cond
     ((or (not (mevedel-note-frame--available-p))
          (not (display-graphic-p)))
      (display-buffer view-buffer
                      '(display-buffer-in-side-window
                        (side . right) (window-width . 0.5)))
      (when focus (select-window (get-buffer-window view-buffer t))))
     ;; Reopening the same conversation must not move the frame under its reader.
     ((and (frame-live-p mevedel-note-frame--frame)
           (eq mevedel-note-frame--view-buffer view-buffer)
           (equal mevedel-note-frame--note-id note-id))
      (make-frame-visible mevedel-note-frame--frame)
      (when focus
        (select-frame-set-input-focus mevedel-note-frame--frame))
      mevedel-note-frame--frame)
     (t
      (let* ((source (mevedel-note-frame--source-window note))
             (parent (if (window-live-p source)
                         (window-frame source)
                       (window-frame (selected-window))))
             (size (mevedel-note-frame--size parent))
             (anchor (mevedel-note-frame--anchor
                      note source (cdr size)))
             (frame (mevedel-note-frame--make view-buffer parent)))
        (unless (eq frame mevedel-note-frame--frame)
          (setq mevedel-note-frame--frame frame))
        (setq mevedel-note-frame--view-buffer view-buffer
              mevedel-note-frame--note-id note-id
              mevedel-note-frame--origin-window
              (if (window-live-p source) source (selected-window)))
        (when (and (overlayp note)
                   (buffer-live-p (overlay-buffer note)))
          (setq mevedel-note-frame--origin
                (copy-marker (overlay-start note))))
        (set-frame-size frame (car size) (cdr size) t)
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
        (with-current-buffer view-buffer (mevedel-note-frame-mode 1))
        (mevedel-note-frame--follow-setup note)
        (when focus (select-frame-set-input-focus frame))
        frame)))))

(defun mevedel-note-frame-close ()
  "Dismiss the note frame."
  (interactive)
  (unless (frame-live-p mevedel-note-frame--frame)
    (mevedel-diagnostic-user-error (mevedel-diagnostic-scope (current-buffer)) "No note frame is open"))
  (delete-frame mevedel-note-frame--frame))


;;
;;; Teardown

(defun mevedel-note-frame--on-delete (frame)
  "Release note frame state when FRAME is deleted.
Runs from `delete-frame-functions' so that every deletion path -- the
dismiss command, \\[delete-frame], or Emacs exit -- restores point."
  (when (eq frame mevedel-note-frame--frame)
    (let* ((view-buffer mevedel-note-frame--view-buffer)
           (origin mevedel-note-frame--origin)
           (window mevedel-note-frame--origin-window)
           (parent (or (frame-parent frame)
                       (and (window-live-p window) (window-frame window)))))
      ;; Hand input focus back before the frame dies.  This hook runs while
      ;; FRAME is still live and still holds focus; deleting a focused child
      ;; frame without moving focus first leaves no frame focused, and Emacs
      ;; stops responding to the keyboard.
      (when (and (frame-live-p parent) (not (eq parent frame)))
        (select-frame-set-input-focus parent))
      (mevedel-note-frame--unfollow)
      (when (buffer-live-p view-buffer)
        (with-current-buffer view-buffer
          (mevedel-note-frame-mode -1)))
      (setq mevedel-note-frame--frame nil
            mevedel-note-frame--view-buffer nil
            mevedel-note-frame--note-id nil
            mevedel-note-frame--origin nil
            mevedel-note-frame--origin-window nil)
      ;; Point and the cursor are settled after the frame is actually gone.
      ;; This hook runs before deletion, so anything done here about focus is
      ;; provisional.
      (setq mevedel-note-frame--pending-restore
            (list :frame parent :window window :origin origin)))))

(defun mevedel-note-frame--after-delete (&optional _frame)
  "Restore point and the cursor once the note frame is gone.
Runs from `after-delete-frame-functions'.  Re-asserting focus before
deletion is not enough: the window system reports focus back to the
parent asynchronously, so until Emacs processes that event the parent
draws no cursor -- it reappears only when the next key arrives.  Focusing
again after deletion and forcing one redisplay settles it immediately."
  (when-let* ((pending mevedel-note-frame--pending-restore))
    (setq mevedel-note-frame--pending-restore nil)
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
          #'mevedel-note-frame--after-delete)

(add-hook 'delete-frame-functions #'mevedel-note-frame--on-delete)


;;
;;; Keys

(defvar-keymap mevedel-note-frame-mode-map
  :doc "Keymap active in a buffer displayed by the note frame.
Bindings are prefixed because the view buffer holds an editable composer,
where a single-letter binding would prevent typing.  `C-g' is left alone;
in a view buffer it aborts the request."
  "C-c C-z" #'mevedel-note-frame-close)

(defun mevedel-note-frame--display-in-parent (buffer alist)
  "Display BUFFER in the note frame's parent frame honoring ALIST.
The frame's root window is dedicated and unsplittable and the frame is a
few lines tall, so anything popped from it -- the transient menu, a
cockpit surface, the patch buffer -- is unusable inside it.  Redirecting
at the display layer covers every such surface, including ones that do
not exist yet."
  (when-let* ((frame mevedel-note-frame--frame)
              ((frame-live-p frame))
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

(define-minor-mode mevedel-note-frame-mode
  "Minor mode for the view buffer while a note frame displays it."
  :lighter ""
  :keymap mevedel-note-frame-mode-map
  :interactive nil
  (if mevedel-note-frame-mode
      (setq-local display-buffer-overriding-action
                  '(mevedel-note-frame--display-in-parent . nil))
    (kill-local-variable 'display-buffer-overriding-action)))

(provide 'mevedel-note-frame)

;;; mevedel-note-frame.el ends here
