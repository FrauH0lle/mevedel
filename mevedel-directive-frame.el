;;; mevedel-directive-frame.el -- Directive frame surface -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Displays a directive's bound execution-session view in a floating child
;; frame anchored at the directive's source position.  The frame shows the
;; real view buffer, so permissions, Ask, patch review, streaming, and the
;; composer work in it unmodified and no second renderer exists.
;;
;; The transcript may be filtered to one directive's turns.  Filtering uses
;; `invisible' text properties and the buffer's invisibility spec, which is
;; buffer-local rather than window-local, so it applies only while the frame
;; is the sole window showing that view.
;;
;; At most one directive frame exists at a time.  Reuse keeps the exact
;; directive identity or retargets every source/filter field before display.
;; The frame is dismissed explicitly; teardown runs from
;; `delete-frame-functions' so every deletion path leaves the directive
;; composer scope and restores point.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-chat'

;; `mevedel-directive-source'
(declare-function mevedel--directive-record
                  "mevedel-directive-source" (directive))

;; `mevedel-instruction-registry'

;; `mevedel-models'
(declare-function mevedel-model-current-label
                  "mevedel-models" (&optional buffer))

;; `mevedel-overlays'
(declare-function mevedel--topmost-instruction
                  "mevedel-overlays" (instruction &optional of-type pred))

;; `mevedel-structs'
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)

;; `mevedel-tools'
(declare-function mevedel-tools-active-count "mevedel-tools" (&optional buffer))

;; `mevedel-turn'
(declare-function mevedel-request-state-label "mevedel-turn" (&optional buffer))

;; `mevedel-view'
(defvar mevedel--data-buffer)
(defvar mevedel--view-buffer)

;; `mevedel-view-composer'
(declare-function mevedel-view--input-marker-position "mevedel-view-composer" ())
(declare-function mevedel-view-back-to-chat "mevedel-view-composer" ())
(declare-function mevedel-view-composer-scope-label
                  "mevedel-view-composer" (&optional scope))
(defvar mevedel-view--composer-scope)

;; `mevedel-view-render'
(declare-function mevedel-view--rendered-turn-starts "mevedel-view-render" ())


;;
;;; Customization

(defcustom mevedel-directive-frame-width 0.6
  "Width of the directive frame as a fraction of its parent frame."
  :type 'float
  :group 'mevedel)

(defcustom mevedel-directive-frame-height 20
  "Height of the directive frame in lines."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-directive-frame-border-width 2
  "Border width of the directive frame in pixels."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-directive-frame-min-height 8
  "Smallest height of the directive frame in lines.
The frame fits itself to its content between this and
`mevedel-directive-frame-height'."
  :type 'integer
  :group 'mevedel)

(defface mevedel-directive-frame-border
  '((((background dark)) :background "#5c7a80")
    (((background light)) :background "#7d979d"))
  "Border of the directive frame while it has focus.
The background attribute is what paints; a face that inherits an unset
background makes the border invisible against the frame."
  :group 'mevedel)

(defface mevedel-directive-frame-border-inactive
  '((((background dark)) :background "#2d383b")
    (((background light)) :background "#c6d2d5"))
  "Border of the directive frame while another frame has focus."
  :group 'mevedel)

(defcustom mevedel-directive-frame-filter t
  "Whether a directive frame filters the transcript to its own directive.
Filtering is skipped whenever the view buffer is displayed elsewhere,
because the invisibility spec that implements it is buffer-local."
  :type 'boolean
  :group 'mevedel)


;;
;;; State

(defvar mevedel-directive-frame--frame nil
  "The single live directive frame, or nil.")

(defvar mevedel-directive-frame--directive-id nil
  "Directive id whose turns the live frame displays.")

(defvar mevedel-directive-frame--view-buffer nil
  "View buffer displayed by the live frame.")

(defvar mevedel-directive-frame--origin nil
  "Marker for the source position the live frame was opened from.")

(defvar mevedel-directive-frame--directive nil
  "Directive overlay the live frame is anchored to.")

(defvar mevedel-directive-frame--source-buffer nil
  "Source buffer carrying the live frame's follow hooks.")

(defvar mevedel-directive-frame--following nil
  "Non-nil while repositioning, so redisplay hooks cannot recurse.")

(defvar mevedel-directive-frame--pending-restore nil
  "Focus and point to restore once the deleted frame is actually gone.
A plist of :frame, :window, and :origin handed from
`delete-frame-functions' to `after-delete-frame-functions'.")

(defvar mevedel-directive-frame--origin-window nil
  "Window that was selected when the live frame was opened.")

(defvar mevedel-directive-frame--filtered-p nil
  "Whether the live frame currently filters the transcript.")

(defconst mevedel-directive-frame--invisible-symbol
  'mevedel-view-other-directive
  "Invisibility symbol applied to turns outside the framed directive.")

(defconst mevedel-directive-frame--parameters
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
  "Child frame parameters shared by every directive frame.
Adapted from `corfu--frame-parameters'.  Unlike a completion popup this
frame accepts focus and shows a cursor, so `no-accept-focus',
`no-focus-on-map', and a nil `cursor-type' are deliberately absent.")


;;
;;; Availability

(defun mevedel-directive-frame--available-p ()
  "Return non-nil when child frames can be used on this display.
Emacs 30 supports child frames only on graphical displays; Emacs 31
supports them on terminals as well.  This is the single gate for the
whole module, so tightening it later needs no call-site changes."
  (or (display-graphic-p)
      (>= emacs-major-version 31)))


;;
;;; Transcript filtering

(defun mevedel-directive-frame--turn-spans ()
  "Return rendered turn spans in the current view buffer.
Each element is (START END . DIRECTIVE-ID), where DIRECTIVE-ID is nil for
ordinary chat turns.  Content before the first turn, such as the header,
is not covered by any span."
  (let* ((limit (mevedel-view--input-marker-position))
         (starts (mevedel-view--rendered-turn-starts))
         spans)
    (while starts
      (let* ((start (car starts))
             (end (or (cadr starts) limit))
             (directive (get-text-property start 'mevedel-view-directive)))
        (push (cons start (cons end (plist-get directive :directive-id)))
              spans))
      (setq starts (cdr starts)))
    (nreverse spans)))

(defun mevedel-directive-frame--filter-elsewhere-p (view-buffer)
  "Return non-nil when VIEW-BUFFER is shown outside the directive frame.
Filtering is buffer-local, so it must not run while another window would
inherit the hidden turns."
  (cl-loop for window in (get-buffer-window-list view-buffer nil t)
           unless (and (frame-live-p mevedel-directive-frame--frame)
                       (eq (window-frame window)
                           mevedel-directive-frame--frame))
           return t))

(defun mevedel-directive-frame--clear-filter (&optional view-buffer)
  "Remove directive filtering from VIEW-BUFFER."
  (let ((buffer (or view-buffer mevedel-directive-frame--view-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (remove-from-invisibility-spec
         mevedel-directive-frame--invisible-symbol)
        (with-silent-modifications
          (let ((inhibit-read-only t))
            (remove-text-properties
             (point-min) (point-max)
             (list 'invisible
                   mevedel-directive-frame--invisible-symbol))))))
    (setq mevedel-directive-frame--filtered-p nil)))

(defun mevedel-directive-frame--apply-filter ()
  "Hide every rendered turn outside the framed directive.
Does nothing when filtering is disabled, no directive is framed, or the
view buffer is displayed outside the directive frame."
  (let ((buffer mevedel-directive-frame--view-buffer))
    (when (and mevedel-directive-frame-filter
               mevedel-directive-frame--directive-id
               (buffer-live-p buffer)
               (not (mevedel-directive-frame--filter-elsewhere-p buffer)))
      (with-current-buffer buffer
        (with-silent-modifications
          (let ((inhibit-read-only t))
            (remove-text-properties
             (point-min) (point-max)
             (list 'invisible
                   mevedel-directive-frame--invisible-symbol))
            (pcase-dolist (`(,start ,end . ,id)
                           (mevedel-directive-frame--turn-spans))
              (unless (equal id mevedel-directive-frame--directive-id)
                (put-text-property
                 start end 'invisible
                 mevedel-directive-frame--invisible-symbol)))))
        (add-to-invisibility-spec
         mevedel-directive-frame--invisible-symbol))
      (setq mevedel-directive-frame--filtered-p t))))

(defun mevedel-directive-frame-refresh-filter ()
  "Re-apply directive filtering and refit the frame after a re-render.
Safe to call from any buffer and when no directive frame is live.  Runs
on the view's throttled render cadence, not per streamed token, so the
frame grows with the answer instead of resizing on every chunk."
  (when (frame-live-p mevedel-directive-frame--frame)
    (when mevedel-directive-frame--filtered-p
      (mevedel-directive-frame--apply-filter))
    (mevedel-directive-frame--fit-height)
    ;; A frame flipped above its directive grows upward, so refitting moves
    ;; where its top belongs.
    (mevedel-directive-frame--follow)))

(defun mevedel-directive-frame-toggle-filter ()
  "Toggle whether the directive frame filters the transcript."
  (interactive)
  (unless (frame-live-p mevedel-directive-frame--frame)
    (user-error "No directive frame is open"))
  (if mevedel-directive-frame--filtered-p
      (progn
        (mevedel-directive-frame--clear-filter)
        (message "mevedel: showing the whole transcript"))
    (let ((mevedel-directive-frame-filter t))
      (mevedel-directive-frame--apply-filter))
    (if mevedel-directive-frame--filtered-p
        (message "mevedel: showing this directive only")
      (message
       "mevedel: cannot filter while the view is displayed elsewhere"))))


;;
;;; Header line

(defun mevedel-directive-frame--header ()
  "Return the condensed header line for the directive frame.
Leads with directive identity, then the state that changes while the
conversation runs.  Session facts the parent frame's status strip
already shows -- session name, workspace root, execution target, preset
-- are deliberately absent: this header has a fraction of the width."
  (require 'mevedel-turn)
  (when (and (boundp 'mevedel--data-buffer)
             (buffer-live-p mevedel--data-buffer))
    (require 'mevedel-models)
    (require 'mevedel-tools)
    (let* ((data-buffer mevedel--data-buffer)
           (id mevedel-directive-frame--directive-id)
           (scope (mevedel-view-composer-scope-label))
           (state (mevedel-request-state-label data-buffer))
           (model (mevedel-model-current-label data-buffer))
           (tools (mevedel-tools-active-count data-buffer)))
      (concat
       (propertize "◆ " 'face 'mevedel-view-directive-scope)
       (propertize (if (and (stringp id) (> (length id) 8))
                       (substring id 0 8)
                     (or id "directive"))
                   'face 'mevedel-view-directive-scope)
       (when scope
         (concat (propertize " · " 'face 'shadow)
                 (propertize scope 'face 'shadow)))
       (when mevedel-directive-frame--filtered-p
         (concat (propertize " · " 'face 'shadow)
                 (propertize "filtered" 'face 'warning)))
       (propertize " · " 'face 'shadow)
       (propertize (or state "idle") 'face 'shadow)
       (propertize " · " 'face 'shadow)
       (propertize (or model "model none") 'face 'shadow)
       (propertize (format " · %d tool%s" tools (if (= tools 1) "" "s"))
                   'face 'shadow)))))


;;
;;; Frame geometry

(defun mevedel-directive-frame--source-window (directive)
  "Return the window showing DIRECTIVE's source buffer, or nil.
Never returns a window inside the directive frame: once that frame has
focus it would otherwise become its own parent, and re-displaying would
reparent the frame to the frame being replaced."
  (when (and (overlayp directive)
             (buffer-live-p (overlay-buffer directive)))
    (let* ((buffer (overlay-buffer directive))
           (candidates
            (cl-loop for window in (get-buffer-window-list buffer nil t)
                     unless (and (frame-live-p mevedel-directive-frame--frame)
                                 (eq (window-frame window)
                                     mevedel-directive-frame--frame))
                     collect window)))
      ;; The same buffer can be on screen more than once.  Prefer the window
      ;; the user is actually in, then any window on the selected frame, and
      ;; only then whatever is left -- otherwise the frame anchors itself to
      ;; a copy of the directive in a window nobody is looking at.
      (or (car (memq (selected-window) candidates))
          (cl-find-if (lambda (window)
                        (eq (window-frame window) (selected-frame)))
                      candidates)
          (car candidates)))))

(defun mevedel-directive-frame--anchor (directive window)
  "Return the pixel position (X . Y) to place DIRECTIVE's frame in WINDOW.
Returns nil when DIRECTIVE has no visible source position, in which case
the caller centers the frame on its parent."
  (when (and (window-live-p window)
             (overlayp directive)
             (buffer-live-p (overlay-buffer directive)))
    (when-let* ((position (window-absolute-pixel-position
                           (overlay-end directive) window)))
      (let* ((frame (window-frame window))
             (parent-width (frame-pixel-width frame))
             (parent-height (frame-pixel-height frame))
             (width (round (* mevedel-directive-frame-width parent-width)))
             (height (* mevedel-directive-frame-height
                        (frame-char-height frame)))
             (line-height (frame-char-height frame))
             (x (max 0 (min (car position) (- parent-width width))))
             (below (+ (cdr position) line-height))
             ;; Flip above the directive when there is no room below.
             (y (if (<= (+ below height) parent-height)
                    below
                  (max 0 (- (cdr position) height)))))
        (cons x y)))))

(defun mevedel-directive-frame--size (parent)
  "Return the pixel size (WIDTH . HEIGHT) of a frame parented to PARENT."
  (cons (round (* mevedel-directive-frame-width (frame-pixel-width parent)))
        (* mevedel-directive-frame-height (frame-char-height parent))))

(defun mevedel-directive-frame--make (buffer parent)
  "Return a child frame of PARENT displaying BUFFER.
Reuses the live directive frame when it still matches PARENT and the
display type, and recreates it otherwise."
  (let* ((graphic (display-graphic-p parent))
         (frame mevedel-directive-frame--frame)
         (params
          `((parent-frame . ,parent)
            (minibuffer . ,(minibuffer-window parent))
            (font . ,(frame-parameter parent 'font))
            (internal-border-width . ,mevedel-directive-frame-border-width)
            (child-frame-border-width
             . ,mevedel-directive-frame-border-width)
            ,@mevedel-directive-frame--parameters)))
    (unless (and (frame-live-p frame)
                 (eq (frame-parent frame) parent)
                 (eq graphic (display-graphic-p frame))
                 (window-live-p (frame-root-window frame)))
      (when (frame-live-p frame) (delete-frame frame))
      (setq frame
            (make-frame
             `((name . "mevedel-directive")
               (width . 0) (height . 0) (visibility . nil)
               ,@params))))
    (let ((window (frame-root-window frame)))
      (set-window-buffer window buffer)
      (set-window-parameter window 'no-delete-other-windows t)
      (set-window-dedicated-p window t)
      ;; Window parameters rather than buffer-local settings: the view buffer
      ;; is shared with the main view, which keeps its own mode line and its
      ;; full-width status strip.
      (set-window-parameter window 'mode-line-format 'none)
      (set-window-parameter window 'header-line-format
                            '(:eval (mevedel-directive-frame--header)))
      (set-window-fringes window 0 0))
    (mevedel-directive-frame--paint-border frame)
    frame))

(defun mevedel-directive-frame--paint-border (&optional frame)
  "Paint FRAME's border to show whether it holds focus.
The border width alone draws nothing: without an explicit background on
`internal-border' and `child-frame-border' the border takes the default
background and is invisible against the frame."
  (let ((frame (or frame mevedel-directive-frame--frame)))
    (when (frame-live-p frame)
      (let ((color (face-attribute
                    (if (frame-focus-state frame)
                        'mevedel-directive-frame-border
                      'mevedel-directive-frame-border-inactive)
                    :background nil 'default)))
        (set-face-background 'internal-border color frame)
        (set-face-background 'child-frame-border color frame)))))

(add-function :after after-focus-change-function
              #'mevedel-directive-frame--paint-border)

(defun mevedel-directive-frame--follow (&optional window _start)
  "Keep the directive frame aligned with its directive as the source scrolls.
Runs from `window-scroll-functions' and `window-configuration-change-hook'
in the directive's source buffer.  The frame hides once the directive
leaves the window and returns when it scrolls back, so the directive and
its frame scroll as one thing.

WINDOW is supplied by `window-scroll-functions'."
  (unless mevedel-directive-frame--following
    (let ((mevedel-directive-frame--following t)
          (frame mevedel-directive-frame--frame)
          (directive mevedel-directive-frame--directive))
      (cond
       ((not (frame-live-p frame))
        (mevedel-directive-frame--unfollow))
       ((not (and (overlayp directive)
                  (buffer-live-p (overlay-buffer directive))))
        (when (frame-visible-p frame) (make-frame-invisible frame)))
       (t
        ;; Stay with the window the frame anchored to.  When the directive's
        ;; buffer is on screen twice, scrolling the other copy must not drag
        ;; the frame across to it.
        (let ((source
               (if (and (window-live-p mevedel-directive-frame--origin-window)
                        (eq (window-buffer
                             mevedel-directive-frame--origin-window)
                            (overlay-buffer directive)))
                   mevedel-directive-frame--origin-window
                 (setq mevedel-directive-frame--origin-window
                       (mevedel-directive-frame--source-window directive)))))
          (if (not (and (window-live-p source)
                        ;; nil WINDOW is the configuration-change hook, which
                        ;; is about the layout rather than one window.
                        (or (null window) (eq window source))
                        (pos-visible-in-window-p
                         (overlay-end directive) source)))
              (when (and (frame-visible-p frame)
                         (or (null window) (eq window source)))
                (make-frame-invisible frame))
            (when-let* ((anchor (mevedel-directive-frame--anchor
                                 directive source))
                        (current (frame-position frame)))
              ;; Only move on a real change: setting the position from a
              ;; redisplay hook triggers redisplay again.
              (unless (and (= (car anchor) (car current))
                           (= (cdr anchor) (cdr current)))
                (set-frame-position frame (car anchor) (cdr anchor))))
            (unless (frame-visible-p frame)
              (make-frame-visible frame)))))))))

(defun mevedel-directive-frame--follow-setup (directive)
  "Track DIRECTIVE's source buffer so the frame scrolls with it."
  (when (and (overlayp directive)
             (buffer-live-p (overlay-buffer directive)))
    (mevedel-directive-frame--unfollow)
    (setq mevedel-directive-frame--directive directive
          mevedel-directive-frame--source-buffer (overlay-buffer directive))
    (with-current-buffer (overlay-buffer directive)
      (add-hook 'window-scroll-functions
                #'mevedel-directive-frame--follow nil t)
      (add-hook 'window-configuration-change-hook
                #'mevedel-directive-frame--follow nil t))))

(defun mevedel-directive-frame--unfollow ()
  "Stop tracking the source buffer and forget the anchored directive."
  (when (buffer-live-p mevedel-directive-frame--source-buffer)
    (with-current-buffer mevedel-directive-frame--source-buffer
      (remove-hook 'window-scroll-functions
                   #'mevedel-directive-frame--follow t)
      (remove-hook 'window-configuration-change-hook
                   #'mevedel-directive-frame--follow t)))
  (setq mevedel-directive-frame--source-buffer nil
        mevedel-directive-frame--directive nil))

(defun mevedel-directive-frame--fit-height ()
  "Fit the directive frame's height to its content.
Bounded by `mevedel-directive-frame-min-height' and
`mevedel-directive-frame-height' so a one-line answer does not collapse
the frame and a long one does not cover the buffer behind it."
  (when (frame-live-p mevedel-directive-frame--frame)
    ;; `vertically' matters: fitting both dimensions widens the frame to the
    ;; longest unwrapped line in the transcript, which readily exceeds the
    ;; parent frame.  Width stays as computed from the parent at open.
    (fit-frame-to-buffer mevedel-directive-frame--frame
                         mevedel-directive-frame-height
                         mevedel-directive-frame-min-height
                         nil nil 'vertically)))


;;
;;; Display

;;;###autoload
(defun mevedel-directive-frame-display (directive view-buffer &optional focus)
  "Display VIEW-BUFFER in a directive frame anchored at DIRECTIVE.
With FOCUS non-nil the frame is selected, which is what an explicit user
action wants; a request dispatch leaves focus where it is.  Falls back to
an ordinary window when child frames are unavailable."
  (let ((directive-id
         (when (overlayp directive)
           (require 'mevedel-overlays)
           (mevedel-directive-id
            (mevedel--directive-record
             (or (mevedel--topmost-instruction directive 'directive)
                 directive))))))
    (cond
     ((not (mevedel-directive-frame--available-p))
      (display-buffer view-buffer
                      '(display-buffer-below-selected
                        (window-height . 0.4)))
      (when focus (select-window (get-buffer-window view-buffer t))))
     ;; A frame already showing this exact directive stays where it is.  The
     ;; discuss action displays twice -- once entering composer scope and once
     ;; dispatching the request -- and repositioning on the second call would
     ;; move the frame out from under whoever is reading it.
     ((and (frame-live-p mevedel-directive-frame--frame)
           (eq mevedel-directive-frame--view-buffer view-buffer)
           (equal mevedel-directive-frame--directive-id directive-id))
      (make-frame-visible mevedel-directive-frame--frame)
      (when focus
        (select-frame-set-input-focus mevedel-directive-frame--frame))
      mevedel-directive-frame--frame)
     (t
      (let* ((source (mevedel-directive-frame--source-window directive))
             (parent (if (window-live-p source)
                         (window-frame source)
                       (window-frame (selected-window))))
             (anchor (mevedel-directive-frame--anchor directive source))
             (size (mevedel-directive-frame--size parent))
             (frame (mevedel-directive-frame--make view-buffer parent)))
        (unless (eq frame mevedel-directive-frame--frame)
          (setq mevedel-directive-frame--frame frame))
        (setq mevedel-directive-frame--view-buffer view-buffer
              mevedel-directive-frame--directive-id directive-id
              mevedel-directive-frame--origin-window
              (if (window-live-p source) source (selected-window)))
        (when (and (overlayp directive)
                   (buffer-live-p (overlay-buffer directive)))
          (setq mevedel-directive-frame--origin
                (copy-marker (overlay-start directive))))
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
        (with-current-buffer view-buffer (mevedel-directive-frame-mode 1))
        (mevedel-directive-frame--follow-setup directive)
        (mevedel-directive-frame--apply-filter)
        (when focus (select-frame-set-input-focus frame))
        frame)))))

(defun mevedel-directive-frame-close ()
  "Dismiss the directive frame."
  (interactive)
  (unless (frame-live-p mevedel-directive-frame--frame)
    (user-error "No directive frame is open"))
  (delete-frame mevedel-directive-frame--frame))


;;
;;; Teardown

(defun mevedel-directive-frame--on-delete (frame)
  "Release directive frame state when FRAME is deleted.
Runs from `delete-frame-functions' so that every deletion path -- the
dismiss command, \\[delete-frame], or Emacs exit -- leaves the directive
composer scope and restores point."
  (when (eq frame mevedel-directive-frame--frame)
    (let* ((view-buffer mevedel-directive-frame--view-buffer)
           (origin mevedel-directive-frame--origin)
           (window mevedel-directive-frame--origin-window)
           (parent (or (frame-parent frame)
                       (and (window-live-p window) (window-frame window)))))
      ;; Hand input focus back before the frame dies.  This hook runs while
      ;; FRAME is still live and still holds focus; deleting a focused child
      ;; frame without moving focus first leaves no frame focused, and Emacs
      ;; stops responding to the keyboard.
      (when (and (frame-live-p parent) (not (eq parent frame)))
        (select-frame-set-input-focus parent))
      (mevedel-directive-frame--unfollow)
      (mevedel-directive-frame--clear-filter view-buffer)
      (when (buffer-live-p view-buffer)
        (with-current-buffer view-buffer
          (mevedel-directive-frame-mode -1)
          (when mevedel-view--composer-scope
            (require 'mevedel-view-composer)
            (mevedel-view-back-to-chat))))
      (setq mevedel-directive-frame--frame nil
            mevedel-directive-frame--view-buffer nil
            mevedel-directive-frame--directive-id nil
            mevedel-directive-frame--origin nil
            mevedel-directive-frame--origin-window nil)
      ;; Point and the cursor are settled after the frame is actually gone.
      ;; This hook runs before deletion, so anything done here about focus is
      ;; provisional.
      (setq mevedel-directive-frame--pending-restore
            (list :frame parent :window window :origin origin)))))

(defun mevedel-directive-frame--after-delete (&optional _frame)
  "Restore point and the cursor once the directive frame is gone.
Runs from `after-delete-frame-functions'.  Re-asserting focus before
deletion is not enough: the window system reports focus back to the
parent asynchronously, so until Emacs processes that event the parent
draws no cursor -- it reappears only when the next key arrives.  Focusing
again after deletion and forcing one redisplay settles it immediately."
  (when-let* ((pending mevedel-directive-frame--pending-restore))
    (setq mevedel-directive-frame--pending-restore nil)
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
          #'mevedel-directive-frame--after-delete)

(add-hook 'delete-frame-functions #'mevedel-directive-frame--on-delete)


;;
;;; Keys

(defvar-keymap mevedel-directive-frame-mode-map
  :doc "Keymap active in a buffer displayed by the directive frame.
Bindings are prefixed because the view buffer holds an editable composer,
where a single-letter binding would prevent typing.  `C-g' is left alone;
in a view buffer it aborts the request."
  "C-c C-f" #'mevedel-directive-frame-toggle-filter
  "C-c C-z" #'mevedel-directive-frame-close)

(defun mevedel-directive-frame--display-in-parent (buffer alist)
  "Display BUFFER in the directive frame's parent frame honoring ALIST.
The frame's root window is dedicated and unsplittable and the frame is a
few lines tall, so anything popped from it -- the transient menu, a
cockpit surface, the patch buffer -- is unusable inside it.  Redirecting
at the display layer covers every such surface, including ones that do
not exist yet."
  (when-let* ((frame mevedel-directive-frame--frame)
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
        ;; typing still goes to the directive frame.
        (select-frame-set-input-focus parent)
        (select-window window))
      window)))

(define-minor-mode mevedel-directive-frame-mode
  "Minor mode for the view buffer while a directive frame displays it."
  :lighter ""
  :keymap mevedel-directive-frame-mode-map
  :interactive nil
  (if mevedel-directive-frame-mode
      (setq-local display-buffer-overriding-action
                  '(mevedel-directive-frame--display-in-parent . nil))
    (kill-local-variable 'display-buffer-overriding-action)))

(provide 'mevedel-directive-frame)

;;; mevedel-directive-frame.el ends here
