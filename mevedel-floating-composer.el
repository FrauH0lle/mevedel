;;; mevedel-floating-composer.el -- Floating composer child frame -*- lexical-binding: t -*-

;; Copyright (C) 2025- FrauH0lle

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; While the selected view window is scrolled away from the composer, a
;; child frame floats over the bottom of that window (the host) and shows
;; the same view buffer from the rail down.  It is a second window on the
;; view buffer, not a copy: draft, rail, keymaps and progress are shared.
;;
;; Its window is excluded from the view's geometry walks, so viewport
;; restoration, composer anchoring and chrome fitting ignore it, while
;; animation visibility still counts it.
;;
;; Focus moves only from `post-command-hook': a scroll command that pushes
;; the selected window's draft cursor off screen hands the cursor to the
;; floating composer at the same draft offset, and a scroll that brings
;; that offset fully back into the host returns it inline.  Timers and
;; window hooks only place, show or hide an unselected floating composer.
;; Settling a card visited with `C-c C-j' returns the cursor to the floating
;; composer when the host does not show the draft position.
;;
;; The floating composer never takes compositor focus: under PGTK a child
;; frame that once held it never returns it to its parent, which left the
;; view unattended.  Hand-over selects its frame and redirects the host
;; frame's keystrokes to it with `redirect-frame-focus'; hand-back clears
;; the redirect and selects the host.  Selecting another window from the
;; floating composer moves the redirect along (`select-frame' does that).
;;
;; Keys follow the selected window: in the floating composer `C-v'/`M-v'
;; and the wheel never scroll above the rail, `C-M-v' scrolls the host and
;; `C-x 2'/`C-x 3' split it.  `C-c C-SPC' toggles the cursor between the
;; composer and the transcript.
;;
;; It spans the host's full width, fringes included, and sits on its
;; bottom edge, without a border.  Its top line is a rule (the header
;; face's overline; `─' characters on a terminal) carrying `↑ N more'
;; while draft lines are scrolled out above and a "Back to bottom" button
;; that returns the cursor inline.
;;
;; On a terminal it is a terminal child frame with its geometry in cells.
;; A terminal sends keystrokes to the selected child frame itself, and
;; `select-frame' does not move a redirect there, so a terminal parent is
;; never redirected.

;;; Code:

(require 'mevedel-note-frame)
(require 'mevedel-utilities)

;; `mevedel-structs'
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-composer'
(declare-function mevedel-view--input-marker-position "mevedel-view-composer" ())
(declare-function mevedel-view--input-start "mevedel-view-composer" ())
(declare-function mevedel-view--position-in-input-region-p
                  "mevedel-view-composer" (position))
(declare-function mevedel-view--position-screen-y
                  "mevedel-view-composer" (position window &optional fully))
(declare-function mevedel-view--scroll-command-p "mevedel-view-composer" (command))
(declare-function mevedel-view--scroll-return "mevedel-view-composer" (window))
(declare-function mevedel-view--set-viewport "mevedel-view-composer" (window viewport))
(declare-function mevedel-view--viewport-at-y "mevedel-view-composer"
                  (window position y))
(declare-function mevedel-view--window-body-geometry "mevedel-view-composer" (window))

;; `mevedel-view-segments'
(declare-function mevedel-view-historical-segment-p "mevedel-view-segments" ())

;; `mevedel-view-stream'
(declare-function mevedel-view--request-progress-region-start "mevedel-view-stream" ())

;; PGTK
(defvar pgtk-wait-for-event-timeout)


;;
;;; Customization

(defun mevedel-floating-composer--set-option (symbol value)
  "Set SYMBOL to VALUE; turning it off removes every floating composer."
  (set-default-toplevel-value symbol value)
  (unless value
    (mevedel-floating-composer--remove-all)))

(defcustom mevedel-floating-composer t
  "Non-nil shows the composer over a view window scrolled away from it.
Scrolling the selected view window away from the composer with the cursor
in the draft moves the cursor into the floating composer; scrolling back
returns it inline.  Turning this off removes every floating composer."
  :type 'boolean
  :initialize #'custom-initialize-default
  :set #'mevedel-floating-composer--set-option
  :group 'mevedel)


;;
;;; State

(defvar-local mevedel-floating-composer--frame nil
  "This view's floating-composer child frame, kept hidden between uses.")

(defvar-local mevedel-floating-composer--host nil
  "Window the floating composer currently floats over.")

(defvar-local mevedel-floating-composer--timer nil
  "Pending coalesced placement update.")

(defvar-local mevedel-floating-composer--redirected nil
  "Frame whose keystrokes this view redirected to its floating composer.
`select-frame' can retarget that redirect at another frame, so releasing
clears it whatever it points at now.")

(defvar mevedel-floating-composer--updating nil
  "Non-nil during an update; `make-frame' can run timers re-entrantly.")


;;
;;; Placement

(defun mevedel-floating-composer--place (edges fringes char-height natural header)
  "Return (WIDTH HEIGHT X Y) in pixels for the floating composer.
EDGES are the host's text-area (LEFT TOP RIGHT BOTTOM) relative to its
frame and FRINGES its (LEFT RIGHT ...) fringe widths.  CHAR-HEIGHT is the
parent's line height, NATURAL the composer's displayed height and HEADER
the top line's height.  The frame covers the host's text area and both
fringes and sits on its bottom edge, so no strip of transcript shows
beside or below it.  Its height is the top line plus NATURAL, capped at
half the host's text height but at least one line."
  (pcase-let* ((`(,left ,top ,right ,bottom) edges)
               (`(,left-fringe ,right-fringe . ,_) fringes)
               (height (+ header (max char-height (min (/ (- bottom top) 2) natural)))))
    (list (+ left-fringe (- right left) right-fringe)
          height
          (- left left-fringe)
          (- bottom height))))

(defun mevedel-floating-composer--start ()
  "Return where the floating composer starts: the rail, else the prompt."
  (when-let* ((pos (or (mevedel-view--request-progress-region-start)
                       (mevedel-view--input-marker-position))))
    (save-excursion
      (goto-char pos)
      (skip-chars-forward "\n")
      (point))))

(defun mevedel-floating-composer--natural-height (window start limit)
  "Return WINDOW's pixel height of START to the buffer end, up to LIMIT.
Cached until the buffer, WINDOW's width or the face remapping (text
scale) changes, so scrolling the host measures nothing.  The default
face's height on WINDOW's frame keys global text scaling.
Measuring fontifies the draft, which modifies the buffer, so the cache
records the buffer's state after measuring: otherwise every key measured
twice."
  (let ((key (lambda () (list (buffer-modified-tick) (window-body-width window t)
                              face-remapping-alist start limit
                              (face-attribute 'default :height (window-frame window)))))
        (cache (window-parameter window 'mevedel-floating-composer-height)))
    (if (equal (funcall key) (car cache))
        (cdr cache)
      (let ((height (cdr (window-text-pixel-size window start (point-max) nil limit))))
        ;; Copied: `face-remap-add-relative' edits the remapping in place.
        (set-window-parameter window 'mevedel-floating-composer-height
                              (cons (copy-tree (funcall key)) height))
        height))))

(defun mevedel-floating-composer--pin (window)
  "Start WINDOW at the rail unless its draft is capped.
A capped draft scrolls just enough to show WINDOW's point, never above
the rail: redisplay alone would recenter it onto transcript rows."
  (when-let* ((start (mevedel-floating-composer--start)))
    (if (window-parameter window 'mevedel-floating-composer-pinned)
        (unless (= start (window-start window))
          (set-window-start window start t))
      (let ((point (window-point window)))
        (when (< (window-start window) start)
          (set-window-start window start t))
        (when-let* ((y (cond ((< point (window-start window)) 0)
                             ((not (mevedel-view--position-screen-y point window t))
                              (- (window-body-height window t)
                                 (save-excursion (goto-char point) (line-pixel-height)))))))
          (set-window-start
           window (max start (car (mevedel-view--viewport-at-y window point y))) t))))
    (unless (zerop (window-vscroll window t))
      (set-window-vscroll window 0 t))))


;;
;;; Frame

(defun mevedel-floating-composer--window ()
  "Return the floating composer's window, or nil."
  (and (frame-live-p mevedel-floating-composer--frame)
       (frame-root-window mevedel-floating-composer--frame)))

(defun mevedel-floating-composer--make (parent)
  "Return this view's floating-composer frame, a hidden child of PARENT."
  (let ((frame mevedel-floating-composer--frame))
    (unless (and (frame-live-p frame) (eq (frame-parent frame) parent))
      ;; Another frame's window hosts it now: tear the old frame down as
      ;; removing does, so deletion's hooks never see it as this view's.
      (when (frame-live-p frame)
        (mevedel-floating-composer--remove))
      (setq frame (let ((pgtk-wait-for-event-timeout nil))
                    ;; Creating it hidden waits like `--hide' does.
                    (make-frame
                     `((name . "mevedel-composer")
                       (parent-frame . ,parent)
                       (minibuffer . ,(minibuffer-window parent))
                       (font . ,(frame-parameter parent 'font))
                       (width . 0) (height . 0) (visibility . nil)
                       ;; It never takes compositor focus; see the Commentary.
                       (no-focus-on-map . t) (no-accept-focus . t)
                       (internal-border-width . 0) (child-frame-border-width . 0)
                       ;; Frame fringes would count in its native width; the
                       ;; window carries the host's fringes instead.
                       (left-fringe . 0) (right-fringe . 0)
                       (mevedel-view-animate-as-parent . t)
                       ,@mevedel--child-frame-parameters))))
      (let ((window (frame-root-window frame)))
        (set-window-buffer window (current-buffer))
        (set-window-dedicated-p window t)
        (dolist (parameter '(mode-line-format tab-line-format))
          (set-window-parameter window parameter 'none))
        (set-window-parameter window 'header-line-format
                              '(:eval (mevedel-floating-composer--top-line)))
        (set-window-parameter window 'mevedel-view-geometry-excluded t)
        (set-window-parameter window 'mevedel-floating-composer t)
        (set-window-parameter window 'no-other-window t)
        (set-window-parameter window 'split-window #'mevedel-floating-composer--split-host)
        (set-window-point window (point-max)))
      (add-hook 'window-buffer-change-functions #'mevedel-floating-composer--check-hosts)
      (setq mevedel-floating-composer--frame frame))
    frame))

(defface mevedel-floating-composer-button '((t :inherit shadow))
  "The floating composer's \"Back to bottom\" button.
Its background is a tint of the theme's, set on each floating composer."
  :group 'mevedel)

(defconst mevedel-floating-composer--button
  (propertize " ↓ Back to bottom "
              'face 'mevedel-floating-composer-button
              'mouse-face 'highlight
              'help-echo "mouse-1: back to the composer at the bottom"
              'local-map (define-keymap "<header-line> <mouse-1>"
                           #'mevedel-floating-composer-back-to-bottom))
  "The clickable button at the right of the floating composer's top line.")

(defun mevedel-floating-composer--style (frame)
  "Draw FRAME's top line as a rule over the theme's default background.
The rule is the header face's overline, which spans the whole header
line, fringes included.  Faces are set again only after a theme reset
them or changed its colors: setting them refreshes FRAME's faces."
  (let ((background (face-background 'default frame t))
        (rule (face-foreground 'shadow frame t)))
    (unless (and (equal rule (face-attribute 'header-line :overline frame))
                 (equal background (face-attribute 'header-line :background frame)))
      (set-face-attribute 'header-line frame :inherit 'default :box nil :underline nil
                          :overline rule :background background)
      (set-face-attribute 'mevedel-floating-composer-button frame :background
                          (mevedel--tint background (face-foreground 'default frame t)
                                         0.10)))))

(defun mevedel-floating-composer--top-line ()
  "Return the top line of the floating composer in the selected window.
On the left `↑ N more' when N draft lines are scrolled out above, on the
right `mevedel-floating-composer--button'.  A terminal cannot draw the
overline, so there `─' characters fill the space between them.  Counting
costs nothing while no draft line is hidden."
  (let* ((window (selected-window))
         (start (mevedel-floating-composer--start))
         (hidden (if (and start (> (window-start window) start))
                     (count-screen-lines start (window-start window) nil window)
                   0))
         (more (if (> hidden 0) (propertize (format " ↑ %d more " hidden) 'face 'shadow) ""))
         (button mevedel-floating-composer--button)
         (gap (- (window-total-width window) (string-width more) (string-width button))))
    (concat more
            (if (display-graphic-p)
                (propertize " " 'display `(space :align-to (- right ,(string-width button))))
              (propertize (make-string (max 0 gap) ?─) 'face 'shadow))
            button)))

(defun mevedel-floating-composer-back-to-bottom (event)
  "Return the cursor inline at the draft position of EVENT's floating composer.
Its host scrolls to show the composer at the bottom, so the floating
composer hides after this command."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (when-let* ((window (mevedel-floating-composer--window))
                (host mevedel-floating-composer--host)
                ((window-live-p host)))
      (mevedel-floating-composer--select host (window-point window))
      (cl-flet ((to-bottom (position)
                  (mevedel-view--set-viewport
                   host (mevedel-view--viewport-at-y
                         host position
                         (- (cdr (mevedel-view--window-body-geometry host))
                            (save-excursion (goto-char position) (line-pixel-height)))))))
        (to-bottom (point-max))
        ;; A draft taller than the window shows the cursor's line instead.
        (unless (mevedel-view--position-screen-y (point) host t)
          (to-bottom (point)))))))

(defun mevedel-floating-composer--show (host)
  "Show the floating composer over HOST, refitting it to the draft."
  (let* ((parent (window-frame host))
         (frame (mevedel-floating-composer--make parent))
         (window (frame-root-window frame))
         (edges (window-inside-pixel-edges host))
         (fringes (window-fringes host))
         (char-height (frame-char-height parent))
         ;; Until drawn, Emacs estimates the top line from its font alone,
         ;; without the overline's margin, and keeps a stale height after a
         ;; font change: a frame too short scrolls its draft.
         (header (max (window-header-line-height window)
                      (+ char-height (if (display-graphic-p parent) overline-margin 0))))
         (start (mevedel-floating-composer--start))
         (cap (/ (- (nth 3 edges) (nth 1 edges)) 2))
         (width (car (mevedel-floating-composer--place
                      edges fringes char-height 0 header)))
         (frame-resize-pixelwise t)
         (window-min-height 1))
    (setq mevedel-floating-composer--host host)
    (mevedel-floating-composer--style frame)
    ;; The host's fringes keep the draft aligned with the transcript text.
    (unless (equal (take 2 fringes) (take 2 (window-fringes window)))
      (set-window-fringes window (car fringes) (cadr fringes)))
    ;; The draft wraps at the final width, so measure only after taking it.
    (unless (= width (frame-text-width frame))
      (set-frame-size frame width (max char-height (frame-text-height frame)) t))
    (let* ((natural (mevedel-floating-composer--natural-height
                     ;; The limit counts from the window's top, header included.
                     window start (+ header cap char-height)))
           (geometry (mevedel-floating-composer--place
                      edges fringes char-height natural header)))
      (set-window-parameter window 'mevedel-floating-composer-pinned (<= natural cap))
      ;; One compound call: a separate resize would first hang the growing
      ;; frame below its old top, and on PGTK that geometry can stick.  It is
      ;; compared with the frame itself: restoring a window configuration, as
      ;; leaving the minibuffer does, rounds the frame to whole lines.
      (unless (equal geometry (list (frame-text-width frame) (frame-text-height frame)
                                    (car (frame-position frame))
                                    (cdr (frame-position frame))))
        (apply #'set-frame-size-and-position-pixelwise frame geometry)))
    (mevedel-floating-composer--pin window)
    (when (< (window-point window) start)
      (set-window-point window (point-max)))
    (unless (frame-visible-p frame)
      (make-frame-visible frame))))

(defun mevedel-floating-composer--hide ()
  "Hide the floating composer, keeping its frame for reuse."
  (when-let* ((frame mevedel-floating-composer--frame)
              ((frame-live-p frame))
              ((frame-visible-p frame)))
    ;; PGTK otherwise runs its event loop for the whole timeout (0.1 s)
    ;; on every hide, waiting for map events a hidden child never gets.
    (let ((pgtk-wait-for-event-timeout nil))
      (make-frame-invisible frame))))

(defun mevedel-floating-composer--select (window position)
  "Select WINDOW, the host or the floating composer, with point at POSITION.
The host's frame keeps compositor focus; when WINDOW is the floating
composer, its parent's keystrokes are redirected to it."
  (let ((parent (frame-parent (window-frame window))))
    (mevedel-floating-composer--release)
    (select-window window)
    (when parent
      (when (display-graphic-p parent)
        (redirect-frame-focus parent (window-frame window)))
      ;; Several views can float over one frame; the last to redirect owns it.
      (set-frame-parameter parent 'mevedel-floating-composer-owner (current-buffer))
      (setq mevedel-floating-composer--redirected parent))
    (goto-char position)))

(defun mevedel-floating-composer--release ()
  "Clear the keystroke redirect this view established, wherever it points.
Leave it when another view redirected the same frame since."
  (when-let* ((parent mevedel-floating-composer--redirected))
    (setq mevedel-floating-composer--redirected nil)
    (when (and (frame-live-p parent)
               (eq (frame-parameter parent 'mevedel-floating-composer-owner)
                   (current-buffer)))
      (set-frame-parameter parent 'mevedel-floating-composer-owner nil)
      (when (frame-focus parent)
        (redirect-frame-focus parent nil)))))

(defun mevedel-floating-composer--remove ()
  "Delete the floating composer, returning a cursor it holds inline."
  (when (timerp mevedel-floating-composer--timer)
    (cancel-timer mevedel-floating-composer--timer))
  (when-let* ((window (mevedel-floating-composer--window)))
    (when (and (eq window (selected-window))
               (window-live-p mevedel-floating-composer--host)
               (eq (window-buffer mevedel-floating-composer--host) (current-buffer)))
      (mevedel-floating-composer--select mevedel-floating-composer--host
                                         (window-point window)))
    ;; A redirect to a deleted frame would outlive it.
    (mevedel-floating-composer--release))
  (let ((frame mevedel-floating-composer--frame))
    ;; Deleting a frame can run timers; an update there must not find this
    ;; half-deleted frame and delete it again, which crashes Emacs.
    (setq mevedel-floating-composer--frame nil
          mevedel-floating-composer--host nil
          mevedel-floating-composer--timer nil)
    (when (frame-live-p frame)
      (delete-frame frame))))

(defun mevedel-floating-composer--split-host (window size side &optional pixelwise)
  "Split the host of floating-composer WINDOW instead of WINDOW.
SIZE, SIDE and PIXELWISE are as for `split-window'."
  (let ((host (buffer-local-value 'mevedel-floating-composer--host (window-buffer window))))
    (unless (window-live-p host)
      (user-error "No transcript window to split"))
    (split-window host size side pixelwise)))

(defun mevedel-floating-composer--remove-all ()
  "Remove every view's floating composer and the host check."
  (remove-hook 'window-buffer-change-functions #'mevedel-floating-composer--check-hosts)
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'mevedel-floating-composer--frame buffer)
      (with-current-buffer buffer
        (mevedel-floating-composer--remove)))))

(defun mevedel-floating-composer--scroll-target ()
  "Return the window `scroll-other-window' scrolls.
From the floating composer that is its host."
  (if (and (window-parameter nil 'mevedel-floating-composer)
           (window-live-p mevedel-floating-composer--host))
      mevedel-floating-composer--host
    ;; Binding the local value lets the global default decide.
    (let ((other-window-scroll-default (default-value 'other-window-scroll-default)))
      (other-window-for-scrolling))))

;;;###autoload
(defun mevedel-floating-composer-toggle ()
  "Move the cursor between the composer and the transcript.
The composer is the floating composer while it is shown, else the inline
draft.  Inline, each toggle returns to where the previous one left."
  (interactive)
  (let ((window (mevedel-floating-composer--window))
        (host mevedel-floating-composer--host))
    (cond
     ((and window (eq window (selected-window)) (window-live-p host))
      (mevedel-floating-composer--select host (window-point host)))
     ((and window (eq host (selected-window)) (frame-visible-p (window-frame window)))
      (let ((point (window-point window)))
        (mevedel-floating-composer--select
         window (if (mevedel-view--position-in-input-region-p point) point (point-max)))))
     (t
      (let ((back (window-parameter nil 'mevedel-floating-composer-back))
            (draft (mevedel-view--position-in-input-region-p (point))))
        (set-window-parameter nil 'mevedel-floating-composer-back (point))
        (goto-char (cond ((and back (<= back (point-max))
                               (xor draft (mevedel-view--position-in-input-region-p back)))
                          back)
                         (draft (window-start))
                         (t (point-max)))))))))

;;
;;; Update

(defun mevedel-floating-composer--host-p (window)
  "Return non-nil when WINDOW can host this view's floating composer.
Note-frame windows and historical segments have none."
  (and mevedel-floating-composer
       (not mevedel-view--agent-transcript-p)
       (eq (window-buffer window) (current-buffer))
       (not (frame-parent (window-frame window)))
       (not (mevedel-view-historical-segment-p))))

(defun mevedel-floating-composer--needed-p (host)
  "Return non-nil when HOST shows neither its cursor's draft nor the composer.
The composer runs to the buffer end, so HOST shows a full row of it when
its start is fully visible.  When the composer covers HOST's top, that is
HOST's first displayed row, or, if only that row's top is clipped, the
next one.  A cursor in the draft costs no visibility query; a HOST
starting above the composer costs one."
  (and (not (mevedel-view--position-in-input-region-p (window-point host)))
       (when-let* ((start (mevedel-floating-composer--start)))
         (let ((top (window-start host)))
           (not (if (< top start)
                    (mevedel-view--position-screen-y start host t)
                  ;; (X Y) when fully visible, else (X Y RTOP RBOT ROWH VPOS).
                  (let ((first (pos-visible-in-window-p top host t)))
                    (or (and first (null (cddr first)))
                        ;; A row clipped below as well leaves nothing under it.
                        (and first (zerop (nth 3 first))
                             (mevedel-view--position-screen-y
                              (save-excursion (goto-char top)
                                              (vertical-motion 1 host)
                                              (point))
                              host t))))))))))

(defun mevedel-floating-composer--update (&optional command)
  "Show, place or hide this view's floating composer.
COMMAND non-nil means the user's command just finished: only then may the
cursor move between the host and the floating composer.  With the option
off, remove every floating composer, however it was turned off."
  (unless mevedel-floating-composer--updating
    (let* ((mevedel-floating-composer--updating t)
           (selected (selected-window))
           (window (mevedel-floating-composer--window))
           (scrolled (and command (mevedel-view--scroll-command-p this-command))))
      (cond
       ((window-minibuffer-p selected))
       ((not mevedel-floating-composer)
        (mevedel-floating-composer--remove-all))
       ((eq selected window)
        (let ((host mevedel-floating-composer--host))
          (cond
           ((not (and (window-live-p host) (mevedel-floating-composer--host-p host)))
            (when command (mevedel-floating-composer--remove)))
           ((and scrolled
                 (mevedel-view--position-screen-y (window-point window) host t))
            (mevedel-floating-composer--select host (window-point window))
            (mevedel-floating-composer--hide))
           (t (mevedel-floating-composer--show host)
              ;; A mouse click selects it without a hand-over.
              (unless (eq (frame-focus (window-frame host)) (window-frame window))
                (mevedel-floating-composer--select window (point)))))))
       ((and (mevedel-floating-composer--host-p selected)
             (mevedel-floating-composer--needed-p selected))
        (mevedel-floating-composer--show selected)
        (when-let* ((scrolled)
                    (offset (mevedel-view--scroll-return selected)))
          ;; The floating composer now owns the draft cursor; a stale inline
          ;; return must not overwrite edits made there.
          (set-window-parameter selected 'mevedel-view-scroll-return nil)
          (mevedel-floating-composer--select
           (mevedel-floating-composer--window)
           (min (point-max) (+ (mevedel-view--input-start) offset)))))
       (t (mevedel-floating-composer--hide))))))

(defun mevedel-floating-composer--run (buffer)
  "Run the coalesced update for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq mevedel-floating-composer--timer nil)
      (mevedel-floating-composer--update))))

(defun mevedel-floating-composer-refresh ()
  "Schedule one placement update of this view's floating composer.
Called on the view's render cadence; never moves the cursor.  Schedules
nothing while the floating composer is hidden and the selected window
has its cursor in this view's draft or shows another buffer."
  (when (and mevedel-floating-composer
             ;; Not the variable: a timer armed under TRAMP can be discarded.
             (not (mevedel--timer-pending-p mevedel-floating-composer--timer))
             (or (let ((frame mevedel-floating-composer--frame))
                   (and (frame-live-p frame) (frame-visible-p frame)))
                 (let ((window (selected-window)))
                   (and (eq (window-buffer window) (current-buffer))
                        (not (mevedel-view--position-in-input-region-p
                              (window-point window)))))))
    (setq mevedel-floating-composer--timer
          (run-at-time 0 nil #'mevedel-floating-composer--run (current-buffer)))))

(defun mevedel-floating-composer--post-command ()
  "Update after a command, which may move the cursor."
  ;; A floating composer left by turning the option off with `setq' goes now.
  (when (or mevedel-floating-composer mevedel-floating-composer--frame)
    (mevedel-floating-composer--update t)))

(defun mevedel-floating-composer--on-window (window)
  "Respond to a size, buffer or selection change of WINDOW.
These run inside redisplay, so they reschedule rather than make frames;
the floating composer's own changes restore its pin and refit it: its
top line is only estimated until first drawn."
  (when (window-parameter window 'mevedel-floating-composer)
    (mevedel-floating-composer--pin window))
  (mevedel-floating-composer-refresh))

(defun mevedel-floating-composer--on-scroll (window start)
  "Keep floating-composer WINDOW from scrolling above the rail to START.
Touchpad momentum scrolls it tick by tick inside one command, redisplaying
in between, so the clamp after the command would come too late.  Scrolling
down is redisplay showing point in a frame still too short; pinning that
back would make redisplay scroll again, forever."
  (when (and (window-parameter window 'mevedel-floating-composer)
             (< start (or (mevedel-floating-composer--start) start)))
    (mevedel-floating-composer--pin window)))

(defun mevedel-floating-composer--check-hosts (frame)
  "Release floating composers over FRAME whose host is gone.
Runs from `window-buffer-change-functions' when a window on FRAME was
deleted or changed buffer, so a keystroke cannot reach an orphaned
floating composer; deleting it waits for a timer, outside redisplay.
Installed when a floating composer is first made and removed when the
option goes off, so views without one pay nothing."
  (dolist (child (frame-list))
    (when (and (eq (frame-parent child) frame)
               (frame-parameter child 'mevedel-view-animate-as-parent))
      (let ((buffer (window-buffer (frame-root-window child))))
        (with-current-buffer buffer
          (let ((host mevedel-floating-composer--host))
            (when (and host (not (and (window-live-p host)
                                      (eq (window-buffer host) buffer))))
              (mevedel-floating-composer--release)
              (run-at-time 0 nil #'mevedel-floating-composer--remove-orphan buffer))))))))

(defun mevedel-floating-composer--remove-orphan (buffer)
  "Remove BUFFER's floating composer if its host is still gone."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((host mevedel-floating-composer--host))
        (unless (and (window-live-p host) (eq (window-buffer host) buffer))
          (mevedel-floating-composer--remove))))))


;;
;;; Cards

(defun mevedel-floating-composer-leave ()
  "Move a cursor the floating composer holds to its host, at the same position.
A command that visits the transcript then acts in the host, so what it
remembers belongs to the host."
  (when-let* ((window (mevedel-floating-composer--window))
              ((eq window (selected-window)))
              (host mevedel-floating-composer--host)
              ((window-live-p host)))
    (mevedel-floating-composer--select host (window-point window))))

(defun mevedel-floating-composer-reveal (window start position)
  "Scroll WINDOW so POSITION shows above its floating composer.
Show the card starting at START at WINDOW's top, else POSITION's line just
above the floating composer.  Do nothing while none floats over WINDOW."
  (when-let* (((eq window mevedel-floating-composer--host))
              (frame mevedel-floating-composer--frame)
              ((frame-live-p frame))
              ((frame-visible-p frame))
              (top (- (cdr (frame-position frame))
                      (nth 1 (window-inside-pixel-edges window))))
              (height (save-excursion (goto-char position) (line-pixel-height)))
              (above-p (lambda ()
                         (when-let* ((y (mevedel-view--position-screen-y position window t)))
                           (<= (+ y height) top)))))
    (unless (funcall above-p)
      (mevedel-view--set-viewport window (mevedel-view--viewport-at-y window start 0))
      (unless (funcall above-p)
        (mevedel-view--set-viewport
         window (mevedel-view--viewport-at-y window position (max 0 (- top height))))))))

(defun mevedel-floating-composer-return (window position)
  "Return the cursor to POSITION in the floating composer over WINDOW.
Only when WINDOW is selected and does not fully show POSITION; the host
keeps its own point.  Return non-nil when the cursor moved."
  (when-let* (((eq window (selected-window)))
              ((eq window mevedel-floating-composer--host))
              (composer (mevedel-floating-composer--window))
              ((frame-visible-p (window-frame composer)))
              ((not (mevedel-view--position-screen-y position window t))))
    (mevedel-floating-composer--select composer position)
    t))

(defun mevedel-floating-composer-setup ()
  "Enable the floating composer in the current interactive view."
  (add-hook 'post-command-hook #'mevedel-floating-composer--post-command 80 t)
  (setq-local other-window-scroll-default #'mevedel-floating-composer--scroll-target)
  (dolist (hook '(window-size-change-functions
                  window-buffer-change-functions
                  window-selection-change-functions))
    (add-hook hook #'mevedel-floating-composer--on-window nil t))
  (add-hook 'window-scroll-functions #'mevedel-floating-composer--on-scroll nil t)
  (add-hook 'kill-buffer-hook #'mevedel-floating-composer--remove nil t))

(provide 'mevedel-floating-composer)
;;; mevedel-floating-composer.el ends here
