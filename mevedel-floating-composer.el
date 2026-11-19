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
;; On a terminal it is a terminal child frame: its geometry is in cells and
;; Emacs draws its border as box characters one cell outside the frame,
;; only while the frame is decorated.  A terminal sends keystrokes to the
;; selected child frame itself, and `select-frame' does not move a redirect
;; there, so a terminal parent is never redirected.

;;; Code:

(require 'mevedel-note-frame)
(require 'mevedel-utilities)

;; `mevedel-view-composer'
(declare-function mevedel-view--input-marker-position "mevedel-view-composer" ())
(declare-function mevedel-view--input-start "mevedel-view-composer" ())
(declare-function mevedel-view--position-in-input-region-p
                  "mevedel-view-composer" (position))
(declare-function mevedel-view--position-screen-y
                  "mevedel-view-composer" (position window &optional fully))
(declare-function mevedel-view--scroll-command-p "mevedel-view-composer" (command))
(declare-function mevedel-view--set-viewport "mevedel-view-composer" (window viewport))
(declare-function mevedel-view--viewport-at-y "mevedel-view-composer"
                  (window position y))

;; `mevedel-view-segments'
(declare-function mevedel-view-historical-segment-p "mevedel-view-segments" ())

;; `mevedel-view-stream'
(declare-function mevedel-view--request-progress-region-start "mevedel-view-stream" ())

;; `mevedel-structs'
(defvar mevedel-view--agent-transcript-p)

;; PGTK
(defvar pgtk-wait-for-event-timeout)


;;
;;; Customization

(defun mevedel-floating-composer--set-option (symbol value)
  "Set SYMBOL to VALUE; turning it off removes every floating composer."
  (set-default-toplevel-value symbol value)
  (unless value
    (remove-hook 'window-buffer-change-functions #'mevedel-floating-composer--check-hosts)
    (dolist (buffer (buffer-list))
      (when (buffer-local-value 'mevedel-floating-composer--frame buffer)
        (with-current-buffer buffer
          (mevedel-floating-composer--remove))))))

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

(defun mevedel-floating-composer--place (edges char-width char-height natural border)
  "Return (WIDTH HEIGHT X Y) in pixels for the floating composer.
EDGES are the host's text-area (LEFT TOP RIGHT BOTTOM) relative to its
frame.  CHAR-WIDTH and CHAR-HEIGHT are the parent's character size,
NATURAL the composer's displayed height and BORDER the frame's border
width.  WIDTH and HEIGHT are the frame's text size: the host's text width
less a two-column inset per side, shrunk so a narrow host still contains
the frame, and NATURAL capped at half the host's text height but at least
one line.  The frame floats half a line above the host's bottom edge."
  (pcase-let* ((`(,left ,top ,right ,bottom) edges)
               (body-width (- right left))
               (inset (max 0 (min (* 2 char-width)
                                  (/ (- body-width (* 2 border) char-width) 2))))
               (height (max char-height (min (/ (- bottom top) 2) natural))))
    (list (max 1 (- body-width (* 2 inset) (* 2 border)))
          height
          (+ left inset)
          (- bottom (/ char-height 2) height (* 2 border)))))

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
scale) changes, so scrolling the host measures nothing.  Measuring
fontifies the draft, which modifies the buffer, so the cache records the
buffer's state after measuring: otherwise every key measured twice."
  (let ((key (lambda () (list (buffer-modified-tick) (window-body-width window t)
                              face-remapping-alist start limit)))
        (cache (window-parameter window 'mevedel-floating-composer-height)))
    (if (equal (funcall key) (car cache))
        (cdr cache)
      (let ((height (cdr (window-text-pixel-size window start (point-max) nil limit))))
        ;; Copied: `face-remap-add-relative' edits the remapping in place.
        (set-window-parameter window 'mevedel-floating-composer-height
                              (cons (copy-tree (funcall key)) height))
        height))))

(defun mevedel-floating-composer--pin (window)
  "Start WINDOW at the rail unless its draft is capped, then it follows point.
Scrolling a capped draft stops at the rail, never showing transcript text."
  (when-let* ((start (mevedel-floating-composer--start))
              ((or (window-parameter window 'mevedel-floating-composer-pinned)
                   (< (window-start window) start))))
    (unless (= start (window-start window))
      (set-window-start window start t))
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
  (let ((frame mevedel-floating-composer--frame)
        (border mevedel-note-frame-border-width))
    (unless (and (frame-live-p frame) (eq (frame-parent frame) parent))
      (when (frame-live-p frame)
        (mevedel-floating-composer--release)
        (delete-frame frame))
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
                       (internal-border-width . ,border)
                       (child-frame-border-width . ,border)
                       ;; Frame fringes count in its native width even when the
                       ;; window hides them, widening it past the inset.
                       (left-fringe . 0) (right-fringe . 0)
                       (mevedel-view-animate-as-parent . t)
                       ;; A terminal draws its border only around a decorated frame.
                       ,@(unless (display-graphic-p parent) '((undecorated . nil)))
                       ,@mevedel--child-frame-parameters))))
      (let ((window (frame-root-window frame)))
        (set-window-buffer window (current-buffer))
        (set-window-dedicated-p window t)
        (dolist (parameter '(mode-line-format header-line-format tab-line-format))
          (set-window-parameter window parameter 'none))
        (set-window-parameter window 'mevedel-view-geometry-excluded t)
        (set-window-parameter window 'mevedel-floating-composer t)
        (set-window-parameter window 'no-other-window t)
        (set-window-parameter window 'split-window #'mevedel-floating-composer--split-host)
        (set-window-point window (point-max)))
      (mevedel-note-frame--paint-border frame)
      (add-hook 'window-buffer-change-functions #'mevedel-floating-composer--check-hosts)
      (setq mevedel-floating-composer--frame frame))
    frame))

(defun mevedel-floating-composer--show (host)
  "Show the floating composer over HOST, refitting it to the draft."
  (let* ((parent (window-frame host))
         (graphic (display-graphic-p parent))
         (frame (mevedel-floating-composer--make parent))
         (window (frame-root-window frame))
         (edges (window-inside-pixel-edges host))
         (char-width (frame-char-width parent))
         (char-height (frame-char-height parent))
         (border (if graphic mevedel-note-frame-border-width 1))
         (start (mevedel-floating-composer--start))
         (cap (/ (- (nth 3 edges) (nth 1 edges)) 2))
         (width (car (mevedel-floating-composer--place
                      edges char-width char-height 0 border)))
         (frame-resize-pixelwise t)
         (window-min-height 1))
    (setq mevedel-floating-composer--host host)
    ;; The draft wraps at the final width, so measure only after taking it.
    (unless (= width (frame-text-width frame))
      (set-frame-size frame width (max char-height (frame-text-height frame)) t))
    (let* ((natural (mevedel-floating-composer--natural-height
                     window start (+ cap char-height)))
           (geometry (mevedel-floating-composer--place
                      edges char-width char-height natural border)))
      ;; A terminal places the frame inside its border cell.
      (unless graphic
        (cl-incf (nth 2 geometry))
        (cl-incf (nth 3 geometry)))
      (set-window-parameter window 'mevedel-floating-composer-pinned (<= natural cap))
      ;; One compound call: a separate resize would first hang the growing
      ;; frame below its old top, and on PGTK that geometry can stick.
      (unless (equal geometry (frame-parameter frame 'mevedel-floating-composer-geometry))
        (set-frame-parameter frame 'mevedel-floating-composer-geometry geometry)
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
    (goto-char position)
    (mevedel-floating-composer--paint-borders)))

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
  (when-let* ((window (mevedel-floating-composer--window)))
    (when (and (eq window (selected-window))
               (window-live-p mevedel-floating-composer--host)
               (eq (window-buffer mevedel-floating-composer--host) (current-buffer)))
      (mevedel-floating-composer--select mevedel-floating-composer--host
                                         (window-point window)))
    ;; A redirect to a deleted frame would outlive it.
    (mevedel-floating-composer--release)
    (delete-frame (window-frame window)))
  (when (timerp mevedel-floating-composer--timer)
    (cancel-timer mevedel-floating-composer--timer))
  (setq mevedel-floating-composer--frame nil
        mevedel-floating-composer--host nil
        mevedel-floating-composer--timer nil))

(defun mevedel-floating-composer--split-host (window size side &optional pixelwise)
  "Split the host of floating-composer WINDOW instead of WINDOW.
SIZE, SIDE and PIXELWISE are as for `split-window'."
  (let ((host (buffer-local-value 'mevedel-floating-composer--host (window-buffer window))))
    (unless (window-live-p host)
      (user-error "No transcript window to split"))
    (split-window host size side pixelwise)))

(defun mevedel-floating-composer--scroll-target ()
  "Return the window `scroll-other-window' scrolls.
From the floating composer that is its host."
  (if (and (window-parameter nil 'mevedel-floating-composer)
           (window-live-p mevedel-floating-composer--host))
      mevedel-floating-composer--host
    ;; Binding the local value lets the global default decide.
    (let ((other-window-scroll-default (default-value 'other-window-scroll-default)))
      (other-window-for-scrolling))))

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

(defun mevedel-floating-composer--active-p (frame)
  "Return non-nil when floating-composer FRAME receives the keystrokes."
  (and (eq frame (selected-frame))
       (frame-focus-state (frame-parent frame))))

(defun mevedel-floating-composer--paint-borders ()
  "Repaint every floating composer's border to show its focus."
  (dolist (frame (frame-list))
    (when (frame-parameter frame 'mevedel-view-animate-as-parent)
      (mevedel-note-frame--paint-border frame #'mevedel-floating-composer--active-p))))

(add-function :after after-focus-change-function
              #'mevedel-floating-composer--paint-borders)


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
  "Return non-nil when HOST shows neither its cursor's draft nor the composer."
  (and (not (mevedel-view--position-in-input-region-p (window-point host)))
       (let ((start (mevedel-floating-composer--start)))
         (and start
              (not (and (mevedel-view--position-screen-y start host t)
                        (mevedel-view--position-screen-y (point-max) host t)))))))

(defun mevedel-floating-composer--update (&optional command)
  "Show, place or hide this view's floating composer.
COMMAND non-nil means the user's command just finished: only then may the
cursor move between the host and the floating composer."
  (unless mevedel-floating-composer--updating
    (let* ((mevedel-floating-composer--updating t)
           (selected (selected-window))
           (window (mevedel-floating-composer--window))
           (scrolled (and command (mevedel-view--scroll-command-p this-command))))
      (cond
       ((window-minibuffer-p selected))
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
                    (offset (window-parameter selected 'mevedel-view-scroll-return)))
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
  (when mevedel-floating-composer
    (mevedel-floating-composer--update t)))

(defun mevedel-floating-composer--on-window (window)
  "Respond to a size, buffer or selection change of WINDOW.
These run inside redisplay, so they reschedule rather than make frames;
the floating composer's own changes restore its pin and repaint its
border, which shows whether it is selected."
  (if (window-parameter window 'mevedel-floating-composer)
      (progn (mevedel-floating-composer--pin window)
             (mevedel-floating-composer--paint-borders))
    (mevedel-floating-composer-refresh)))

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
              (top (- (nth 3 (frame-parameter frame 'mevedel-floating-composer-geometry))
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
  (add-hook 'kill-buffer-hook #'mevedel-floating-composer--remove nil t))

(provide 'mevedel-floating-composer)
;;; mevedel-floating-composer.el ends here
