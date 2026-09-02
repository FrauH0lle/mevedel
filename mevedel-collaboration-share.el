;;; mevedel-collaboration-share.el --- Collaboration share surface -*- lexical-binding: t; -*-

;;; Commentary:

;; Presents collaboration bearer links and QR codes in a dedicated child
;; frame, with an ordinary-window fallback for terminal displays and child
;; frame failures.

;;; Code:

;; `mevedel-directive-frame'
(defvar mevedel--child-frame-parameters)
(defvar mevedel-directive-frame-border-width)

;; `qrencode'
(declare-function qrencode "ext:qrencode"
                  (s &optional mode errcorr return-raw))

(defface mevedel-collaboration-share-border
  '((t :inherit mevedel-directive-frame-border))
  "Border colour of the share child frame.
The share panel borrows the directive frame\='s border so both float
above the session with the same weight."
  :group 'mevedel)

(defvar mevedel-collaboration--share-frame nil
  "Live share child frame, or nil.")

(defvar-local mevedel-collaboration--share-room nil
  "Room whose bearer links this share buffer presents.")

(defvar-local mevedel-collaboration--share-which 'view
  "Which link's QR the share buffer shows: `view', `full', or `owner'.")

(defconst mevedel-collaboration--share-tiers
  '((view  . :link-view)
    (full  . :link-full)
    (owner . :link-owner))
  "Bearer tiers in ascending authority, mapped to their room link slot.")

(defun mevedel-collaboration--share-content (room which)
  "Return the share buffer text for ROOM showing WHICH link's QR.

WHICH is `view', `full', or `owner'.  One QR at a time, the view link
by default: codes side by side is how a colleague scans the wrong one
and walks away with write authority."
  (let* ((link (plist-get room (alist-get
                                which mevedel-collaboration--share-tiers)))
         ;; The QR is the convenience; the link beneath it is the
         ;; payload.  An encoder that is missing or signals must cost
         ;; the code, never the share.
         (code (condition-case error-data
                   (if (fboundp 'qrencode)
                       (propertize (qrencode link) 'face '(:height 1.6))
                     (error "QR encoder unavailable"))
                 (error
                  (propertize
                   (format "QR unavailable (%s); copy the link below."
                           (error-message-string error-data))
                   'face 'shadow)))))
    (concat
     (propertize (format "Share: %s\n" (plist-get room :session-label))
                 'face 'bold)
     (pcase which
       ('owner
        (propertize
         (concat "OWNER link — full control, and additionally the two\n"
                 "authorities that otherwise need you at the keyboard:\n"
                 "changing permission mode and creating a session.\n")
         'face 'error))
       ('full
        (propertize
         "FULL CONTROL link — grants prompting, interrupting, answering\n"
         'face 'error))
       (_ (propertize "View link — read-only\n" 'face 'success)))
     "\n"
     ;; Scaled so a phone camera resolves the half-block modules from a
     ;; normal viewing distance.
     code
     "\n\n"
     link
     "\n\n"
     (propertize
      (concat "TAB show "
              (symbol-name (mevedel-collaboration--share-next-tier which))
              " QR"
              "  ·  c copy view  ·  f copy full  ·  o copy owner"
              "  ·  q close\n"
              "Links are bearer credentials; treat them like secrets.")
      'face 'shadow))))

(defun mevedel-collaboration--share-next-tier (which)
  "Return the tier shown after WHICH when cycling the share buffer."
  (let ((tiers (mapcar #'car mevedel-collaboration--share-tiers)))
    (or (cadr (memq which tiers)) (car tiers))))

(defun mevedel-collaboration--share-render ()
  "Repaint the current share buffer from its room and selection."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (mevedel-collaboration--share-content
             mevedel-collaboration--share-room
             mevedel-collaboration--share-which))
    (goto-char (point-min))))

(defun mevedel-collaboration-share-toggle ()
  "Show the next bearer link's QR."
  (interactive)
  (setq mevedel-collaboration--share-which
        (mevedel-collaboration--share-next-tier
         mevedel-collaboration--share-which))
  (mevedel-collaboration--share-render))

(defun mevedel-collaboration-share-copy-view ()
  "Copy the view link to the kill ring."
  (interactive)
  (kill-new (plist-get mevedel-collaboration--share-room :link-view))
  (message "mevedel: view link copied"))

(defun mevedel-collaboration-share-copy-full ()
  "Copy the full-control link to the kill ring."
  (interactive)
  (kill-new (plist-get mevedel-collaboration--share-room :link-full))
  (message "mevedel: full-control link copied"))

(defun mevedel-collaboration-share-copy-owner ()
  "Copy the owner link to the kill ring."
  (interactive)
  (kill-new (plist-get mevedel-collaboration--share-room :link-owner))
  (message "mevedel: owner link copied"))

(defun mevedel-collaboration-share-quit ()
  "Close the share surface."
  (interactive)
  (let ((frame mevedel-collaboration--share-frame)
        (buffer (get-buffer "*mevedel share*")))
    (setq mevedel-collaboration--share-frame nil)
    (when (frame-live-p frame)
      (delete-frame frame))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defvar-keymap mevedel-collaboration--share-map
  :doc "Keys available in the collaboration share buffer."
  "TAB" #'mevedel-collaboration-share-toggle
  "<tab>" #'mevedel-collaboration-share-toggle
  "c" #'mevedel-collaboration-share-copy-view
  "f" #'mevedel-collaboration-share-copy-full
  "o" #'mevedel-collaboration-share-copy-owner
  "q" #'mevedel-collaboration-share-quit)

(defun mevedel-collaboration--fit-share-frame (frame parent)
  "Size FRAME to its buffer in pixels, bounded by PARENT.

`fit-frame-to-buffer\=' works in canonical character heights, and the QR
is scaled well above one: the rounding loses part of a line and clips
the key legend off the bottom.  Pixels are what the content actually
occupies."
  (let* ((window (frame-root-window frame))
         (border (* 2 (or (frame-parameter frame 'internal-border-width) 0)))
         (max-width (max 200 (- (frame-pixel-width parent) 80)))
         (max-height (max 200 (- (frame-pixel-height parent) 80)))
         (size (window-text-pixel-size window nil nil max-width max-height)))
    (set-frame-size frame
                    (min max-width (+ (car size) border))
                    (min max-height (+ (cdr size) border))
                    t)))

(defun mevedel-collaboration--center-frame (frame parent)
  "Place FRAME at the centre of PARENT."
  (let ((width (frame-pixel-width frame))
        (height (frame-pixel-height frame)))
    (set-frame-position
     frame
     (max 0 (/ (- (frame-pixel-width parent) width) 2))
     (max 0 (/ (- (frame-pixel-height parent) height) 2)))))

(defun mevedel-collaboration--show-share-frame (room)
  "Present ROOM\='s bearer links and QR code on a dedicated surface.
A centred child frame on a graphical display, an ordinary window
otherwise."
  ;; Cold entry point: the border face inherits the directive frame's,
  ;; and an unrealized parent face would leave the border invisible.
  (require 'mevedel-directive-frame)
  (when (or mevedel-collaboration--share-frame
            (get-buffer "*mevedel share*"))
    (mevedel-collaboration-share-quit))
  (let ((buffer (get-buffer-create "*mevedel share*"))
        (parent (selected-frame)))
    (with-current-buffer buffer
      (setq-local mevedel-collaboration--share-room room)
      (setq-local mevedel-collaboration--share-which 'view)
      (setq buffer-read-only t
            truncate-lines t
            cursor-type nil
            mode-line-format nil)
      (use-local-map mevedel-collaboration--share-map)
      (mevedel-collaboration--share-render))
    (if (display-graphic-p parent)
        (condition-case nil
            (let ((frame
                   (make-frame
                    `((name . "mevedel-share")
                      (parent-frame . ,parent)
                      (minibuffer . ,(minibuffer-window parent))
                      (font . ,(frame-parameter parent 'font))
                      (internal-border-width
                       . ,mevedel-directive-frame-border-width)
                      (child-frame-border-width
                       . ,mevedel-directive-frame-border-width)
                      (width . 0) (height . 0) (visibility . nil)
                      ,@mevedel--child-frame-parameters))))
              (let ((window (frame-root-window frame)))
                (set-window-buffer window buffer)
                (set-window-dedicated-p window t)
                ;; A window parameter rather than the buffer\='s own
                ;; setting, so a fallback window elsewhere keeps its
                ;; mode line.
                (set-window-parameter window 'mode-line-format 'none)
                (set-window-fringes window 0 0))
              ;; The border only draws once the faces carry a colour.
              (let ((color (face-attribute 'mevedel-collaboration-share-border
                                           :background nil t)))
                (set-face-background 'internal-border color frame)
                (set-face-background 'child-frame-border color frame))
              (mevedel-collaboration--fit-share-frame frame parent)
              (mevedel-collaboration--center-frame frame parent)
              (make-frame-visible frame)
              (select-frame-set-input-focus frame)
              (setq mevedel-collaboration--share-frame frame))
          ;; Creating a frame realizes every face for it, so a defect
          ;; entirely outside mevedel -- a theme whose face specs form
          ;; an inheritance cycle -- signals here.  The share must still
          ;; be presentable: fall back to an ordinary window.
          (error (pop-to-buffer buffer)))
      (pop-to-buffer buffer))))

(defun mevedel-collaboration-share-present (room)
  "Copy ROOM's full link and present both bearer links.
The links render in the share surface rather than in *Messages*, whose
log is durable and easy to leak."
  (require 'qrencode nil t)
  (kill-new (plist-get room :link-full))
  (mevedel-collaboration--show-share-frame room)
  (message "mevedel: full-control link copied to kill ring"))

(defun mevedel-collaboration-share-dismiss (room)
  "Close the share surface when it presents ROOM."
  (when-let* ((buffer (get-buffer "*mevedel share*"))
              ((eq room (buffer-local-value
                         'mevedel-collaboration--share-room buffer))))
    (mevedel-collaboration-share-quit)))

(provide 'mevedel-collaboration-share)
;;; mevedel-collaboration-share.el ends here
