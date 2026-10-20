;;; mevedel-view-native.el --- Independent animation surfaces -*- lexical-binding: t -*-

;;; Commentary:

;; A Wayland surface presents the animation module's prepared samples without
;; waking Emacs redisplay for every frame.  This module owns placement,
;; invalidation and teardown; `mevedel-native' builds and loads the module.
;; Unsupported displays keep the ordinary text renderer.  No native callback
;; calls back into Lisp.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'xml)
(require 'seq)
(require 'mevedel-structs)
(require 'mevedel-utilities)
(require 'mevedel-native)
(require 'mevedel-view-animation)

;; `font.c' (graphical builds only)
(declare-function font-info "font.c" (name &optional frame))

;; `mevedel-view'
(declare-function mevedel-view--set-spinner-option "mevedel-view" (symbol value))
(autoload 'mevedel-view--set-spinner-option "mevedel-view")

;; `native/mevedel-view-native.c'
(declare-function mevedel-view-native--close "ext:mevedel-view-native" (handle))
(declare-function mevedel-view-native--live-p "ext:mevedel-view-native" (handle))
(declare-function mevedel-view-native--move "ext:mevedel-view-native" (handle x y))
(declare-function mevedel-view-native--open "ext:mevedel-view-native"
                  (parent geometry font background timeline elapsed))
(declare-function mevedel-view-native--sample "ext:mevedel-view-native" (handle))
(declare-function mevedel-view-native--stats "ext:mevedel-view-native" ())
(declare-function mevedel-view-native--supported-p "ext:mevedel-view-native" (parent))

(defcustom mevedel-view-native-enabled t
  "Use independent animation surfaces when this display supports them.
The optional module requires PGTK on Wayland, a C compiler, pkg-config,
and Emacs, GTK 3 and Wayland development headers.  It builds once on first
animation use, under `mevedel-user-dir'.  Other displays use ordinary text
animation."
  :type 'boolean
  :initialize #'custom-initialize-default
  :set #'mevedel-view--set-spinner-option
  :group 'mevedel)

(defvar mevedel-view-native--views (make-hash-table :test #'eq)
  "Views subscribed to native placement changes and their rearm callbacks.")
(defvar-local mevedel-view-native--entries nil
  "Live (KEY SIGNATURE HANDLE PLACEMENT TEXT) entries for this view's surfaces.")
(defvar mevedel-view-native--presenting nil
  "Non-nil while committing native presentation at the redisplay boundary.")
(defvar-local mevedel-view-native--pending nil
  "Latest (SPECS EPOCH REARM) coalesced until the parent redisplay.")
(defvar-local mevedel-view-native--content-tick nil
  "Character modification tick when the current surfaces were placed.")
(defvar-local mevedel-view-native--last-samples nil
  "Last presented phases retained through layout invalidation.")
(defvar-local mevedel-view-native--rearm-timer nil
  "One deferred placement check after layout changes.")
(defvar-local mevedel-view-native--settling nil
  "Non-nil while changed text or window geometry awaits redisplay.")
(defvar mevedel-view-native--presented nil
  "(SPECS . HANDLED) of the presentation running at the redisplay boundary.
Its rearm callback reschedules the stream's own timers and asks again with
the same specs; answering from this pair saved a second placement walk.")
(defvar mevedel-view-native--timelines nil
  "Recently prepared timelines, most recent first, keyed by their inputs.
A surface replaced by invalidation reuses its markup instead of escaping
every sample again.")

(defvar-local mevedel-view-native--checked nil
  "Alist of each window's layout inputs when its surfaces were last verified.")

(defvar-local mevedel-view-native--failed nil
  "Recent surface signatures the module refused, and specs that signaled.
A signature holds the label, its style, font, colors and geometry, so
the same label is tried again once its layout changes.  A spec, (STYLE
LABEL FACE PERIOD), stays with text animation until its label changes or
animation stops.  Either way the surfaces before it are not reopened on
each semantic update.")

(defun mevedel-view-native--overlays (window)
  "Return the overlays that can move or repaint this view's surfaces in WINDOW.
Overlay edits leave the text ticks alone; only those between the window
start and the last surface can affect one, through their faces, extent and
properties that hide or insert text."
  (when-let* ((end (cl-loop for entry in mevedel-view-native--entries
                            maximize (or (marker-position (cdr (caar entry))) 0))))
    (mapcar (lambda (overlay)
              (list overlay (overlay-start overlay) (overlay-end overlay)
                    (overlay-get overlay 'invisible) (overlay-get overlay 'display)
                    (overlay-get overlay 'before-string)
                    (overlay-get overlay 'after-string)
                    (copy-tree (overlay-get overlay 'face))
                    (copy-tree (overlay-get overlay 'priority))
                    (overlay-get overlay 'window)))
            (overlays-in (window-start window) (1+ end)))))

(defun mevedel-view-native--layout-key (window)
  "Return the inputs that can move or uncover this view's surfaces in WINDOW.
Pixel placement needs `posn-at-point', a display walk from the window
start: checked before every redisplay, it was 30% of editor CPU while a
reply streamed.  Placement can change only with the text or its
properties (decorative frames aside), overlays, invisibility, its layout
\(see `mevedel--window-text-layout'), the window's start, point, position
or scroll, frame focus, visible child frames, the region or the surfaces
and their targets."
  (list (mevedel-view--content-tick)
        (if (consp buffer-invisibility-spec)
            (copy-sequence buffer-invisibility-spec)
          buffer-invisibility-spec)
        (mevedel-view-native--overlays window)
        (and (use-region-p) (cons (region-beginning) (region-end)))
        (mevedel--window-text-layout window)
        (window-start window) (window-point window) (window-hscroll window)
        (window-vscroll window t)
        (window-inside-pixel-edges window)
        (mevedel-view-native--opaque-p (window-frame window))
        (frame-focus-state (window-frame window))
        (cl-count-if (lambda (frame)
                       (and (eq (frame-parent frame) (window-frame window))
                            (eq (frame-visible-p frame) t)))
                     (frame-list))
        (mapcar (lambda (entry)
                  (list (nth 2 entry)
                        (marker-position (car (caar entry)))
                        (marker-position (cdr (caar entry)))))
                mevedel-view-native--entries)))

(defun mevedel-view-native--opaque-p (frame)
  "Return non-nil unless FRAME is translucent.
A surface paints an opaque background over the label: under translucent
text or a translucent frame it would show a solid box, or the text it
covers through its own translucency."
  (cl-every (lambda (parameter)
              (let ((value (frame-parameter frame parameter)))
                (when (consp value) (setq value (car value)))
                (or (not (numberp value))
                    (if (floatp value) (>= value 1.0) (>= value 100)))))
            '(alpha alpha-background)))

(defun mevedel-view-native-available-p (frame)
  "Return non-nil when FRAME can host an independent animation surface."
  (and mevedel-view-native-enabled (not noninteractive)
       (eq (framep frame) 'pgtk) (not (frame-parent frame))
       (mevedel-view-native--opaque-p frame)
       (mevedel-native-load)
       (mevedel-view-native--supported-p (frame-parameter frame 'window-id))))

(defun mevedel-view-native--markup (sample foreground)
  "Convert SAMPLE's foreground runs to Pango markup over FOREGROUND."
  (let ((start 0) pieces)
    (while (< start (length sample))
      (let* ((end (next-single-property-change start 'face sample (length sample)))
             (face (get-text-property start 'face sample))
             (color (or (and (listp face) (plist-get face :foreground)) foreground)))
        (push (format "<span foreground=\"%s\">%s</span>"
                      (xml-escape-string color t)
                      ;; Labels quote model-supplied tool arguments.  Dropping
                      ;; characters XML cannot carry changes the rendered
                      ;; width, so the module declines and text animates.
                      (xml-escape-string (substring-no-properties sample start end) t))
              pieces)
        (setq start end)))
    (apply #'concat (nreverse pieces))))

(defun mevedel-view-native--timeline (style label period face frame)
  "Prepare native markup for STYLE, LABEL, PERIOD, FACE and FRAME.
The few most recent timelines are kept; the palette is part of their key."
  (when-let* ((colors (mevedel-view-animation--colors face frame)))
    (let ((key (list style label period face colors)))
      (or (cdr (assoc key mevedel-view-native--timelines))
          (when-let* ((sequence (mevedel-view-animation-sequence
                                 style label period face frame)))
            (dotimes (i (1- (length sequence)))
              (let ((entry (aref sequence (1+ i))))
                (aset entry 1 (mevedel-view-native--markup
                               (aref entry 1) (car colors)))))
            (push (cons key sequence) mevedel-view-native--timelines)
            (setq mevedel-view-native--timelines
                  (seq-take mevedel-view-native--timelines 8))
            sequence)))))

(defun mevedel-view-native--visible-p (target window)
  "Return non-nil when TARGET overlaps WINDOW's displayed buffer range.
A clipped start does not make the rest of the label disappear.  Such a
window must participate in the decision to use ordinary text presentation."
  (when-let* ((start (marker-position (car target)))
              (end (marker-position (cdr target)))
              (visible-end (window-end window)))
    (and (< (window-start window) end) (< start visible-end))))

(defun mevedel-view-native--background (position window)
  "Return the background drawn at POSITION in WINDOW, or nil.
Merge the faces redisplay merges there: overlays for every window or for
WINDOW by priority (`hl-line' among them), then text properties, then the
default face, through this buffer's face remapping."
  (let* ((overlays (seq-filter (lambda (overlay)
                                 (memq (overlay-get overlay 'window) (list nil window)))
                               (overlays-at position t)))
         (faces (delq nil (append (mapcar (lambda (overlay) (overlay-get overlay 'face))
                                          overlays)
                                  (list (get-text-property position 'face)
                                        (get-text-property position 'font-lock-face)
                                        'default))))
         (buffer (current-buffer)))
    (with-selected-frame (window-frame window)
      ;; Selecting a frame makes its selected window's buffer current; the
      ;; remapping that applies is this buffer's.
      (with-current-buffer buffer
        (let ((background (aref (face-attributes-as-vector faces)
                                (cl-position :background internal-lisp-face-attributes))))
          (and (stringp background) (color-name-to-rgb background) background))))))

(defun mevedel-view-native--placement (target window)
  "Return TARGET's unoccluded single-line placement in WINDOW, or nil.
The placement is (FRAME GEOMETRY FONT BACKGROUND)."
  (let* ((frame (window-frame window))
         (start (marker-position (car target)))
         (end (marker-position (cdr target)))
         (point (window-point window)))
    (when (and start end (< start end)
               (eq (frame-visible-p frame) t) (frame-focus-state frame)
               (mevedel-view-native--opaque-p frame)
               (not (use-region-p))
               (not (and (<= start point) (< point end)))
               (not (cl-some (lambda (other)
                               (and (eq (frame-parent other) frame)
                                    (eq (frame-visible-p other) t)))
                             (frame-list))))
      (when-let* ((first (posn-at-point start window))
                  ((= start (posn-point first)))
                  (xy (posn-x-y first))
                  ;; One screen line.  A second `posn-at-point' would walk
                  ;; from the window start again: 0.7 ms of the 1.2 ms a
                  ;; placement took while a reply streamed.
                  ((<= end (save-excursion
                             (goto-char start)
                             (vertical-motion 1 window)
                             (point))))
                  (font (font-at start window)))
        (let* ((edges (window-inside-pixel-edges window))
               (x (+ (car edges) (car xy)))
               (y (+ (cadr edges) (cdr xy)))
               (width (car (window-text-pixel-size window start end)))
               (height (cdr (posn-object-width-height first)))
               (info (font-info font frame))
               (ascent (aref info 8))
               (family (font-get font :family))
               (weight (font-get font :weight))
               (slant (font-get font :slant))
               (size (font-get font :size))
               (background (mevedel-view-native--background start window)))
          ;; A row just inserted can report no height before it is laid
          ;; out; that is a later placement, not a failure.
          (when (and background (> width 0) (> height 0) (<= (+ x width) (nth 2 edges))
                     (>= y (cadr edges)) (<= (+ y height) (nth 3 edges)))
            (list frame (vector x y width height ascent)
                  (format "%s %s %s %spx" family
                          (if (memq weight '(normal regular book)) "" weight)
                          (if (eq slant 'normal) "" slant) size)
                  background)))))))

(defun mevedel-view-native--clear ()
  "Release this view's current native surfaces."
  (dolist (entry mevedel-view-native--entries)
    (when (fboundp 'mevedel-view-native--sample)
      (setf (alist-get (caar entry) mevedel-view-native--last-samples nil nil #'eq)
            (mevedel-view-native--sample (nth 2 entry))))
    (mevedel-view-native--close (nth 2 entry)))
  (setq mevedel-view-native--entries nil))

(defun mevedel-view-native-sample (target)
  "Return TARGET's last native presentation phase, including invalidated pixels."
  (if-let* ((entry (cl-find target mevedel-view-native--entries :key #'caar :test #'eq)))
      (mevedel-view-native--sample (nth 2 entry))
    (cdr (assq target mevedel-view-native--last-samples))))

(defun mevedel-view-native-stop ()
  "Release this view's surfaces, pending placement check, and observers."
  (mevedel-view-native--clear)
  (when mevedel-view-native--rearm-timer
    (cancel-timer mevedel-view-native--rearm-timer))
  (setq mevedel-view-native--rearm-timer nil mevedel-view-native--settling nil
        mevedel-view-native--last-samples nil mevedel-view-native--pending nil
        mevedel-view-native--failed nil)
  (remhash (current-buffer) mevedel-view-native--views)
  (remove-hook 'kill-buffer-hook #'mevedel-view-native-stop t)
  (remove-hook 'window-scroll-functions #'mevedel-view-native--invalidate t)
  (remove-hook 'pre-redisplay-functions #'mevedel-view-native--before-redisplay t)
  (when (zerop (hash-table-count mevedel-view-native--views))
    (remove-hook 'window-state-change-functions #'mevedel-view-native--window-change)))

(defun mevedel-view-native--invalidate (&rest _)
  "Hide stale surfaces until changed text or window geometry is redisplayed."
  (mevedel-view-native--clear)
  (setq mevedel-view-native--settling t)
  (when mevedel-view-native--rearm-timer
    (cancel-timer mevedel-view-native--rearm-timer))
  (let ((buffer (current-buffer)))
    (setq mevedel-view-native--rearm-timer
          (run-at-time
           0.2 nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq mevedel-view-native--rearm-timer nil
                       mevedel-view-native--settling nil)
                 (when-let* ((rearm (gethash buffer mevedel-view-native--views)))
                   (funcall rearm)))))))))

(defun mevedel-view-native--before-redisplay (window)
  "Hide stale surfaces before WINDOW displays changed text or geometry.
Transcript projection suppresses modification hooks, so check its character
version here.  Decorative display properties leave this version unchanged.
Surfaces just presented here were placed for every window already."
  (let ((presented nil))
    (when-let* ((pending mevedel-view-native--pending))
      (setq mevedel-view-native--pending nil
            presented t)
      ;; Projection writers can delete and reinsert the same row.  Present only
      ;; their final state, at the same boundary as the parent text surface.
      (let* ((inhibit-redisplay nil)
             (mevedel-view-native--presenting t)
             (mevedel-view-native--presented
              (cons (car pending)
                    (mevedel-view-native-sync (car pending)
                                              (max 0.0 (- (float-time) (cadr pending)))
                                              (nth 2 pending)))))
        (funcall (nth 2 pending))))
    (let ((key (and mevedel-view-native--entries (window-live-p window)
                    (mevedel-view-native--layout-key window))))
      (when (and mevedel-view-native--entries
                 (or
                  ;; The module closes a surface itself when its frame unmaps
                  ;; or changes scale, or drawing fails; nothing else would
                  ;; notice for tool rows without an elapsed-time label.
                  (and (fboundp 'mevedel-view-native--live-p)
                       (cl-some (lambda (entry)
                                  (not (mevedel-view-native--live-p (nth 2 entry))))
                                mevedel-view-native--entries))
                  (and (not presented)
                       (or (not key)
                           (not (equal key (alist-get window mevedel-view-native--checked))))
                       (cl-some
                        (lambda (entry)
                          (let* ((target (caar entry))
                                 (start (marker-position (car target)))
                                 (end (marker-position (cdr target))))
                            (or (not start) (not end)
                                (and (not (eql mevedel-view-native--content-tick
                                               (buffer-chars-modified-tick)))
                                     (not (equal (nth 4 entry)
                                                 (buffer-substring-no-properties start end))))
                                (and (eq window (cadar entry))
                                     (not (equal (nth 3 entry)
                                                 (mevedel-view-native--placement
                                                  target window)))))))
                        mevedel-view-native--entries))))
        (mevedel-view-native--invalidate)
        ;; Invalidation removed the surfaces the overlay inputs were taken from.
        (setq key (and (window-live-p window) (mevedel-view-native--layout-key window))))
      (setq mevedel-view-native--content-tick (buffer-chars-modified-tick))
      (when (window-live-p window)
        (setf (alist-get window mevedel-view-native--checked)
              (or key (mevedel-view-native--layout-key window))))))
  (setq mevedel-view-native--checked
        (cl-remove-if-not (lambda (entry) (window-live-p (car entry)))
                          mevedel-view-native--checked)))


(defun mevedel-view-native--window-change (_frame)
  "Invalidate registered views after a window configuration change."
  (maphash (lambda (buffer _callback)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (mevedel-view-native--invalidate))))
           mevedel-view-native--views))

(defun mevedel-view-native-sync (specs elapsed rearm)
  "Queue SPECS at ELAPSED seconds, calling REARM after layout changes.
Each spec is (TARGET STYLE LABEL FACE PERIOD), with a marker pair TARGET.
Coalesce presentation at the parent redisplay boundary.  Return targets
already entirely handled by native surfaces; the caller schedules
ordinary text animation for the others.  Lifecycle and placement are local
to this module, including invalidation while a composer is being edited.
Empty SPECS still wait for that boundary, since a writer may reinsert the
row first, but mark the view's windows for redisplay: pre-redisplay hooks
run only for windows being redisplayed, and a surface whose teardown waits
for an unchanged window keeps animating on its own.  An error or a refused
surface hands that target back to text animation, closing only its own
surfaces, and its spec keeps text animation until its label changes or
animation stops: retrying reopened it about once a second."
  (cond
   ((and mevedel-view-native--presenting mevedel-view-native--presented
         (equal specs (car mevedel-view-native--presented)))
    (cdr mevedel-view-native--presented))
   ((and (not mevedel-view-native--presenting)
         (or mevedel-view-native--entries mevedel-view-native--pending
             (and specs mevedel-view-native-enabled (not noninteractive)
                  (cl-some #'mevedel-view-native-available-p (frame-list)))))
    (progn
      (setq mevedel-view-native--pending (list specs (- (float-time) elapsed) rearm))
      (unless specs (force-window-update (current-buffer)))
      ;; Sample after global redisplay hooks have restored paint overlays such
      ;; as per-window hl-line after a semantic row replacement.
      (add-hook 'pre-redisplay-functions #'mevedel-view-native--before-redisplay 90 t)
      (cl-loop for spec in specs
               when (cl-find (car spec) mevedel-view-native--entries
                             :key #'caar :test #'eq)
               collect (car spec))))
   (t
    (setq mevedel-view-native--pending nil)
    (let (handled next)
      (when (and specs mevedel-view-native-enabled (not noninteractive)
                 (cl-some #'mevedel-view-native-available-p (frame-list)))
        (puthash (current-buffer) rearm mevedel-view-native--views)
        (add-hook 'kill-buffer-hook #'mevedel-view-native-stop nil t)
        (add-hook 'window-scroll-functions #'mevedel-view-native--invalidate nil t)
        (add-hook 'pre-redisplay-functions #'mevedel-view-native--before-redisplay 90 t)
        (add-hook 'window-state-change-functions #'mevedel-view-native--window-change)
        (unless mevedel-view-native--settling
          (dolist (spec specs)
            (pcase-let ((`(,target ,style ,label ,face ,period) spec))
              (let (candidates failed refused opened)
                (if (member (cdr spec) mevedel-view-native--failed)
                    (setq failed t)
                  ;; An error or a refused surface affects only this label.
                  (condition-case nil
                      (dolist (window (get-buffer-window-list (current-buffer) nil t))
                        (when (mevedel-view-native--visible-p target window)
                          (let* ((placement (mevedel-view-native--placement target window))
                                 (frame (car placement))
                                 (geometry (cadr placement))
                                 (font (nth 2 placement))
                                 (background (nth 3 placement))
                                 (key (list target window))
                                 (colors (and frame (mevedel-view-animation--colors face frame)))
                                 (signature (and colors
                                                 (list style label face period frame font colors
                                                       background
                                                       (append (seq-subseq geometry 2) nil))))
                                 ;; Semantic redraws release their markers even when
                                 ;; the label and its on-screen geometry are unchanged,
                                 ;; and text streamed in above moves it.  Prefer the
                                 ;; surface at the same place; otherwise move one whose
                                 ;; own target is no longer requested.  Reopening
                                 ;; rebuilt every sample's layout and buffers after each
                                 ;; streamed render, about 9% editor CPU.
                                 (unused (lambda (entry)
                                           (and (eq window (cadar entry))
                                                (equal signature (cadr entry))
                                                (not (cl-find
                                                      (nth 2 entry) (append candidates next)
                                                      :key (lambda (item) (nth 2 item)))))))
                                 (old (or (assoc key mevedel-view-native--entries)
                                          (cl-find-if
                                           (lambda (entry)
                                             (and (funcall unused entry)
                                                  (equal placement (nth 3 entry))))
                                           mevedel-view-native--entries)
                                          (cl-find-if
                                           (lambda (entry)
                                             (and (funcall unused entry)
                                                  (not (assq (caar entry) specs))))
                                           mevedel-view-native--entries)))
                                 handle)
                            (when (and placement colors (mevedel-view-native-available-p frame)
                                       (not (member signature mevedel-view-native--failed)))
                              (when (and old (equal signature (cadr old))
                                         (mevedel-view-native--move (nth 2 old)
                                                                    (aref geometry 0) (aref geometry 1)))
                                (setq handle (nth 2 old)))
                              (unless handle
                                (setq handle (mevedel-view-native--open
                                              (frame-parameter frame 'window-id) geometry font
                                              background
                                              (mevedel-view-native--timeline style label period face frame)
                                              (float elapsed)))
                                (if handle
                                    (push handle opened)
                                  (setq refused (copy-tree signature)))))
                            (if handle (push (list key signature handle placement
                                                   (buffer-substring-no-properties
                                                    (car target) (cdr target))) candidates)
                              (setq failed t)))))
                    (error (setq refused (copy-sequence (cdr spec))))))
                (when refused
                  (setq failed t
                        mevedel-view-native--failed
                        (seq-take (cons refused mevedel-view-native--failed) 8)))
                (if (or failed (not candidates))
                    (dolist (handle (delete-dups (append opened (mapcar #'caddr candidates))))
                      (mevedel-view-native--close handle))
                  (push target handled)
                  (setq next (append candidates next))))))))
      (dolist (entry mevedel-view-native--entries)
        (unless (cl-find (nth 2 entry) next :key (lambda (item) (nth 2 item)))
          (mevedel-view-native--close (nth 2 entry))))
      (setq mevedel-view-native--entries next
            mevedel-view-native--content-tick (buffer-chars-modified-tick)
            mevedel-view-native--last-samples
            (cl-remove-if-not (lambda (entry) (assq (car entry) specs))
                              mevedel-view-native--last-samples))
      (unless (and specs mevedel-view-native-enabled) (mevedel-view-native-stop))
      handled))))

(defun mevedel-view-native-unload-function ()
  "Release presentations and pending hooks before unloading this feature."
  (dolist (buffer (buffer-list))
    (when (or (gethash buffer mevedel-view-native--views)
              (local-variable-p 'mevedel-view-native--pending buffer))
      (with-current-buffer buffer (mevedel-view-native-stop))))
  nil)

(provide 'mevedel-view-native)
;;; mevedel-view-native.el ends here
