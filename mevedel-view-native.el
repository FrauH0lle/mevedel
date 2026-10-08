;;; mevedel-view-native.el --- Independent animation surfaces -*- lexical-binding: t -*-

;;; Commentary:

;; A Wayland surface presents the animation module's prepared samples without
;; waking Emacs redisplay for every frame.  This module owns optional module
;; construction, placement, invalidation and teardown.  Unsupported displays
;; keep the ordinary text renderer.  No native callback calls back into Lisp.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'xml)
(require 'seq)
(require 'mevedel-structs)
(require 'mevedel-view-animation)

;; `mevedel-structs'
(defvar mevedel-user-dir)

;; `mevedel-utilities'
(autoload 'mevedel-library-source-directory "mevedel-utilities")

;; `mevedel-view'
(declare-function mevedel-view--set-spinner-option "mevedel-view" (symbol value))
(autoload 'mevedel-view--set-spinner-option "mevedel-view")

;; `url'
(declare-function url-retrieve-synchronously "url" (url &optional silent inhibit-cookies timeout))
(defvar url-http-response-status)

;; `native/mevedel-view-native.c'
(declare-function mevedel-view-native--close "ext:mevedel-view-native" (handle))
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

(defconst mevedel-view-native--directory
  (mevedel-library-source-directory (or load-file-name buffer-file-name))
  "Package source directory, including its native module source.")

(defvar mevedel-view-native--load-state nil
  "Native module loading result: nil, ready, or an explanatory string.
A failure is not retried in this session; set this to nil to retry.")

(defun mevedel-view-native--include-flags ()
  "Return `-I' flags locating the running Emacs's `emacs-module.h'.
An Emacs installed under its own prefix keeps the header in that prefix's
include directory, and one run from its build tree beside the binary."
  (cl-loop for directory in (list (expand-file-name "../include" invocation-directory)
                                  invocation-directory)
           when (file-exists-p (expand-file-name "emacs-module.h" directory))
           collect (concat "-I" directory)))
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

(defun mevedel-view-native--layout-key (window)
  "Return the inputs that can move or uncover this view's surfaces in WINDOW.
Pixel placement needs `posn-at-point', a display walk from the window
start: checked before every redisplay, it was 30% of editor CPU while a
reply streamed.  Placement can change only with the text, the window's
start, point, size or scroll, frame focus, visible child frames, the
region, face remapping, font size, line spacing or the surfaces and their
targets.  Text scaling edits the remapping list in place, so it is copied."
  (list (buffer-chars-modified-tick)
        (and (use-region-p) (cons (region-beginning) (region-end)))
        (copy-tree face-remapping-alist)
        ;; `window-font-width' realizes faces: 14% of samples per redisplay.
        ;; Buffer-local fonts are in the remapping; the frame's are here.
        line-spacing (frame-char-width (window-frame window))
        (frame-char-height (window-frame window))
        (window-start window) (window-point window) (window-hscroll window)
        (window-inside-pixel-edges window)
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

(defconst mevedel-view-native--release-url
  "https://github.com/FrauH0lle/mevedel/releases/download/native-%s/mevedel-view-native-%s.so"
  "Prebuilt module download, by source hash and architecture.")

(defun mevedel-view-native--source ()
  "Return the native module's C source file."
  (file-name-concat mevedel-view-native--directory "native/mevedel-view-native.c"))

(defun mevedel-view-native--source-hash ()
  "Return the first 16 hex digits of the module source's SHA-256."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally (mevedel-view-native--source))
    (substring (secure-hash 'sha256 (current-buffer)) 0 16)))

(defun mevedel-view-native--arch ()
  "Return this system's architecture as prebuilt modules name it."
  (car (split-string system-configuration "-")))

(defun mevedel-view-native--pin ()
  "Return the pinned SHA-256 of the prebuilt module for this source and system.
CI builds each source revision and records its checksums in
native/prebuilt.eld; a download that does not match is never loaded."
  (when-let* ((file (file-name-concat mevedel-view-native--directory
                                      "native/prebuilt.eld"))
              ((file-readable-p file))
              (pins (with-temp-buffer
                      (insert-file-contents file)
                      (read (current-buffer)))))
    (cdr (assoc (mevedel-view-native--arch)
                (cdr (assoc (mevedel-view-native--source-hash) pins))))))

(defun mevedel-view-native--prebuilt-path ()
  "Return where a downloaded prebuilt module for this source is kept."
  (file-name-concat mevedel-user-dir "native"
                    (format "prebuilt-%s-%s%s" (mevedel-view-native--source-hash)
                            (mevedel-view-native--arch) module-file-suffix)))

(defun mevedel-view-native--verified-prebuilt ()
  "Return the downloaded prebuilt module when it matches its pin, or nil."
  (when-let* ((pin (mevedel-view-native--pin))
              (path (mevedel-view-native--prebuilt-path))
              ((file-exists-p path)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (and (equal pin (secure-hash 'sha256 (current-buffer))) path))))

(defun mevedel-view-native--build ()
  "Return the locally built module, building it first when needed."
  (let* ((source (mevedel-view-native--source))
         (hash (with-temp-buffer
                 (insert-file-contents-literally source)
                 (insert system-configuration emacs-version)
                 (secure-hash 'sha256 (current-buffer))))
         (directory (file-name-concat mevedel-user-dir "native"))
         (module (file-name-concat
                  directory (concat "view-" (substring hash 0 16)
                                    module-file-suffix))))
    (unless (file-exists-p module)
      (unless (and (executable-find "cc") (executable-find "pkg-config"))
        (error "Native animation needs cc and pkg-config"))
      (make-directory directory t)
      (let ((temporary (make-temp-file (file-name-concat directory "build-")))
            (default-directory directory))
        (unwind-protect
            (with-temp-buffer
              (unless (zerop (call-process
                              "pkg-config" nil t nil "--cflags" "--libs"
                              "gtk+-3.0" "wayland-client"))
                (error "GTK/Wayland development files are unavailable"))
              (let ((flags (split-string-and-unquote (buffer-string))))
                (erase-buffer)
                ;; No -Werror: a warning from newer headers must not
                ;; disable the renderer; development builds use it.
                (unless (zerop (apply #'call-process "cc" nil t nil
                                      "-shared" "-fPIC" "-O2"
                                      (append (mevedel-view-native--include-flags)
                                              (list source "-o" temporary)
                                              flags '("-lm"))))
                  (error "Native animation compilation failed: %s"
                         (string-trim (buffer-string)))))
              (rename-file temporary module t))
          (when (file-exists-p temporary) (delete-file temporary)))))
    module))

(defun mevedel-view-native--report-failure ()
  "Say once why native animation is unavailable and how to enable it.
Without it, animation redraws the whole editor, several times the CPU."
  (message "mevedel: native animation unavailable (%s); animations use more CPU.  %s"
           mevedel-view-native--load-state
           (if (mevedel-view-native--pin)
               "M-x mevedel-view-native-install downloads a prebuilt module."
             "Install cc, pkg-config and the GTK 3/Wayland development files to build it.")))

(defun mevedel-view-native--load ()
  "Load the optional native presenter once at first use.
A cached local build comes first, then a verified prebuilt download, then
a new local build.  A failure is reported once and not retried."
  (unless mevedel-view-native--load-state
    (setq mevedel-view-native--load-state
          (condition-case err
              (progn
                (module-load (or (mevedel-view-native--verified-prebuilt)
                                 (mevedel-view-native--build)))
                'ready)
            (error (error-message-string err))))
    (unless (eq mevedel-view-native--load-state 'ready)
      (mevedel-view-native--report-failure)))
  (eq mevedel-view-native--load-state 'ready))

(defun mevedel-view-native--download (url)
  "Return URL's body as a unibyte string, or signal an error."
  (require 'url)
  (let ((buffer (url-retrieve-synchronously url t t 60)))
    (unless buffer (error "Download failed: %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (unless (eql 200 (bound-and-true-p url-http-response-status))
            (error "Download failed with HTTP status %s"
                   (bound-and-true-p url-http-response-status)))
          (set-buffer-multibyte nil)
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n")
          (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer buffer))))

;;;###autoload
(defun mevedel-view-native-install ()
  "Download the prebuilt native animation module for this system and load it.
For systems without a C compiler or the GTK 3/Wayland development files.
The download must match the checksum pinned in mevedel's source."
  (interactive)
  (let ((pin (or (mevedel-view-native--pin)
                 (user-error "No prebuilt module for %s at this mevedel revision"
                             (mevedel-view-native--arch))))
        (path (mevedel-view-native--prebuilt-path))
        (url (format mevedel-view-native--release-url
                     (mevedel-view-native--source-hash) (mevedel-view-native--arch))))
    (message "Downloading %s..." url)
    (let ((data (mevedel-view-native--download url)))
      (unless (equal pin (secure-hash 'sha256 data))
        (user-error "Downloaded module does not match its pinned checksum"))
      (make-directory (file-name-directory path) t)
      (let ((temporary (make-temp-file (concat path "-"))))
        (unwind-protect
            (let ((coding-system-for-write 'no-conversion))
              (write-region data nil temporary nil 'silent)
              (rename-file temporary path t))
          (when (file-exists-p temporary) (delete-file temporary)))))
    (setq mevedel-view-native--load-state nil)
    (if (mevedel-view-native--load)
        (message "Native animation module installed")
      (user-error "Native animation module did not load: %s"
                  mevedel-view-native--load-state))))

(defun mevedel-view-native-available-p (frame)
  "Return non-nil when FRAME can host an independent animation surface."
  (and mevedel-view-native-enabled (not noninteractive)
       (eq (framep frame) 'pgtk) (not (frame-parent frame))
       (fboundp 'module-load) module-file-suffix
       (mevedel-view-native--load)
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

(defun mevedel-view-native--placement (target window)
  "Return TARGET's unoccluded single-line placement in WINDOW, or nil."
  (let* ((frame (window-frame window))
         (start (marker-position (car target)))
         (end (marker-position (cdr target)))
         (point (window-point window)))
    (when (and start end (< start end)
               (eq (frame-visible-p frame) t) (frame-focus-state frame)
               (not (use-region-p))
               (not (and (<= start point) (< point end)))
               (not (cl-some (lambda (other)
                               (and (eq (frame-parent other) frame)
                                    (eq (frame-visible-p other) t)))
                             (frame-list))))
      (when-let* ((first (posn-at-point start window))
                  (last (posn-at-point end window))
                  ((= start (posn-point first)))
                  ((= end (posn-point last)))
                  (xy (posn-x-y first)) (end-xy (posn-x-y last))
                  ((= (cdr xy) (cdr end-xy)))
                  (font (font-at start window)))
        (let* ((edges (window-inside-pixel-edges window))
               (x (+ (car edges) (car xy)))
               (y (+ (cadr edges) (cdr xy)))
               (width (- (car end-xy) (car xy)))
               (height (cdr (posn-object-width-height first)))
               (info (font-info font frame))
               (ascent (aref info 8))
               (family (font-get font :family))
               (weight (font-get font :weight))
               (slant (font-get font :slant))
               (size (font-get font :size)))
          (when (and (> width 0) (<= (+ x width) (nth 2 edges))
                     (>= y (cadr edges)) (<= (+ y height) (nth 3 edges)))
            (list frame (vector x y width height ascent)
                  (format "%s %s %s %spx" family
                          (if (memq weight '(normal regular book)) "" weight)
                          (if (eq slant 'normal) "" slant) size))))))))

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
        mevedel-view-native--last-samples nil mevedel-view-native--pending nil)
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
version here.  Decorative display properties leave this version unchanged."
  (when-let* ((pending mevedel-view-native--pending))
    (setq mevedel-view-native--pending nil)
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
  (when (and mevedel-view-native--entries
             (or (not (window-live-p window))
                 (not (equal (mevedel-view-native--layout-key window)
                             (alist-get window mevedel-view-native--checked))))
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
                                       (mevedel-view-native--placement target window)))))))
              mevedel-view-native--entries))
    (mevedel-view-native--invalidate))
  (setq mevedel-view-native--content-tick (buffer-chars-modified-tick))
  (when (window-live-p window)
    (setf (alist-get window mevedel-view-native--checked)
          (mevedel-view-native--layout-key window)))
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
for an unchanged window keeps animating on its own.  An error while
placing or preparing a surface closes the surfaces opened so far and
hands every target back to text animation."
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
      (add-hook 'pre-redisplay-functions #'mevedel-view-native--before-redisplay nil t)
      (cl-loop for spec in specs
               when (cl-find (car spec) mevedel-view-native--entries
                             :key #'caar :test #'eq)
               collect (car spec))))
   (t
    (setq mevedel-view-native--pending nil)
    (let (handled next opened)
      (when (and specs mevedel-view-native-enabled (not noninteractive)
                 (cl-some #'mevedel-view-native-available-p (frame-list)))
        (puthash (current-buffer) rearm mevedel-view-native--views)
        (add-hook 'kill-buffer-hook #'mevedel-view-native-stop nil t)
        (add-hook 'window-scroll-functions #'mevedel-view-native--invalidate nil t)
        (add-hook 'pre-redisplay-functions #'mevedel-view-native--before-redisplay nil t)
        (add-hook 'window-state-change-functions #'mevedel-view-native--window-change)
        (condition-case nil
            (unless mevedel-view-native--settling
              (dolist (spec specs)
                (pcase-let ((`(,target ,style ,label ,face ,period) spec))
                  (let (candidates failed)
                    (dolist (window (get-buffer-window-list (current-buffer) nil t))
                      (when (mevedel-view-native--visible-p target window)
                        (let* ((placement (mevedel-view-native--placement target window))
                               (frame (car placement))
                               (geometry (cadr placement))
                               (font (nth 2 placement))
                               (key (list target window))
                               (colors (and frame (mevedel-view-animation--colors face frame)))
                               (signature (and colors
                                               (list style label face period frame font colors
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
                          (when (and placement colors (mevedel-view-native-available-p frame))
                            (when (and old (equal signature (cadr old))
                                       (mevedel-view-native--move (nth 2 old)
                                                                  (aref geometry 0) (aref geometry 1)))
                              (setq handle (nth 2 old)))
                            (unless handle
                              (setq handle (mevedel-view-native--open
                                            (frame-parameter frame 'window-id) geometry font
                                            (cdr colors)
                                            (mevedel-view-native--timeline style label period face frame)
                                            (float elapsed)))
                              (when handle (push handle opened))))
                          (if handle (push (list key signature handle placement
                                                 (buffer-substring-no-properties
                                                  (car target) (cdr target))) candidates)
                            (setq failed t)))))
                    (if (or failed (not candidates))
                        (dolist (entry candidates) (mevedel-view-native--close (nth 2 entry)))
                      (push target handled)
                      (setq next (append candidates next)))))))
          (error
           (mapc #'mevedel-view-native--close opened)
           (setq handled nil next nil))))
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
