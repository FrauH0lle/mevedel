;;; native-wayland.el --- Native presenter scenario on a real compositor -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded by a pgtk daemon that `test-niwa-view-native-wayland.el' starts
;; against a headless compositor.  It drives the real module through a frame
;; on that display and writes what it observed to the file named by
;; NIWA_NATIVE_RESULT, then exits.  The Wayland requests themselves are
;; in the daemon's WAYLAND_DEBUG trace, which the test reads.

;;; Code:

(require 'cl-lib)
(require 'hl-line)
(require 'niwa-view-native)

(defvar niwa-native-wayland--results nil
  "Plist of observations, newest first.")

(defun niwa-native-wayland--note (key value)
  "Record VALUE under KEY."
  (setq niwa-native-wayland--results
        (plist-put niwa-native-wayland--results key value)))

(defun niwa-native-wayland--wait (seconds &optional predicate)
  "Run the event loop for SECONDS, or until PREDICATE holds."
  (let ((end (+ (float-time) seconds)))
    (while (and (< (float-time) end)
                (not (and predicate (funcall predicate))))
      (sit-for 0.02 t))))

(defun niwa-native-wayland--frame (display)
  "Return a focused frame on DISPLAY showing a view-like buffer."
  (let ((frame (make-frame-on-display
                display '((width . 80) (height . 20) (menu-bar-lines . 0)
                          (tool-bar-lines . 0) (vertical-scroll-bars . nil)))))
    ;; New windows get the compositor's focus.  `x-focus-frame' aborted a
    ;; pgtk daemon here.
    (select-frame frame)
    (niwa-native-wayland--wait
     5 (lambda () (and (eq (frame-visible-p frame) t) (frame-focus-state frame))))
    frame))

(defun niwa-native-wayland--present (specs)
  "Queue SPECS, as a view does, and let the next redisplay present them."
  (let (rearm)
    ;; As the view's own rearm does: sync again with the same callback.
    (setq rearm (lambda () (niwa-view-native-sync specs 0 rearm)))
    (funcall rearm))
  (redisplay t)
  (niwa-native-wayland--wait 0.3))

(defun niwa-native-wayland--scenario (display)
  "Exercise the presenter on DISPLAY."
  (let* ((frame (niwa-native-wayland--frame display))
         (buffer (get-buffer-create "*native-wayland*")))
    (niwa-native-wayland--note :frame-ready (and (frame-focus-state frame) t))
    (progn
      (switch-to-buffer buffer)
      (insert "first line\nsecond line\nWorking...\n")
      (goto-char (point-max))
      (redisplay t)
      (let* ((window (selected-window))
             (target (cons (copy-marker 24) (copy-marker 34)))
             (specs (list (list target 'bounce "Working..." 'default 0.05))))
        (niwa-native-wayland--note :available (and (niwa-view-native-available-p frame) t))
        (let ((placement (niwa-view-native--placement target window)))
          (niwa-native-wayland--note :placement (cdr placement))
          ;; The surface is positioned relative to the toplevel; the text
          ;; area may start below or right of it.
          (niwa-native-wayland--note
           :native-offset
           (let ((outer (frame-edges frame 'outer-edges))
                 (native (frame-edges frame 'native-edges)))
             (list (- (nth 0 native) (nth 0 outer)) (- (nth 1 native) (nth 1 outer))))))
        (niwa-native-wayland--present specs)
        (let ((handle (nth 2 (car niwa-view-native--entries))))
          (niwa-native-wayland--note :opened (and handle (niwa-view-native--live-p handle) t))
          (niwa-native-wayland--note :stats-open (niwa-view-native--stats))
          ;; Two moves before the first position callback can arrive: the
          ;; surface must wait for the commit carrying the last one.
          (when handle
            (let ((geometry (cadr (nth 3 (car niwa-view-native--entries)))))
              (niwa-view-native--move handle (aref geometry 0) (+ 7 (aref geometry 1)))
              (niwa-view-native--move handle (aref geometry 0) (aref geometry 1))))
          (niwa-native-wayland--wait 0.3)
          ;; Text inserted above moves the same surface.
          (goto-char (point-min))
          (insert "inserted\n")
          (goto-char (point-max))
          (niwa-native-wayland--present specs)
          (let ((moved (nth 2 (car niwa-view-native--entries))))
            (niwa-native-wayland--note :moved-same (eq moved handle))
            (niwa-native-wayland--note :moved-placement (cdr (nth 3 (car niwa-view-native--entries)))))
          ;; Painting an existing overlay changes neither text nor its extent.
          (let ((overlay (make-overlay (car target) (cdr target))))
            (redisplay t)
            (let ((before (nth 2 (car niwa-view-native--entries))))
              (overlay-put overlay 'face '(:background "#203040"))
              (niwa-native-wayland--wait
               3 (lambda ()
                   (redisplay t)
                   (equal (nth 3 (nth 3 (car niwa-view-native--entries))) "#203040")))
              (niwa-native-wayland--note
               :repainted (let ((entry (car niwa-view-native--entries)))
                            (and entry (not (eq before (nth 2 entry)))
                                 (equal (nth 3 (nth 3 entry)) "#203040")
                                 (niwa-view-native--live-p (nth 2 entry)) t))))
            (delete-overlay overlay)
            (redisplay t)
            (niwa-native-wayland--wait 0.5))
          ;; The window-local highlight is updated by the global redisplay
          ;; hook.  Replacing the rail first collapses its old overlay.
          (let ((global-hl-line-sticky-flag 'window))
            (set-face-attribute 'hl-line nil :background "#304050")
            (global-hl-line-mode 1)
            (unwind-protect
                (progn
                  ;; Start with an already highlighted row.  The production
                  ;; stream scheduler, absent here, owns transition rearming.
                  (niwa-view-native-stop)
                  (goto-char (cdr target))
                  (redisplay t)
                  (niwa-native-wayland--present specs)
                  (niwa-native-wayland--note
                   :hl-line-ready
                   (let ((entry (car niwa-view-native--entries)))
                     (and entry (equal (nth 3 (nth 3 entry)) "#304050")
                          (niwa-view-native--live-p (nth 2 entry)) t)))
                  (let ((highlighted (nth 2 (car niwa-view-native--entries)))
                        (stable t) samples)
                    (dotimes (tick 3)
                      (let ((start (marker-position (car target))))
                        (delete-region start (1+ (marker-position (cdr target))))
                        (goto-char start)
                        (insert (format "Working... · %ds\n" tick))
                        (set-marker (car target) nil)
                        (set-marker (cdr target) nil)
                        (setq target (cons (copy-marker start) (copy-marker (+ start 10))))
                        (setf (caar specs) target)
                        (goto-char (cdr target)))
                      (niwa-native-wayland--present specs)
                      (let ((entry (car niwa-view-native--entries)))
                        (push (list (and entry t) (eq highlighted (nth 2 entry))
                                    (cdr (nth 3 entry))
                                    (list (marker-position (car target))
                                          (marker-position (cdr target)) (point))
                                    (niwa-view-native--stats)) samples)
                        (setq stable
                              (and stable entry (eq highlighted (nth 2 entry))
                                   (equal (nth 3 (nth 3 entry)) "#304050")
                                   (niwa-view-native--live-p highlighted)))))
                    (niwa-native-wayland--note :hl-line-samples (nreverse samples))
                    (niwa-native-wayland--note :hl-line-stable (and stable t))))
              (global-hl-line-mode -1)))
          (goto-char (point-max))
          (niwa-native-wayland--present specs)
          ;; Pixel scrolling changes no buffer position or window edges.
          (let ((before (aref (cadr (niwa-view-native--placement target window)) 1)))
            (set-window-vscroll window 3 t)
            (redisplay t)
            (niwa-native-wayland--wait 0.5)
            (niwa-native-wayland--note
             :pixel-scrolled
             (let ((placement (nth 3 (car niwa-view-native--entries))))
               (and placement (= (aref (cadr placement) 1) (- before 3))
                    (equal placement (niwa-view-native--placement target window)))))
            (set-window-vscroll window 0 t)
            (redisplay t)
            (niwa-native-wayland--wait 0.5))
          ;; An overlay hiding the first line moves it without a text edit.
          (let ((overlay (make-overlay 1 10)))
            (overlay-put overlay 'invisible t)
            (redisplay t)
            (niwa-native-wayland--wait 0.5)
            (niwa-native-wayland--note
             :overlay-placement
             (list (cdr (nth 3 (car niwa-view-native--entries)))
                   (cdr (niwa-view-native--placement target window))))
            (delete-overlay overlay)
            (redisplay t)
            (niwa-native-wayland--wait 0.5))
          ;; The module closes a surface itself when its frame unmaps or
          ;; changes scale; close one behind Lisp's back the same way.  The
          ;; next redisplay must replace it without a semantic update.
          (niwa-native-wayland--wait
           3 (lambda () (redisplay t) (car niwa-view-native--entries)))
          (let ((before (nth 2 (car niwa-view-native--entries))))
            (niwa-native-wayland--note :restored (and before t))
            (niwa-view-native--close before)
            (niwa-native-wayland--wait
             3 (lambda ()
                 (redisplay t)
                 (let ((entry (car niwa-view-native--entries)))
                   (and entry (not (eq (nth 2 entry) before))
                        (niwa-view-native--live-p (nth 2 entry))))))
            (niwa-native-wayland--note
             :reopened (let ((entry (car niwa-view-native--entries)))
                         (and entry (not (eq (nth 2 entry) before))
                              (niwa-view-native--live-p (nth 2 entry)) t))))
          (niwa-native-wayland--note :stats-end (niwa-view-native--stats))
          ;; A live surface must disappear when its parent becomes translucent.
          (set-frame-parameter frame 'alpha-background 80)
          (redisplay t)
          (niwa-native-wayland--wait 0.5)
          (niwa-native-wayland--note
           :translucent (and (not niwa-view-native--entries)
                             (not (niwa-view-native--placement target window))))
          (niwa-view-native-stop))))
    frame))

(defun niwa-native-wayland--run ()
  "Run the placement scenario, record what it observed and exit.
Display teardown and reconnection run in native-wayland-close.el."
  (let ((status 0))
    (condition-case err
        (niwa-native-wayland--scenario (getenv "NIWA_NATIVE_DISPLAY"))
      (error (setq status 1)
             (niwa-native-wayland--note :error (error-message-string err))))
    (with-temp-file (getenv "NIWA_NATIVE_RESULT")
      (let ((print-length nil) (print-level nil))
        (prin1 niwa-native-wayland--results (current-buffer))))
    (kill-emacs status)))

(run-at-time 0.5 nil #'niwa-native-wayland--run)

;;; native-wayland.el ends here
