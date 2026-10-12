;;; present-wayland.el --- PGTK presentation scenario on a real compositor -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded by a pgtk daemon that `test-mevedel-present-wayland.el' starts
;; against a headless compositor.  It enables cheaper presentation, then
;; marks phases in the daemon's WAYLAND_DEBUG trace on stderr: a one-character
;; insertion, redisplays that change nothing, the visible bell, and painting
;; while a Lisp thread waits.  Observations go to
;; the file named by MEVEDEL_NATIVE_RESULT.

;;; Code:

(require 'cl-lib)
(require 'mevedel-present)

(defvar present-wayland--results nil
  "Plist of observations.")

(defun present-wayland--note (key value)
  "Record VALUE under KEY."
  (setq present-wayland--results (plist-put present-wayland--results key value)))

(defun present-wayland--wait (seconds &optional predicate)
  "Run the event loop for SECONDS, or until PREDICATE holds."
  (let ((end (+ (float-time) seconds)))
    (while (and (< (float-time) end)
                (not (and predicate (funcall predicate))))
      (sit-for 0.02 t))))

(defun present-wayland--mark (phase)
  "Mark the start of PHASE in the Wayland trace."
  (princ (format "MEVEDEL-PHASE %s\n" phase) #'external-debugging-output))

(defun present-wayland--scenario (display)
  "Exercise cheaper presentation on DISPLAY."
  (blink-cursor-mode -1)
  (let ((frame (make-frame-on-display
                display '((width . 80) (height . 20) (menu-bar-lines . 0)
                          (tool-bar-lines . 0) (vertical-scroll-bars . nil)))))
    ;; `x-focus-frame' aborted a pgtk daemon; new windows get focus anyway.
    (select-frame frame)
    (present-wayland--wait
     5 (lambda () (and (eq (frame-visible-p frame) t) (frame-focus-state frame))))
    (switch-to-buffer (get-buffer-create "*present*"))
    (dotimes (i 15) (insert (format "line %d\n" i)))
    (redisplay t)
    (present-wayland--wait 0.3)
    (present-wayland--note :enabled (mevedel-present-ensure frame))
    (present-wayland--note :state mevedel-present--state)
    (present-wayland--note :size (list (frame-pixel-width frame) (frame-pixel-height frame)
                                       (frame-scale-factor frame)))
    ;; The next draw registers the frame and switches its window to GL.
    (redraw-frame frame)
    (redisplay t)
    (present-wayland--wait 0.5)
    (present-wayland--mark "insert")
    (goto-char (point-max))
    (insert "x")
    (redisplay t)
    (present-wayland--wait 0.5)
    (present-wayland--mark "idle")
    (dotimes (_ 10)
      (redisplay t)
      (sit-for 0.05 t))
    (present-wayland--mark "bell")
    ;; The bell draws on a copy of the frame's surface, recording no damage.
    (let ((visible-bell t)) (ding))
    (redisplay t)
    (present-wayland--wait 0.3)
    (present-wayland--mark "thread")
    ;; GLib dispatch from the waiting thread painted with the main thread's
    ;; GL context and crashed.
    (let ((thread (make-thread (lambda () (dotimes (_ 20) (sleep-for 0.05))) "present-wayland")))
      (dotimes (_ 20)
        (insert "y")
        (redisplay t)
        (sit-for 0.05 t))
      (thread-join thread))
    (present-wayland--mark "end")
    (present-wayland--note :survived t)
    (present-wayland--note :stats (mevedel-present--stats))
    frame))

(defun present-wayland--run ()
  "Run the scenario, record what it observed and exit.
The display is not closed: a pgtk daemon aborts when the terminal of a
displayed frame is deleted."
  (let ((status 0))
    (condition-case err
        (present-wayland--scenario (getenv "MEVEDEL_NATIVE_DISPLAY"))
      (error (setq status 1)
             (present-wayland--note :error (error-message-string err))))
    (with-temp-file (getenv "MEVEDEL_NATIVE_RESULT")
      (let ((print-length nil) (print-level nil))
        (prin1 present-wayland--results (current-buffer))))
    (kill-emacs status)))

(run-at-time 0.5 nil #'present-wayland--run)

;;; present-wayland.el ends here
