;;; present-wayland-thread.el --- A Lisp thread showing a PGTK frame -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded by a pgtk daemon that `test-mevedel-present-wayland.el' starts
;; against a headless compositor.  With cheaper presentation enabled, a
;; Lisp thread hides and shows a frame: waiting for the map event runs
;; GTK's own main loop iteration in that thread, which painted with the
;; main thread's GL context and crashed.  It runs in a daemon of its own:
;; stock pgtk hangs here after another thread waited in `sleep-for'.
;; Observations go to the file named by MEVEDEL_NATIVE_RESULT.

;;; Code:

(require 'mevedel-present)

(defvar present-wayland-thread--results nil
  "Plist of observations.")

(defun present-wayland-thread--wait (seconds &optional predicate)
  "Run the event loop for SECONDS, or until PREDICATE holds."
  (let ((end (+ (float-time) seconds)))
    (while (and (< (float-time) end)
                (not (and predicate (funcall predicate))))
      (sit-for 0.02 t))))

(defun present-wayland-thread--frame (display)
  "Make a small frame on DISPLAY."
  (make-frame-on-display
   display '((width . 40) (height . 10) (menu-bar-lines . 0)
             (tool-bar-lines . 0) (vertical-scroll-bars . nil))))

(defun present-wayland-thread--scenario (display)
  "Show a frame on DISPLAY from a Lisp thread with presentation enabled."
  (blink-cursor-mode -1)
  (let ((frame (present-wayland-thread--frame display)))
    (select-frame frame)
    (present-wayland-thread--wait
     5 (lambda () (and (eq (frame-visible-p frame) t) (frame-focus-state frame))))
    (setq present-wayland-thread--results
          (list :state (progn (mevedel-present-ensure frame) mevedel-present--state)))
    (redraw-frame frame)
    (redisplay t)
    (present-wayland-thread--wait 0.5)
    (let ((other (present-wayland-thread--frame display)))
      (present-wayland-thread--wait 1)
      (thread-join (make-thread (lambda ()
                                  (make-frame-invisible other)
                                  (make-frame-visible other))
                                "present-wayland-thread"))
      (present-wayland-thread--wait 0.5)
      (setq present-wayland-thread--results
            (plist-put present-wayland-thread--results :visible
                       (frame-visible-p other))))))

(defun present-wayland-thread--run ()
  "Run the scenario, record what it observed and exit."
  (let ((status 0))
    (condition-case err
        (present-wayland-thread--scenario (getenv "MEVEDEL_NATIVE_DISPLAY"))
      (error (setq status 1)
             (setq present-wayland-thread--results
                   (plist-put present-wayland-thread--results
                              :error (error-message-string err)))))
    (with-temp-file (getenv "MEVEDEL_NATIVE_RESULT")
      (prin1 present-wayland-thread--results (current-buffer)))
    (kill-emacs status)))

(run-at-time 0.5 nil #'present-wayland-thread--run)

;;; present-wayland-thread.el ends here
