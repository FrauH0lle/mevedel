;;; present-wayland-thread.el --- A Lisp thread showing a PGTK frame -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded by a pgtk daemon that `test-mevedel-present-wayland.el' starts
;; against a headless compositor.  With cheaper presentation enabled, a
;; Lisp thread shows a hidden frame: waiting for the map event runs
;; GTK's own main loop iteration in that thread, which painted with the
;; main thread's GL context and crashed.  The main thread yields the Lisp
;; lock without holding GTK's main context while the worker maps its frame.
;; Observations go to the file named by MEVEDEL_NATIVE_RESULT.

;;; Code:

(require 'mevedel-present)

(defvar present-wayland-thread--results nil
  "Plist of observations.")

(defvar present-wayland-thread--progress nil
  "Last worker state sent to the parent test.")

(defun present-wayland-thread--checkpoint (checkpoint &optional worker)
  "Send CHECKPOINT and the last observable state of WORKER to the parent."
  (let ((progress (list :checkpoint checkpoint
                        :live-p (and worker (thread-live-p worker))
                        :blocker (and worker (thread--blocker worker)))))
    (unless (equal progress present-wayland-thread--progress)
      (setq present-wayland-thread--progress progress)
      (princ (format "MEVEDEL-CHECKPOINT %S\n" progress)
             #'external-debugging-output))))

(defun present-wayland-thread--wait (seconds &optional predicate)
  "Run the event loop for SECONDS, or until PREDICATE holds."
  (let ((end (+ (float-time) seconds)))
    (while (and (< (float-time) end)
                (not (and predicate (funcall predicate))))
      (sit-for 0.02 t))))

(defun present-wayland-thread--frame (display visible)
  "Make a small frame on DISPLAY, initially VISIBLE or hidden."
  (make-frame-on-display
   display `((width . 40) (height . 10) (menu-bar-lines . 0)
             (tool-bar-lines . 0) (vertical-scroll-bars . nil)
             (visibility . ,visible))))

(defun present-wayland-thread--scenario (display)
  "Show a frame on DISPLAY from a Lisp thread with presentation enabled."
  (present-wayland-thread--checkpoint 'creating-first-frame)
  (blink-cursor-mode -1)
  (let ((frame (present-wayland-thread--frame display t)))
    (present-wayland-thread--checkpoint 'waiting-for-focus)
    (select-frame frame)
    (present-wayland-thread--wait
     5 (lambda () (and (eq (frame-visible-p frame) t) (frame-focus-state frame))))
    (present-wayland-thread--checkpoint 'enabling-presentation)
    (setq present-wayland-thread--results
          (list :state (progn (mevedel-present-ensure frame) mevedel-present--state)))
    (redraw-frame frame)
    (redisplay t)
    (present-wayland-thread--wait 0.5)
    (present-wayland-thread--checkpoint 'creating-second-frame)
    ;; Hiding on the worker can deadlock in PGTK's nested GTK wait before
    ;; reaching the map-event regression this scenario exercises.
    (let ((other (present-wayland-thread--frame display nil)))
      (when (frame-visible-p other)
        (error "Frame was already visible before the worker started"))
      (present-wayland-thread--checkpoint 'starting-worker)
      (let* ((main (current-thread))
             (worker (make-thread
                      (lambda ()
                        (setq present-wayland-thread--results
                              (plist-put present-wayland-thread--results :thread-side
                                         (not (eq main (current-thread)))))
                        (present-wayland-thread--checkpoint 'showing-frame (current-thread))
                        (make-frame-visible other)
                        (setq present-wayland-thread--results
                              (plist-put present-wayland-thread--results :show-returned t)))
                      "present-wayland-thread")))
        ;; `sleep-for' can hold GTK's context while yielding the Lisp lock;
        ;; the worker needs that context while it holds the Lisp lock.
        (let ((end (+ (float-time) 5)))
          (while (and (thread-live-p worker) (< (float-time) end))
            (present-wayland-thread--checkpoint
             (plist-get present-wayland-thread--progress :checkpoint) worker)
            (thread-yield)))
        (when (thread-live-p worker)
          (error "Frame-show thread did not finish: %S"
                 (list :checkpoint (plist-get present-wayland-thread--progress :checkpoint)
                       :live-p (thread-live-p worker)
                       :blocker (thread--blocker worker)
                       :worker-backtrace (backtrace--frames-from-thread worker)
                       :observed present-wayland-thread--results)))
        (thread-join worker)
        (present-wayland-thread--checkpoint 'worker-finished worker))
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

(present-wayland-thread--checkpoint 'waiting-for-scenario)
(run-at-time 0.5 nil #'present-wayland-thread--run)

;;; present-wayland-thread.el ends here
