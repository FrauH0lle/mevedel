;;; native-wayland-close.el --- Display teardown and reconnect -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the actual GTK closed signal by deleting an Emacs terminal,
;; retaining its native handle, and connecting again to the same compositor.
;; Delete the frame first so stock Emacs cancels its scale timer, then
;; reconnect before waiting: Emacs 31 leaves the closed Wayland fd in its
;; input wait set, so waiting with no replacement connection aborts Emacs.

;;; Code:

(require 'cl-lib)
(require 'mevedel-view-native)

(defun mevedel-native-close--run ()
  "Close and reopen three real displays, retaining their native handles."
  (let ((status 0) observations handles)
    (condition-case err
        (dotimes (_ 3)
          (let* ((frame (make-frame-on-display
                         (getenv "MEVEDEL_NATIVE_DISPLAY")
                         '((width . 50) (height . 12)
                           (menu-bar-lines . 0) (tool-bar-lines . 0))))
                 (terminal (frame-terminal frame))
                 (deadline (+ (float-time) 5)))
            (select-frame frame)
            (switch-to-buffer (get-buffer-create "*native-display-close*"))
            (erase-buffer)
            (insert "first line\nWorking...\n")
            (goto-char (point-max))
            (while (and (< (float-time) deadline)
                        (not (frame-focus-state frame)))
              (sit-for 0.02 t))
            (redisplay t)
            (let* ((target (cons (copy-marker 12) (copy-marker 22)))
                   (placement (mevedel-view-native--placement target (selected-window)))
                   (before (and (mevedel-view-native-available-p frame)
                                (mevedel-view-native--stats)))
                   (handle (and before placement
                                (mevedel-view-native--open
                                 (frame-parameter frame 'window-id)
                                 (cadr placement) (nth 2 placement) (nth 3 placement)
                                 (mevedel-view-native--timeline
                                  'bounce "Working..." 0.05 'default frame) 0.0))))
              (unless (and handle (mevedel-view-native--live-p handle))
                (error "No live surface on reopened display"))
              (push handle handles)
              (sit-for 0.2 t)
              (let ((running (mevedel-view-native--stats)))
                (delete-frame frame t)
                (delete-terminal terminal t)
                ;; No event-loop wait between closing and the next connection.
                (push (list :terminal-closed (not (terminal-live-p terminal))
                            :active (aref (mevedel-view-native--stats) 0)
                            :submitted (- (aref running 1) (aref before 1))
                            :handles-closed
                            (cl-every (lambda (old)
                                        (not (mevedel-view-native--live-p old)))
                                      handles))
                      observations))
              ;; Keep the last successful close observable if reconnection crashes.
              (with-temp-file (getenv "MEVEDEL_NATIVE_RESULT")
                (prin1 observations (current-buffer))))))
      (error (setq status 1 observations
                   (list :error (error-message-string err) :cycles observations))))
    (with-temp-file (getenv "MEVEDEL_NATIVE_RESULT")
      (prin1 observations (current-buffer)))
    (kill-emacs status)))

(run-at-time 0.5 nil #'mevedel-native-close--run)

;;; native-wayland-close.el ends here
