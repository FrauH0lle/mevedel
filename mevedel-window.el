;;; mevedel-window.el --- Temporary surface window lifecycle -*- lexical-binding: t -*-

;;; Commentary:
;; Temporary inspection buffers restore the invoking frame, not a guessed
;; owner buffer.  Keep this presentation state separate from session ownership.

;;; Code:

(defvar-local mevedel-window--configuration nil
  "Window configuration preceding this temporary surface.")

(defvar-local mevedel-window--return nil
  "Function to call after closing this temporary surface.")

(defvar-local mevedel-window--origin-window nil
  "Window that invoked this temporary surface, possibly on another frame.")

(put 'mevedel-window--configuration 'permanent-local t)
(put 'mevedel-window--origin-window 'permanent-local t)
(put 'mevedel-window--return 'permanent-local t)

(defun mevedel-window-display-buffer (buffer &optional noselect return)
  "Display temporary BUFFER and remember the invoking window configuration.
NOSELECT leaves focus alone.  RETURN runs after quitting the surface.
Redisplaying an already visible surface retains its original return state."
  (let ((visible (get-buffer-window-list buffer nil t))
        (origin (selected-window))
        ;; Child-frame composers redirect display into their parent.  Capture
        ;; before display, then retain only the frame the action actually used.
        (configurations
         (mapcar (lambda (frame)
                   (cons frame (current-window-configuration frame)))
                 (frame-list))))
    (let ((window (display-buffer
                   buffer '((display-buffer-reuse-window
                             display-buffer-pop-up-window)))))
      (when (and window (not (memq window visible)))
        (with-current-buffer buffer
          (setq mevedel-window--configuration
                (alist-get (window-frame window) configurations)
                mevedel-window--origin-window origin
                mevedel-window--return return)))
      (when window
        (if noselect
            (when (window-live-p origin)
              (unless (eq (selected-frame) (window-frame origin))
                (select-frame-set-input-focus (window-frame origin)))
              (select-window origin))
          (select-window window)))
      window)))

(defun mevedel-window-restore (configuration origin &optional preserve-positions)
  "Restore CONFIGURATION and select live invoking window ORIGIN.
PRESERVE-POSITIONS keeps navigation in windows still showing the same buffer."
  (let ((positions
         (when (and preserve-positions configuration
                    (frame-live-p (window-configuration-frame configuration)))
           (mapcar (lambda (window)
                     (list window (window-buffer window)
                           (window-point window) (window-start window)))
                   (window-list (window-configuration-frame configuration)
                                'no-minibuf)))))
    (when (and configuration
               (frame-live-p (window-configuration-frame configuration)))
      (set-window-configuration configuration)
      (dolist (entry positions)
        (when (and (window-live-p (car entry))
                   (eq (window-buffer (car entry)) (nth 1 entry)))
          (set-window-point (car entry) (nth 2 entry))
          (set-window-start (car entry) (nth 3 entry) t)))))
  (when (window-live-p origin)
    (unless (eq (selected-frame) (window-frame origin))
      (select-frame-set-input-focus (window-frame origin)))
    (select-window origin)))

(defun mevedel-window-quit (&optional buffer)
  "Close temporary BUFFER and restore its invoking frame exactly.
BUFFER defaults to the current buffer.  Dead original buffers cannot be
restored; Emacs chooses their replacement without displaying an owner."
  (let* ((buffer (or buffer (current-buffer)))
         (configuration (buffer-local-value 'mevedel-window--configuration buffer))
         (origin (buffer-local-value 'mevedel-window--origin-window buffer))
         (return (buffer-local-value 'mevedel-window--return buffer)))
    (when-let* ((window (get-buffer-window buffer)))
      (quit-window nil window))
    (when (kill-buffer buffer)
      (mevedel-window-restore configuration origin)
      (when return (funcall return)))))

(provide 'mevedel-window)
;;; mevedel-window.el ends here
