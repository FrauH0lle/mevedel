;;; -*- lexical-binding: t -*-
(require 'json)
(defvar cpu-life-data nil)
(defvar cpu-life-view nil)
(defvar cpu-life-frames nil)
(defvar cpu-life-original nil)
(defvar cpu-life-step 0)
(defvar cpu-life-results nil)
(defvar cpu-life-timer nil)
(defvar cpu-life-tab-mode nil)
(defun cpu-life-focus (original &optional frame)
  (if (memq (or frame (selected-frame)) cpu-life-frames) t
    (funcall original frame)))
(defun cpu-life-check (name pass)
  (push (cons name (if pass t :json-false)) cpu-life-results)
  (unless pass (error "Lifecycle check failed: %s" name)))
(defun cpu-life-clean ()
  (when (timerp cpu-life-timer) (cancel-timer cpu-life-timer))
  (advice-remove 'frame-focus-state #'cpu-life-focus)
  (let ((kill-buffer-query-functions nil))
    (when (buffer-live-p cpu-life-view) (kill-buffer cpu-life-view))
    (when (buffer-live-p cpu-life-data) (kill-buffer cpu-life-data)))
  (dolist (frame cpu-life-frames)
    (when (frame-live-p frame) (delete-frame frame t)))
  (when (frame-live-p cpu-life-original) (select-frame cpu-life-original))
  (unless cpu-life-tab-mode (tab-bar-mode -1))
  (with-temp-file "/tmp/mevedel-cpu-view-lifecycle.json"
    (insert (json-encode (nreverse cpu-life-results)))))
(defun cpu-life-hidden ()
  (dolist (window (get-buffer-window-list cpu-life-view nil t))
    (set-window-buffer window cpu-life-data))
  (with-current-buffer cpu-life-view
    (mevedel-view--schedule-render 'full cpu-life-data 0)
    (cpu-life-check (format "hidden-%d-retains-render" cpu-life-step)
                    (and (eq mevedel-view--pending-render-kind 'full)
                         (not mevedel-view--render-timer)))))
(defun cpu-life-resumed (name)
  (with-current-buffer cpu-life-view
    (cpu-life-check name (not mevedel-view--pending-render-kind))
    (cpu-life-check (concat name "-draft")
                    (equal "> lifecycle\nsecond line" (mevedel-view--input-text)))))
(defun cpu-life-next ()
  (condition-case err
      (progn
        (with-selected-frame (car cpu-life-frames)
          (pcase cpu-life-step
            (0 (cpu-life-hidden))
            (1 (display-buffer cpu-life-view '(display-buffer-pop-up-window)))
            (2 (cpu-life-resumed "fresh-window-resumes")
               (let ((tab-bar-new-tab-choice cpu-life-data)) (tab-bar-new-tab))
               (cpu-life-hidden))
            (3 (tab-bar-switch-to-prev-tab))
            (4 (cpu-life-resumed "tab-restoration-resumes")
               (cpu-life-hidden)
               (let ((frame (make-frame '((name . "CPU lifecycle second frame")
                                          (width . 70) (height . 25)))))
                 (push frame cpu-life-frames)
                 (set-window-buffer (frame-selected-window frame) cpu-life-view)))
            (5 (cpu-life-resumed "other-frame-resumes")
               (with-current-buffer cpu-life-view
                 (mevedel-view--start-spinner "Working...")
                 (let ((inhibit-read-only t))
                   (goto-char (point-min))
                   (insert (make-string 300 ?\n)))
                 (goto-char (point-min)))
               (let ((window (get-buffer-window cpu-life-view t)))
                 (set-window-point window 1)
                 (set-window-start window 1)))
            (6 (with-current-buffer cpu-life-view
                 (cpu-life-check "offscreen-no-spinner-timer"
                                 (and (not (mevedel-view--animation-visible-p))
                                      (not mevedel-view--spinner-timer)))) )
            (7 (with-current-buffer cpu-life-view
                 (cpu-life-check "offscreen-probe-stays-quiet"
                                 (and (not mevedel-view--spinner-timer)
                                      (not mevedel-view--spinner-probe-timer)))))))
        (setq cpu-life-step (1+ cpu-life-step))
        (if (> cpu-life-step 7) (cpu-life-clean)
          (setq cpu-life-timer (run-at-time 1 nil #'cpu-life-next))))
    (error
     (push (cons "error" (error-message-string err)) cpu-life-results)
     (cpu-life-clean))))
(defun cpu-life-run ()
  (setq cpu-life-original (selected-frame)
        cpu-life-tab-mode tab-bar-mode
        cpu-life-step 0 cpu-life-results nil
        cpu-life-data (generate-new-buffer " *cpu lifecycle data*")
        cpu-life-view (generate-new-buffer " *cpu lifecycle view*"))
  (with-current-buffer cpu-life-data
    (mevedel--transcript-org-mode)
    (setq-local mevedel--session nil mevedel--current-request nil
                mevedel-view--abort-function #'ignore))
  (mevedel-view--setup cpu-life-view cpu-life-data)
  (with-current-buffer cpu-life-view
    (setq-local mevedel-view-spinner-style 'ascii
                mevedel-view-spinner-power-policy 'full)
    (goto-char (mevedel-view--input-start))
    (insert "> lifecycle\nsecond line"))
  (setq cpu-life-frames
        (list (make-frame '((name . "CPU lifecycle verification")
                           (width . 70) (height . 25)))))
  (set-window-buffer (frame-selected-window (car cpu-life-frames)) cpu-life-view)
  (advice-add 'frame-focus-state :around #'cpu-life-focus)
  (setq cpu-life-timer (run-at-time 1 nil #'cpu-life-next))
  t)
