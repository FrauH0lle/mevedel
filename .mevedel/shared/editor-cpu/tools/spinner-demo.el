;;; spinner-demo.el --- Every spinner style, continuous vs cadenced -*- lexical-binding: t -*-
;; Requires shimmer-demo.el.  "today" animates continuously at its current
;; rate (color styles at the new 12 fps ceiling, glyphs at their natural
;; cadence).  "new way" plays one full cycle inside a 1 s window every 4 s
;; (after a 0.6 s start delay) and rests on its first frame in between.

(require 'shimmer-demo)

(defvar-local spinner-demo--timers nil)
(defvar-local spinner-demo--start nil)

(defconst spinner-demo--cycles
  ;; Animation seconds played during the 1 s window: one full cycle
  ;; (two rotations for the four-glyph ascii spinner).
  '((breathe . 3.6) (bounce . 3.6) (braille . 1.2) (ascii . 0.96)
    (dots . 0.96) (ellipsis . 1.92)))

(defun spinner-demo--today (style label)
  "Return a continuous renderer for STYLE and LABEL."
  (lambda (seconds)
    (mevedel-view-animation-frame style label seconds 'mevedel-view-spinner)))

(defun spinner-demo--cadenced (style label)
  "Return a renderer for STYLE and LABEL that moves only while sweeping."
  (if (eq style 'shimmer)
      (lambda (seconds)
        (shimmer-demo--codex-frame label seconds 'mevedel-view-spinner))
    (let ((cycle (alist-get style spinner-demo--cycles)))
      (lambda (seconds)
        (let ((phase (shimmer-demo--phase seconds)))
          (mevedel-view-animation-frame
           style label (if phase (* (/ phase shimmer-demo--sweep) cycle) 0.0)
           'mevedel-view-spinner))))))

(defun spinner-demo--span (render next-delay)
  "Insert an animated span drawn by RENDER, rescheduled after NEXT-DELAY."
  (let* ((buffer (current-buffer))
         (start (point-marker))
         end tick)
    (insert (shimmer-demo--faced (funcall render 0.0)))
    (setq end (point-marker))
    (setq tick
          (lambda ()
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (let ((seconds (- (float-time) spinner-demo--start))
                      (inhibit-read-only t))
                  (with-silent-modifications
                    (put-text-property start end 'display
                                       (shimmer-demo--faced (funcall render seconds))))
                  (setq spinner-demo--timers
                        (cons (run-at-time (funcall next-delay seconds) nil tick)
                              (seq-filter (lambda (tm) (memq tm timer-list))
                                          spinner-demo--timers))))))))
    (push (run-at-time 0 nil tick) spinner-demo--timers)))

(defun spinner-demo--today-delay (style)
  "Return the current scheduler's period for STYLE."
  (let ((natural (mevedel-view-animation-period style)))
    (lambda (_) (max (/ 1.0 12) (or natural 1.0)))))

(defun spinner-demo--cadenced-delay (_seconds-or-nil)
  "Return a delay function: 30 fps while sweeping, else sleep to the next sweep."
  (lambda (seconds)
    (let ((s (- seconds shimmer-demo--delay)))
      (max 0.001
           (cond ((shimmer-demo--phase seconds) (/ 1.0 30))
                 ((< s 0) (- s))
                 (t (- shimmer-demo--interval (mod s shimmer-demo--interval))))))))

(defun spinner-demo--row (title style label)
  "Insert TITLE with STYLE on LABEL, today and the new way."
  (insert (propertize (format "%-22s" title) 'face 'shadow))
  (spinner-demo--span (spinner-demo--today style label)
                      (spinner-demo--today-delay style))
  (insert (make-string 6 ?\s))
  (move-to-column 50 t)
  (spinner-demo--span (spinner-demo--cadenced style label)
                      (spinner-demo--cadenced-delay nil))
  (insert "\n\n"))

(defun spinner-demo-stop ()
  "Cancel this demo's timers."
  (mapc #'cancel-timer spinner-demo--timers)
  (setq spinner-demo--timers nil))

(defun spinner-demo ()
  "Show every spinner style continuously and cadenced, side by side."
  (interactive)
  (let ((buffer (get-buffer-create "*spinner demo*")))
    (with-current-buffer buffer
      (spinner-demo-stop)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (setq spinner-demo--start (float-time))
        (add-hook 'kill-buffer-hook #'spinner-demo-stop nil t)
        (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
        (insert (propertize "Spinner styles (q to close)\n\n" 'face 'bold))
        (insert (propertize (format "%-22s%-28s%s\n\n" "" "today" "new way: 1 s of every 4 s")
                            'face 'shadow))
        (insert (propertize "Request label\n\n" 'face 'bold))
        (dolist (style '(shimmer breathe bounce dots ellipsis braille ascii))
          (spinner-demo--row (format "  %s%s" style (if (eq style 'shimmer) " (default)" ""))
                             style (if (eq style 'ellipsis) "Thinking" "Thinking...")))
        (insert (propertize "Pending tool rows\n\n" 'face 'bold))
        (dolist (style '(braille ascii dots))
          (spinner-demo--row (format "  %s%s" style (if (eq style 'braille) " (default)" ""))
                             style ""))
        (insert (propertize
                 "Tool rows show only the indicator; in the view it precedes \"Calling Bash…\".\n"
                 'face 'shadow))))
    (switch-to-buffer buffer)
    (goto-char (point-min))
    t))
