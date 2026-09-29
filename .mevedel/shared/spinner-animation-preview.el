;;; spinner-animation-preview.el --- Temporary native motion study -*- lexical-binding: t; -*-

;;; Commentary:
;; Session preview requested by the user, 2026-09-27.  Not package code.
;; Load this file and call `mevedel-spinner-demo-open'.
;; Uses the current frame's colors; does not modify mevedel configuration.

;;; Code:

(require 'color)
(require 'cl-lib)

(defvar-local mevedel-spinner-demo--timer nil)

(defun mevedel-spinner-demo--frame (style seconds palette)
  "Return a frame for STYLE at elapsed SECONDS, using foreground PALETTE."
  (let ((tick (floor (/ seconds 0.12))))
    (pcase style
      ((or 'shimmer 'breathe 'bounce)
       (let* ((text (copy-sequence "Working…"))
              (center (if (eq style 'bounce)
                          (- 3.5 (* 3.5 (cos (/ (* seconds float-pi) 1.8))))
                        (- (* (/ (mod seconds 3.6) 3.6) 16) 4))))
         (dotimes (i (length text))
           (let* ((intensity
                   (if (eq style 'breathe)
                       (/ (- 1 (cos (/ (* seconds float-pi) 1.8))) 2)
                     (max 0.0 (- 1 (/ (abs (- i center)) 2.5)))))
                  (color (elt palette (round (* (1- (length palette)) intensity)))))
             (put-text-property i (1+ i) 'face `(:foreground ,color) text)))
         text))
      ('dots
       (concat (nth (mod (floor (/ tick 2)) 4)
                    '("●···" "·●··" "··●·" "···●")) " Working…"))
      ('ellipsis
       (let ((n (mod (floor (/ tick 4)) 4)))
         (concat "Working" (make-string n ?.) (make-string (- 3 n) ?\s))))
      ('braille
       (concat (string (aref "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" (mod tick 10))) " Working…"))
      ('ascii
       (concat (string (aref "-\\|/" (mod tick 4))) " Working…"))
      (_ "Working…"))))

(defun mevedel-spinner-demo-open ()
  "Open a disposable native animation gallery; SPC pauses and q closes."
  (interactive)
  (let ((buffer (get-buffer-create "*Working animation preview*")))
    (with-current-buffer buffer
      (when (timerp mevedel-spinner-demo--timer)
        (cancel-timer mevedel-spinner-demo--timer))
      (let ((inhibit-read-only t)) (erase-buffer))
      (special-mode)
      (setq-local truncate-lines t)
      (let* ((foreground (color-name-to-rgb (face-foreground 'default nil t)))
             (background (color-name-to-rgb (face-background 'default nil t)))
             (foreground (or foreground '(0.9 0.9 0.9)))
             (background (or background '(0.1 0.1 0.1)))
             (palette
              (vconcat
               (cl-loop for i below 64
                        for weight = (+ 0.52 (* 0.48 (/ i 63.0)))
                        collect
                        (apply #'color-rgb-to-hex
                               (cl-mapcar (lambda (fg bg)
                                            (+ (* weight fg) (* (- 1 weight) bg)))
                                          foreground background)))))
             (seconds 0.0) (last-time (float-time))
             (paused nil) (spans nil)
             (map (make-sparse-keymap)))
        (let ((inhibit-read-only t))
          (insert (propertize "Working — native Emacs animation study\n\n" 'face 'bold))
          (insert "Your current theme and font · target 60 fps (16.7 ms)\n"
                  "SPC: pause/resume    q: close and stop timer\n\n")
          (dolist (entry '((shimmer . "Text shimmer")
                           (breathe . "Breathing text")
                           (bounce . "Bouncing light")
                           (dots . "Traveling dots")
                           (ellipsis . "Animated ellipsis")
                           (braille . "Braille spinner")
                           (ascii . "Rotating line (ASCII)")
                           (static . "Static label")))
            (insert (propertize (cdr entry) 'face 'bold) "\n  ")
            (let* ((start (point))
                   (frame (mevedel-spinner-demo--frame (car entry) 0 palette))
                   (frames
                    (when (memq (car entry) '(shimmer breathe bounce))
                      (vconcat
                       (cl-loop for i below 216
                                collect (mevedel-spinner-demo--frame
                                         (car entry) (/ i 60.0) palette))))))
              (insert frame)
              (push (list (car entry) start (point) frames) spans))
            (insert "\n\n"))
          (insert "Color effects: 64 shades, precomputed at 60 frames/second.\n"
                  "Glyphs keep their original cadence; animation speed is unchanged.\n"
                  "Actual frame delivery depends on Emacs redisplay and load.\n"
                  "Theme colors are sampled on opening; reopen after changing theme.\n"
                  "This is a preview only; your Working indicator is unchanged.\n"))
        (setq-local header-line-format " Animation preview — playing")
        (set-keymap-parent map special-mode-map)
        (define-key map (kbd "SPC")
                    (lambda () (interactive)
                      (setq paused (not paused))
                      (setq last-time (float-time))
                      (setq header-line-format
                            (if paused " Animation preview — paused"
                              " Animation preview — playing"))))
        (define-key map (kbd "q")
                    (lambda () (interactive) (quit-window t)))
        (use-local-map map)
        (setq mevedel-spinner-demo--timer
              (run-at-time
               (/ 1.0 60) (/ 1.0 60)
               (lambda ()
                 (let* ((now (float-time))
                        (delta (max 0.0 (- now last-time))))
                   (setq last-time now)
                   (when (and (buffer-live-p buffer)
                              (get-buffer-window buffer t) (not paused))
                     (with-current-buffer buffer
                       (let ((inhibit-read-only t)
                             (inhibit-modification-hooks t))
                         (setq seconds (+ seconds delta))
                         (dolist (span spans)
                           (let* ((frames (nth 3 span))
                                  (frame
                                   (if frames
                                       (aref frames (mod (floor (* seconds 60)) 216))
                                     (mevedel-spinner-demo--frame
                                      (car span) seconds palette))))
                             (unless (equal-including-properties
                                      frame (or (get-text-property (nth 1 span) 'display)
                                                (buffer-substring (nth 1 span) (nth 2 span))))
                               (put-text-property
                                (nth 1 span) (nth 2 span) 'display frame))))
                         (set-buffer-modified-p nil))))))))
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (when (timerp mevedel-spinner-demo--timer)
                      (cancel-timer mevedel-spinner-demo--timer)))
                  nil t)
        (goto-char (point-min))
        (set-buffer-modified-p nil)))
    (pop-to-buffer buffer)))

;;; spinner-animation-preview.el ends here
