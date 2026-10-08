;;; shimmer-demo.el --- Compare shimmer frame rates and variants -*- lexical-binding: t -*-
;; Rows A/B use mevedel's real `mevedel-view-animation-frame'.  Rows C-E use a
;; softer highlight: a cosine bell several characters wide, optionally slower.
;; q closes the buffer and cancels every demo timer.

(require 'cl-lib)
(require 'color)
(require 'mevedel-view-animation)

(defvar-local shimmer-demo--timers nil)
(defvar-local shimmer-demo--start nil)

(defun shimmer-demo--soft-frame (label seconds face half-width cycle)
  "Return LABEL at SECONDS with a cosine highlight HALF-WIDTH chars wide."
  (let* ((palette (mevedel-view-animation--palette
                   (face-foreground face nil t) (face-background 'default nil t)))
         (len (length label))
         (travel (+ len (* 2 half-width)))
         (center (- (* (/ (mod seconds cycle) cycle) travel) half-width))
         (sample (copy-sequence label)))
    (dotimes (i len)
      (let* ((d (abs (- i center)))
             (intensity (if (< d half-width)
                            (/ (+ 1 (cos (/ (* float-pi d) half-width))) 2)
                          0.0))
             (shade (aref palette (min 63 (max 0 (round (* 63 intensity)))))))
        (put-text-property i (1+ i) 'face `(:foreground ,shade) sample)))
    sample))

(defun shimmer-demo--faced (string)
  "Return STRING with the spinner face appended beneath its colors."
  (let ((string (copy-sequence string)))
    (add-face-text-property 0 (length string) 'mevedel-view-spinner t string)
    string))

(defun shimmer-demo--row (title fps render)
  "Insert TITLE and an animated span updated FPS times a second via RENDER."
  (insert (propertize (format "%-44s" title) 'face 'shadow))
  (let ((start (point-marker))
        (buffer (current-buffer)))
    (insert (shimmer-demo--faced (funcall render 0.0)))
    (let ((end (point-marker)))
      (insert "\n\n")
      (push (run-at-time
             0 (/ 1.0 fps)
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (let ((inhibit-read-only t)
                         (seconds (- (float-time) shimmer-demo--start)))
                     (with-silent-modifications
                       (put-text-property start end 'display
                                          (shimmer-demo--faced
                                           (funcall render seconds)))))))))
            shimmer-demo--timers))))

(defun shimmer-demo-stop ()
  "Cancel this demo's timers."
  (mapc #'cancel-timer shimmer-demo--timers)
  (setq shimmer-demo--timers nil))

(defun shimmer-demo ()
  "Show the shimmer at several frame rates and in softer variants."
  (interactive)
  (let ((buffer (get-buffer-create "*shimmer demo*"))
        (label "Thinking..."))
    (with-current-buffer buffer
      (shimmer-demo-stop)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (setq shimmer-demo--start (float-time))
        (add-hook 'kill-buffer-hook #'shimmer-demo-stop nil t)
        (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
        (insert (propertize "Shimmer comparison (q to close)\n\n" 'face 'bold))
        (let ((original (lambda (s) (mevedel-view-animation-frame
                                     'shimmer label s 'mevedel-view-spinner))))
          (shimmer-demo--row "A  before: current shimmer, ~31 fps" 31 original)
          (shimmer-demo--row "B  new default: current shimmer, 12 fps" 12 original))
        (shimmer-demo--row "C  option 2: soft wide highlight, 12 fps" 12
                           (lambda (s) (shimmer-demo--soft-frame
                                        label s 'mevedel-view-spinner 4.0 3.6)))
        (shimmer-demo--row "D  option 2: soft wide + slower (5 s), 12 fps" 12
                           (lambda (s) (shimmer-demo--soft-frame
                                        label s 'mevedel-view-spinner 4.0 5.0)))
        (shimmer-demo--row "E  option 2: soft wide + slower, 8 fps" 8
                           (lambda (s) (shimmer-demo--soft-frame
                                        label s 'mevedel-view-spinner 4.0 5.0)))
        (shimmer-demo--cadenced-row "F  Codex-style: 1 s sweep / 4 s, 30 fps in sweep" 30 label)
        (shimmer-demo--cadenced-row "G  Codex-style: 1 s sweep / 4 s, 15 fps in sweep" 15 label)
        (insert (propertize "All rows animate in one frame, so Emacs CPU is high while this is open.\n"
                            'face 'shadow))))
    (switch-to-buffer buffer)
    (goto-char (point-min))
    t))

;;; Codex-style cadenced sweep: 1 s sweep every 4 s after a 0.6 s delay.

(defconst shimmer-demo--delay 0.6)
(defconst shimmer-demo--sweep 1.0)
(defconst shimmer-demo--interval 4.0)

(defun shimmer-demo--phase (seconds)
  "Return seconds into the current sweep at SECONDS, or nil between sweeps."
  (let ((s (- seconds shimmer-demo--delay)))
    (and (>= s 0)
         (let ((into (mod s shimmer-demo--interval)))
           (and (< into shimmer-demo--sweep) into)))))

(defun shimmer-demo--codex-frame (label seconds face)
  "Return LABEL at SECONDS like Codex's summary shimmer, using FACE."
  (let* ((palette (mevedel-view-animation--palette
                   (face-foreground face nil t) (face-background 'default nil t)))
         (width (float (string-width label)))
         (half (max 3.0 (* 0.1 width)))
         (phase (shimmer-demo--phase seconds))
         (position (and phase (- (* (/ phase shimmer-demo--sweep) (+ width (* 2 half))) half)))
         (sample (copy-sequence label)))
    (dotimes (i (length label))
      (let* ((distance (if position (min 1.0 (/ (abs (- (+ i 0.5) position)) half)) 1.0))
             (intensity (* 0.5 (+ 1 (cos (* float-pi distance)))))
             (shade (aref palette (min 63 (max 0 (round (* 63 intensity)))))))
        (put-text-property i (1+ i) 'face `(:foreground ,shade) sample)))
    sample))

(defun shimmer-demo--cadenced-row (title fps label)
  "Insert TITLE and a Codex-style span drawn at FPS only while sweeping."
  (insert (propertize (format "%-44s" title) 'face 'shadow))
  (let* ((start (point-marker))
         (buffer (current-buffer))
         (render (lambda (s) (shimmer-demo--faced
                              (shimmer-demo--codex-frame label s 'mevedel-view-spinner))))
         end tick)
    (insert (funcall render 0.0))
    (setq end (point-marker))
    (insert "\n\n")
    (setq tick
          (lambda ()
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (let* ((seconds (- (float-time) shimmer-demo--start))
                       (phase (shimmer-demo--phase seconds))
                       (inhibit-read-only t))
                  (with-silent-modifications
                    (put-text-property start end 'display (funcall render seconds)))
                  ;; Between sweeps nothing moves: sleep until the next one.
                  (let* ((s (- seconds shimmer-demo--delay))
                         (delay (if phase (/ 1.0 fps)
                                  (if (< s 0) (- s)
                                    (- shimmer-demo--interval (mod s shimmer-demo--interval))))))
                    (setq shimmer-demo--timers
                          (cons (run-at-time (max 0.001 delay) nil tick)
                                (seq-filter (lambda (tm) (memq tm timer-list))
                                            shimmer-demo--timers)))))))))
    (push (run-at-time 0 nil tick) shimmer-demo--timers)))
(provide 'shimmer-demo)
