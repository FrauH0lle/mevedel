;;; tool-demo.el --- Synchronized tool-row shimmer variants -*- lexical-binding: t -*-
;; Requires shimmer-demo.el.  One shared timer redraws every row: 30 fps during
;; the 1 s sweep, nothing in the 3 s between sweeps.  Each row shimmers only its
;; animated part; the rest keeps the ordinary live-tail face.

(require 'shimmer-demo)

(defvar-local tool-demo--timer nil)
(defvar-local tool-demo--spans nil
  "List of (START END ANIMATED REST FACE).")

(defun tool-demo--render (animated rest face seconds)
  (let ((head (shimmer-demo--codex-frame animated seconds face)))
    (add-face-text-property 0 (length head) face t head)
    (concat head (propertize rest 'face 'mevedel-view-ephemeral))))

(defun tool-demo--row (title animated rest face)
  (insert (propertize (format "%-30s" title) 'face 'shadow))
  (let ((start (point-marker)))
    (insert (tool-demo--render animated rest face 0.0))
    (push (list start (point-marker) animated rest face) tool-demo--spans)
    (insert "\n\n")))

(defun tool-demo--tick (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((seconds (- (float-time) shimmer-demo--start))
             (phase (shimmer-demo--phase seconds))
             (inhibit-read-only t))
        (with-silent-modifications
          (dolist (span tool-demo--spans)
            (pcase-let ((`(,start ,end ,animated ,rest ,face) span))
              (put-text-property start end 'display
                                 (tool-demo--render animated rest face seconds)))))
        (let* ((s (- seconds shimmer-demo--delay))
               (delay (cond (phase (/ 1.0 30))
                            ((< s 0) (- s))
                            (t (- shimmer-demo--interval
                                  (mod s shimmer-demo--interval))))))
          (setq tool-demo--timer
                (run-at-time (max 0.001 delay) nil #'tool-demo--tick buffer)))))))

(defun tool-demo-stop ()
  (when (timerp tool-demo--timer) (cancel-timer tool-demo--timer))
  (setq tool-demo--timer nil))

(defun tool-demo ()
  (interactive)
  (let ((buffer (get-buffer-create "*tool row demo*"))
        (cmd "Bash: npm run test -- --watch=false --reporter=dot...")
        (read "Read: mevedel-view-stream.el..."))
    (with-current-buffer buffer
      (tool-demo-stop)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (setq shimmer-demo--start (float-time) tool-demo--spans nil)
        (add-hook 'kill-buffer-hook #'tool-demo-stop nil t)
        (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
        (insert (propertize "Synchronized tool-row shimmer (q to close)\n\n" 'face 'bold))
        (tool-demo--row "request label" "Thinking..." "" 'mevedel-view-spinner)
        (insert (propertize "Whole row animated\n\n" 'face 'bold))
        (tool-demo--row "  short" (concat "Calling " read) "" 'mevedel-view-ephemeral)
        (tool-demo--row "  long" (concat "Calling " cmd) "" 'mevedel-view-ephemeral)
        (insert (propertize "Only \"Calling\" animated\n\n" 'face 'bold))
        (tool-demo--row "  short" "Calling" (concat " " read) 'mevedel-view-ephemeral)
        (tool-demo--row "  long" "Calling" (concat " " cmd) 'mevedel-view-ephemeral)
        (insert (propertize "\"Calling TOOL\" animated\n\n" 'face 'bold))
        (tool-demo--row "  short" "Calling Read" ": mevedel-view-stream.el..." 'mevedel-view-ephemeral)
        (tool-demo--row "  long" "Calling Bash" (substring cmd 4) 'mevedel-view-ephemeral))
      (tool-demo--tick buffer))
    (switch-to-buffer buffer)
    (goto-char (point-min))
    t))

(provide 'tool-demo)
