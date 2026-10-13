;;; view-cursor-wayland.el --- Hidden markup cursor regression -*- lexical-binding: t -*-

;;; Commentary:
;; Repeatedly clearing the modified flag forces a mode-line redisplay.  With
;; both header rows present, Emacs can then draw point on hidden markup at the
;; top of the window instead of its wrapped paragraph near the bottom.

;;; Code:

(setq load-path (read (getenv "MEVEDEL_TEST_LOAD_PATH")))
(require 'mevedel-view)
(setq mevedel-present-enabled nil)
(run-at-time
 0.1 nil
 (lambda ()
   (let ((status 0) result)
     (condition-case err
         (let ((frame (make-frame-on-display
                       (getenv "MEVEDEL_NATIVE_DISPLAY")
                       '((width . 143) (height . 58) (menu-bar-lines . 0)
                         (tool-bar-lines . 0) (vertical-scroll-bars . nil)))))
           (select-frame frame)
           (blink-cursor-mode -1)
           (sit-for 0.1)
           (switch-to-buffer (get-buffer-create "*cursor-regression*"))
           (mevedel-surface-mode)
           (local-set-key [left] #'backward-char)
           (local-set-key [right] #'forward-char)
           (setq-local buffer-invisibility-spec t
                       auto-window-vscroll nil scroll-conservatively 101
                       header-line-format "Prompt" tab-line-format '("Status"))
           (insert (apply #'concat (make-list 60 "ordinary line\n")))
           (let ((start (point)))
             (insert "Right now there are up to 5 `Calling Tool: arg...` lines "
                     "(`mevedel-view-pending-tools-visible-max`), each added and "
                     "removed separately. With fast parallel calls, that makes "
                     "the area flicker. They also use a different style from "
                     "the settled group just above them, which already reads "
                     "well: `✓ Read 7 files, matched 1 glob, ran 5 commands…`.\n")
             (save-excursion
               (goto-char start)
               (while (search-forward "`" nil t)
                 (put-text-property (1- (point)) (point) 'invisible 'markdown-ts--markup)))
             (goto-char start)
             (search-forward "✓"))
           (let ((target (point)) rows)
             (goto-char (point-max))
             (insert " rest\nmore text\n")
             (goto-char target)
             (recenter -1)
             (redisplay t)
             (dolist (key '(nil [left] [left] [right] [right]))
               (when key (execute-kbd-macro key))
               (redisplay t)
               (push (list :point (point) :cursor (window-cursor-info)
                           :start (window-start) :hidden (and (invisible-p (point)) t)) rows))
             (setq rows (nreverse rows))
             (setq result (list :rows rows :line-height (frame-char-height)
                                :modified (buffer-modified-p)))))
       (error (setq status 1 result (list :error (error-message-string err)))))
     (with-temp-file (getenv "MEVEDEL_NATIVE_RESULT") (prin1 result (current-buffer)))
     (kill-emacs status))))

;;; view-cursor-wayland.el ends here
