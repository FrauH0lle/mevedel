;;; view-composer-wayland.el --- Composer column regression -*- lexical-binding: t -*-

;;; Commentary:
;; Observe actual display positions: overlay properties alone do not prove
;; alignment at end of buffer, where Emacs does not apply line-prefix.

;;; Code:

(setq load-path (read (getenv "MEVEDEL_TEST_LOAD_PATH")))
(require 'mevedel-view)
(require 'mevedel-view-composer)
(setq mevedel-present-enabled nil)

(defun mevedel-test-composer--cursor ()
  "Return the actual cursor's pixel coordinates after redisplay."
  (redisplay t)
  (let ((cursor (window-cursor-info)))
    (cons (aref cursor 1) (aref cursor 2))))

(run-at-time
 0.1 nil
 (lambda ()
   (let ((status 0) result alignment edges)
     (condition-case err
         (let ((compiled
                (mapcar (lambda (symbol)
                          (byte-code-function-p (symbol-function symbol)))
                        '(mevedel-view--setup
                          mevedel-view--update-composer-indent
                          mevedel-view--update-composer-end)))
               (frame (make-frame-on-display
                       (getenv "MEVEDEL_NATIVE_DISPLAY")
                       '((width . 80) (height . 40) (menu-bar-lines . 0)
                         (tool-bar-lines . 0) (vertical-scroll-bars . nil)))))
           (select-frame frame)
           (blink-cursor-mode -1)
           (let ((data (get-buffer-create " *composer-data*")))
             (with-current-buffer data (org-mode))
             (mevedel-view--setup (get-buffer-create "*composer*") data))
           (switch-to-buffer "*composer*")
           (setq-local mevedel--session (mevedel-session--create :name "test"))
           (dolist (mode '(ask edits full-auto directive ask))
             (mevedel-view--clear-input)
             (goto-char (point-max))
             (insert "> first\n" (make-string 200 ?a) "\nlast\n")
             (setq-local mevedel-permission-mode
                         (if (eq mode 'directive) 'ask mode))
             (setq mevedel-view--composer-scope
                   (and (eq mode 'directive)
                        (list :directive-id "test" :action 'retry
                              :record (mevedel-directive--create :request "Fix it"))))
             (mevedel-view-refresh-input-prompt)
             (goto-char (mevedel-view--input-start))
             (redisplay t)
             (let ((first (posn-x-y (posn-at-point))) second wrapped last)
               (forward-line 1)
               (setq second (posn-x-y (posn-at-point)))
               (vertical-motion 1)
               (setq wrapped (posn-x-y (posn-at-point)))
               (forward-line 1)
               (setq last (posn-x-y (posn-at-point)))
               (forward-line 1)
               (redisplay t)
               (push (list mode first second wrapped last (window-cursor-info)) alignment))
             ;; Sweep the right edge, including the natural wrap boundary.
             ;; Compare actual cursors, not overlay properties or columns.
             (let* ((overlay mevedel-view--composer-keymap-overlay)
                    (indent (overlay-get overlay 'line-prefix))
                    (width (- (window-body-width) (string-width indent))))
               (dolist (delta '(-3 -2 -1 0 1))
                 (mevedel-view--clear-input)
                 (goto-char (point-max))
                 (insert (make-string (+ width delta) ?a))
                 (overlay-put overlay 'after-string nil)
                 (let ((reference (mevedel-test-composer--cursor))
                       populated always-present newline typed untyped restored)
                   (mevedel-view--update-composer-end)
                   (setq populated (mevedel-test-composer--cursor))
                   ;; Negative control: the rejected static suffix must move
                   ;; the cursor differently for at least one edge length.
                   (overlay-put overlay 'after-string
                                (concat indent (propertize " " 'cursor t)))
                   (setq always-present (mevedel-test-composer--cursor))
                   (mevedel-view--update-composer-end)
                   (insert "\n")
                   (setq newline (mevedel-test-composer--cursor))
                   (insert "a")
                   (setq typed (mevedel-test-composer--cursor))
                   (delete-char -1)
                   (setq untyped (mevedel-test-composer--cursor))
                   (delete-char -1)
                   (setq restored (mevedel-test-composer--cursor))
                   (push (list mode delta reference populated always-present
                               newline typed untyped restored)
                         edges)))))
           (setq result (list :compiled compiled :character-width (window-font-width)
                              :alignment alignment :edges edges)))
       (error (setq status 1 result (list :error (error-message-string err)))))
     (with-temp-file (getenv "MEVEDEL_NATIVE_RESULT") (prin1 result (current-buffer)))
     (kill-emacs status))))

;;; view-composer-wayland.el ends here
