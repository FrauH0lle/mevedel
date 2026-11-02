;;; test-mevedel-view-composer-keymaps.el --- Composer keyboard routing tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise composer commands through Emacs key lookup and execution with
;; competing contextual keymaps, independently of any third-party package.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-history)

(mevedel-deftest mevedel-view-composer-keyboard-routing ()
  ,test
  (test)
  :doc "Word overlays do not hide unrelated composer keyboard commands"
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (insert "`word`\nsecond line")
      (let* ((start (mevedel-view--input-start))
             (word (make-overlay (1+ start) (+ start 5)))
             (context-map (define-keymap "<mouse-3>" #'ignore)))
        (overlay-put word 'keymap context-map)
        (goto-char (+ start 2))
        (should (eq (key-binding (kbd "C-c RET")) #'mevedel-view-send))
        (execute-kbd-macro (kbd "C-c C-u"))
        (should (equal (mevedel-view--input-text) "")))))

  :doc "Contextual property maps retain their own keys and compose prefixes"
  (dolist (kind '(overlay text-property local-map))
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (insert "`word`\nsecond line")
        (let* ((start (mevedel-view--input-start))
               (map (define-keymap "C-c x" #'backward-char
                                  "<mouse-3>" #'ignore)))
          (if (eq kind 'overlay)
              (let ((outer (make-overlay start (+ start 6)))
                    (inner (make-overlay (1+ start) (+ start 5))))
                (overlay-put outer 'keymap (define-keymap "C-c z" #'ignore))
                (overlay-put inner 'keymap map)
                (overlay-put inner 'priority 50))
            (put-text-property (1+ start) (+ start 5)
                               (if (eq kind 'local-map) 'local-map 'keymap) map))
          (goto-char (+ start 2))
          (dolist (binding '(("C-c RET" . mevedel-view-send)
                             ("C-c TAB" . mevedel-view-send-follow-up)
                             ("C-c C-e" . mevedel-pending-inputs-open)
                             ("C-c C-u" . mevedel-view-history-clear-input)
                             ("C-y" . mevedel-view-yank-dwim)
                             ("M-p" . mevedel-view-history-previous)
                             ("M-n" . mevedel-view-history-next)
                             ("M-r" . mevedel-view-history-search)
                             ("C-<tab>" . mevedel-view-toggle-plan-mode)
                             ("<backtab>" . mevedel-view-cycle-permission-mode)))
            (should (eq (key-binding (kbd (car binding))) (cdr binding))))
          (should (eq (key-binding (kbd "C-c x")) #'backward-char))
          (should (eq (key-binding [mouse-3]) #'ignore))
          (define-key map (kbd "C-c RET") #'forward-char)
          (if (eq kind 'local-map)
              ;; A local-map replaces the major map, below emulation maps.
              (should (eq (key-binding (kbd "C-c RET")) #'mevedel-view-send))
            (should (eq (key-binding (kbd "C-c RET")) #'forward-char))
            (execute-kbd-macro (kbd "C-c RET"))
            (should (= (point) (+ start 3))))
          (should (equal (mevedel-view--input-text) "`word`\nsecond line"))))))

  :doc "Lookup tracks current point, prefixes, meta events and command remapping"
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (use-local-map (copy-keymap (current-local-map)))
      (local-set-key (kbd "C-z") #'move-beginning-of-line)
      (local-set-key (kbd "C-c x") #'forward-char)
      (insert "`word`\nsecond line")
      (let ((start (mevedel-view--input-start)))
        (dolist (position (list start (1+ start) (+ start 3) (+ start 6) (point-max)))
          (goto-char position)
          (should (eq (key-binding (kbd "C-c RET")) #'mevedel-view-send))
          (should (eq (key-binding (kbd "ESC p")) #'mevedel-view-history-previous))
          (should (eq (key-binding (vector (event-convert-list '(meta p))))
                      #'mevedel-view-history-previous))
          (should (eq (key-binding (kbd "C-a")) #'mevedel-view-history-beginning-of-line))
          (should (eq (key-binding (kbd "C-z")) #'mevedel-view-history-beginning-of-line))
          (should (eq (key-binding (kbd "C-c x")) #'forward-char)))
        (goto-char (+ start 3))
        (execute-kbd-macro (kbd "C-z"))
        (should (= (point) start))
        (dolist (position (list (point-min) (1- start)))
          (goto-char position)
          (should-not (eq (key-binding (kbd "C-c RET")) #'mevedel-view-send))
          (should (eq (key-binding (kbd "C-c C-k")) #'mevedel-view-abort))
          (should (eq (key-binding (kbd "C-z")) #'beginning-of-visual-line)))
        (goto-char start)
        (mevedel-view-refresh-input-prompt)
        (should (eq (key-binding (kbd "C-c RET")) #'mevedel-view-send))
        (should (equal (mevedel-view--input-text) "`word`\nsecond line"))
        (should (= (point) (mevedel-view--input-start))))))

  :doc "Root and side composers keep distinct bindings across rebuilds"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (dolist (side '(nil t nil))
        (mevedel-view--setup view-buf data-buf (list :side-conversation-p side))
        (goto-char (point-max))
        (should (eq (key-binding (kbd "C-c RET")) #'mevedel-view-send))
        (should (eq (key-binding (kbd "C-c C-k"))
                    (if side #'mevedel-view-abort #'mevedel-view-cancel-composer-state)))
        (should (eq (eq (key-binding (kbd "C-c C-z")) #'mevedel-side-conversation-close)
                    side))
        (should (eq (eq (key-binding (kbd "M-p")) #'mevedel-view-history-previous)
                    (not side)))
        (should (eq (eq (key-binding (kbd "C-<tab>")) #'mevedel-view-toggle-plan-mode)
                    (not side))))
      (mevedel-view--setup view-buf data-buf '(:agent-transcript-p t))
      (should-not (eq (key-binding (kbd "C-c RET")) #'mevedel-view-send))
      (mevedel-view--setup view-buf data-buf)
      (goto-char (point-max))
      (should (eq (key-binding (kbd "C-c RET")) #'mevedel-view-send))
      (set-marker mevedel-view--input-marker nil)
      (should-not (eq (key-binding (kbd "C-c RET")) #'mevedel-view-send))
      (fundamental-mode)
      (should-not (eq (key-binding (kbd "C-c RET")) #'mevedel-view-send))))

  :doc "Composer outranks ordinary mode maps but respects overriding maps"
  (let* ((other (define-keymap "C-c RET" #'backward-char "C-y" #'backward-char))
         (minor-mode-map-alist (cons (cons t other) minor-mode-map-alist))
         (emulation-mode-map-alists (cons (list (cons t other)) emulation-mode-map-alists)))
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (insert "draft")
        (goto-char (+ (mevedel-view--input-start) 2))
        (should (eq (key-binding (kbd "C-c RET")) #'mevedel-view-send))
        (should (eq (key-binding (kbd "C-y")) #'mevedel-view-yank-dwim))
        (let ((overriding-local-map (define-keymap "C-c RET" #'forward-char)))
          (should (eq (key-binding (kbd "C-c RET")) #'forward-char)))
        (let ((overriding-terminal-local-map (define-keymap "C-c RET" #'backward-char))
              (before (point)))
          (execute-kbd-macro (kbd "C-c RET"))
          (should (= (point) (1- before)))))))

  :doc "Registration derives newly configured top-level keys from the owned maps"
  (let ((mevedel-view--composer-keymap (copy-keymap mevedel-view--composer-keymap))
        (mevedel-view--side-conversation-keymap
         (copy-keymap mevedel-view--side-conversation-keymap)))
    (define-key mevedel-view--composer-keymap (kbd "C-v") #'forward-char)
    (keymap-set mevedel-view--side-conversation-keymap "C-v" #'backward-char)
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (dolist (side '(nil t))
          (mevedel-view--setup view-buf data-buf (list :side-conversation-p side))
          (insert "draft")
          (goto-char (+ (mevedel-view--input-start) 2))
          (should (eq (key-binding (kbd "C-v"))
                      (if side #'backward-char #'forward-char)))
          (let ((before (point)))
            (execute-kbd-macro (kbd "C-v"))
            (should (= (point) (+ before (if side -1 1)))))
          (goto-char (point-min))
          (should (eq (key-binding (kbd "C-v")) #'scroll-up-command)))))))

(provide 'test-mevedel-view-composer-keymaps)
;;; test-mevedel-view-composer-keymaps.el ends here
