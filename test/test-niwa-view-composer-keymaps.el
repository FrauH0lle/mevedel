;;; test-niwa-view-composer-keymaps.el --- Composer keyboard routing tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise composer commands through Emacs key lookup and execution with
;; competing contextual keymaps, independently of any third-party package.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-history)

(niwa-deftest niwa-view-composer-keyboard-routing ()
  ,test
  (test)
  :doc "Word overlays do not hide unrelated composer keyboard commands"
  (niwa-view-test--with-buffers
    (save-window-excursion
      (niwa-view-composer-insert "`word`\nsecond line" nil view-buf)
      (switch-to-buffer (niwa-view-composer-buffer view-buf))
      (let* ((start (niwa-view-composer-draft-start view-buf))
             (word (make-overlay (1+ start) (+ start 5)))
             (context-map (define-keymap "<mouse-3>" #'ignore)))
        (overlay-put word 'keymap context-map)
        (goto-char (+ start 2))
        (should (eq (key-binding (kbd "C-c RET")) #'niwa-view-send))
        (execute-kbd-macro (kbd "C-c C-u"))
        (should (equal (niwa-view-composer-draft-text view-buf t) "")))))

  :doc "Contextual property maps retain their own keys and compose prefixes"
  (dolist (kind '(overlay text-property local-map))
    (niwa-view-test--with-buffers
      (save-window-excursion
        (niwa-view-composer-insert "`word`\nsecond line" nil view-buf)
        (switch-to-buffer (niwa-view-composer-buffer view-buf))
        (let* ((start (niwa-view-composer-draft-start view-buf))
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
          (dolist (binding '(("C-c RET" . niwa-view-send)
                             ("C-c TAB" . niwa-view-send-follow-up)
                             ("C-c C-e" . niwa-pending-inputs-open)
                             ("C-c C-u" . niwa-view-history-clear-input)
                             ("C-y" . niwa-view-yank-dwim)
                             ("M-p" . niwa-view-history-previous)
                             ("M-n" . niwa-view-history-next)
                             ("M-r" . niwa-view-history-search)
                             ("C-<tab>" . niwa-view-toggle-plan-mode)
                             ("<backtab>" . niwa-view-cycle-permission-mode)))
            (should (eq (key-binding (kbd (car binding))) (cdr binding))))
          (should (eq (key-binding (kbd "C-c x")) #'backward-char))
          (should (eq (key-binding [mouse-3]) #'ignore))
          (define-key map (kbd "C-c RET") #'forward-char)
          (if (eq kind 'local-map)
              ;; A local-map replaces the major map, below emulation maps.
              (should (eq (key-binding (kbd "C-c RET")) #'niwa-view-send))
            (should (eq (key-binding (kbd "C-c RET")) #'forward-char))
            (execute-kbd-macro (kbd "C-c RET"))
            (should (= (point) (+ start 3))))
          (should (equal (niwa-view-composer-draft-text view-buf t)
                         "`word`\nsecond line"))))))

  :doc "Composer and transcript resolve their own prefixes and command remappings"
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer (niwa-view-composer-buffer view-buf))
      (use-local-map (copy-keymap (current-local-map)))
      (local-set-key (kbd "C-z") #'move-beginning-of-line)
      (local-set-key (kbd "C-c x") #'forward-char)
      (niwa-view-composer-insert "`word`\nsecond line" nil view-buf)
      (let ((start (niwa-view-composer-draft-start view-buf)))
        (dolist (position (list start (1+ start) (+ start 3) (+ start 6)
                                (niwa-view-composer-draft-end view-buf)))
          (goto-char position)
          (should (eq (key-binding (kbd "C-c RET")) #'niwa-view-send))
          (should (eq (key-binding (kbd "ESC p")) #'niwa-view-history-previous))
          (should (eq (key-binding (vector (event-convert-list '(meta p))))
                      #'niwa-view-history-previous))
          (should (eq (key-binding (kbd "C-a")) #'niwa-view-history-beginning-of-line))
          (should (eq (key-binding (kbd "C-z")) #'niwa-view-history-beginning-of-line))
          (should (eq (key-binding (kbd "C-c x")) #'forward-char)))
        (goto-char (+ start 3))
        (execute-kbd-macro (kbd "C-z"))
        (should (= (point) start))
        (with-current-buffer view-buf
          (use-local-map (copy-keymap (current-local-map)))
          (local-set-key (kbd "C-z") #'move-beginning-of-line)
          (should-not (eq (key-binding (kbd "C-c RET")) #'niwa-view-send))
          (should (eq (key-binding (kbd "C-c C-k")) #'niwa-view-abort))
          (should (eq (key-binding (kbd "C-z")) #'beginning-of-visual-line)))
        (goto-char start)
        (niwa-view-refresh-input-prompt)
        (should (eq (key-binding (kbd "C-c RET")) #'niwa-view-send))
        (should (equal (niwa-view-composer-draft-text view-buf t)
                       "`word`\nsecond line"))
        (should (= (point) (niwa-view-composer-draft-start view-buf))))))

  :doc "Root and side composers keep distinct bindings across rebuilds"
  (niwa-view-test--with-buffers
    (let ((composer (niwa-view-composer-buffer view-buf)))
      (dolist (side '(nil t nil))
        (niwa-view--setup view-buf data-buf (list :side-conversation-p side))
        (with-current-buffer (niwa-view-composer-buffer view-buf)
          (goto-char (niwa-view-composer-draft-end view-buf))
          (should (eq (key-binding (kbd "C-c RET")) #'niwa-view-send))
          (should (eq (key-binding (kbd "C-c C-k"))
                      (if side #'niwa-view-abort #'niwa-view-cancel-composer-state)))
          (should (eq (eq (key-binding (kbd "C-c C-z")) #'niwa-side-conversation-close)
                      side))
          (should (eq (eq (key-binding (kbd "M-p")) #'niwa-view-history-previous)
                      (not side)))
          (should (eq (eq (key-binding (kbd "C-<tab>")) #'niwa-view-toggle-plan-mode)
                      (not side)))))
      (niwa-view--setup view-buf data-buf '(:agent-transcript-p t))
      (should-not (buffer-live-p composer))
      (should-not (niwa-view-composer-buffer view-buf))
      (with-current-buffer view-buf
        (should-not (eq (key-binding (kbd "C-c RET")) #'niwa-view-send)))
      (niwa-view--setup view-buf data-buf)
      (with-current-buffer (niwa-view-composer-buffer view-buf)
        (goto-char (niwa-view-composer-draft-end view-buf))
        (should (eq (key-binding (kbd "C-c RET")) #'niwa-view-send))
        (fundamental-mode)
        (should-not (eq (key-binding (kbd "C-c RET")) #'niwa-view-send)))))

  :doc "Composer outranks ordinary mode maps but respects overriding maps"
  (let* ((other (define-keymap "C-c RET" #'backward-char "C-y" #'backward-char))
         (minor-mode-map-alist (cons (cons t other) minor-mode-map-alist))
         (emulation-mode-map-alists (cons (list (cons t other)) emulation-mode-map-alists)))
    (niwa-view-test--with-buffers
      (save-window-excursion
        (niwa-view-composer-insert "draft" nil view-buf)
        (switch-to-buffer (niwa-view-composer-buffer view-buf))
        (goto-char (+ (niwa-view-composer-draft-start view-buf) 2))
        (should (eq (key-binding (kbd "C-c RET")) #'niwa-view-send))
        (should (eq (key-binding (kbd "C-y")) #'niwa-view-yank-dwim))
        (let ((overriding-local-map (define-keymap "C-c RET" #'forward-char)))
          (should (eq (key-binding (kbd "C-c RET")) #'forward-char)))
        (let ((overriding-terminal-local-map (define-keymap "C-c RET" #'backward-char))
              (before (point)))
          (execute-kbd-macro (kbd "C-c RET"))
          (should (= (point) (1- before)))))))

  :doc "Registration derives newly configured top-level keys from the owned maps"
  (let ((niwa-view--composer-keymap (copy-keymap niwa-view--composer-keymap))
        (niwa-view--side-conversation-keymap
         (copy-keymap niwa-view--side-conversation-keymap)))
    (define-key niwa-view--composer-keymap (kbd "C-v") #'forward-char)
    (keymap-set niwa-view--side-conversation-keymap "C-v" #'backward-char)
    (niwa-view-test--with-buffers
      (save-window-excursion
        (dolist (side '(nil t))
          (niwa-view--setup view-buf data-buf (list :side-conversation-p side))
          (niwa-view-composer-insert "draft" nil view-buf)
          (switch-to-buffer (niwa-view-composer-buffer view-buf))
          (goto-char (+ (niwa-view-composer-draft-start view-buf) 2))
          (should (eq (key-binding (kbd "C-v"))
                      (if side #'backward-char #'forward-char)))
          (let ((before (point)))
            (execute-kbd-macro (kbd "C-v"))
            (should (= (point) (+ before (if side -1 1)))))
          (with-current-buffer view-buf
            (should (eq (key-binding (kbd "C-v")) #'scroll-up-command))))))))

(provide 'test-niwa-view-composer-keymaps)
;;; test-niwa-view-composer-keymaps.el ends here
