;;; test-niwa-view-markdown-realignment.el --- Markdown realignment tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-markdown)
(require 'niwa-view-stream)

(niwa-deftest niwa-view--realign-markdown ()
  ,test
  (test)
  :doc "re-lays out stale tables without touching view or data-buffer state"
  (let ((data-buffer (generate-new-buffer " *niwa-realign-data*")))
    (unwind-protect
        (niwa-test--with-displayed-buffer
          (setq-local niwa--data-buffer data-buffer)
          (with-current-buffer data-buffer
            (insert "authoritative transcript\n"))
          (let ((cell (mapconcat #'identity (make-list 30 "word") " "))
                (draft "> a multiline draft\n> whose text must survive\n"))
            (insert "| A | B |\n|---|---|\n| " cell " | short |\n")
            (niwa-view-table-decorate (point-min) (point-max) nil)
            (let ((table-end (next-single-property-change
                              (point-min) 'niwa-view-table-source
                              nil (point-max))))
              (put-text-property (point-min) table-end
                                 'niwa-view-table-layout nil))
            (goto-char (point-max))
            (insert draft)
            (goto-char (point-max))
            (search-backward "survive")
            (set-buffer-modified-p nil)
            (setq buffer-undo-list nil)
            (niwa-view--realign-markdown)
            (should (eql (window-body-width (selected-window) t)
                         (get-text-property (point-min)
                                            'niwa-view-table-width)))
            (should (looking-at-p "survive"))
            (should (string-suffix-p draft (buffer-substring-no-properties
                                            (point-min) (point-max))))
            (should-not (buffer-modified-p))
            (should-not buffer-undo-list)
            (should (equal "authoritative transcript\n"
                           (with-current-buffer data-buffer
                             (buffer-string))))
            ;; This property is not carried by table replacement, so its
            ;; survival proves a fresh table was not rebuilt.
            (put-text-property (point-min) (1+ (point-min))
                               'niwa-test-fresh t)
            (set-buffer-modified-p nil)
            (setq buffer-undo-list nil)
            (niwa-view--realign-markdown)
            (should (get-text-property (point-min) 'niwa-test-fresh))
            (should-not (buffer-modified-p))
            (should-not buffer-undo-list)))
      (kill-buffer data-buffer)))

  :doc "is a no-op for an undisplayed buffer"
  (with-temp-buffer
    (insert "| a | b |\n|---|---|\n| 1 | 2 |\n")
    (niwa-view-table-decorate (point-min) (point-max) nil)
    (let ((before (buffer-string)))
      (niwa-view--realign-markdown)
      (should (equal before (buffer-string)))))

  :doc "ignores a dead buffer"
  (let ((buffer (generate-new-buffer " *niwa-dead*")))
    (kill-buffer buffer)
    (niwa-view--realign-markdown buffer))

  :doc "lays out for the window whose change scheduled the job"
  (niwa-test--with-displayed-buffer
    (let* ((cell (mapconcat #'identity (make-list 30 "word") " "))
           (buffer (current-buffer))
           ;; A side-by-side split gives the two windows different
           ;; widths (divider column), so the assertion discriminates
           ;; which window the realign actually targeted.
           (other (split-window nil nil 'right)))
      (unwind-protect
          (progn
            (set-window-buffer other buffer)
            (insert "| A | B |\n|---|---|\n| " cell " | short |\nanchor\n")
            (niwa-view-table-decorate (point-min) (point-max) nil)
            (let ((table-end (next-single-property-change
                              (point-min) 'niwa-view-table-source
                              nil (point-max))))
              (put-text-property (point-min) table-end
                                 'niwa-view-table-layout nil))
            (should-not (eql (window-body-width other t)
                             (window-body-width (selected-window) t)))
            (goto-char (point-min))
            (search-forward "anchor")
            (set-window-start other (line-beginning-position))
            (niwa-view--realign-markdown buffer other)
            (should-not (get-text-property (point-min) 'niwa-view-table-layout))
            (save-excursion
              (goto-char (window-start other))
              (should (looking-at-p "anchor")))
            ;; The table only becomes work when the changed window shows it.
            (set-window-start other (point-min))
            (niwa-view--realign-markdown buffer other)
            (should (eql (window-body-width other t)
                         (get-text-property (point-min)
                                            'niwa-view-table-width)))
            (should (= (point-min) (window-start other))))
        (when (window-live-p other)
          (delete-window other)))))

  :doc "a pass with no table or image to lay out leaves the view alone"
  (niwa-test--with-displayed-buffer
    (insert "Plain text without tables.\n")
    (setq niwa-view--image-layout-width
          (window-body-width (selected-window) t))
    (let ((mutations 0))
      (cl-letf (((symbol-function 'niwa-view-render-mutate)
                 (lambda (&rest _) (cl-incf mutations))))
        (niwa-view--realign-markdown (current-buffer) (selected-window))
        (should (= 0 mutations))
        ;; A new width needs a pass.
        (setq niwa-view--image-layout-width 1)
        (niwa-view--realign-markdown (current-buffer) (selected-window))
        (should (= 1 mutations))))))

(niwa-deftest niwa-view--realign-on-window-change ()
  ,test
  (test)
  :doc "debounces window changes onto one cancellable idle timer"
  (niwa-test--with-displayed-buffer
    (unwind-protect
        (progn
          (niwa-view--realign-on-window-change (selected-window))
          (should (timerp niwa-view--realign-timer))
          (let ((first niwa-view--realign-timer))
            (niwa-view--realign-on-window-change (selected-window))
            (should (timerp niwa-view--realign-timer))
            (should-not (memq first timer-idle-list))))
      (niwa-view--cancel-realign-timer))
    (should-not niwa-view--realign-timer))

  :doc "does not wake the editor after a scroll when layout is already current"
  (niwa-test--with-displayed-buffer
    (insert "Plain progress text.\n")
    (unwind-protect
        (progn
          (niwa-view--realign-markdown)
          (niwa-view--realign-on-window-change (selected-window))
          (should-not niwa-view--realign-timer)
          ;; A resize still needs a deferred image scan.
          (setq niwa-view--image-layout-width 1)
          (niwa-view--realign-on-window-change (selected-window))
          (should (timerp niwa-view--realign-timer)))
      (niwa-view--cancel-realign-timer)))

  :doc "schedules a newly visible stale table even at the remembered image width"
  (niwa-test--with-displayed-buffer
    (insert "| A | B |\n|---|---|\n| one | two |\n")
    (niwa-view-table-decorate (point-min) (point-max) nil)
    (put-text-property (point-min) (point-max) 'niwa-view-table-layout nil)
    (setq niwa-view--image-layout-width (window-body-width (selected-window) t))
    (unwind-protect
        (progn
          (niwa-view--realign-on-window-change (selected-window))
          (should (timerp niwa-view--realign-timer))
          (niwa-view--realign-markdown)
          (niwa-view--realign-on-window-change (selected-window))
          (should-not niwa-view--realign-timer))
      (niwa-view--cancel-realign-timer)))

  :doc "lays out a stale table above the tail after streaming text and scrolling up"
  (niwa-test--with-displayed-buffer
    (niwa-view-mode)
    (let ((inhibit-read-only t)
          (window (selected-window)))
      (insert "| A | B |\n|---|---|\n| one | two |\n")
      (niwa-view-table-decorate (point-min) (point-max) nil)
      (insert (make-string 300 ?\n))
      (setq niwa-view--image-layout-width (window-body-width window t))
      (unwind-protect
          (progn
            ;; The window follows streamed text far below the stale table.
            (set-window-start window (- (point-max) 5) t)
            (insert "streamed chunk\n")
            (niwa-view--realign-on-window-change window)
            (should-not niwa-view--realign-timer)
            (set-window-start window (point-min) t)
            (niwa-view--realign-on-window-change window)
            (should (timerp niwa-view--realign-timer))
            (niwa-view--realign-markdown (current-buffer) window)
            (should (eql (window-body-width window t)
                         (get-text-property (point-min) 'niwa-view-table-width))))
        (niwa-view--cancel-realign-timer)))))

(niwa-deftest niwa-view--enable-markdown-realign ()
  ,test
  (test)
  :doc "installs local hooks and cancels pending work on mode change and kill"
  (let ((buffer (generate-new-buffer " *niwa-realign-hooks*"))
        (configuration (current-window-configuration)) timer)
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (niwa-view--enable-markdown-realign)
            (should (member #'niwa-view--realign-on-window-change
                            window-size-change-functions))
            (should (member #'niwa-view--realign-on-window-change
                            window-buffer-change-functions))
            (should (member #'niwa-view--realign-on-window-change
                            window-scroll-functions))
            (should (member #'niwa-view--realign-on-window-change
                            post-command-hook))
            (niwa-view--realign-on-window-change (selected-window))
            (let ((old-timer niwa-view--realign-timer))
              (fundamental-mode)
              (should-not (memq old-timer timer-idle-list))
              (should-not niwa-view--realign-timer))
            (niwa-view--enable-markdown-realign)
            (niwa-view--realign-on-window-change (selected-window))
            (setq timer niwa-view--realign-timer)
            (should (memq timer timer-idle-list)))
          (kill-buffer buffer)
          (should-not (memq timer timer-idle-list)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (set-window-configuration configuration))))

(provide 'test-niwa-view-markdown-realignment)

;;; test-niwa-view-markdown-realignment.el ends here
