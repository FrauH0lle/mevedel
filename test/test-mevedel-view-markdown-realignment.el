;;; test-mevedel-view-markdown-realignment.el --- Markdown realignment tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-markdown)

(mevedel-deftest mevedel-view--realign-markdown ()
  ,test
  (test)
  :doc "re-lays out stale tables without touching view or data-buffer state"
  (let ((data-buffer (generate-new-buffer " *mevedel-realign-data*")))
    (unwind-protect
        (mevedel-test--with-displayed-buffer
          (require 'mevedel-view-table)
          (setq-local mevedel--data-buffer data-buffer)
          (with-current-buffer data-buffer
            (insert "authoritative transcript\n"))
          (let ((cell (mapconcat #'identity (make-list 30 "word") " "))
                (draft "> a multiline draft\n> whose text must survive\n"))
            (insert "| A | B |\n|---|---|\n| " cell " | short |\n")
            (mevedel-view-table-decorate (point-min) (point-max) nil)
            (let ((table-end (next-single-property-change
                              (point-min) 'mevedel-view-table-source
                              nil (point-max))))
              (put-text-property (point-min) table-end
                                 'mevedel-view-table-width nil))
            (goto-char (point-max))
            (insert draft)
            (goto-char (point-max))
            (search-backward "survive")
            (set-buffer-modified-p nil)
            (setq buffer-undo-list nil)
            (mevedel-view--realign-markdown)
            (should (eql (window-body-width (selected-window) t)
                         (get-text-property (point-min)
                                            'mevedel-view-table-width)))
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
                               'mevedel-test-fresh t)
            (set-buffer-modified-p nil)
            (setq buffer-undo-list nil)
            (mevedel-view--realign-markdown)
            (should (get-text-property (point-min) 'mevedel-test-fresh))
            (should-not (buffer-modified-p))
            (should-not buffer-undo-list)))
      (kill-buffer data-buffer)))

  :doc "is a no-op for an undisplayed buffer"
  (with-temp-buffer
    (require 'mevedel-view-table)
    (insert "| a | b |\n|---|---|\n| 1 | 2 |\n")
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (let ((before (buffer-string)))
      (mevedel-view--realign-markdown)
      (should (equal before (buffer-string)))))

  :doc "ignores a dead buffer"
  (let ((buffer (generate-new-buffer " *mevedel-dead*")))
    (kill-buffer buffer)
    (mevedel-view--realign-markdown buffer))

  :doc "lays out for the window whose change scheduled the job"
  (mevedel-test--with-displayed-buffer
    (require 'mevedel-view-table)
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
            (mevedel-view-table-decorate (point-min) (point-max) nil)
            (let ((table-end (next-single-property-change
                              (point-min) 'mevedel-view-table-source
                              nil (point-max))))
              (put-text-property (point-min) table-end
                                 'mevedel-view-table-width nil))
            (should-not (eql (window-body-width other t)
                             (window-body-width (selected-window) t)))
            (goto-char (point-min))
            (search-forward "anchor")
            (set-window-start other (line-beginning-position))
            (mevedel-view--realign-markdown buffer other)
            (should (eql (window-body-width other t)
                         (get-text-property (point-min)
                                            'mevedel-view-table-width)))
            (save-excursion
              (goto-char (window-start other))
              (should (looking-at-p "anchor"))))
        (when (window-live-p other)
          (delete-window other))))))

(mevedel-deftest mevedel-view--realign-on-window-change ()
  ,test
  (test)
  :doc "debounces window changes onto one cancellable idle timer"
  (mevedel-test--with-displayed-buffer
    (unwind-protect
        (progn
          (mevedel-view--realign-on-window-change (selected-window))
          (should (timerp mevedel-view--realign-timer))
          (let ((first mevedel-view--realign-timer))
            (mevedel-view--realign-on-window-change (selected-window))
            (should (timerp mevedel-view--realign-timer))
            (should-not (memq first timer-idle-list))))
      (mevedel-view--cancel-realign-timer))
    (should-not mevedel-view--realign-timer)))

(mevedel-deftest mevedel-view--enable-markdown-realign ()
  ,test
  (test)
  :doc "installs local window hooks and cancels the pending timer on kill"
  (let ((buffer (generate-new-buffer " *mevedel-realign-hooks*"))
        timer)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (mevedel-view--enable-markdown-realign)
            (should (member #'mevedel-view--realign-on-window-change
                            window-size-change-functions))
            (should (member #'mevedel-view--realign-on-window-change
                            window-buffer-change-functions))
            (mevedel-view--realign-on-window-change (selected-window))
            (setq timer mevedel-view--realign-timer)
            (should (memq timer timer-idle-list)))
          (kill-buffer buffer)
          (should-not (memq timer timer-idle-list)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'test-mevedel-view-markdown-realignment)

;;; test-mevedel-view-markdown-realignment.el ends here
