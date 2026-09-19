;;; test-mevedel-view-table-deferred.el --- Visible idle tables -*- lexical-binding: t -*-
;;; Commentary:
;; Exercise the normal view decoration and mutation entry points.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-markdown)
(require 'mevedel-view-render)

(mevedel-deftest mevedel-view--realign-markdown/composer ()
                 ,test
                 (test)
                 :doc "idle formatting preserves the actual composer, source, selection and canonical copying"
                 (save-window-excursion
                   (mevedel-view-test--with-buffers
                    (let ((source "| First | Second |\n|---|---|\n| target | other words here |\n"))
                      (mevedel-view-test--insert-data data-buf source 'response)
                      (with-current-buffer view-buf
                        (set-window-buffer (selected-window) view-buf)
                        (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
                        (mevedel-view--full-rerender)
                        (goto-char (point-min))
                        (search-forward "target")
                        (set-mark (point))
                        (backward-char 6)
                        (setq mark-active t deactivate-mark nil)
                        (let ((data-text (with-current-buffer data-buf (buffer-string))))
                          (mevedel-view--realign-markdown)
                          (should mark-active)
                          (should (equal "target" (buffer-substring-no-properties (region-beginning) (region-end))))
                          (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
                          (should (equal-including-properties data-text (with-current-buffer data-buf (buffer-string))))
                          (let ((beg (previous-single-property-change (1+ (point)) 'mevedel-view-table-source nil (point-min)))
                                (end (next-single-property-change (point) 'mevedel-view-table-source nil (point-max))))
                            (should (equal (string-trim-right source)
                                           (substring-no-properties (mevedel-view--buffer-substring-filter beg end)))))))))))

(mevedel-deftest mevedel-view-table-rerender/deferred ()
                 ,test
                 (test)
                 :doc "opens raw, formats one visible table, and discovers off-screen work on scroll"
                 (mevedel-test--with-displayed-buffer
                  (mevedel-view-mode)
                  (let ((inhibit-read-only t)
                        (source "| A | B |\n|---|---|\n| target | value |\n")
                        (draft "> draft\nsecond line\n"))
                    (unwind-protect
                        (progn
                          (insert source "\n" source (make-string 200 ?\n) source draft)
                          (mevedel-view--decorate-markdown-in-range (point-min) (point-max))
                          (should-not (string-search "│" (buffer-string)))
                          (should (get-text-property (point-min) 'mevedel-view-table-source))
                          (goto-char (point-min))
                          (search-forward "target")
                          (set-mark (point))
                          (backward-char 6)
                          (setq mark-active t deactivate-mark nil buffer-undo-list nil)
                          (set-buffer-modified-p nil)
                          (set-window-start (selected-window) (point-min))
                          (mevedel-view--realign-markdown)
                          (should (looking-at-p "target"))
                          (should mark-active)
                          (should (equal "target" (buffer-substring (point) (mark))))
                          (should-not (buffer-modified-p))
                          (should-not buffer-undo-list)
                          (should (timerp mevedel-view--realign-timer))
                          (should (= 2 (how-many "^| A |" (point-min) (point-max))))
                          (mevedel-view--realign-markdown)
                          (should (= 1 (how-many "^| A |" (point-min) (point-max))))
                          (mevedel-view--realign-markdown)
                          (should (= 1 (how-many "^| A |" (point-min) (point-max))))
                          (goto-char (point-max))
                          (search-backward "| A |")
                          (set-window-start (selected-window) (point))
                          (mevedel-view--realign-markdown)
                          (should (= 0 (how-many "^| A |" (point-min) (point-max))))
                          (should (string-suffix-p draft (buffer-string))))
                      (mevedel-view--cancel-realign-timer))))

                 :doc "queued formatting resolves shifted table positions after the active writer"
                 (mevedel-test--with-displayed-buffer
                  (mevedel-view-mode)
                  (let ((inhibit-read-only t))
                    (unwind-protect
                        (progn
                          (insert "| A | B |\n|---|---|\n| target | value |\n")
                          (mevedel-view--decorate-markdown-in-range (point-min) (point-max))
                          (goto-char (point-min))
                          (set-window-start (selected-window) (point-min))
                          (mevedel-view-render-mutate
                           'test
                           (lambda ()
                             (mevedel-view--realign-markdown)
                             (should-not (string-search "│" (buffer-string)))
                             (insert "prefix\n")))
                          (should (string-prefix-p "prefix\n│" (buffer-string))))
                      (mevedel-view--cancel-realign-timer))))

                 :doc "hidden tables wait for unfolding while the next visible table renders"
  (dolist (kind '(overlay property))
    (mevedel-test--with-displayed-buffer
      (mevedel-view-mode)
      (let ((inhibit-read-only t)
            (source "| A | B |\n|---|---|\n| target | value |\n")
            hidden)
        (unwind-protect
            (progn
              (insert source "\n" source)
              (mevedel-view--decorate-markdown-in-range (point-min) (point-max))
              (if (eq kind 'overlay)
                  (progn
                    (setq hidden (make-overlay (point-min) (1+ (length source))))
                    (overlay-put hidden 'invisible t))
                (put-text-property (point-min) (1+ (length source)) 'invisible t))
              (goto-char (1+ (length source)))
              (set-window-start (selected-window) (point-min))
              (mevedel-view--realign-markdown)
              (should (eq (char-after (point-min)) ?|))
              (should (invisible-p (point-min)))
              (should (string-search "│" (buffer-string)))
              (if hidden (delete-overlay hidden)
                (remove-text-properties (point-min) (1+ (length source)) '(invisible nil)))
              (mevedel-view--realign-markdown)
              (should (eq (char-after (point-min)) ?│)))
          (when hidden (delete-overlay hidden))
          (mevedel-view--cancel-realign-timer)))))

  :doc "partly folded tables still render their visible rows and preserve the fold"
  (mevedel-test--with-displayed-buffer
    (mevedel-view-mode)
    (let ((inhibit-read-only t) hidden)
      (unwind-protect
          (progn
            (insert "| A | B |\n|---|---|\n| target | value |\n")
            (mevedel-view--decorate-markdown-in-range (point-min) (point-max))
            (goto-char (point-min))
            (forward-line 1)
            (setq hidden (make-overlay (point-min) (point)))
            (overlay-put hidden 'invisible t)
            (set-window-start (selected-window) (point-min))
            (mevedel-view--realign-markdown)
            (should (eq (char-after (point-min)) ?│))
            (should (invisible-p (point-min)))
            (goto-char (point-min))
            (search-forward "target")
            (should-not (invisible-p (1- (point)))))
        (when hidden (delete-overlay hidden))
        (mevedel-view--cancel-realign-timer))))

  :doc "pending input postpones formatting without losing the scheduled work"
                 (mevedel-test--with-displayed-buffer
                  (mevedel-view-mode)
                  (let ((inhibit-read-only t))
                    (unwind-protect
                        (progn
                          (insert "| A | B |\n|---|---|\n| target | value |\n")
                          (mevedel-view--decorate-markdown-in-range (point-min) (point-max))
                          (cl-letf (((symbol-function 'input-pending-p) (lambda () t)))
                            (mevedel-view--realign-markdown))
                          (should-not (string-search "│" (buffer-string)))
                          (should (timerp mevedel-view--realign-timer)))
                      (mevedel-view--cancel-realign-timer)))))

(provide 'test-mevedel-view-table-deferred)
;;; test-mevedel-view-table-deferred.el ends here
