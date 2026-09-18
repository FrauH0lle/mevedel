;;; test-mevedel-view-table-graphical.el --- Real font layout -*- lexical-binding: t -*-
;;; Commentary:
;; Run in graphical Emacs under the same isolated HOME/XDG roots as Eask.
;; Batch runs skip this case; mocked pixel metrics cannot prove alignment.
;;; Code:
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view-markdown)
(require 'mevedel-view-render)
(require 'mevedel-view-table)

(mevedel-deftest mevedel-view-table-graphical-layout ()
  ,test
  (test)
  :doc "real borders align with proportional fonts, faces, Unicode and reflow"
  (progn
    (skip-unless (display-graphic-p))
    (skip-unless (and (member "DejaVu Sans" (font-family-list))
                      (member "DejaVu Sans Mono" (font-family-list))))
    (let ((original-font (frame-parameter nil 'font))
          (source (concat "| Name | Description |\n|---|---|\n"
                          "| α界 🙂 | target and [link](https://example.org) with repeated words for wrapping |\n"
                          "| WWW iii | monospaced |\n| small | |\n")))
      (unwind-protect
          (dolist (case '(("DejaVu Sans Mono-11" nil)
                          ("DejaVu Sans Mono-11" ((default (:family "DejaVu Sans" :height 1.1))))
                          ("DejaVu Sans-11" nil)
                          ("DejaVu Sans-11" ((default (:height 1.3))))
                          ("DejaVu Sans-11" ((mevedel-view-table-header (:height 1.3))
                                             (mevedel-view-table-zebra (:height 1.2))))))
            (set-frame-font (car case) nil t)
            (mevedel-test--with-displayed-buffer
              (setq-local face-remapping-alist (cadr case))
              (insert source)
              (save-excursion
                (goto-char (point-min))
                (search-forward "monospaced")
                (put-text-property (match-beginning 0) (point) 'face
                                   '(:family "DejaVu Sans Mono" :height 1.1)))
              (setq-local line-prefix "  ")
              (mevedel-view-table-decorate (point-min) (point-max) nil)
              (dotimes (narrow 2)
                (when (= narrow 1)
                  (split-window-right)
                  (mevedel-view-table-rerender (selected-window)))
                (goto-char (point-min))
                (redisplay t)
                (let (rows xs)
                  (cl-loop for pos from (point-min) below (point-max) do
                           (cond
                            ((and (memq (char-after pos) '(?│ ?├ ?┼ ?┤))
                                  (not (get-text-property pos 'mevedel-view-table-cell)))
                             (let ((position (posn-at-point pos)))
                               (should position)
                               (push (car (posn-x-y position)) xs)))
                            ((eq (char-after pos) ?\n)
                             (when xs (push (nreverse xs) rows))
                             (setq xs nil))))
                  (when xs (push (nreverse xs) rows))
                  (ert-info ((format "Font/remapping %S, narrow %S, borders %S" case narrow rows))
                    (should (> (length rows) 4))
                    (dolist (row rows)
                      (should (= (length row) 3))
                      (should (equal row (car rows))))))
                (should (equal (string-trim-right source)
                               (substring-no-properties
                                (get-text-property (point-min) 'mevedel-view-table-source))))
                (should (text-property-not-all (point-min) (point-max) 'mevedel-view-url nil)))))
        (set-frame-font original-font nil t)))))

(provide 'test-mevedel-view-table-graphical)
;;; test-mevedel-view-table-graphical.el ends here
