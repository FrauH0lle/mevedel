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
(require 'mevedel-view)
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
          (frame-resize-pixelwise t)
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
            (set-frame-font (car case) t nil t)
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
        (set-frame-font original-font t nil t)))))

(mevedel-deftest mevedel-view-table--visible-stale ()
  ,test
  (test)
  :doc "skips window-end's display simulation when no stale table reaches the window"
  (mevedel-test--with-displayed-buffer
    (insert "| a | b |\n|---|---|\n| 1 | 2 |\n")
    (let ((table-end (point)))
      (insert (make-string 400 ?\n) "| c | d |\n")
      (put-text-property (point-min) (point-max) 'mevedel-view-table-source "src")
      (put-text-property table-end (- (point-max) 10) 'mevedel-view-table-source nil)
      (put-text-property (point-min) (point-max) 'mevedel-view-table-width
                         (window-body-width (selected-window) t))
      ;; The first table is unformatted but entirely above the window start.
      (put-text-property (point-min) table-end 'mevedel-view-table-width nil)
      (set-window-start (selected-window) (+ table-end 5) t)
      (let ((noninteractive nil))
        (cl-letf (((symbol-function 'window-end)
                   (lambda (&rest _) (error "Simulated display"))))
          (should-not (mevedel-view-table--visible-stale (selected-window)))))
      (set-window-start (selected-window) (point-min) t)
      (should (= (point-min)
                 (car (mevedel-view-table--visible-stale (selected-window)))))))
  :doc "finds a table only once it is actually on screen"
  (progn
    (skip-unless (display-graphic-p))
    (mevedel-test--with-displayed-buffer
      (mevedel-view-mode)
      (let ((inhibit-read-only t)
            (source "| A | B |\n|---|---|\n| visible | table |\n"))
        (unwind-protect
            (progn
              (insert (propertize " " 'display
                                  `(space :height (,(+ 100 (window-pixel-height))))))
              (insert "\n")
              (let ((table (point)))
                (insert source)
                (mevedel-view--decorate-markdown-in-range table (point-max))
                (goto-char (point-min))
                (set-window-start (selected-window) (point-min))
                (redisplay t)
                ;; One tall display row fills the window; counting text rows
                ;; would incorrectly classify the table below it as visible.
                (should-not (mevedel-view-table--visible-stale (selected-window)))
                (goto-char table)
                (set-window-start (selected-window) table)
                ;; Discover changed window start without waiting for redisplay.
                (should (= table (car (mevedel-view-table--visible-stale (selected-window)))))
                (redisplay t)
                (should (= table (car (mevedel-view-table--visible-stale (selected-window)))))))
          (mevedel-view--cancel-realign-timer))))))

(mevedel-deftest mevedel-view-table--layout
  (:doc "cached redraws track actual face and source fontification changes")
  (progn
    (skip-unless (display-graphic-p))
    (let ((height (face-attribute 'mevedel-view-table-header :height nil)))
      (unwind-protect
          (mevedel-test--with-displayed-buffer
            (mevedel-view-mode)
            (let ((inhibit-read-only t)
                  (source (copy-sequence "| Header text | Other |\n|---|---|\n| cell | value |\n"))
                  (window (selected-window))
                  (render (symbol-function 'mevedel-view-table--render-source))
                  (calls 0))
              ,prepare
              (cl-letf (((symbol-function 'mevedel-view-table--render-source)
                         (lambda (&rest arguments)
                           (cl-incf calls)
                           (apply render arguments))))
                (cl-labels ((redraw ()
                              (erase-buffer)
                              (insert source)
                              (mevedel-view-table-decorate (point-min) (point-max) nil)
                              (mevedel-view-table-rerender window)
                              (redisplay t)))
                  (redraw)
                  (should (= calls 1))
                  (let ((before (buffer-string))
                        (before-layout (mevedel-view-table--layout
                                        (string-trim-right source) window 0)))
                    (redraw)
                    (should (= calls 1))
                    (should (equal-including-properties before (buffer-string)))
                    ,change
                    (redraw)
                    (should (= calls 2))
                    (let ((fresh (mevedel-view-table--render-source
                                  (string-trim-right source) window 0)))
                      (should-not (equal-including-properties
                                   before-layout fresh))
                      (should (equal (string-trim-right (buffer-string))
                                     (substring-no-properties fresh)))
                      (dotimes (position (length fresh))
                        (dolist (property '(face font-lock-face display))
                          (should (equal (get-text-property position property fresh)
                                         (get-text-property (1+ position) property)))))))))))
        (set-face-attribute 'mevedel-view-table-header nil :height height))))
  (prepare change)
  nil (set-face-attribute 'mevedel-view-table-header nil :height 3.0)
  nil (put-text-property (string-match "cell" source) (+ 4 (string-match "cell" source))
                        'font-lock-face '(:height 2.0 :weight bold) source)
  (put-text-property (string-match "cell" source) (+ 4 (string-match "cell" source))
                     'font-lock-face (list :height 1.0) source)
  (let* ((position (string-match "cell" source))
         (face (get-text-property position 'font-lock-face source)))
    ;; Keep the comparison snapshot independent of the deliberately mutated
    ;; face value that the rendered cell originally shared with SOURCE.
    (setq before-layout (copy-sequence before-layout))
    (let ((cell (string-match "cell" before-layout)))
      (put-text-property cell (+ cell 4) 'font-lock-face
                         (list :height 1.0) before-layout))
    (setf (plist-get face :height) 2.0)
    (maphash (lambda (key _rendered)
               (should (= 1.0 (plist-get (get-text-property position 'font-lock-face (car key))
                                         :height))))
             mevedel-view-table--layout-cache)))

(provide 'test-mevedel-view-table-graphical)
;;; test-mevedel-view-table-graphical.el ends here
