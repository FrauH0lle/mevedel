;;; test-niwa-view-table-graphical.el --- Real font layout -*- lexical-binding: t -*-
;;; Commentary:
;; Run in graphical Emacs under the same isolated HOME/XDG roots as Eask.
;; Batch runs skip this case; mocked pixel metrics cannot prove alignment.
;;; Code:
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-markdown)
(require 'niwa-view-render)
(require 'niwa-view-table)

(niwa-deftest niwa-view-table-graphical-layout ()
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
                          "| α界 🙂 | **target** and [link](https://example.org) with repeated words for wrapping |\n"
                          "| *em* ~~old~~ | `x|y` |\n"
                          "| WWW iii | monospaced |\n| small | |\n")))
      (unwind-protect
          (dolist (case '(("DejaVu Sans Mono-11" nil)
                          ("DejaVu Sans Mono-11" ((default (:family "DejaVu Sans" :height 1.1))))
                          ("DejaVu Sans-11" nil)
                          ("DejaVu Sans-11" ((default (:height 1.3))))
                          ("DejaVu Sans-11" ((niwa-view-table-header (:height 1.3))
                                             (niwa-view-table-zebra (:height 1.2))))))
            (set-frame-font (car case) t nil t)
            (niwa-test--with-displayed-buffer
              (setq-local face-remapping-alist (cadr case))
              (insert source)
              (save-excursion
                (goto-char (point-min))
                (search-forward "monospaced")
                (put-text-property (match-beginning 0) (point) 'face
                                   '(:family "DejaVu Sans Mono" :height 1.1)))
              (setq-local line-prefix "  ")
              (niwa-view-table-decorate (point-min) (point-max) nil)
              (dotimes (narrow 2)
                (when (= narrow 1)
                  (split-window-right)
                  (niwa-view-table-rerender (selected-window)))
                (goto-char (point-min))
                (redisplay t)
                (let (rows xs)
                  (cl-loop for pos from (point-min) below (point-max) do
                           (cond
                            ((and (memq (char-after pos) '(?│ ?├ ?┼ ?┤))
                                  (not (get-text-property pos 'niwa-view-table-cell)))
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
                                (get-text-property (point-min) 'niwa-view-table-source))))
                (should (text-property-not-all (point-min) (point-max) 'niwa-view-url nil)))))
        (set-frame-font original-font t nil t)))))

(niwa-deftest niwa-view-table--visible-stale ()
  ,test
  (test)
  :doc "skips window-end's display simulation when no stale table reaches the window"
  (niwa-test--with-displayed-buffer
    (insert "| a | b |\n|---|---|\n| 1 | 2 |\n")
    (let ((table-end (point)))
      (insert (make-string 400 ?\n) "| c | d |\n")
      (put-text-property (point-min) (point-max) 'niwa-view-table-source "src")
      (put-text-property table-end (- (point-max) 10) 'niwa-view-table-source nil)
      (put-text-property (point-min) (point-max) 'niwa-view-table-layout
                         (niwa-view-table--window-layout (selected-window)))
      ;; The first table is unformatted but entirely above the window start.
      (put-text-property (point-min) table-end 'niwa-view-table-layout nil)
      (set-window-start (selected-window) (+ table-end 5) t)
      (let ((noninteractive nil))
        (cl-letf (((symbol-function 'window-end)
                   (lambda (&rest _) (error "Simulated display"))))
          (should-not (niwa-view-table--visible-stale (selected-window)))))
      (set-window-start (selected-window) (point-min) t)
      (should (= (point-min)
                 (car (niwa-view-table--visible-stale (selected-window)))))))
  :doc "finds a table only once it is actually on screen"
  (progn
    (skip-unless (display-graphic-p))
    (niwa-test--with-displayed-buffer
      (niwa-view-mode)
      (let ((inhibit-read-only t)
            (source "| A | B |\n|---|---|\n| visible | table |\n"))
        (unwind-protect
            (progn
              (insert (propertize " " 'display
                                  `(space :height (,(+ 100 (window-pixel-height))))))
              (insert "\n")
              (let ((table (point)))
                (insert source)
                (niwa-view--decorate-markdown-in-range table (point-max))
                (goto-char (point-min))
                (set-window-start (selected-window) (point-min))
                (redisplay t)
                ;; One tall display row fills the window; counting text rows
                ;; would incorrectly classify the table below it as visible.
                (should-not (niwa-view-table--visible-stale (selected-window)))
                (goto-char table)
                (set-window-start (selected-window) table)
                ;; Discover changed window start without waiting for redisplay.
                (should (= table (car (niwa-view-table--visible-stale (selected-window)))))
                (redisplay t)
                (should (= table (car (niwa-view-table--visible-stale (selected-window)))))))
          (niwa-view--cancel-realign-timer))))))

(niwa-deftest niwa-view-table--layout
  (:doc "cached redraws track actual face and source fontification changes")
  (progn
    (skip-unless (display-graphic-p))
    (let ((height (face-attribute 'niwa-view-table-header :height nil)))
      (unwind-protect
          (niwa-test--with-displayed-buffer
            (niwa-view-mode)
            (let ((inhibit-read-only t)
                  (source (copy-sequence "| Header text | Other |\n|---|---|\n| cell | value |\n"))
                  (window (selected-window))
                  (render (symbol-function 'niwa-view-table--render-source))
                  (calls 0))
              ,prepare
              (cl-letf (((symbol-function 'niwa-view-table--render-source)
                         (lambda (&rest arguments)
                           (cl-incf calls)
                           (apply render arguments))))
                (cl-labels ((redraw ()
                              (erase-buffer)
                              (insert source)
                              (niwa-view-table-decorate (point-min) (point-max) nil)
                              (niwa-view-table-rerender window)
                              (redisplay t)))
                  (redraw)
                  (should (= calls 1))
                  (let ((before (buffer-string))
                        (before-layout (niwa-view-table--entry-rendered
                                        (niwa-view-table--layout
                                         (string-trim-right source) window 0))))
                    (redraw)
                    (should (= calls 1))
                    (should (equal-including-properties before (buffer-string)))
                    ,change
                    (redraw)
                    (should (= calls 2))
                    (let ((fresh (niwa-view-table--render-source
                                  (string-trim-right source) window 0)))
                      (should-not (equal-including-properties
                                   before-layout fresh))
                      (should (equal (string-trim-right (buffer-string))
                                     (substring-no-properties fresh)))
                      (dotimes (position (length fresh))
                        (dolist (property '(face font-lock-face display))
                          (should (equal (get-text-property position property fresh)
                                         (get-text-property (1+ position) property)))))))))))
        (set-face-attribute 'niwa-view-table-header nil :height height))))
  (prepare change)
  nil (set-face-attribute 'niwa-view-table-header nil :height 3.0)
  nil (put-text-property (string-match "cell" source) (+ 4 (string-match "cell" source))
                        'font-lock-face '(:height 2.0 :weight bold) source)
  (put-text-property (string-match "cell" source) (+ 4 (string-match "cell" source))
                     'font-lock-face (list :height 1.0) source)
  (let* ((position (string-match "cell" source))
         (face (get-text-property position 'font-lock-face source)))
    (setf (plist-get face :height) 2.0)
    (maphash (lambda (key _rendered)
               (should (= 1.0 (plist-get (get-text-property position 'font-lock-face key)
                                         :height))))
             niwa-view-table--layout-cache))
  (put-text-property (string-match "cell" source) (+ 4 (string-match "cell" source))
                     'font-lock-face (list :height 1.0) source)
  (let* ((position (string-match "cell" source))
         (original (substring-no-properties source)))
    ;; Revisit the first layout using independent input after mutating SOURCE.
    (put-text-property position (+ position 4) 'font-lock-face
                       (list :height 1.0) original)
    (setf (plist-get (get-text-property position 'font-lock-face source) :height) 2.0)
    (should (equal-including-properties
             (niwa-view-table--entry-rendered
              (niwa-view-table--layout (string-trim-right original) window 0))
             (funcall render (string-trim-right original) window 0)))
    (should (= calls 1))))

(niwa-deftest niwa-view-table--display-cell/markdown
  (:doc "inline Markdown uses prose faces and zero-width delimiters inside a cell")
  (progn
    (skip-unless (and (require 'markdown-ts-mode nil t)
                      (niwa-view--markdown-grammars-ready-p)))
    (let* ((niwa-view-hide-markdown-markup t)
           (source "| **Bold** | *italic* | `x|y` | ~~Obsolete~~ | [Docs](https://example.org) |\n|---|---|---|---|---|")
           (rendered (niwa-view-table--render-source source nil 0)))
      (dolist (expect '(("Bold" . markdown-ts-bold) ("italic" . markdown-ts-emphasis)
                        ("x|y" . markdown-ts-code-span) ("Obsolete" . markdown-ts-strikethrough)))
        (let ((pos (string-match (regexp-quote (car expect)) rendered)))
          (should pos)
          (dolist (property '(face font-lock-face))
            (should (memq (cdr expect) (flatten-tree (get-text-property pos property rendered)))))))
      (should-not (string-match-p "[*`~]" rendered))
      (should (equal "https://example.org"
                     (get-text-property (string-match "Docs" rendered) 'niwa-view-url rendered)))
      (should (memq 'link (flatten-tree (get-text-property (string-match "Docs" rendered)
                                                         'face rendered))))
      (should (= (string-width (car (split-string rendered "\n")))
                 (string-width "│ Bold │ italic │ x|y │ Obsolete │ Docs │")))
      (let* ((old (string-match "Obsolete" source))
             (new (string-match "Obsolete" rendered))
             (mapping (niwa-view-table--position-map source rendered)))
        (should (= new (aref mapping old)))
        (should (= (+ new 2) (aref mapping (+ old 2))))))))

(niwa-deftest niwa-view-table--visible-stale/font-and-color
  (:doc "same-width text scale invalidates a layout; a color change does not")
  (progn
    (skip-unless (display-graphic-p))
    (niwa-test--with-displayed-buffer
      (let* ((window (selected-window))
             (frame (window-frame window))
             (face 'niwa-view-table-header)
             (color (face-attribute face :foreground frame)))
        (insert "| First | Second |\n|---|---|\n| cell | value |\n")
        (niwa-view-table-decorate (point-min) (point-max) nil)
        (unwind-protect
            (progn
              (set-face-attribute face frame :foreground "red")
              (should-not (niwa-view-table--visible-stale window))
              (setq-local face-remapping-alist '((default (:height 1.5) default)))
              (should (niwa-view-table-rerender window))
              (should-not (niwa-view-table--visible-stale window)))
          (set-face-attribute face frame :foreground color))))))

(niwa-deftest niwa-view-table-graphical-line-height
  (:doc "mixed inline Markdown keeps wide and wrapped rows on one pixel grid")
  (progn
    (skip-unless (and (display-graphic-p) (require 'markdown-ts-mode nil t)
                      (niwa-view--markdown-grammars-ready-p)
                      (member "DejaVu Sans" (font-family-list))))
    (let* ((frame (selected-frame))
           (font (frame-parameter frame 'font))
           (code 'markdown-ts-code-span)
           (height (face-attribute code :height frame)))
      (unwind-protect
          (progn
            (set-frame-font "DejaVu Sans-11" t nil t)
            (set-face-attribute code frame :height 1.5)
            (niwa-test--with-displayed-buffer
              (insert "| Case | Details |\n|---|---|\n"
                      "| **Bold** and *italic* | `x|y` and [Docs](https://example.org) |\n"
                      "| ~~Old~~ | Several words that wrap in a narrow window |\n\nAfter\n")
              (niwa-view-table-decorate (point-min) (point-max) nil)
              (dotimes (narrow 2)
                (when (= narrow 1)
                  (split-window-right)
                  (face-remap-add-relative 'default :height 1.2)
                  (should (niwa-view-table-rerender (selected-window))))
                (goto-char (point-min))
                (set-window-start (selected-window) (point-min))
                (redisplay t)
                (let (tops)
                  (while (get-text-property (point) 'niwa-view-table-source)
                    (push (cdr (posn-x-y (posn-at-point))) tops)
                    (forward-line 1))
                  (push (cdr (posn-x-y (posn-at-point))) tops)
                  (let ((heights (seq-mapn #'- tops (cdr tops))))
                    (ert-info ((format "Narrow: %s; row heights: %S" narrow heights))
                      (should (>= (length heights) 4))
                      (should (> (car heights) 0))
                      (should (apply #'= heights))))))))
        (set-face-attribute code frame :height height)
        (set-frame-font font t nil t)))))

(provide 'test-niwa-view-table-graphical)
;;; test-niwa-view-table-graphical.el ends here
