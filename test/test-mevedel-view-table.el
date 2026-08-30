;;; test-mevedel-view-table.el --- Rendered table tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view-markdown)
(require 'mevedel-view-render)
(require 'mevedel-view-table)

(mevedel-deftest mevedel-view-table-fontified-source ()
  ,test
  (test)

  :doc "a table fontified as Markdown still renders as box-drawing rows"
  ;; The renderer replaces pipe tables with its own rows and must strip the
  ;; fontifier's table faces; a change of Markdown mode changes those face
  ;; names, so the two are tested together rather than apart.
  (when (mevedel-view--markdown-fontify-mode)
    (with-temp-buffer
      (insert (mevedel-view--fontify-as
               "| Fn | Doc |\n|---|---|\n| lisp | head |\n"
               'markdown-mode))
      (mevedel-view-table-decorate (point-min) (point-max) nil)
      (goto-char (point-min))
      (should (search-forward "│ Fn   │ Doc  │" nil t))
      (should (search-forward "├──────┼──────┤" nil t))
      (should (search-forward "│ lisp │ head │" nil t))
      (goto-char (point-min))
      (should-not (search-forward "|---|---|" nil t))
      ;; No raw table fontification survives into the rendered rows.
      (let ((pos (point-min)))
        (while (< pos (point-max))
          (let ((next (or (next-single-property-change pos 'font-lock-face)
                          (point-max)))
                (face (get-text-property pos 'font-lock-face)))
            (dolist (one (if (listp face) face (list face)))
              (should-not (memq one mevedel-view-table--raw-faces)))
            (setq pos next)))))))


(mevedel-deftest mevedel-view-table-decorate ()
  ,test
  (test)
  :doc "renders two or more pipe rows as aligned box-drawing rows"
  (with-temp-buffer
    (insert "before\n| Name | Role |\n|---|---|\n| Ada | Developer |\nafter\n")
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "│ Name │ Role      │" text))
      (should (string-match-p "├──────┼───────────┤" text))
      (should (string-match-p "│ Ada  │ Developer │" text))
      (should (string-prefix-p "before\n" text))
      (should (string-suffix-p "after\n" text))
      (should-not (string-match-p "^| Name" text))))

  :doc "retains the canonical source and layout width as text properties"
  (with-temp-buffer
    (let ((source "| A | B |\n|---|---|\n| 1 | 2 |"))
      (insert source "\n")
      (mevedel-view-table-decorate (point-min) (point-max) nil)
      (goto-char (point-min))
      (should (equal source
                     (substring-no-properties
                      (get-text-property (point) 'mevedel-view-table-source))))
      ;; Undisplayed temp buffer: laid out via string-width, width nil,
      ;; so a later display marks it stale.
      (should-not (get-text-property (point) 'mevedel-view-table-width))
      (should (get-text-property (point) 'mevedel-view-no-linkify))))

  :doc "a single pipe row is not a table"
  (with-temp-buffer
    (let ((text "| lonely | row |\nprose\n"))
      (insert text)
      (mevedel-view-table-decorate (point-min) (point-max) nil)
      (should (equal text (buffer-substring-no-properties
                           (point-min) (point-max))))))

  :doc "renders a table with no separator row as all data rows"
  (with-temp-buffer
    (insert "| a | b |\n| c | d |\n")
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "│ a │ b │" text))
      (should (string-match-p "│ c │ d │" text))
      (should-not (string-match-p "├" text))))

  :doc "escaped pipes are literal cell content"
  (with-temp-buffer
    (insert "| Expr | Result |\n|---|---|\n| a \\| b | pipe |\n")
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "│ a | b" text))
      (should-not (string-match-p "\\\\|" text))))

  :doc "inline-code pipes, escapes, and backticks stay in one cell"
  (with-temp-buffer
    (insert "| Fn | Doc |\n|---|---|\n| `a|b` | `c\\|d` |\n")
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (should (string-search "│ `a|b` │ `c\\|d` │"
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))

  :doc "multiple tables render independently"
  (with-temp-buffer
    (insert "| a | b |\n|---|---|\n| 1 | 2 |\n\nmiddle\n\n"
            "| x | y |\n|---|---|\n| 9 | 8 |\n")
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "│ 1 │ 2 │" text))
      (should (string-match-p "│ 9 │ 8 │" text))
      (should (string-match-p "\nmiddle\n" text))
      (goto-char (point-min))
      (search-forward "│ 1 │")
      (search-forward "│ 9 │")
      (should-not
       (equal (get-text-property (pos-bol) 'mevedel-view-table-source)
              (progn (goto-char (point-min))
                     (get-text-property (point)
                                        'mevedel-view-table-source))))))

  :doc "tables inside avoid ranges stay raw"
  (with-temp-buffer
    (let ((text "| a | b |\n|---|---|\n| 1 | 2 |\n"))
      (insert text)
      (mevedel-view-table-decorate (point-min) (point-max)
                                   (list (cons (point-min) (point-max))))
      (should (equal text (buffer-substring-no-properties
                           (point-min) (point-max))))))

  :doc "linkify-exempt rows stay raw"
  (with-temp-buffer
    (let ((text "| a | b |\n|---|---|\n| 1 | 2 |\n"))
      (insert text)
      (add-text-properties (point-min) (point-max)
                           '(mevedel-view-no-linkify t))
      (mevedel-view-table-decorate (point-min) (point-max) nil)
      (should (equal text (buffer-substring-no-properties
                           (point-min) (point-max))))))

  :doc "an already rendered table is not rendered again"
  (with-temp-buffer
    (insert "| a | b |\n|---|---|\n| 1 | 2 |\n")
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (let ((once (buffer-string)))
      (mevedel-view-table-decorate (point-min) (point-max) nil)
      (should (equal once (buffer-string)))))

  :doc "view-owned properties survive the rewrite"
  (with-temp-buffer
    (insert "| a | b |\n|---|---|\n| 1 | 2 |\n")
    (add-text-properties (point-min) (point-max)
                         '(mevedel-view-source task-table
                           mevedel-view-turn-id 7
                           read-only t))
    (let ((inhibit-read-only t))
      (mevedel-view-table-decorate (point-min) (point-max) nil))
    (goto-char (point-min))
    (let ((table-end (next-single-property-change
                      (point-min) 'mevedel-view-table-source nil (point-max))))
      (should-not (text-property-not-all (point-min) table-end
                                         'mevedel-view-source 'task-table))
      (should-not (text-property-not-all (point-min) table-end
                                         'mevedel-view-turn-id 7))
      (should-not (text-property-not-all (point-min) table-end
                                         'read-only t))))

  :doc "image bookkeeping never survives into a rendered row"
  (with-temp-buffer
    (insert "| Shot | Note |\n|---|---|\n| pic.png | ok |\n")
    (goto-char (point-min))
    (search-forward "pic.png")
    (add-text-properties (match-beginning 0) (match-end 0)
                         '(display (image :file "pic.png")
                           mevedel-view-image-source "pic.png"
                           mevedel-view-image-ratio 0.5
                           mevedel-view-image-width 400))
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (should-not (text-property-not-all (point-min) (point-max)
                                       'mevedel-view-image-source nil))
    (should-not (text-property-not-all (point-min) (point-max)
                                       'mevedel-view-image-ratio nil))
    (should-not (text-property-not-all (point-min) (point-max)
                                       'display nil)))

  :doc "raw table fontification is stripped from cells and the trailing newline"
  (with-temp-buffer
    (insert "| Fn | Doc |\n|---|---|\n| lisp | head |\n")
    (add-text-properties (point-min) (1- (point-max))
                         '(font-lock-face markdown-ts-table))
    ;; The delimiter row carries its own face.
    (goto-char (point-min))
    (search-forward "|---|---|")
    (put-text-property (match-beginning 0) (match-end 0)
                       'font-lock-face 'markdown-ts-table-delimiter-cell)
    ;; The trailing newline is fontified as part of the raw table too.
    (put-text-property (1- (point-max)) (point-max)
                       'font-lock-face 'markdown-ts-table)
    ;; A cell mixing the table face with another face keeps the other.
    (goto-char (point-min))
    (search-forward "lisp")
    (put-text-property (match-beginning 0) (match-end 0)
                       'font-lock-face '(markdown-ts-code-span
                                         markdown-ts-table-cell))
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (goto-char (point-min))
    (search-forward "lisp")
    (should (eq 'markdown-ts-code-span
                (get-text-property (match-beginning 0) 'font-lock-face)))
    (search-forward "head")
    (should-not (get-text-property (match-beginning 0) 'font-lock-face))
    (should-not (get-text-property (1- (point-max)) 'font-lock-face)))

  :doc "cells keep faces and button properties from earlier passes"
  (with-temp-buffer
    (insert "| Fn | Doc |\n|---|---|\n| lisp | head |\n")
    (goto-char (point-min))
    (search-forward "lisp")
    (add-text-properties (match-beginning 0) (match-end 0)
                         '(font-lock-face bold mevedel-view-url "x"))
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (goto-char (point-min))
    (search-forward "lisp")
    (should (eq 'bold (get-text-property (match-beginning 0)
                                         'font-lock-face)))
    (should (equal "x" (get-text-property (match-beginning 0)
                                          'mevedel-view-url))))

  :doc "wide tables wrap cells to the displaying window width"
  (mevedel-test--with-displayed-buffer
    (insert "| Alpha | Beta |\n|---|---|\n"
            "| this cell is far too long to fit inside a narrow window "
            "without wrapping onto continuation lines | short |\n")
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (let* ((limit (floor (* (window-body-width (selected-window)) 0.9)))
           (lines (split-string (buffer-substring-no-properties
                                 (point-min) (point-max))
                                "\n" t)))
      (should (> (length lines) 3))
      (dolist (line lines)
        (should (<= (string-width line) limit)))
      (should (eql (window-body-width (selected-window) t)
                   (get-text-property (point-min)
                                      'mevedel-view-table-width)))))

  :doc "variable-pitch ASCII uses the window pixel measurement path"
  ;; `window-font-width' signals in batch, so the space measurement that
  ;; scales pixels back into columns has to be stubbed as well.  Without
  ;; it the pixel path cannot complete at all, and asserting on
  ;; measurement counts would only witness measurements whose result the
  ;; caller discards.
  (mevedel-test--with-displayed-buffer
    (let ((measurements 0))
      (variable-pitch-mode 1)
      (insert "| Name | Role |\n|---|---|\n| millie | reviewer |\n")
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&optional _display) t))
                ((symbol-function 'window-font-width)
                 (lambda (&rest _) 10))
                ((symbol-function 'buffer-text-pixel-size)
                 (lambda (&rest _)
                   (cl-incf measurements)
                   (cons (* 10 (string-width (buffer-string))) 1))))
        (mevedel-view-table-decorate (point-min) (point-max) nil))
      (should (> measurements 0))))

  :doc "plain ASCII column widths need no measurement at all"
  (mevedel-test--with-displayed-buffer
    (let ((measurements 0))
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&optional _display) t))
                ((symbol-function 'window-font-width)
                 (lambda (&rest _) 10))
                ((symbol-function 'buffer-text-pixel-size)
                 (lambda (&rest _)
                   (cl-incf measurements)
                   (cons (* 10 (string-width (buffer-string))) 1))))
        (should (equal [1 1 1 1]
                       (mevedel-view-table--char-widths
                        "Name" (selected-window)))))
      (should (= 0 measurements))))

  :doc "a measurement failure falls back to character widths"
  (mevedel-test--with-displayed-buffer
    (variable-pitch-mode 1)
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _display) t))
              ((symbol-function 'window-font-width)
               (lambda (&rest _) (error "No font in batch"))))
      (should (equal [1 1 1 1]
                     (mevedel-view-table--char-widths
                      "Name" (selected-window))))))

  :doc "a line-prefix inset narrows the usable width"
  (mevedel-test--with-displayed-buffer
    (let ((cell (make-string 120 ?x)))
      (insert "| A |\n|---|\n| " cell " |\n")
      (add-text-properties (point-min) (point-max)
                           '(line-prefix "    " wrap-prefix "    "))
      (mevedel-view-table-decorate (point-min) (point-max) nil)
      (let ((limit (floor (* (- (window-body-width (selected-window)) 4)
                             0.9))))
        (dolist (line (split-string (buffer-substring-no-properties
                                     (point-min) (point-max))
                                    "\n" t))
          (should (<= (string-width line) limit)))
        (should (equal "    " (get-text-property (point-min) 'line-prefix)))
        (should (equal "    " (get-text-property
                               (1- (next-single-property-change
                                    (point-min) 'mevedel-view-table-source
                                    nil (point-max)))
                               'line-prefix))))))

  :doc "a wider wrap-prefix narrows continuation rows"
  (mevedel-test--with-displayed-buffer
    (let ((cell (make-string 120 ?x)))
      (insert "| A |\n|---|\n| " cell " |\n")
      (add-text-properties (point-min) (point-max)
                           '(line-prefix "  " wrap-prefix "      "))
      (mevedel-view-table-decorate (point-min) (point-max) nil)
      (let ((limit (floor (* (- (window-body-width (selected-window)) 6)
                             0.9))))
        (dolist (line (split-string (buffer-substring-no-properties
                                     (point-min) (point-max))
                                    "\n" t))
          (should (<= (string-width line) limit))))))

  :doc "CJK cells count double-width characters"
  (with-temp-buffer
    (insert "| Name | Kanji |\n|---|---|\n| ai | 日本語 |\n| bee | xy |\n")
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (let ((lines (split-string (buffer-substring-no-properties
                                (point-min) (point-max))
                               "\n" t)))
      (should (apply #'= (mapcar #'string-width lines)))))

  :doc "emoji presentation sequences keep borders aligned"
  (with-temp-buffer
    (insert "| Status | Note |\n|---|---|\n| ⚠️ warn | ok |\n")
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (let ((lines (split-string (buffer-substring-no-properties
                                (point-min) (point-max))
                               "\n" t)))
      (should (apply #'= (mapcar #'string-width lines))))))

(mevedel-deftest mevedel-view-table-rerender ()
  ,test
  (test)
  :doc "is a no-op when the buffer is not displayed"
  (with-temp-buffer
    (insert "| a | b |\n|---|---|\n| 1 | 2 |\n")
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (let ((before (buffer-string)))
      (mevedel-view-table-rerender)
      (should (equal before (buffer-string)))))

  :doc "re-renders a stale table for the displaying window and stops"
  (mevedel-test--with-displayed-buffer
    (let ((cell (mapconcat #'identity (make-list 30 "word") " ")))
      (insert "| A | B |\n|---|---|\n| " cell " | short |\n")
      (mevedel-view-table-decorate (point-min) (point-max) nil)
      ;; Force a nil stored width as an off-screen layout would have.
      (let ((inhibit-read-only t))
        (put-text-property (point-min)
                           (next-single-property-change
                            (point-min) 'mevedel-view-table-source
                            nil (point-max))
                           'mevedel-view-table-width nil))
      (mevedel-view-table-rerender)
      (let ((width (window-body-width (selected-window) t)))
        (should (eql width (get-text-property (point-min)
                                              'mevedel-view-table-width)))
        (dolist (line (split-string (buffer-substring-no-properties
                                     (point-min) (point-max))
                                    "\n" t))
          (should (<= (string-width line)
                      (floor (* (window-body-width (selected-window))
                                0.9)))))
        ;; A second pass with matching width leaves the text alone.
        (let ((before (buffer-string)))
          (mevedel-view-table-rerender)
          (should (equal before (buffer-string)))))))

  :doc "re-renders multiple stale tables in one backward pass"
  (mevedel-test--with-displayed-buffer
    (insert "| a | b |\n|---|---|\n| 1 | 2 |\n\n"
            "| x | y |\n|---|---|\n| 9 | 8 |\n")
    (mevedel-view-table-decorate (point-min) (point-max) nil)
    (put-text-property (point-min) (point-max)
                       'mevedel-view-table-width nil)
    (mevedel-view-table-rerender)
    (goto-char (point-min))
    (let ((count 0)
          match)
      (while (setq match (text-property-search-forward
                          'mevedel-view-table-source))
        (setq count (1+ count))
        (should (eql (window-body-width (selected-window) t)
                     (get-text-property (prop-match-beginning match)
                                        'mevedel-view-table-width))))
      (should (= 2 count))))

  :doc "a blanket display keymap at the table start never clobbers cell keymaps"
  (mevedel-test--with-displayed-buffer
    (let ((cell-map (make-sparse-keymap))
          (display-map (make-sparse-keymap)))
      (insert "| Fn | Doc |\n|---|---|\n| lisp | head |\n")
      (goto-char (point-min))
      (search-forward "lisp")
      (put-text-property (match-beginning 0) (match-end 0)
                         'keymap cell-map)
      (mevedel-view-table-decorate (point-min) (point-max) nil)
      ;; Simulate the display-keymap pass: stamp a keymap wherever the
      ;; rendered table has none, which covers its first border char.
      (let ((pos (point-min))
            (table-end (next-single-property-change
                        (point-min) 'mevedel-view-table-source
                        nil (point-max))))
        (while (< pos table-end)
          (let ((next (or (next-single-property-change pos 'keymap
                                                       nil table-end)
                          table-end)))
            (unless (get-text-property pos 'keymap)
              (put-text-property pos next 'keymap display-map))
            (setq pos next)))
        ;; Force a rerender as a window resize would.
        (put-text-property (point-min) table-end
                           'mevedel-view-table-width nil))
      (mevedel-view-table-rerender)
      (goto-char (point-min))
      (search-forward "lisp")
      (should (eq cell-map (get-text-property (match-beginning 0) 'keymap)))
      (should (eq display-map (get-text-property (point-min) 'keymap))))))

(provide 'test-mevedel-view-table)

;;; test-mevedel-view-table.el ends here
