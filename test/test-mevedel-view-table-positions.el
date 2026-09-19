;;; test-mevedel-view-table-positions.el --- Table marker policy -*- lexical-binding: t -*-
;;; Commentary:
;; Cell identity, decoration and removed whitespace through table reflow.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-view-markdown)
(require 'mevedel-view-table)

(mevedel-deftest mevedel-view-table--splice ()
                 ,test
                 (test)
                 :doc "both marker insertion types retain the table's start and end boundaries"
                 (dolist (prefix '("" "before\n"))
                   (with-temp-buffer
                     (insert prefix "old table" "\nafter")
                     (let* ((beg (1+ (length prefix))) (end (+ beg 9))
                            (markers (list (copy-marker beg nil) (copy-marker beg t)
                                           (copy-marker end nil) (copy-marker end t))))
                       (unwind-protect
                           (progn
                             (mevedel-view-table--splice beg end "new")
                             (should (equal (mapcar #'marker-position markers)
                                            (list beg beg (+ beg 3) (+ beg 3)))))
                         (mapc (lambda (marker) (set-marker marker nil)) markers)))))

                 :doc "failed insertion rolls back text, markers and evaporating overlays"
                 (with-temp-buffer
                   (insert "Before old text After")
                   (let* ((original (buffer-string)) (marker (copy-marker 10))
                          (ov (make-overlay 9 12)) (target (current-buffer))
                          (insert-fn (symbol-function 'insert)))
                     (overlay-put ov 'evaporate t)
                     (unwind-protect
                         (progn
                           (cl-letf (((symbol-function 'insert)
                                      (lambda (&rest args)
                                        (if (and (eq target (current-buffer)) (equal args '("NEW")))
                                            (error "Injected insertion failure")
                                          (apply insert-fn args)))))
                             (should-error (mevedel-view-table--splice 8 16 "NEW")))
                           (should (equal original (buffer-string)))
                           (should (= marker 10))
                           (should (= (overlay-start ov) 9))
                           (should (= (overlay-end ov) 12)))
                       (set-marker marker nil)
                       (delete-overlay ov)))))

(mevedel-deftest mevedel-view-table--render-region/markers ()
                 ,test
                 (test)
                 :doc "raw links and cell text keep hidden excursion markers, overlays and undo state"
                 (dolist (source '("| First | Second |\n|---|---|\n| target | another cell |\n"
                                   "| First | Second |\n|---|---|\n| words before target words after | alpha β界 `a|b` x\\|y |\n"
                                   "| First | Second |\n|---|---|\n| [target](https://example.org) | other |\n"))
                   (with-temp-buffer
                     (insert "Before\n" source "After\n")
                     (goto-char (point-min))
                     (search-forward "target")
                     (let* ((end (point)) (start (- end 6))
                            (markers (list (copy-marker (+ start 2)) (copy-marker (+ start 2) t)))
                            (ov (make-overlay start end nil t nil))
                            (outside (copy-marker (- (point-max) 3)))
                            (buffer-undo-list t))
                       (overlay-put ov 'evaporate t)
                       (overlay-put ov 'test-property 'kept)
                       (goto-char (+ start 2))
                       (unwind-protect
                           (progn
                             (save-excursion
                               (cl-letf (((symbol-function 'mevedel-view-table--usable-columns) (lambda (&rest _) 40)))
                                 (mevedel-view-table-decorate (point-min) (point-max) nil)))
                             (should (looking-at-p "rget"))
                             (should (eq buffer-undo-list t))
                             (dolist (marker markers)
                               (goto-char marker)
                               (should (looking-at-p "rget")))
                             (should (equal "target" (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))
                             (should (eq (overlay-get ov 'test-property) 'kept))
                             (goto-char outside)
                             (should (looking-at-p "er\n")))
                         (mapc (lambda (marker) (set-marker marker nil)) (cons outside markers))
                         (delete-overlay ov))))))

(mevedel-deftest mevedel-view-table--position-map ()
                 ,test
                 (test)
                 :doc "padding follows its cell edge"
                 (progn
                   (let
                       ((old
	                 (concat "│ "
		                 (propertize "alpha" 'mevedel-view-table-cell 2
			                     'mevedel-view-table-cell-offset 0)
		                 "       │"))
                        (new
	                 (concat "│ "
		                 (propertize "alpha" 'mevedel-view-table-cell 2
			                     'mevedel-view-table-cell-offset 0)
		                 " │")))
                     (should (= 7 (aref (mevedel-view-table--position-map old new) 10)))))
                 :doc "removed whitespace follows the next surviving character"
                 (progn
                   (let
                       ((old
	                 (concat "│ "
		                 (propertize "alpha   beta" 'mevedel-view-table-cell 2
			                     'mevedel-view-table-cell-offset 0)
		                 " │"))
                        (new
	                 (concat "│ "
		                 (propertize "alpha" 'mevedel-view-table-cell 2
			                     'mevedel-view-table-cell-offset 0)
		                 " │\n│ "
		                 (propertize "beta" 'mevedel-view-table-cell 2
			                     'mevedel-view-table-cell-offset 8)
		                 "  │")))
                     (should (= 12 (aref (mevedel-view-table--position-map old new) 8)))))
                 :doc "separator interiors clamp before the next junction"
                 (progn
                   (let ((old "├─────────────┼───┤") (new "├───────┼───┤"))
                     (should (= 7 (aref (mevedel-view-table--position-map old new) 10)))
                     (should (= 8 (aref (mevedel-view-table--position-map old new) 14)))))
                 :doc "row boundaries survive reflow and literal pipes are content"
                 (progn
                   (let*
                       ((source
	                 "| Long header | Empty | Third |\n|---|---|---|\n| alpha   beta gamma delta epsilon zeta eta theta | | `a|b` x\\|y │ |\n| last | | tail |")
                        (old
	                 (cl-letf
	                     (((symbol-function 'mevedel-view-table--usable-columns)
	                       (lambda (&rest _) 120)))
	                   (mevedel-view-table--render-source source nil 0)))
                        (new
	                 (cl-letf
	                     (((symbol-function 'mevedel-view-table--usable-columns)
	                       (lambda (&rest _) 35)))
	                   (mevedel-view-table--render-source source nil 0)))
                        (mapping (mevedel-view-table--position-map old new)))
                     (let*
	                 ((old-sep (string-match "├" old))
	                  (new-sep (string-match "├" new))
	                  (old-junction (string-match "┼" old old-sep))
	                  (new-junction (string-match "┼" new new-sep)))
                       (should (= new-sep (aref mapping old-sep)))
                       (should (= new-junction (aref mapping old-junction))))
                     (dolist (pair (list (cons old new) (cons new old)))
                       (let
	                   ((map (mevedel-view-table--position-map (car pair) (cdr pair)))
	                    (pos 0))
	                 (while (string-match "^[│├]" (car pair) pos)
	                   (setq pos (match-beginning 0))
	                   (should
	                    (equal (get-text-property pos 'mevedel-view-table-row (car pair))
		                   (get-text-property (aref map pos) 'mevedel-view-table-row
				                      (cdr pair))))
	                   (should
	                    (= (aref (car pair) pos) (aref (cdr pair) (aref map pos))))
	                   (cl-incf pos))))
                     (should
                      (= 4
	                 (length
	                  (nth 2 (aref (gethash 2 (mevedel-view-table--grid source)) 0)))))))
                 :doc "trimmed raw padding attaches to content"
                 (progn
                   (let*
                       ((source "| A | B |\n|---|---|\n|    target    | |")
                        (new (mevedel-view-table--render-source source nil 0))
                        (map (mevedel-view-table--position-map source new))
                        (raw-target (string-match "target" source))
                        (new-target (string-match "target" new)))
                     (should (= new-target (aref map (1- raw-target))))
                     (should (= (+ new-target 6) (aref map (+ raw-target 7))))))
                 :doc "identical text retains every boundary"
                 (progn
                   (let
                       ((text
	                 (mevedel-view-table--render-source "| A | B |\n|---|---|\n| words | |" nil
			                                    0)))
                     (should
                      (equal (vconcat (number-sequence 0 (length text)))
	                     (mevedel-view-table--position-map text (copy-sequence text))))))
                 )

(mevedel-deftest mevedel-view-table--render-region/reflow ()
                 ,test
                 (test)
                 :doc "both marker types follow every surviving cell character across width changes"
                 (dolist (source '("| Header one | Empty | Third |\n|---|---|---|\n| alpha   beta gamma delta repeated words repeated words | | `a|b` x\\|y |\n| tail | | last |\n"
                                   "| A | B |\n|---|---|\n| [link](https://example.org) and other words for wrapping | mixed α界 text |\n"))
                   (dolist (widths '((80 35) (35 120) (80 80)))
                     (with-temp-buffer
                       (insert source)
                       (cl-letf (((symbol-function 'mevedel-view-table--usable-columns)
                                  (lambda (&rest _) (car widths))))
                         (mevedel-view-table-decorate (point-min) (point-max) nil))
                       (let ((before (buffer-string)) markers)
                         (maphash
                          (lambda (key pos)
                            (when (and (< pos (length before))
                                       (get-text-property pos 'mevedel-view-table-cell before)
                                       (not (string-match-p "[[:space:]]" (substring before pos (1+ pos)))))
                              (dolist (insertion '(nil t))
                                (push (cons (copy-marker (1+ pos) insertion) key) markers))))
                          (mevedel-view-table--positions before))
                         (unwind-protect
                             (progn
                               (cl-letf (((symbol-function 'mevedel-view-table--usable-columns)
                                          (lambda (&rest _) (cadr widths))))
                                 (mevedel-view-table--render-region
                                  (point-min) (1- (point-max)) (string-trim-right source)))
                               (let ((after (mevedel-view-table--positions (buffer-string))))
                                 (dolist (entry markers)
                                   (when-let* ((pos (gethash (cdr entry) after)))
                                     (should (= (marker-position (car entry)) (1+ pos)))))))
                           (dolist (entry markers) (set-marker (car entry) nil))))))))

(provide 'test-mevedel-view-table-positions)
;;; test-mevedel-view-table-positions.el ends here
