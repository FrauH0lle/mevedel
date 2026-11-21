;;; test-niwa-view-render-stability.el --- Reader stability regressions -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise redraw boundaries and positions while the view changes above readers.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-view-disclosure)
(require 'niwa-view-markdown)

(niwa-deftest niwa-view-render-live-update/reader-anchors ()
  ,test
  (test)
  :doc "keeps the pinned prompt and draft through streaming and full redraws"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data data-buf "*** Visible question\n" nil)
      (niwa-view-test--insert-data data-buf "Initial answer.\n" 'response)
      (with-current-buffer view-buf
        (switch-to-buffer view-buf)
        (niwa-view--full-rerender)
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 3)
        (goto-char (point-min))
        (search-forward "Initial answer.")
        (set-window-start nil (line-beginning-position) t)
        (let ((top (window-start)))
          (should (string-search "Visible question" (niwa-view--sticky-prompt-line)))
          (niwa-view-stream-begin-turn
           niwa-view--status-marker
           (with-current-buffer data-buf (copy-marker (point-max))))
          (niwa-view-test--insert-data data-buf "More output.\n" 'response)
          (niwa-view-render-live-update data-buf)
          (should (= top (window-start)))
          (should (string-search "Visible question" (niwa-view--sticky-prompt-line)))
          (niwa-view--full-rerender)
          (should (string-search "Visible question" (niwa-view--sticky-prompt-line)))
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))))))

  :doc "full and retained redraws preserve group readers and composer windows"
  (dolist (retained '(nil t))
    (dolist (target '("4 calls" "f0.el" "content 0"))
      (save-window-excursion
        (niwa-view-test--with-buffers
          (niwa-view-test--insert-data data-buf "Inspecting files.\n" 'response)
          (dotimes (i 4)
            (niwa-view-test--insert-data
             data-buf
             (format (concat "#+begin_tool\n"
                             "(:name \"Read\" :args (:file_path \"f%d.el\"))\n\n"
                             "content %d\n#+end_tool\n") i i)
             `(tool . ,(format "call_%d" i))))
          (with-current-buffer view-buf
            (switch-to-buffer view-buf)
            (delete-other-windows)
            (niwa-view-test--insert-composer-draft "> draft\nsecond line" 3)
            (niwa-view-stream-begin-turn
             niwa-view--status-marker
             (with-current-buffer data-buf (copy-marker (point-min))))
            (niwa-view-render-live-update data-buf)
            (goto-char (point-min))
            (search-forward "4 calls: read 4 files")
            (niwa-view-toggle-section)
            (when (equal target "content 0")
              (search-forward "f0.el")
              (niwa-view-toggle-section))
            (when retained
              (dotimes (_ 2) (niwa-view-render-live-update data-buf))
              (should (> (marker-position niwa-view--live-data-tail-start)
                         (marker-position niwa-view--data-turn-start))))
            (goto-char (point-min))
            (search-forward target)
            (goto-char (match-beginning 0))
            (set-mark (+ (point) 2))
            (setq mark-active t)
            (set-window-start nil (line-beginning-position) t)
            (let ((column (current-column))
                  (other (split-window-right))
                  (index 4)
                  (reader (selected-window))
                  (composer (niwa-view-composer-buffer))
                  composer-window)
              (set-window-buffer other view-buf)
              (setq composer-window (niwa-view-composer-display other))
              (should (eq (window-buffer composer-window) composer))
              (set-window-point composer-window (+ (niwa-view-composer-draft-start) 3))
              (with-current-buffer composer
                (set-mark (+ (niwa-view-composer-draft-start) 1))
                (setq mark-active t))
              (dolist (refresh '(full live live full live live))
                (niwa-view-test--insert-data
                 data-buf
                 (format (concat "#+begin_tool\n"
                                 "(:name \"Read\" :args (:file_path \"f%d.el\"))\n\n"
                                 "next\n#+end_tool\n") index)
                 `(tool . ,(format "call_%d" index)))
                (cl-incf index)
                (if (eq refresh 'full)
                    (niwa-view--full-rerender)
                  (niwa-view-render-live-update data-buf))
                (should (looking-at-p
                         (if (equal target "4 calls")
                             "[0-9]+ calls: read [0-9]+ files"
                           (regexp-quote target))))
                (should (= column (current-column)))
                (should (= (mark) (+ (point) 2)))
                (should mark-active)
                (should (eq reader (selected-window)))
                (should (= (window-point) (point)))
                (should (= (window-start) (line-beginning-position)))
                (should (= (window-point composer-window)
                           (+ (niwa-view-composer-draft-start) 3)))
                (with-current-buffer composer
                  (should (= (mark) (+ (niwa-view-composer-draft-start) 1)))
                  (should mark-active))
                (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
                (should (string-match-p "Read: f0.el" (buffer-string)))))))))))

(niwa-deftest niwa-view-zone-reconcile/reader-stability ()
  ,test
  (test)
  :doc "status growth preserves prompt position"
  (niwa-view-test--with-buffers
   (with-current-buffer view-buf
     (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
     (niwa-view-zone-reconcile
      'status niwa-view--status-marker niwa-view--status-marker
      '((:namespace status :id agents :body "One agent")))
     (niwa-view-zone-reconcile
      'interaction niwa-view--interaction-marker niwa-view--interaction-marker
      '((:namespace interaction :id permission :body "Permission choice: allow once")))
     (save-window-excursion
       (set-window-buffer (selected-window) view-buf)
       (let ((start (plist-get (niwa-view-zone-fragment-bounds 'interaction 'permission) :start)))
         (goto-char (+ start 5))
         (set-mark (+ start 10))
         (setq mark-active t)
         (set-window-start (selected-window) start t))
       (dolist (body '("One agent\nTwo agents\nThree agents" "One agent" ""))
         (niwa-view-zone-reconcile
          'status niwa-view--status-marker niwa-view--interaction-marker
          (and (not (equal body ""))
               (list (list :namespace 'status :id 'agents :body body))))
         (let ((start (plist-get (niwa-view-zone-fragment-bounds 'interaction 'permission) :start)))
           (should (= (point) (+ start 5)))
           (should (= (mark) (+ start 10)))
           (should mark-active)
           (should (= (window-start) start))))
       (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))))))

(niwa-deftest niwa-view--full-rerender/selection-stability ()
  ,test
  (test)
  :doc "rerender preserves selected transcript text"
  (dolist (reverse '(nil t))
    (niwa-view-test--with-buffers
     (niwa-view-test--insert-data data-buf "First answer.\n" 'response)
     (niwa-view-test--insert-data data-buf "\n\n*** Next prompt\n\n" nil)
     (niwa-view-test--insert-data data-buf "Selected target text stays here.\n" 'response)
     (with-current-buffer view-buf
       (niwa-view--full-rerender)
       (goto-char (point-min))
       (search-forward "target text")
       (set-mark (- (point) (length "target text")))
       (setq mark-active t)
       (when reverse (exchange-point-and-mark))
       (with-current-buffer data-buf
         (goto-char 3)
         (insert (make-string 100 ?x)))
       (niwa-view--full-rerender)
       (should mark-active)
       (should (eq reverse (< (point) (mark))))
       (should (equal "target text" (buffer-substring-no-properties (region-beginning) (region-end))))))))

(niwa-deftest niwa-view-toggle-section/failure-stability ()
  ,test
  (test)
  :doc "failed disclosure retains visible header"
  (dolist (partial '(nil t))
    (niwa-view-test--with-buffers
     (niwa-view-test--insert-data data-buf "A stable response paragraph.\n" 'response)
     (with-current-buffer view-buf
       (niwa-view--full-rerender)
       (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
       (goto-char (point-min))
       (search-forward "stable response")
       (niwa-view-toggle-section)
       (let ((before (buffer-substring-no-properties (point-min) (point-max)))
             (boundary (niwa-view-transcript-tail)))
         (cl-letf (((symbol-function 'niwa-view-render-insert-expanded-disclosure)
                    (lambda (&rest _)
                      (when partial (insert "Partial replacement\n"))
                      (error "Injected rendering failure"))))
           (should-error
            (niwa-view-toggle-section)))
         (should (equal before (buffer-substring-no-properties (point-min) (point-max))))
         (should (= boundary (niwa-view-transcript-tail)))
         (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
         (should-not niwa-view-render--owner)
         (should (get-text-property (point) 'niwa-view-collapsed))
         (niwa-view-toggle-section)
         (should (string-search "A stable response paragraph." (buffer-string)))
         (should-not (get-text-property (point) 'niwa-view-collapsed)))))))

(niwa-deftest niwa-view-render-live-update/redisplay-stability ()
  ,test
  (test)
  :doc "no visible partial rebuild"
  (niwa-view-test--with-buffers
   (niwa-view-test--insert-data data-buf "Stable paragraph must remain visible.\n" 'response)
   (with-current-buffer view-buf
     (setq niwa-view--data-turn-start (with-current-buffer data-buf (copy-marker 1)))
     (setq niwa-view--in-flight-turn-start (copy-marker niwa-view--status-marker))
     (niwa-view--full-rerender)
     (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
     (niwa-view-render-invalidate-live-tail)
     (let ((original (symbol-function 'niwa-view--fontify-response))
           observed)
       (cl-letf (((symbol-function 'niwa-view--fontify-response)
                  (lambda (&rest args)
                    (setq observed
                          (list :redisplay-inhibited inhibit-redisplay
                                :paragraph-present
                                (and (string-search "Stable paragraph" (buffer-string)) t)))
                    (apply original args))))
         (niwa-view-render-live-update data-buf))
       (should observed)
       (should (or (plist-get observed :redisplay-inhibited)
                   (plist-get observed :paragraph-present)))
       (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
       (should (= (+ (niwa-view-composer-draft-start) 4)
                  (with-current-buffer (niwa-view-composer-buffer) (point))))))))

(niwa-deftest niwa-view--realign-markdown/cell-stability ()
  ,test
  (test)
  :doc "table resize keeps reader in cell"
  (dolist (prefix '("" "some words that wrap before the target cell content "))
    (niwa-test--with-displayed-buffer
     (insert "| First | Second |\n|---|---|\n| " prefix "target | another cell |\n")
     (niwa-view-table-decorate (point-min) (point-max) nil)
     (goto-char (point-min))
     (search-forward "target")
     (set-mark (point))
     (setq mark-active t deactivate-mark nil)
     (backward-char (length "target"))
     (let ((other (split-window nil nil 'right)))
       (unwind-protect
           (progn
             (set-window-buffer other (current-buffer))
             (set-window-point other (point))
             (should (looking-at-p "target"))
             (let ((before (buffer-substring-no-properties (point-min) (point-max)))
                   (before-point (point)))
               (niwa-view--realign-markdown (current-buffer) (selected-window))
               (ert-info ((format "Prefix %S; before %d: %S; after %d: %S"
                                  prefix before-point before (point)
                                  (buffer-substring-no-properties (point-min) (point-max))))
                         (should (looking-at-p "target"))))
             (should mark-active)
             (should-not deactivate-mark)
             (should (equal "target" (buffer-substring-no-properties (region-beginning) (region-end))))
             (should (= (window-point other) (point))))
         (when (window-live-p other) (delete-window other)))))))

(niwa-deftest niwa-view--call-preserving-window-state/failure ()
  ,test
  (test)
  :doc "restores a transcript selection when its writer fails after redraw"
  (niwa-view-test--with-buffers
   (niwa-view-test--insert-data data-buf "Selected target text.\n" 'response)
   (with-current-buffer view-buf
     (niwa-view--full-rerender)
     (goto-char (point-min))
     (search-forward "target")
     (set-mark (- (point) 6))
     (setq mark-active t)
     (let ((before (point)))
       (should-error
        (niwa-view--call-preserving-window-state
         (lambda ()
           (goto-char (point-min))
           (set-mark (point))
           (setq mark-active nil)
           (error "Injected writer failure"))))
       (should (= before (point)))
       (should mark-active)
       (should (equal "target" (buffer-substring-no-properties (region-beginning) (region-end))))))))

(niwa-deftest niwa-view--full-rerender/cross-view-isolation ()
  ,test
  (test)
  :doc "nested child rendering cannot substitute its response into the parent view"
  (unwind-protect
      (niwa-view-test--with-buffers
       (niwa-view-test--insert-data data-buf "PARENT response stays in parent.\n" 'response)
       (let ((parent-view view-buf)
             (parent-data data-buf))
         (niwa-view-test--with-buffers
          (niwa-view-test--insert-data data-buf "CHILD response stays in child.\n" 'response)
          (let ((child-view view-buf)
                (ensure (symbol-function 'font-lock-ensure))
                entered)
            (cl-letf (((symbol-function 'niwa-view--markdown-fontify-mode)
                       (lambda () 'fundamental-mode))
                      ((symbol-function 'font-lock-ensure)
                       (lambda (&rest args)
                         (unless entered
                           (setq entered t)
                           (with-current-buffer child-view
                             (niwa-view--full-rerender)))
                         (apply ensure args))))
              (with-current-buffer parent-view
                (niwa-view--full-rerender)))
            (should entered)
            (with-current-buffer parent-data
              (should (string-search "PARENT response" (buffer-string)))
              (should-not (string-search "CHILD response" (buffer-string))))
            (with-current-buffer child-view
              (should (string-search "CHILD response" (buffer-string))))
            (with-current-buffer parent-view
              (should (string-search "PARENT response" (buffer-string)))
              (should-not (string-search "CHILD response" (buffer-string))))))))
    (niwa-view--release-markdown-fontify-buffer)))

(provide 'test-niwa-view-render-stability)
;;; test-niwa-view-render-stability.el ends here
