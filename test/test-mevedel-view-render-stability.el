;;; test-mevedel-view-render-stability.el --- Reader stability regressions -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise redraw boundaries and positions while the view changes above readers.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)
(require 'mevedel-view-disclosure)
(require 'mevedel-view-markdown)

(mevedel-deftest mevedel-view-render-live-update/reader-anchors ()
  ,test
  (test)
  :doc "full and retained redraws preserve group readers and composer windows"
  (dolist (retained '(nil t))
    (dolist (target '("Read 4 files" "f0.el" "content 0"))
      (save-window-excursion
        (mevedel-view-test--with-buffers
          (mevedel-view-test--insert-data data-buf "Inspecting files.\n" 'response)
          (dotimes (i 4)
            (mevedel-view-test--insert-data
             data-buf
             (format (concat "#+begin_tool\n"
                             "(:name \"Read\" :args (:file_path \"f%d.el\"))\n\n"
                             "content %d\n#+end_tool\n") i i)
             `(tool . ,(format "call_%d" i))))
          (with-current-buffer view-buf
            (switch-to-buffer view-buf)
            (delete-other-windows)
            (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 3)
            (mevedel-view-stream-begin-turn
             mevedel-view--status-marker
             (with-current-buffer data-buf (copy-marker (point-min))))
            (mevedel-view-render-live-update data-buf)
            (goto-char (point-min))
            (search-forward "Read 4 files")
            (mevedel-view-toggle-section)
            (when (equal target "content 0")
              (search-forward "f0.el")
              (mevedel-view-toggle-section))
            (when retained
              (dotimes (_ 2) (mevedel-view-render-live-update data-buf))
              (should (> (marker-position mevedel-view--live-data-tail-start)
                         (marker-position mevedel-view--data-turn-start))))
            (goto-char (point-min))
            (search-forward target)
            (goto-char (match-beginning 0))
            (set-mark (+ (point) 2))
            (setq mark-active t)
            (set-window-start nil (line-beginning-position) t)
            (let ((column (current-column))
                  (other (split-window-right))
                  (index 4))
              (set-window-buffer other view-buf)
              (set-window-point other (+ (mevedel-view--input-start) 3))
              (dolist (refresh '(full live live full live live))
                (mevedel-view-test--insert-data
                 data-buf
                 (format (concat "#+begin_tool\n"
                                 "(:name \"Read\" :args (:file_path \"f%d.el\"))\n\n"
                                 "next\n#+end_tool\n") index)
                 `(tool . ,(format "call_%d" index)))
                (cl-incf index)
                (if (eq refresh 'full)
                    (mevedel-view--full-rerender)
                  (mevedel-view-render-live-update data-buf))
                (should (looking-at-p
                         (if (equal target "Read 4 files")
                             "Read [0-9]+ files"
                           (regexp-quote target))))
                (should (= column (current-column)))
                (should (= (mark) (+ (point) 2)))
                (should mark-active)
                (should (= (window-point) (point)))
                (should (= (window-start) (line-beginning-position)))
                (should (= (window-point other)
                           (+ (mevedel-view--input-start) 3)))
                (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
                (should (string-match-p "Read: f0.el" (buffer-string)))))))))))

(mevedel-deftest mevedel-view-zone-reconcile/reader-stability ()
  ,test
  (test)
  :doc "status growth preserves prompt position"
  (mevedel-view-test--with-buffers
   (with-current-buffer view-buf
     (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
     (mevedel-view-zone-reconcile
      'status mevedel-view--status-marker mevedel-view--status-marker
      '((:namespace status :id agents :body "One agent")))
     (mevedel-view-zone-reconcile
      'interaction mevedel-view--interaction-marker mevedel-view--interaction-marker
      '((:namespace interaction :id permission :body "Permission choice: allow once")))
     (save-window-excursion
       (set-window-buffer (selected-window) view-buf)
       (let ((start (plist-get (mevedel-view-zone-fragment-bounds 'interaction 'permission) :start)))
         (goto-char (+ start 5))
         (set-mark (+ start 10))
         (setq mark-active t)
         (set-window-start (selected-window) start t))
       (dolist (body '("One agent\nTwo agents\nThree agents" "One agent" ""))
         (mevedel-view-zone-reconcile
          'status mevedel-view--status-marker mevedel-view--interaction-marker
          (and (not (equal body ""))
               (list (list :namespace 'status :id 'agents :body body))))
         (let ((start (plist-get (mevedel-view-zone-fragment-bounds 'interaction 'permission) :start)))
           (should (= (point) (+ start 5)))
           (should (= (mark) (+ start 10)))
           (should mark-active)
           (should (= (window-start) start))))
       (should (equal "> draft\nsecond line" (mevedel-view--input-text)))))))

(mevedel-deftest mevedel-view--full-rerender/selection-stability ()
  ,test
  (test)
  :doc "rerender preserves selected transcript text"
  (dolist (reverse '(nil t))
    (mevedel-view-test--with-buffers
     (mevedel-view-test--insert-data data-buf "First answer.\n" 'response)
     (mevedel-view-test--insert-data data-buf "\n\n*** Next prompt\n\n" nil)
     (mevedel-view-test--insert-data data-buf "Selected target text stays here.\n" 'response)
     (with-current-buffer view-buf
       (mevedel-view--full-rerender)
       (goto-char (point-min))
       (search-forward "target text")
       (set-mark (- (point) (length "target text")))
       (setq mark-active t)
       (when reverse (exchange-point-and-mark))
       (with-current-buffer data-buf
         (goto-char 3)
         (insert (make-string 100 ?x)))
       (mevedel-view--full-rerender)
       (should mark-active)
       (should (eq reverse (< (point) (mark))))
       (should (equal "target text" (buffer-substring-no-properties (region-beginning) (region-end))))))))

(mevedel-deftest mevedel-view-toggle-section/failure-stability ()
  ,test
  (test)
  :doc "failed disclosure retains visible header"
  (dolist (partial '(nil t))
    (mevedel-view-test--with-buffers
     (mevedel-view-test--insert-data data-buf "A stable response paragraph.\n" 'response)
     (with-current-buffer view-buf
       (mevedel-view--full-rerender)
       (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
       (goto-char (point-min))
       (search-forward "stable response")
       (mevedel-view-toggle-section)
       (let ((before (buffer-substring-no-properties (point-min) (point-max)))
             (input-start (mevedel-view--input-start)))
         (cl-letf (((symbol-function 'mevedel-view-render-insert-expanded-disclosure)
                    (lambda (&rest _)
                      (when partial (insert "Partial replacement\n"))
                      (error "Injected rendering failure"))))
           (should-error
            (mevedel-view-toggle-section)))
         (should (equal before (buffer-substring-no-properties (point-min) (point-max))))
         (should (= input-start (mevedel-view--input-start)))
         (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
         (should-not mevedel-view-render--owner)
         (should (get-text-property (point) 'mevedel-view-collapsed))
         (mevedel-view-toggle-section)
         (should (string-search "A stable response paragraph." (buffer-string)))
         (should-not (get-text-property (point) 'mevedel-view-collapsed)))))))

(mevedel-deftest mevedel-view-render-live-update/redisplay-stability ()
  ,test
  (test)
  :doc "no visible partial rebuild"
  (mevedel-view-test--with-buffers
   (mevedel-view-test--insert-data data-buf "Stable paragraph must remain visible.\n" 'response)
   (with-current-buffer view-buf
     (setq mevedel-view--data-turn-start (with-current-buffer data-buf (copy-marker 1)))
     (setq mevedel-view--in-flight-turn-start (copy-marker mevedel-view--status-marker))
     (mevedel-view--full-rerender)
     (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
     (mevedel-view-render-invalidate-live-tail)
     (let ((original (symbol-function 'mevedel-view--fontify-response))
           observed)
       (cl-letf (((symbol-function 'mevedel-view--fontify-response)
                  (lambda (&rest args)
                    (setq observed
                          (list :redisplay-inhibited inhibit-redisplay
                                :paragraph-present
                                (and (string-search "Stable paragraph" (buffer-string)) t)))
                    (apply original args))))
         (mevedel-view-render-live-update data-buf))
       (should observed)
       (should (or (plist-get observed :redisplay-inhibited)
                   (plist-get observed :paragraph-present)))
       (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
       (should (= (point) (+ 4 (mevedel-view--input-start))))))))

(mevedel-deftest mevedel-view--realign-markdown/cell-stability ()
  ,test
  (test)
  :doc "table resize keeps reader in cell"
  (dolist (prefix '("" "some words that wrap before the target cell content "))
    (mevedel-test--with-displayed-buffer
     (insert "| First | Second |\n|---|---|\n| " prefix "target | another cell |\n")
     (mevedel-view-table-decorate (point-min) (point-max) nil)
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
               (mevedel-view--realign-markdown (current-buffer) (selected-window))
               (ert-info ((format "Prefix %S; before %d: %S; after %d: %S"
                                  prefix before-point before (point)
                                  (buffer-substring-no-properties (point-min) (point-max))))
                         (should (looking-at-p "target"))))
             (should mark-active)
             (should-not deactivate-mark)
             (should (equal "target" (buffer-substring-no-properties (region-beginning) (region-end))))
             (should (= (window-point other) (point))))
         (when (window-live-p other) (delete-window other)))))))

(mevedel-deftest mevedel-view--call-preserving-window-state/failure ()
  ,test
  (test)
  :doc "restores a transcript selection when its writer fails after redraw"
  (mevedel-view-test--with-buffers
   (mevedel-view-test--insert-data data-buf "Selected target text.\n" 'response)
   (with-current-buffer view-buf
     (mevedel-view--full-rerender)
     (goto-char (point-min))
     (search-forward "target")
     (set-mark (- (point) 6))
     (setq mark-active t)
     (let ((before (point)))
       (should-error
        (mevedel-view--call-preserving-window-state
         (lambda ()
           (goto-char (point-min))
           (set-mark (point))
           (setq mark-active nil)
           (error "Injected writer failure"))))
       (should (= before (point)))
       (should mark-active)
       (should (equal "target" (buffer-substring-no-properties (region-beginning) (region-end))))))))

(mevedel-deftest mevedel-view--full-rerender/cross-view-isolation ()
  ,test
  (test)
  :doc "nested child rendering cannot substitute its response into the parent view"
  (unwind-protect
      (mevedel-view-test--with-buffers
       (mevedel-view-test--insert-data data-buf "PARENT response stays in parent.\n" 'response)
       (let ((parent-view view-buf)
             (parent-data data-buf))
         (mevedel-view-test--with-buffers
          (mevedel-view-test--insert-data data-buf "CHILD response stays in child.\n" 'response)
          (let ((child-view view-buf)
                (ensure (symbol-function 'font-lock-ensure))
                entered)
            (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
                       (lambda () 'fundamental-mode))
                      ((symbol-function 'font-lock-ensure)
                       (lambda (&rest args)
                         (unless entered
                           (setq entered t)
                           (with-current-buffer child-view
                             (mevedel-view--full-rerender)))
                         (apply ensure args))))
              (with-current-buffer parent-view
                (mevedel-view--full-rerender)))
            (should entered)
            (with-current-buffer parent-data
              (should (string-search "PARENT response" (buffer-string)))
              (should-not (string-search "CHILD response" (buffer-string))))
            (with-current-buffer child-view
              (should (string-search "CHILD response" (buffer-string))))
            (with-current-buffer parent-view
              (should (string-search "PARENT response" (buffer-string)))
              (should-not (string-search "CHILD response" (buffer-string))))))))
    (mevedel-view--release-markdown-fontify-buffer)))

(provide 'test-mevedel-view-render-stability)
;;; test-mevedel-view-render-stability.el ends here
