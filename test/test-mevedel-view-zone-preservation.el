;;; test-mevedel-view-zone-preservation.el --- Zone redraw invariants -*- lexical-binding: t; -*-

;;; Commentary:

;; Managed-zone redraws must preserve drafts, semantic positions, neighboring
;; zones and foreign overlays regardless of their text replacement strategy.

;;; Code:

(require 'mevedel-view)
(require 'mevedel-view-stream)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-view-zone-redraw-invariants ()
  ,test
  (test)
  :doc "property changes preserve semantic anchors, composer point and sanitized undo"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let* ((render (lambda (face)
                       (mevedel-view-zone-reconcile
                        'status mevedel-view--status-marker mevedel-view--status-marker
                        (list (list :namespace 'status :id 'row
                                    :body (propertize "status" 'face face
                                                      'mevedel-view-zone-anchor 'row))))))
             (region (funcall render 'bold))
             (anchored-position (+ (overlay-start region) 2))
             (anchor (mevedel-view-zone-position-anchor anchored-position))
             (position (point)))
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        (funcall render 'italic)
        (should (= anchored-position (mevedel-view-zone-anchor-position anchor)))
        (should (eq 'italic (get-text-property anchored-position 'face)))
        (should (= position (point)))
        (should-not buffer-undo-list)
        (should (equal "> draft\nsecond line" (mevedel-view--input-text))))))

  :doc "an elapsed tick preserves a leading-> multiline draft, undo, zone anchors and foreign overlays"
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (let ((now (seconds-to-time 1000.25))
            (float (symbol-function 'float-time))
            (mevedel-view-spinner-style 'static)
            (mevedel-view-tool-spinner-style 'static)
            (mevedel-view-spinner-power-policy 'full))
        (cl-letf (((symbol-function 'current-time) (lambda () now))
                  ((symbol-function 'float-time)
                   (lambda (&optional time) (funcall float (or time now)))))
          (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
          (mevedel-view-zone-reconcile
           'status mevedel-view--status-marker mevedel-view--status-marker
           '((:namespace status :id task :body "Task")))
          (mevedel-view-zone-reconcile
           'interaction mevedel-view--interaction-marker mevedel-view--input-marker
           '((:namespace interaction :id card :body "Question")))
          (mevedel-view--start-spinner "Working...")
          (setq mevedel-view--spinner-start-time (seconds-to-time 988.25))
          (mevedel-view--ensure-request-progress)
          (setq mevedel-view--spinner-last-second 1000)
          (let* ((region (mevedel-view-zone-region 'progress))
                 (line (make-overlay (overlay-end region) (point-max)))
                 (position (point))
                 (anchored-position (+ (overlay-start region) 2))
                 (anchor (mevedel-view-zone-position-anchor anchored-position))
                 (regions (mapcar (lambda (zone)
                                    (let ((other (mevedel-view-zone-region zone)))
                                      (list other (overlay-start other) (overlay-end other))))
                                  '(status interaction))))
            (buffer-enable-undo)
            (setq buffer-undo-list nil)
            (insert "x")
            (undo-boundary)
            (setq now (seconds-to-time 1001.25))
            (mevedel-view--spinner-tick)
            (should (= (1+ position) (point)))
            (should (equal "> drxaft\nsecond line" (mevedel-view--input-text)))
            (should (= anchored-position (mevedel-view-zone-anchor-position anchor)))
            (should (= (overlay-end region) (overlay-start line)))
            (should (save-excursion
                      (goto-char (overlay-start region))
                      (search-forward "13s" (overlay-end region) t)))
            (dolist (state regions)
              (should (= (nth 1 state) (overlay-start (car state))))
              (should (= (nth 2 state) (overlay-end (car state)))))
            (should (equal buffer-undo-list
                           (mevedel-view--composer-undo-list
                            buffer-undo-list (mevedel-view--input-start))))
            (undo-start)
            (undo-more 1)
            (should (equal "> draft\nsecond line" (mevedel-view--input-text))))
          (mevedel-view--stop-spinner))))))

(provide 'test-mevedel-view-zone-preservation)
;;; test-mevedel-view-zone-preservation.el ends here
