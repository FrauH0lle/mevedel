;;; test-niwa-view-pair.el -- Docked pair repair decisions -*- lexical-binding: t -*-

;;; Commentary:

;; Test layout decisions without relying on redisplay.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(require 'niwa-view-pair)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-disclosure)
(require 'niwa-view-stream)

(defun niwa-view-pair-test--hidden-in (window position)
  "Return non-nil if WINDOW displays POSITION through a replacing overlay."
  (seq-some (lambda (overlay)
              (and (overlay-get overlay 'display)
                   (memq (overlay-get overlay 'window) (list nil window))))
            (overlays-at position)))

(niwa-deftest niwa-view-pair-plan ()
  ,test
  (test)

  :doc "Keep a restored pair, dock another view, and remove foreign/orphan docks."
  (should
   (equal (niwa-view-pair-plan '((left draft dock) (right draft foreign))
                                 '((dock . draft) (foreign . other) (stray . draft)))
          '(:keep ((left . dock)) :add (right) :remove (foreign stray))))

  :doc "A complete layout leaves no additions or removals."
  (should
   (equal (niwa-view-pair-plan '((left draft one) (right draft two))
                                 '((one . draft) (two . draft)))
          '(:keep ((left . one) (right . two)) :add nil :remove nil))))

(niwa-deftest niwa-view-pair-refresh-turn-rule/latest-tool-row ()
  (niwa-view-test--with-buffers
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer view-buf)
      (should (niwa-view-composer-display (selected-window)))
      (let ((window (selected-window)))
        ,begin
        (niwa-view-test--insert-data
         data-buf "(:name \"Read\" :args (:file_path \"last.el\"))\n\nTool detail\n"
         '(tool . "last-tool"))
        ,render
        (niwa-view-pair-refresh-turn-rule)
        ;; The closing rule after the row is hidden in the paired window.
        (should (niwa-view-pair-test--hidden-in
                 window (1- (niwa-view-transcript-tail))))
        (dolist (expanded '(t nil t))
          (goto-char (point-min))
          (search-forward "Read: last.el")
          (let ((row (line-beginning-position)))
            (goto-char row)
            (call-interactively #'niwa-view-toggle-section)
            (should (eq expanded (and (save-excursion
                                        (search-forward "Tool detail" nil t))
                                      t)))
            ;; A toggle reinserts the row just above the hidden rule.  Every
            ;; character of the row's text must still display in the window.
            (goto-char row)
            (should (looking-at-p ".*Read: last\\.el"))
            (dotimes (offset (- (line-end-position) row))
              (should-not (niwa-view-pair-test--hidden-in
                           window (+ row offset))))
            ;; The hidden span is exactly where a fresh refresh places it,
            ;; so neither the row nor its terminating newline leaks out.
            (let ((span (mapcar (lambda (overlay)
                                  (cons (overlay-start overlay)
                                        (overlay-end overlay)))
                                niwa-view-pair--hidden-rules)))
              (should span)
              (niwa-view-pair-refresh-turn-rule)
              (should (equal span
                             (mapcar (lambda (overlay)
                                       (cons (overlay-start overlay)
                                             (overlay-end overlay)))
                                     niwa-view-pair--hidden-rules)))))))))
  (begin render)
  :doc "TAB keeps the latest tool row displayed after the turn ended"
  nil
  (niwa-view--full-rerender)
  :doc "TAB keeps the latest tool row displayed while the turn streams"
  (niwa-view-stream-begin-turn
   (niwa-view--history-insertion-marker)
   (with-current-buffer data-buf (point-max)))
  (niwa-view--render-stream-update data-buf))

(provide 'test-niwa-view-pair)
;;; test-niwa-view-pair.el ends here
