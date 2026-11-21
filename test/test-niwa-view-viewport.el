;;; test-niwa-view-viewport.el --- Viewport policy interface -*- lexical-binding: t -*-

;;; Commentary:
;; Callers resolve starts; viewport decides which window can still use them.

;;; Code:
(require 'ert)
(require 'niwa-view-viewport)

(ert-deftest niwa-view-viewport/lost-dead-and-switched ()
  (save-window-excursion
    (delete-other-windows)
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (dotimes (_ 80) (insert "row\n"))
      (let* ((reader (selected-window))
             (switched (split-window-below))
             (dead (split-window-right))
             (fallback (- (point-max) 4))
             (last-page (- fallback 16)))
        (set-window-start reader (point-max) t)
        (setq-local niwa-view--status-marker (point-marker))
        (goto-char fallback)
        (let ((snapshot (niwa-view-viewport-capture nil)))
          (delete-window dead)
          (with-temp-buffer
            (insert "other buffer\nother row\n")
            (set-window-buffer switched (current-buffer))
            (set-window-start switched 14 t)
            (with-current-buffer (window-buffer reader)
              (cl-letf (((symbol-function 'niwa-view--last-page)
                         (lambda (window)
                           (should (eq reader window))
                           (should (= fallback (window-start window)))
                           (cons last-page 0))))
                (niwa-view-viewport-apply
                 snapshot `((,dead 1 :lost-p t) (,switched 1 :lost-p nil)
                            (,reader ,fallback :lost-p t)))))
            (should (= last-page (window-start reader)))
            (should (= 14 (window-start switched)))))))))

(provide 'test-niwa-view-viewport)
;;; test-niwa-view-viewport.el ends here
