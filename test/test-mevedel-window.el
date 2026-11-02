;;; test-mevedel-window.el --- Temporary window lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise native display actions, mode changes and return continuations.

;;; Code:

(require 'mevedel-window)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-window-display-buffer ()
  ,test
  (test)

  :doc "noselect preserves focus and redisplay retains the original return state"
  (save-window-excursion
    (let ((buffer (generate-new-buffer " *window-surface*"))
          (origin (selected-window))
          first second)
      (unwind-protect
          (progn
            (delete-other-windows)
            (set-window-configuration (current-window-configuration))
            (let ((before (window-state-get)))
              (mevedel-window-display-buffer buffer t (lambda () (setq first t)))
              (should (eq origin (selected-window)))
              ;; Surface rerendering may reset its major mode.  Window state
              ;; belongs to the buffer's presentation, not that mode.
              (with-current-buffer buffer (special-mode))
              (mevedel-window-display-buffer buffer nil (lambda () (setq second t)))
              (should (eq buffer (window-buffer (selected-window))))
              (mevedel-window-quit)
              (should first)
              (should-not second)
              (should (equal before (window-state-get)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(mevedel-deftest mevedel-window-quit ()
  ,test
  (test)

  :doc "same-window display rules restore the replaced buffer and selected window"
  (save-window-excursion
    (let* ((buffer (generate-new-buffer " *window-same*"))
           (origin (selected-window))
           (owner (window-buffer origin))
           (display-buffer-alist
            '(("window-same" (display-buffer-same-window)))))
      (unwind-protect
          (progn
            (mevedel-window-display-buffer buffer)
            (should (eq origin (selected-window)))
            (should (eq buffer (window-buffer origin)))
            (mevedel-window-quit)
            (should-not (buffer-live-p buffer))
            (should (eq origin (selected-window)))
            (should (eq owner (window-buffer origin))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(mevedel-deftest mevedel-window-restore ()
  ,test
  (test)

  :doc "menu restoration preserves intentional navigation in surviving windows"
  (save-window-excursion
    (with-temp-buffer
      (insert "first line\nsecond line\nthird line\n")
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (goto-char (point-min))
      (let ((configuration (current-window-configuration))
            (origin (selected-window)))
        (goto-char (point-max))
        (let ((position (point)))
          (split-window-right)
          (mevedel-window-restore configuration origin t)
          (should (one-window-p))
          (should (eq origin (selected-window)))
          (should (= position (window-point origin))))))))

(mevedel-deftest mevedel-window-quit-preserves-other-windows
  ()
  (save-window-excursion
    (let ((left-buffer (generate-new-buffer " *window-edit-left*"))
          (right-buffer (generate-new-buffer " *window-edit-right*"))
          (replacement (generate-new-buffer " *window-edit-replacement*"))
          (surface (generate-new-buffer " *window-edit-surface*"))
          (split-height-threshold 0)
          (split-width-threshold nil)
          (display-buffer-alist nil))
      (unwind-protect
          (progn
            (with-current-buffer right-buffer
              (dotimes (line 500) (insert (format "line %d\n" line))))
            (delete-other-windows)
            (switch-to-buffer left-buffer)
            (let ((origin (selected-window))
                  (right (split-window-right)))
              (set-window-buffer right right-buffer)
              (set-window-configuration (current-window-configuration))
              (let ((before (window-state-get)))
                (mevedel-window-display-buffer surface)
                (should (eq right-buffer (window-buffer right)))
                (pcase ',change
                  ('buffer (set-window-buffer right replacement))
                  ('resize (window-resize right -3 t))
                  ('navigation
                   (with-current-buffer right-buffer
                     (goto-char (point-min))
                     (forward-line 300)
                     (set-window-point right (point))
                     (forward-line -10)
                     (set-window-start right (point) t))))
                (let ((position (window-point right))
                      (start (window-start right))
                      (width (window-total-width right)))
                  (mevedel-window-quit surface)
                  (should (eq origin (selected-window)))
                  (should (eq left-buffer (window-buffer origin)))
                  (should (eq (if (eq ',change 'buffer)
                                  replacement right-buffer)
                              (window-buffer right)))
                  (should (= position (window-point right)))
                  (should (= start (window-start right)))
                  (should (= width (window-total-width right)))
                  (when (eq ',change 'none)
                    (should (equal before (window-state-get))))))))
        (dolist (buffer (list left-buffer right-buffer replacement surface))
          (when (buffer-live-p buffer) (kill-buffer buffer))))))
  (change)
  :doc "a buffer switched elsewhere while the surface is open survives quit"
  buffer
  :doc "point and scrolling in another window survive surface quit"
  navigation
  :doc "deliberate resizing of another window survives surface quit"
  resize
  :doc "an unchanged frame still restores its exact original layout"
  none)

(provide 'test-mevedel-window)
;;; test-mevedel-window.el ends here
