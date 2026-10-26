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

(provide 'test-mevedel-window)
;;; test-mevedel-window.el ends here
