;;; test-mevedel-view-composer-burst.el --- Incoming growth policy -*- lexical-binding: t -*-

;;; Commentary:
;; Incoming output consumes a short view's free space before scrolling.
;; Batch tests model visibility only; native probes verify pixel geometry.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-composer)

(mevedel-deftest mevedel-view--restore-composer-anchors-burst ()
  (let ((burst ,burst)
        (draft ,draft))
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (delete-other-windows)
        (mevedel-view-test--insert-composer-draft draft 3)
        (set-window-start nil (point-min) t)
        (let* ((window (selected-window))
               (height (window-body-height window))
               (before-y (mevedel-view-test--composer-screen-y window))
               (before-end-y (mevedel-view--position-screen-y (point-max) window))
               (rows (if burst (* 3 height) 1)))
          ;; Establish an actually non-full view with a visible draft.
          (should before-y)
          (should before-end-y)
          (should (< before-end-y (- height 2)))
          (mevedel-view--call-preserving-user-view-state
           (lambda ()
             (mevedel-view--call-with-render-boundaries-advancing
              (lambda ()
                (let ((inhibit-read-only t)
                      (inhibit-modification-hooks t))
                  (goto-char (mevedel-view--history-insertion-marker))
                  (dotimes (n rows)
                    (insert (format "incoming line %d\n" n))))))))
          (should (equal draft (mevedel-view--input-text)))
          (should (= 3 (- (point) (mevedel-view--input-start))))
          (should (= 3 (- (window-point window) (mevedel-view--input-start))))
          (if burst
              (progn
                ;; Derive the last page independently with unit-height rows,
                ;; not the production pixel viewport helper under test.
                (should (= (window-start window)
                           (save-excursion
                             (goto-char (point-max))
                             (vertical-motion (- (1- height)) window)
                             (point))))
                (should (= (1- height)
                           (mevedel-view--position-screen-y (point-max) window))))
            (should (= (point-min) (window-start window)))
            (should (= (1+ before-y)
                       (mevedel-view-test--composer-screen-y window))))))))
  (burst draft)
  :doc "Oversized incoming growth fills the last page from a short view."
  t "draft"
  :doc "Oversized incoming growth fills the page without changing a multiline draft."
  t "> draft\nsecond line"
  :doc "Small incoming growth still consumes free space without scrolling."
  nil "> draft\nsecond line")

(provide 'test-mevedel-view-composer-burst)
;;; test-mevedel-view-composer-burst.el ends here
