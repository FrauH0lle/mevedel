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

(mevedel-deftest mevedel-view-transcript-follow-burst ()
  (let ((burst ,burst)
        (draft ,draft))
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (delete-other-windows)
        (mevedel-view-test--insert-composer-draft draft 3)
        (set-window-start nil (point-min) t)
        (goto-char (point-max))
        (let* ((window (selected-window))
               (composer (mevedel-view-composer-display window))
               (height (window-body-height window))
               (rows (if burst (* 3 height) 1))
               (composer-pos (+ (mevedel-view-composer-draft-start) 3)))
          (set-window-point composer composer-pos)
          (select-window composer)
          (redisplay t)
          (with-current-buffer view-buf
            (let ((before-y (mevedel-view-test--composer-screen-y window))
                  (before-end-y (mevedel-view--position-screen-y (point-max) window))
                  (composer-edges (window-pixel-edges composer))
                  (composer-start (window-start composer)))
              (should before-y)
              (should before-end-y)
              (should (< before-end-y (- (cdr (mevedel-view--window-body-geometry window))
                                         (* 2 (frame-char-height)))))
              (let ((native (symbol-function 'pos-visible-in-window-p)))
                (cl-letf (((symbol-function 'pos-visible-in-window-p)
                           (if noninteractive
                               (lambda (pos win &optional partial)
                                 (if (eq win window)
                                     (mevedel-view--position-screen-y pos win (not partial))
                                   (funcall native pos win partial)))
                             native)))
                  (should (pos-visible-in-window-p (point-max) window))
                  (mevedel-view-call-preserving-user-view-state
                   (lambda ()
                     (mevedel-view--call-with-render-boundaries-advancing
                      (lambda ()
                        (let ((inhibit-read-only t)
                              (inhibit-modification-hooks t))
                          (goto-char (mevedel-view--history-insertion-marker))
                          (dotimes (n rows)
                            (insert (format "incoming line %d\n" n)))))))
                   nil t)))
              (redisplay t)
              (should (eq (selected-window) composer))
              (should (equal composer-edges (window-pixel-edges composer)))
              (should (= composer-start (window-start composer)))
              (should (= before-y (mevedel-view-test--composer-screen-y window)))
              (should (equal draft (mevedel-view-composer-draft-text)))
              (should (= 3 (mevedel-view-test--composer-point-offset)))
              (should (= composer-pos (window-point composer)))
              (if burst
                  (progn
                    ;; Derive the last page by rows, independently of restoration.
                    (should (= (window-start window)
                               (save-excursion
                                 (goto-char (point-max))
                                 (vertical-motion (- (1- height)) window)
                                 (point))))
                    (should (= (* (1- height) (if noninteractive 1 (frame-char-height)))
                               (mevedel-view--position-screen-y (point-max) window))))
                (should (= (point-min) (window-start window)))
                (should (= (+ before-end-y (if noninteractive 1 (frame-char-height)))
                           (mevedel-view--position-screen-y (point-max) window))))))))))
  (burst draft)
  :doc "Oversized incoming growth fills the last page from a short view."
  t "draft"
  :doc "Oversized incoming growth fills the page without changing a multiline draft."
  t "> draft\nsecond line"
  :doc "Small incoming growth still consumes free space without scrolling."
  nil "> draft\nsecond line")

(provide 'test-mevedel-view-composer-burst)
;;; test-mevedel-view-composer-burst.el ends here
