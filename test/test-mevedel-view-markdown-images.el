;;; test-mevedel-view-markdown-images.el --- Responsive image tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view-markdown)

(mevedel-deftest mevedel-view--image-sizing ()
  ,test
  (test)
  :doc "a fixed pixel setting passes through unmeasured"
  (let ((mevedel-view-inline-image-max-width 600))
    (should (equal '(600 . nil)
                   (mevedel-view--image-sizing (selected-window)))))

  :doc "a ratio setting resolves against the window's pixel width"
  (let ((mevedel-view-inline-image-max-width 0.5))
    (let ((width (window-body-width (selected-window) t)))
      (should (equal (cons (max 1 (floor (* 0.5 width))) width)
                     (mevedel-view--image-sizing (selected-window))))))

  :doc "a ratio without a live window falls back to the frame width"
  (let ((mevedel-view-inline-image-max-width 0.5))
    (let ((sizing (mevedel-view--image-sizing nil)))
      (should (integerp (car sizing)))
      (should-not (cdr sizing))))

  :doc "values outside the documented integer and ratio ranges fail"
  (dolist (setting '(0 -1 0.0 -0.5 1.5 nil))
    (let ((mevedel-view-inline-image-max-width setting))
      (should-error (mevedel-view--image-sizing (selected-window))))))

(mevedel-deftest mevedel-view--rerender-images ()
  ,test
  (test)
  :doc "recreates only stale ratio-sized images at the window width"
  (let ((file (make-temp-file "mevedel-image-ratio-" nil ".png")))
    (unwind-protect
        (mevedel-test--with-displayed-buffer
          (let ((mevedel-view-inline-image-max-width 0.5)
                (width (window-body-width (selected-window) t))
                (created 0))
            (insert (format "![shot](%s)\n" file))
            (cl-letf (((symbol-function 'display-images-p)
                       (lambda (&optional _display) t))
                      ((symbol-function 'create-image)
                       (lambda (path &rest args)
                         (setq created (1+ created))
                         (list 'image :file path
                               :max-width (plist-get args :max-width)))))
              (mevedel-view--decorate-local-images-in-range
               (point-min) (point-max))
              (goto-char (point-min))
              (should (equal file (get-text-property
                                   (point) 'mevedel-view-image-source)))
              (should (eql 0.5 (get-text-property
                                (point) 'mevedel-view-image-ratio)))
              (should (eql width (get-text-property
                                  (point) 'mevedel-view-image-width)))
              (should (eql (max 1 (floor (* 0.5 width)))
                           (plist-get (cdr (get-text-property
                                            (point) 'display))
                                      :max-width)))
              (should (= 1 created))
              ;; Stale stored width forces one recreation.
              (put-text-property (point) (1+ (point))
                                 'mevedel-view-image-width nil)
              ;; The image keeps the ratio it was rendered with even if the
              ;; buffer's default later becomes fixed-width.
              (setq mevedel-view-inline-image-max-width 600)
              (mevedel-view--rerender-images)
              (should (eql width (get-text-property
                                  (point) 'mevedel-view-image-width)))
              (should (eql (max 1 (floor (* 0.5 width)))
                           (plist-get (cdr (get-text-property
                                            (point) 'display))
                                      :max-width)))
              (should (= 2 created))
              ;; A fresh image is left alone.
              (mevedel-view--rerender-images)
              (should (= 2 created)))))
      (delete-file file)))

  :doc "fixed pixel sizing never realigns"
  (let ((file (make-temp-file "mevedel-image-fixed-" nil ".png")))
    (unwind-protect
        (mevedel-test--with-displayed-buffer
          (let ((mevedel-view-inline-image-max-width 600))
            (insert (format "![shot](%s)\n" file))
            (cl-letf (((symbol-function 'display-images-p)
                       (lambda (&optional _display) t))
                      ((symbol-function 'create-image)
                       (lambda (path &rest args)
                         (list 'image :file path
                               :max-width (plist-get args :max-width)))))
              (mevedel-view--decorate-local-images-in-range
               (point-min) (point-max))
              (goto-char (point-min))
              (should-not (get-text-property
                           (point) 'mevedel-view-image-source))
              (should-not (get-text-property
                           (point) 'mevedel-view-image-ratio))
              (should (eql 600 (plist-get (cdr (get-text-property
                                                (point) 'display))
                                          :max-width)))
              (mevedel-view--rerender-images))))
      (delete-file file)))

  :doc "an off-screen ratio image realigns when first displayed"
  (let ((file (make-temp-file "mevedel-image-offscreen-" nil ".png"))
        (buffer (generate-new-buffer " *mevedel-image-offscreen*"))
        (config (current-window-configuration)))
    (unwind-protect
        (cl-letf (((symbol-function 'display-images-p)
                   (lambda (&optional _display) t))
                  ((symbol-function 'create-image)
                   (lambda (path &rest args)
                     (list 'image :file path
                           :max-width (plist-get args :max-width)))))
          (with-current-buffer buffer
            (let ((mevedel-view-inline-image-max-width 0.5))
              (insert (format "![shot](%s)\n" file))
              (mevedel-view--decorate-local-images-in-range
               (point-min) (point-max))
              (should (equal file (get-text-property
                                   (point-min) 'mevedel-view-image-source)))
              (should (eql 0.5 (get-text-property
                                (point-min) 'mevedel-view-image-ratio)))
              (should-not (get-text-property
                           (point-min) 'mevedel-view-image-width))))
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (let ((mevedel-view-inline-image-max-width 0.5))
              (mevedel-view--rerender-images (selected-window))
              (should (eql (window-body-width (selected-window) t)
                           (get-text-property
                            (point-min) 'mevedel-view-image-width))))))
      (set-window-configuration config)
      (kill-buffer buffer)
      (delete-file file))))

(provide 'test-mevedel-view-markdown-images)

;;; test-mevedel-view-markdown-images.el ends here
