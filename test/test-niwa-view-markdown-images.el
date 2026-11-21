;;; test-niwa-view-markdown-images.el --- Responsive image tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-markdown)

(niwa-deftest niwa-view--put-image-display ()
  ,test
  (test)
  :doc "default previews use half the previous 600-pixel width"
  (let ((file (make-temp-file "niwa-image-preview-" nil ".png"))
        (niwa-view-inline-image-max-width
         (eval (car (get 'niwa-view-inline-image-max-width 'standard-value)) t)))
    (unwind-protect
        (with-temp-buffer
          (insert file)
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (path &rest args)
                       (list 'image :file path
                             :max-width (plist-get args :max-width)))))
            (niwa-view--put-image-display (point-min) (point-max) file)
            (let ((spec (cdr (get-text-property (point-min) 'display))))
              (should (equal file (plist-get spec :file)))
              (should (eql 300 (plist-get spec :max-width))))))
      (delete-file file)))

  :doc "previews open the original file with RET or the mouse button binding"
  (let ((file (make-temp-file "niwa-image-open-" nil ".png"))
        ;; Batch Emacs cannot activate image-mode; still visit the real file.
        (auto-mode-alist (cons '("\\.png\\'" . fundamental-mode) auto-mode-alist)))
    (unwind-protect
        (progn
          (with-temp-file file
            (set-buffer-multibyte nil)
            (insert (base64-decode-string
                     (concat "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwC"
                             "AAAAC0lEQVR42mP8/x8AAwMCAO+aD1sAAAAASUVORK5CYII="))))
          (dolist (size '(300 0.5))
            (dolist (markdown '(nil t))
              (niwa-test--with-displayed-buffer
                (let ((niwa-view-inline-image-max-width size))
                  (insert (if markdown (format "![preview](%s)" file) file)
                          "\n")
                  (put-text-property (point-min) (point-max)
                                     'keymap niwa-view--display-map)
                  (put-text-property (point-min) (point-max)
                                     'font-lock-face
                                     '(:underline t :overline t
                                       :strike-through t :box t))
                  (cl-letf (((symbol-function 'display-images-p)
                             (lambda (&optional _display) t))
                            ((symbol-function 'create-image)
                             (lambda (path &rest _) (list 'image :file path))))
                    (niwa-view--decorate-markdown-in-range
                     (point-min) (point-max))
                    (when (floatp size)
                      (put-text-property (point-min) (1- (point-max))
                                         'niwa-view-image-width nil)
                      (niwa-view--rerender-images)))
                  (goto-char (point-min))
                  (let ((button (button-at (point))))
                    (should button)
                    (should (= (button-start button) (point-min)))
                    (should (= (button-end button) (1- (point-max))))
                    ;; The view fontifies with `font-lock-defaults' (nil t),
                    ;; whose unfontify pass strips the `face' property once
                    ;; jit-lock reaches the line.  Check what redisplay sees
                    ;; afterwards, including the button's category face.
                    ;; Batch Emacs declines `font-lock-mode', whose teardown
                    ;; drops the `font-lock-face' alias the view buffer has;
                    ;; install it after fontifying.
                    (setq-local font-lock-defaults '(nil t))
                    (font-lock-ensure)
                    (setq-local char-property-alias-alist
                                '((face font-lock-face)))
                    (let ((face (get-char-property (point) 'face)))
                      (dolist (attribute '(:underline :overline :strike-through :box))
                        (should (plist-member face attribute))
                        (should-not (plist-get face attribute))))
                    (should (button-get button 'follow-link))
                    (should (eq t (mouse-on-link-p (point))))
                    (should (eq (key-binding (kbd "RET")) 'push-button))
                    (should (eq (key-binding [mouse-2]) 'push-button))
                    (save-window-excursion
                      (let ((last-command-event 13))
                        (call-interactively (key-binding (kbd "RET"))))
                      (should (equal buffer-file-name file)))
                    (save-window-excursion
                      (let ((event (list 'mouse-2
                                         (list (selected-window) (point)
                                               '(0 . 0) 0))))
                        (funcall (key-binding [mouse-2]) event))
                      (should (equal (buffer-local-value
                                      'buffer-file-name
                                      (window-buffer (selected-window)))
                                     file)))))))))
      (when-let* ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (delete-file file))))

(niwa-deftest niwa-view--image-sizing ()
  ,test
  (test)
  :doc "a fixed pixel setting passes through unmeasured"
  (let ((niwa-view-inline-image-max-width 600))
    (should (equal '(600 . nil)
                   (niwa-view--image-sizing (selected-window)))))

  :doc "a ratio setting resolves against the window's pixel width"
  (let ((niwa-view-inline-image-max-width 0.5))
    (let ((width (window-body-width (selected-window) t)))
      (should (equal (cons (max 1 (floor (* 0.5 width))) width)
                     (niwa-view--image-sizing (selected-window))))))

  :doc "a ratio without a live window falls back to the frame width"
  (let ((niwa-view-inline-image-max-width 0.5))
    (let ((sizing (niwa-view--image-sizing nil)))
      (should (integerp (car sizing)))
      (should-not (cdr sizing))))

  :doc "values outside the documented integer and ratio ranges fail"
  (dolist (setting '(0 -1 0.0 -0.5 1.5 nil))
    (let ((niwa-view-inline-image-max-width setting))
      (should-error (niwa-view--image-sizing (selected-window))))))

(niwa-deftest niwa-view--rerender-images ()
  ,test
  (test)
  :doc "recreates only stale ratio-sized images at the window width"
  (let ((file (make-temp-file "niwa-image-ratio-" nil ".png")))
    (unwind-protect
        (niwa-test--with-displayed-buffer
          (let ((niwa-view-inline-image-max-width 0.5)
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
              (niwa-view--decorate-local-images-in-range
               (point-min) (point-max))
              (goto-char (point-min))
              (should (equal file (get-text-property
                                   (point) 'niwa-view-image-source)))
              (should (eql 0.5 (get-text-property
                                (point) 'niwa-view-image-ratio)))
              (should (eql width (get-text-property
                                  (point) 'niwa-view-image-width)))
              (should (eql (max 1 (floor (* 0.5 width)))
                           (plist-get (cdr (get-text-property
                                            (point) 'display))
                                      :max-width)))
              (should (= 1 created))
              ;; Stale stored width forces one recreation.
              (put-text-property (point) (1+ (point))
                                 'niwa-view-image-width nil)
              ;; The image keeps the ratio it was rendered with even if the
              ;; buffer's default later becomes fixed-width.
              (setq niwa-view-inline-image-max-width 600)
              (niwa-view--rerender-images)
              (should (eql width (get-text-property
                                  (point) 'niwa-view-image-width)))
              (should (eql (max 1 (floor (* 0.5 width)))
                           (plist-get (cdr (get-text-property
                                            (point) 'display))
                                      :max-width)))
              (should (= 2 created))
              ;; A fresh image is left alone.
              (niwa-view--rerender-images)
              (should (= 2 created)))))
      (delete-file file)))

  :doc "repeats the whole-buffer scan only for a new width or a new image"
  (let ((file (make-temp-file "niwa-image-scan-" nil ".png")))
    (unwind-protect
        (niwa-test--with-displayed-buffer
          (let ((niwa-view-inline-image-max-width 0.5)
                (searches 0))
            (insert (format "![shot](%s)\n" file))
            (cl-letf* (((symbol-function 'display-images-p)
                        (lambda (&optional _display) t))
                       ((symbol-function 'create-image)
                        (lambda (path &rest args)
                          (list 'image :file path
                                :max-width (plist-get args :max-width))))
                       (search (symbol-function 'text-property-search-forward))
                       ((symbol-function 'text-property-search-forward)
                        (lambda (&rest args)
                          (cl-incf searches)
                          (apply search args))))
              (niwa-view--decorate-local-images-in-range
               (point-min) (point-max))
              (niwa-view--rerender-images)
              (should (> searches 0))
              (setq searches 0)
              (niwa-view--rerender-images)
              (should (= 0 searches))
              (goto-char (point-max))
              (insert (format "![again](%s)\n" file))
              (niwa-view--decorate-local-images-in-range
               (line-beginning-position 0) (point-max))
              (niwa-view--rerender-images)
              (should (> searches 0)))))
      (delete-file file)))

  :doc "fixed pixel sizing never realigns"
  (let ((file (make-temp-file "niwa-image-fixed-" nil ".png")))
    (unwind-protect
        (niwa-test--with-displayed-buffer
          (let ((niwa-view-inline-image-max-width 600))
            (insert (format "![shot](%s)\n" file))
            (cl-letf (((symbol-function 'display-images-p)
                       (lambda (&optional _display) t))
                      ((symbol-function 'create-image)
                       (lambda (path &rest args)
                         (list 'image :file path
                               :max-width (plist-get args :max-width)))))
              (niwa-view--decorate-local-images-in-range
               (point-min) (point-max))
              (goto-char (point-min))
              (should-not (get-text-property
                           (point) 'niwa-view-image-source))
              (should-not (get-text-property
                           (point) 'niwa-view-image-ratio))
              (should (eql 600 (plist-get (cdr (get-text-property
                                                (point) 'display))
                                          :max-width)))
              (niwa-view--rerender-images))))
      (delete-file file)))

  :doc "an off-screen ratio image realigns when first displayed"
  (let ((file (make-temp-file "niwa-image-offscreen-" nil ".png"))
        (buffer (generate-new-buffer " *niwa-image-offscreen*"))
        (config (current-window-configuration)))
    (unwind-protect
        (cl-letf (((symbol-function 'display-images-p)
                   (lambda (&optional _display) t))
                  ((symbol-function 'create-image)
                   (lambda (path &rest args)
                     (list 'image :file path
                           :max-width (plist-get args :max-width)))))
          (with-current-buffer buffer
            (let ((niwa-view-inline-image-max-width 0.5))
              (insert (format "![shot](%s)\n" file))
              (niwa-view--decorate-local-images-in-range
               (point-min) (point-max))
              (should (equal file (get-text-property
                                   (point-min) 'niwa-view-image-source)))
              (should (eql 0.5 (get-text-property
                                (point-min) 'niwa-view-image-ratio)))
              (should-not (get-text-property
                           (point-min) 'niwa-view-image-width))))
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (let ((niwa-view-inline-image-max-width 0.5))
              (niwa-view--rerender-images (selected-window))
              (should (eql (window-body-width (selected-window) t)
                           (get-text-property
                            (point-min) 'niwa-view-image-width))))))
      (set-window-configuration config)
      (kill-buffer buffer)
      (delete-file file))))

(provide 'test-niwa-view-markdown-images)

;;; test-niwa-view-markdown-images.el ends here
