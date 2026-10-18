;;; test-mevedel-view-geometry.el --- Native viewport geometry tests -*- lexical-binding: t -*-

;;; Commentary:

;; These tests require an interactive display: batch Emacs's initial frame
;; deliberately returns nil from `pos-visible-in-window-p'.  The shared view
;; fixture models unit-height geometry for batch policy tests, not these tests.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view-composer)
(require 'mevedel-view-zone)

(defmacro mevedel-view-geometry-test--with-window (&rest body)
  "Run BODY in an isolated, redisplayed text window."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (not noninteractive))
     (save-window-excursion
       (delete-other-windows)
       (with-temp-buffer
         (switch-to-buffer (current-buffer))
         (setq-local truncate-lines nil)
         (let ((scroll-margin 0)
               (scroll-conservatively 10000)
               (resize-mini-windows nil)
               (auto-window-vscroll nil))
           (redisplay t)
           (let ((body-height (window-body-height nil t))
                 (body-width (window-body-width nil t)))
             (unwind-protect
                 (progn ,@body)
               ;; A desktop resize invalidates the geometry run, not the assertion.
               (should (= body-height (window-body-height nil t)))
               (should (= body-width (window-body-width nil t))))))))))

(mevedel-deftest mevedel-view--viewport-at-y
  (:doc "Zero Y, exact placement, and unchanged viewports work on native displays.")
  (mevedel-view-geometry-test--with-window
    (dotimes (_ 80) (insert "history\n"))
    (setq-local mevedel-view--input-marker (copy-marker (point)))
    (insert "draft")
    (let* ((window (selected-window))
           (input (mevedel-view--input-start))
           (viewport (mevedel-view--viewport-at-y window input 0)))
      (mevedel-view--set-viewport window viewport)
      (redisplay t)
      (should (= 0 (mevedel-view--position-screen-y input window)))
      (should (equal (list (cons window 0)) (mevedel-view--composer-anchors)))
      (cl-letf (((symbol-function 'set-window-start)
                 (lambda (&rest _) (ert-fail "Redundant start setter")))
                ((symbol-function 'set-window-vscroll)
                 (lambda (&rest _) (ert-fail "Redundant vscroll setter"))))
        (mevedel-view--set-viewport window viewport))
      (goto-char (point-min))
      (set-window-start window (point-min) t)
      (redisplay t)
      (should-not (mevedel-view--composer-anchors))
      (should-not (mevedel-view--window-shows-end-p window)))))

(mevedel-deftest mevedel-view-geometry-image ()
  (let ((chrome ,chrome))
    (mevedel-view-geometry-test--with-window
      (skip-unless (and (display-graphic-p) (image-type-available-p 'svg)))
      (when chrome
        (setq-local header-line-format "Header" tab-line-format "Tab")
        (redisplay t)
        (setq body-height (window-body-height nil t)))
      (dotimes (_ 50) (insert "history\n"))
      (let ((image-start (point)))
        (insert (propertize "I" 'display
                            (create-image
                             "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='200'><rect width='40' height='200' fill='red'/></svg>"
                             'svg t)) "\n")
        (let ((deletion-start (point)))
          (dotimes (_ 5) (insert "text\n"))
          (setq-local mevedel-view--input-marker (copy-marker (point)))
          (insert "draft")
          (set-window-start nil image-start t)
          (set-window-vscroll nil 80 t)
          (redisplay t)
          (let* ((window (selected-window))
                 (before (mevedel-view--position-screen-y (point-max) window))
                 (scroll (window-vscroll window t)))
            (should (= 80 scroll))
            (should before)
            (mevedel-view--call-preserving-window-state
             (lambda () (delete-region deletion-start (+ deletion-start 5))))
            (redisplay t)
            (should (= before (mevedel-view--position-screen-y (point-max) window)))
            (should (= image-start (window-start window)))
            (should (< (window-vscroll window t) scroll))
            (should (> (window-vscroll window t) 0))
            ;; The top image is clipped; a fully-visible query rejects it.
            (should (mevedel-view--position-screen-y image-start window))
            (should-not (mevedel-view--position-screen-y image-start window t)))))))
  (chrome)
  :doc "A partially shown image retains Y with the same start and different vscroll." nil
  :doc "Header and tab heights do not contaminate the image anchor's body Y." t)

(mevedel-deftest mevedel-view-geometry-image-height
  (:doc "An image-height change preserves native Y without a character-count change.")
  (mevedel-view-geometry-test--with-window
    (skip-unless (and (display-graphic-p) (image-type-available-p 'svg)))
    (dotimes (_ 50) (insert "history\n"))
    (let* ((image-start (point))
           (image (lambda (height)
                    (create-image
                     (format "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='%d'><rect width='40' height='%d' fill='red'/></svg>"
                             height height)
                     'svg t))))
      (insert (propertize "I" 'display (funcall image 200)) "\n")
      (dotimes (_ 5) (insert "text\n"))
      (setq-local mevedel-view--input-marker (copy-marker (point)))
      (insert "draft")
      (set-window-start nil image-start t)
      (set-window-vscroll nil 80 t)
      (redisplay t)
      (let* ((window (selected-window))
             (before (mevedel-view--position-screen-y (point-max) window)))
        (should before)
        (mevedel-view--call-preserving-window-state
         (lambda ()
           (put-text-property image-start (1+ image-start)
                              'display (funcall image 170))))
        (redisplay t)
        (should (= before (mevedel-view--position-screen-y (point-max) window)))
        (should (= image-start (window-start window)))
        (should (= 50 (window-vscroll window t)))))))

(mevedel-deftest mevedel-view-geometry-draft
  (:doc "Shrinking a wrapped draft retains EOF Y and deliberate blank space.")
  (mevedel-view-geometry-test--with-window
    (dotimes (_ 100) (insert "history\n"))
    (setq-local mevedel-view--input-marker (copy-marker (point)))
    (insert (make-string (* 3 (window-body-width)) ?x))
    (let* ((window (selected-window))
           (y (/ (window-body-height window t) 2)))
      (mevedel-view--set-viewport
       window (mevedel-view--viewport-at-y window (point-max) y))
      (redisplay t)
      (let ((before (mevedel-view--position-screen-y (point-max) window)))
        (should before)
        (mevedel-view--capture-draft-window-state
         (mevedel-view--input-start) (point-max))
        (delete-region (mevedel-view--input-start) (point-max))
        (mevedel-view--restore-draft-window-state)
        (redisplay t)
        (should (= before (mevedel-view--position-screen-y (point-max) window)))))))

(mevedel-deftest mevedel-view-geometry-draft-final-image ()
  (let ((blank ,blank))
    (mevedel-view-geometry-test--with-window
      (skip-unless (and (display-graphic-p) (image-type-available-p 'svg)))
      (dotimes (_ 100) (insert "history\n"))
      (setq-local mevedel-view--input-marker (copy-marker (point)))
      (insert (propertize "I" 'display
                          (create-image
                           "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='180'><rect width='40' height='180' fill='red'/></svg>"
                           'svg t))
              " final draft line")
      (let* ((window (selected-window))
             (height (cdr (window-text-pixel-size window (point-max) (point-max))))
             (y (- (window-body-height window t) height blank)))
        (should (> y 0))
        (mevedel-view--set-viewport
         window (mevedel-view--viewport-at-y window (point-max) y))
        (redisplay t)
        (let ((before-blank (mevedel-view--window-blank-pixels window))
              (before-y (mevedel-view--position-screen-y (point-max) window t)))
          (should before-blank)
          (should (= blank before-blank))
          ;; Observe the actual cursor row independently of production's
          ;; fresh iterator calculation.  Point is at EOF after redisplay.
          (let ((row (window-line-height nil window)))
            (should row)
            (should (= blank (- (window-body-height window t)
                                (nth 2 row) (car row)))))
          ;; Removing the image changes the final line's height, not its
          ;; screen-row count.  Keeping EOF Y alone would add blank pixels.
          (mevedel-view--call-preserving-draft-window-state
           (lambda ()
             (delete-region (mevedel-view--input-start)
                            (1+ (mevedel-view--input-start)))))
          (redisplay t)
          (should (equal " final draft line"
                         (buffer-substring-no-properties
                          (mevedel-view--input-start) (point-max))))
          (should (= before-blank (mevedel-view--window-blank-pixels window)))
          (let ((row (window-line-height nil window)))
            (should row)
            (should (= blank (- (window-body-height window t)
                                (nth 2 row) (car row)))))
          (should (> (mevedel-view--position-screen-y (point-max) window t)
                     before-y))))))
  (blank)
  :doc "Shrinking the final image line keeps a full draft window full." 0
  :doc "Shrinking the final image line retains deliberate blank pixels." 60)

(mevedel-deftest mevedel-view-geometry-end-row
  (:doc "Fresh EOF rows match native cursor rows before and after mutation.")
  (mevedel-view-geometry-test--with-window
    (dotimes (_ 100) (insert "history\n"))
    (insert "draft")
    (let ((window (selected-window)))
      (mevedel-view--set-viewport
       window (mevedel-view--viewport-at-y window (point-max) 300))
      (set-window-vscroll window 7 t)
      (redisplay t)
      (let* ((native (window-line-height nil window))
             (expected (cons (nth 2 native) (car native))))
        (should (equal expected (mevedel-view--window-end-row window)))
        (delete-region (- (point-max) 5) (point-max))
        ;; Query before redisplay; the final line is now empty.
        (should (equal expected (mevedel-view--window-end-row window)))
        (redisplay t)
        (let ((row (window-line-height nil window)))
          (should (equal expected (cons (nth 2 row) (car row))))))
      (goto-char (point-min))
      (set-window-start window (point-min) t)
      (redisplay t)
      (should-not (mevedel-view--window-end-row window)))))

(provide 'test-mevedel-view-geometry)
;;; test-mevedel-view-geometry.el ends here
