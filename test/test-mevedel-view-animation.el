;;; test-mevedel-view-animation.el --- Animation frame tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view-animation)

(mevedel-deftest mevedel-view-animation-period
  (:doc "Natural frame cadence is independent of the caller's frame ceiling.")
  (progn
  (should (= (mevedel-view-animation-period 'shimmer) (/ 1.0 60)))
  (should (= (mevedel-view-animation-period 'breathe) (/ 1.0 60)))
  (should (= (mevedel-view-animation-period 'bounce) (/ 1.0 60)))
  (should (= (mevedel-view-animation-period 'braille) 0.12))
  (should (= (mevedel-view-animation-period 'ascii) 0.12))
  (should (= (mevedel-view-animation-period 'dots) 0.24))
  (should (= (mevedel-view-animation-period 'ellipsis) 0.48))
  (should-not (mevedel-view-animation-period 'static))))

(mevedel-deftest mevedel-view-animation-prefixes
  (:doc "All non-static tool prefixes are discoverable without matching all rows.")
  (progn
  (dolist (style '(braille ascii dots))
    (dotimes (step (pcase style ('braille 10) ('ascii 4) ('dots 4)))
      (should (member (mevedel-view-animation-frame
                       style "" (* step (mevedel-view-animation-period style))
                       'default)
                      mevedel-view-animation-prefixes))))
  (should-not (member "" mevedel-view-animation-prefixes))))

(mevedel-deftest mevedel-view-animation-frame
  (:doc "Glyph styles have stable geometry and time-derived phase.")
  (progn
  (dolist (style '(braille ascii dots ellipsis static))
    (let* ((first (mevedel-view-animation-frame style "Working" 0 'default))
           (later (mevedel-view-animation-frame style "Working" 1.2 'default)))
      (should (= (string-width first) (string-width later)))
      (should (string-match-p "Working" first))))
  (should (equal (mevedel-view-animation-frame 'static "Working" 42 'default)
                 "Working"))
  (should (equal (mevedel-view-animation-frame 'static "" 42 'default) ""))
  (should (equal (mevedel-view-animation-frame 'braille "" 0 'default)
                 "⠋ "))
  (should (equal (mevedel-view-animation-frame 'ascii "" 0 'default)
                 "- "))
  (should (equal (mevedel-view-animation-frame 'dots "" 0 'default)
                 "●··· "))
  (should (equal (mevedel-view-animation-frame 'braille "Work" 0 'default)
                 (mevedel-view-animation-frame 'braille "Work" 1.2 'default)))
  (should (equal (mevedel-view-animation-frame 'braille "Work" 0.12 'default)
                 "⠙ Work"))
  (should (equal (mevedel-view-animation-frame 'ellipsis "Work" 0.48 'default)
                 "Work.  "))))

(mevedel-deftest mevedel-view-animation--palette
  (:doc "A theme-derived palette contains exactly 64 shades.")
  (let ((palette (mevedel-view-animation--palette "#ffffff" "#000000")))
    (should (= (length palette) 64))
    (should-not (equal (aref palette 0) (aref palette 63)))))

(mevedel-deftest mevedel-view-animation--prefix-end
  (:doc "The bounded animated prefix never ends inside a combining cluster.")
  (progn
    (should (= (mevedel-view-animation--prefix-end
                (concat (make-string 47 ?a) "é" (make-string 30 ?z)))
               47))
    (should (= (mevedel-view-animation--prefix-end "é") 2))))

(mevedel-deftest mevedel-view-animation-invalidate
  (:doc "A theme or face change clears prepared samples and changes shades.")
  (let ((mevedel-view-animation--cache nil)
        (foreground "#ffffff"))
    (cl-letf (((symbol-function 'mevedel-view-animation--colors)
               (lambda (_face _frame) (cons foreground "#000000"))))
      (let* ((before (mevedel-view-animation-frame
                      'breathe "Working" 1 'default))
             (shade (get-text-property 0 'face before)))
        (should (mevedel-view-animation-color-ready-p
                 'breathe "Working" 'default nil))
        (setq foreground "#ff0000")
        (should (eq before (mevedel-view-animation-frame
                            'breathe "Working" 1 'default)))
        (mevedel-view-animation-invalidate)
        (should-not mevedel-view-animation--cache)
        (should-not (mevedel-view-animation-color-ready-p
                     'breathe "Working" 'default nil))
        (let ((after (mevedel-view-animation-frame
                      'breathe "Working" 1 'default)))
          (should-not (equal shade (get-text-property 0 'face after))))))))

(mevedel-deftest mevedel-view-animation-check-colors
  (:doc "The low-frequency check invalidates stale face and fallback banks.")
  (let ((mevedel-view-animation--cache nil)
        (foreground "#ffffff"))
    (cl-letf (((symbol-function 'mevedel-view-animation--colors)
               (lambda (_face _frame)
                 (when foreground (cons foreground "#000000")))))
      (mevedel-view-animation-frame 'shimmer "Working" 0 'default)
      (should mevedel-view-animation--cache)
      (mevedel-view-animation-check-colors)
      (should mevedel-view-animation--cache)
      (setq foreground "#ff0000")
      (mevedel-view-animation-check-colors)
      (should-not mevedel-view-animation--cache)
      (setq foreground nil)
      (mevedel-view-animation-frame 'shimmer "Working" 0 'default)
      (should mevedel-view-animation--cache)
      (setq foreground "#ffffff")
      (mevedel-view-animation-check-colors)
      (should-not mevedel-view-animation--cache))))

(mevedel-deftest mevedel-view-animation--color-frame
  (:doc "Prepared color samples are bounded, reusable, and time indexed.")
  (let ((mevedel-view-animation--cache nil))
    (cl-letf (((symbol-function 'mevedel-view-animation--colors)
               (lambda (_face _frame) '("#ffffff" . "#000000"))))
      (dolist (style '(shimmer breathe bounce))
        (let* ((first (mevedel-view-animation-frame style "Working…" 0 'default))
               (same (mevedel-view-animation-frame style "Working…" 3.6 'default))
               (next (mevedel-view-animation-frame style "Working…" 1.0 'default)))
          (should (eq first same))
          (should (= (string-width first) (string-width next)))
          (should (equal (substring-no-properties first) "Working…"))
          (should (get-text-property 0 'face first))
          (should-not (equal-including-properties first next))))
      (let* ((long (concat (make-string 48 ?a) "é" (make-string 500 ?x)))
             (sample (mevedel-view-animation-frame 'shimmer long 0 'default)))
        (should (equal (substring-no-properties sample) long))
        (should-not (get-text-property 49 'face sample)))
      (dotimes (i 20)
        (mevedel-view-animation-frame 'shimmer (format "Status %d" i)
                                      0 'default))
      (should (<= (length mevedel-view-animation--cache)
                  mevedel-view-animation--cache-limit)))))

(mevedel-deftest mevedel-view-animation--colors
  (:doc "Unavailable display colors select a readable fixed-width fallback.")
  (progn
  (let ((mevedel-view-animation--cache nil)
        (resolutions 0))
    (cl-letf (((symbol-function 'mevedel-view-animation--colors)
               (lambda (_face _frame) (cl-incf resolutions) nil)))
      (dolist (style '(shimmer breathe bounce))
        (let ((a (mevedel-view-animation-frame style "é Working" 0 'default))
              (b (mevedel-view-animation-frame style "é Working" 1 'default)))
          (should (string-match-p "Working" a))
          (should (= (string-width a) (string-width b)))))
      (should (= resolutions 3))))
  (should-not (mevedel-view-animation--colors 'nonexistent-face nil))))

(mevedel-deftest mevedel-view-animation-low-color-terminal
  (:doc "Low-color terminals use glyphs even when faces resolve to valid colors.")
  (let ((mevedel-view-animation--cache nil)
        (cells 8))
    (cl-letf (((symbol-function 'display-color-p) (lambda (&optional _frame) t))
              ((symbol-function 'display-graphic-p) (lambda (&optional _frame) nil))
              ((symbol-function 'display-color-cells)
               (lambda (&optional _frame) cells))
              ((symbol-function 'face-foreground)
               (lambda (&rest _) "#ffffff"))
              ((symbol-function 'face-background)
               (lambda (&rest _) "#000000")))
      (dolist (style '(shimmer breathe bounce))
        (should-not (mevedel-view-animation--colors 'default nil))
        (let ((sample (mevedel-view-animation-frame
                       style "Working" 0 'default)))
          (should (string-match-p "Working" sample))
          (should-not (get-text-property 0 'face sample))
          (should-not (mevedel-view-animation-color-available-p
                       style "Working" 'default nil))))
      (setq cells 16)
      (should-not (mevedel-view-animation--colors 'default nil))
      (setq cells 256)
      (should (equal (mevedel-view-animation--colors 'default nil)
                     '("#ffffff" . "#000000"))))))

(mevedel-deftest mevedel-view-animation-missing-braille
  (:doc "Colorless and explicit Braille styles fall back to ASCII on unsupported displays.")
  (progn
    (let ((mevedel-view-animation--cache nil))
      (cl-letf (((symbol-function 'char-displayable-p)
                 (lambda (_character) nil))
                ((symbol-function 'mevedel-view-animation--colors)
                 (lambda (_face _frame) nil)))
        (should (equal (mevedel-view-animation-frame
                        'braille "Working" 0 'default)
                       "- Working"))
        (should (equal (mevedel-view-animation-frame
                        'shimmer "Working" 0 'default)
                       "- Working"))))
    (cl-letf (((symbol-function 'char-displayable-p)
               (lambda (_character) t)))
      (should (equal (mevedel-view-animation-frame
                      'braille "Working" 0 'default :multiple)
                     "- Working")))))

(mevedel-deftest mevedel-view-animation-missing-dots
  (:doc "Dots retain cadence and width when either glyph is unavailable.")
  (progn
    (dolist (missing '(?● ?·))
      (let ((mevedel-view-animation--dots-cache nil))
        (cl-letf (((symbol-function 'char-displayable-p)
                   (lambda (char) (not (eq char missing)))))
          (should (equal (mevedel-view-animation-frame
                          'dots "Working" 0 'default)
                         "*... Working"))
          (should (equal (mevedel-view-animation-frame
                          'dots "Working" 0.24 'default)
                         ".*.. Working"))
          (should (= (string-width (mevedel-view-animation-frame
                                    'dots "Working" 0 'default))
                     (string-width (mevedel-view-animation-frame
                                    'dots "Working" 0.24 'default)))))))
    (let ((mevedel-view-animation--dots-cache nil))
      (cl-letf (((symbol-function 'char-displayable-p)
                 (lambda (_char) t)))
        (should (equal (mevedel-view-animation-frame
                        'dots "Working" 0 'default)
                       "●··· Working"))
        (cl-letf (((symbol-function 'get-buffer-window-list)
                   (lambda (&rest _) nil)))
          (should (equal (mevedel-view-animation-frame
                          'dots "Working" 0 'default :multiple)
                         "*... Working")))))
    (should (member "*... " mevedel-view-animation-prefixes))))

(mevedel-deftest mevedel-view-animation-dots-target-cache
  (:doc "Dots probe only their displayed frames and refresh at semantic cadence.")
  (let ((mevedel-view-animation--cache nil)
        (mevedel-view-animation--dots-cache nil)
        (queries 0)
        (visible (selected-frame)))
    (cl-letf (((symbol-function 'mevedel-view-animation--dots-frame-supported-p)
               (lambda (frame)
                 (cl-incf queries)
                 (eq frame visible))))
      (should (equal "●··· Working"
                     (mevedel-view-animation-frame
                      'dots "Working" 0 'default (list visible))))
      (should (equal "·●·· Working"
                     (mevedel-view-animation-frame
                      'dots "Working" 0.24 'default (list visible))))
      (should (= queries 1))
      (should (equal "*... Working"
                     (mevedel-view-animation-frame
                      'dots "Working" 0 'default (list visible 'unsupported))))
      (should (= queries 2))
      (mevedel-view-animation-check-colors)
      (should (equal "●··· Working"
                     (mevedel-view-animation-frame
                      'dots "Working" 0 'default (list visible))))
      (should (= queries 3)))))

(mevedel-deftest mevedel-view-animation-dots-selected-frame-cache
  (:doc "An omitted dots display target follows the current selected frame.")
  (let ((mevedel-view-animation--dots-cache nil)
        (selected 'supported)
        (queries 0))
    (cl-letf (((symbol-function 'selected-frame) (lambda () selected))
              ((symbol-function 'mevedel-view-animation--dots-frame-supported-p)
               (lambda (frame)
                 (cl-incf queries)
                 (eq frame 'supported))))
      (should (equal (mevedel-view-animation-frame 'dots "Working" 0 'default)
                     "●··· Working"))
      (setq selected 'unsupported)
      (should (equal (mevedel-view-animation-frame 'dots "Working" 0 'default)
                     "*... Working"))
      (should (= queries 2)))))

(provide 'test-mevedel-view-animation)
;;; test-mevedel-view-animation.el ends here
