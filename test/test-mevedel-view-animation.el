;;; test-mevedel-view-animation.el --- Animation frame tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view-animation)
(require 'mevedel-view)
(require 'mevedel-view-stream)

(mevedel-deftest mevedel-view-animation-color-ready-p
  (:doc "Seven live views retain prepared banks after global cache eviction.")
  (let ((mevedel-view-animation--cache nil)
        (mevedel-user-dir
         (file-name-as-directory
          (make-temp-file "mevedel-animation-user-" t)))
        (mevedel-permission-mode 'ask)
        (mevedel-plugin-extra-roots nil)
        pairs
        (prepares 0))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-view-animation--colors)
                   (lambda (_face _frame) '("#ffffff" . "#000000")))
                  ((symbol-function 'mevedel-view-animation--prepare)
                   (let ((original (symbol-function
                                    'mevedel-view-animation--prepare)))
                     (lambda (style label colors frame)
                       (cl-incf prepares)
                       (funcall original style label colors frame)))))
          (dotimes (i 7)
            (let ((data (generate-new-buffer " *animation-data*"))
                  (view (generate-new-buffer " *animation-view*")))
              (push (cons view data) pairs)
              (with-current-buffer data
                (mevedel--transcript-org-mode)
                (setq-local mevedel--current-request nil)
                (setq-local mevedel--session nil)
                (setq-local gptel-response-separator "\n\n")
                (setq-local mevedel-view--abort-function
                            #'mevedel-view-test--abort-interactions))
              (mevedel-view--setup view data)
              (with-current-buffer view
                (should (mevedel-view-animation-color-available-p
                         'shimmer (format "Working %d" i) 'default nil)))))
          (should (= prepares 7))
          (should (= (length mevedel-view-animation--cache) 6))
          (cl-loop for (view . _data) in (reverse pairs)
                   for i from 0 do
                   (with-current-buffer view
                     (let ((label (format "Working %d" i)))
                       (should (mevedel-view-animation-color-ready-p
                                'shimmer label 'default nil))
                       (should (mevedel-view-animation-frame
                                'shimmer label 1.0 'default)))))
          (should (= prepares 7))
          (with-current-buffer (caar pairs)
            (dotimes (i 12)
              (mevedel-view-animation-frame
               'shimmer (format "Replacement %d" i) 1.0 'default))
            (should (<= (length mevedel-view-animation--view-cache)
                        mevedel-view-animation--view-cache-limit)))
          (mevedel-view-animation-invalidate)
          (cl-loop for (view . _data) in pairs
                   for i downfrom 6 do
                   (with-current-buffer view
                     (should-not (mevedel-view-animation-color-ready-p
                                  'shimmer (format "Working %d" i)
                                  'default nil))))
          (with-current-buffer (caar pairs)
            (should (mevedel-view-animation-color-available-p
                     'shimmer "Working 6" 'default nil))
            (should (mevedel-view-animation-color-ready-p
                     'shimmer "Working 6" 'default nil))))
      (dolist (pair pairs)
        (when (buffer-live-p (car pair))
          (with-current-buffer (car pair)
            (mevedel-view-stream-stop))
          (kill-buffer (car pair)))
        (when (buffer-live-p (cdr pair))
          (kill-buffer (cdr pair))))
      (when (file-directory-p mevedel-user-dir)
        (delete-directory mevedel-user-dir t)))))

(mevedel-deftest mevedel-view-animation-period
  (:doc "Natural frame cadence is independent of the caller's frame ceiling.")
  (progn
  (should (= (mevedel-view-animation-period 'shimmer) (/ 1.0 60)))
  (should (= (mevedel-view-animation-period 'breathe) (/ 1.0 8)))
  (should (= (mevedel-view-animation-period 'bounce) (/ 1.0 8)))
  (should (= (mevedel-view-animation-period 'braille) 0.24))
  (should (= (mevedel-view-animation-period 'ascii) 0.24))
  (should (= (mevedel-view-animation-period 'dots) 0.48))
  (should (= (mevedel-view-animation-period 'ellipsis) 0.96))
  (should-not (mevedel-view-animation-period 'static))))

(mevedel-deftest mevedel-view-animation-sweep-phase ()
  ,test
  (test)
  :doc "sweeps one second every four after a short delay"
  (should-not (mevedel-view-animation-sweep-phase 0.0))
  (should-not (mevedel-view-animation-sweep-phase 0.5))
  (should (< (abs (- 0.2 (mevedel-view-animation-sweep-phase 0.8))) 1e-9))
  (should-not (mevedel-view-animation-sweep-phase 1.7))
  (should-not (mevedel-view-animation-sweep-phase 4.5))
  (should (mevedel-view-animation-sweep-phase 4.7)))

(mevedel-deftest mevedel-view-animation-next-delay ()
  ,test
  (test)
  :doc "sleeps between sweeps and steps through them"
  ;; Before the first sweep and at rest, wait for the next one.
  (should (< (abs (- 0.6 (mevedel-view-animation-next-delay 'shimmer 0.0 0.05))) 1e-9))
  (should (< (abs (- 3.0 (mevedel-view-animation-next-delay 'shimmer 1.6 0.05))) 1e-9))
  ;; Within a sweep, step by the frame period but stop at its end.
  (should (= 0.05 (mevedel-view-animation-next-delay 'shimmer 0.8 0.05)))
  (should (< (abs (- 0.02 (mevedel-view-animation-next-delay 'shimmer 1.58 0.05))) 1e-9))
  ;; Rounding just short of a sweep does not wake twice.
  (should (= 0.05 (mevedel-view-animation-next-delay 'shimmer (- 4.6 1e-12) 0.05)))
  ;; Other styles move continuously.
  (should (= 0.24 (mevedel-view-animation-next-delay 'braille 1.6 0.24))))

(mevedel-deftest mevedel-view-animation--sweep-sample ()
  ,test
  (test)
  :doc "keeps normal foreground at rest and fades only the moving band"
  (dolist (colors '(("#ffffff" . "#000000") ("#000000" . "#ffffff")))
    (let* ((palette (mevedel-view-animation--palette (car colors) (cdr colors)))
           (shades (lambda (tick)
                     (let ((sample (mevedel-view-animation--sweep-sample
                                    "Thinking..." palette tick)))
                       (mapcar (lambda (i)
                                 (plist-get (get-text-property i 'face sample)
                                            :foreground))
                               (number-sequence 0 10))))))
      (should (equal (list (aref palette 63)) (delete-dups (funcall shades 0))))
      (let ((middle (funcall shades 30)))
        ;; Both themes keep normal text around a band faded toward the background.
        (should (> (length (delete-dups (copy-sequence middle))) 3))
        (should (equal (car middle) (aref palette 63)))
        (should (equal (car (last middle)) (aref palette 63)))
        (should-not (equal (nth 5 middle) (aref palette 63)))))))

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
  (:doc "Styles retain geometry; color motion fades from normal foreground.")
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
                 (mevedel-view-animation-frame 'braille "Work" 2.4 'default)))
  (should (equal (mevedel-view-animation-frame 'braille "Work" 0.24 'default)
                 "⠙ Work"))
  (should (equal (mevedel-view-animation-frame 'ellipsis "Work" 0.96 'default)
                 "Work.  "))
  (dolist (colors '(("#ffffff" . "#000000") ("#000000" . "#ffffff")))
    (let* ((mevedel-view-animation--cache nil)
           (palette (mevedel-view-animation--palette (car colors) (cdr colors)))
           (normal (aref palette 63))
           (faded (aref palette 0)))
      (cl-letf (((symbol-function 'mevedel-view-animation--colors)
                 (lambda (_face _frame) colors)))
        ;; Breathe starts at normal contrast, fades out, then returns.
        (dolist (seconds '(0 1.8 3.6))
          (let ((sample (mevedel-view-animation-frame
                         'breathe "Working..." seconds 'default)))
            (dotimes (i (length sample))
              (should (equal (plist-get (get-text-property i 'face sample)
                                        :foreground)
                             (if (= seconds 1.8) faded normal))))))
        ;; Bounce moves the faded band; surrounding letters remain normal.
        (dolist (seconds '(0 1.8))
          (let ((sample (mevedel-view-animation-frame
                         'bounce "Working..." seconds 'default)))
            (should (equal (plist-get (get-text-property 0 'face sample)
                                      :foreground)
                           (if (zerop seconds) faded normal)))
            (should (equal (plist-get (get-text-property 7 'face sample)
                                      :foreground)
                           (if (zerop seconds) normal faded))))))))))

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

(mevedel-deftest mevedel-view-animation--color-frame ()
  ,test
  (test)
  :doc "Prepared color samples are bounded, reusable, and time indexed."
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
                  mevedel-view-animation--cache-limit))))
  :doc "Equal labels retain separate banks for remapped views and in-place edits."
  (let ((mevedel-view-animation--cache nil))
    (cl-letf (((symbol-function 'display-color-p) (lambda (&optional _) t))
              ((symbol-function 'display-color-cells) (lambda (&optional _) 256)))
      (with-temp-buffer
        (setq major-mode 'mevedel-view-mode)
        (setq-local face-remapping-alist
                    (list (list 'default (list :foreground "#ffffff" :background "#000000")
                                'default)))
        (let ((dark (mevedel-view-animation-frame 'shimmer "Working" 1 'default)))
          (with-temp-buffer
            (setq major-mode 'mevedel-view-mode)
            (setq-local face-remapping-alist
                        '((default (:foreground "#000000" :background "#ffffff") default)))
            (should-not (equal-including-properties
                         dark (mevedel-view-animation-frame 'shimmer "Working" 1 'default)))
            ;; Maintenance must resolve each shared bank with its own remapping.
            (mevedel-view-animation-check-colors)
            (should (= 2 (length mevedel-view-animation--cache))))
          (should (eq dark (mevedel-view-animation-frame 'shimmer "Working" 1 'default)))
          (setf (plist-get (cadr (assq 'default face-remapping-alist)) :foreground)
                "#ff0000")
          (should-not (mevedel-view-animation-color-ready-p 'shimmer "Working" 'default nil))
          (should-not (equal-including-properties
                       dark (mevedel-view-animation-frame 'shimmer "Working" 1 'default))))))))

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
  (should-not (mevedel-view-animation--colors 'nonexistent-face nil))
  (with-temp-buffer
    (let ((face-remapping-alist
           '((default (:background "#21242b") default)
             (mevedel-view-spinner (:foreground "#5b6268") mevedel-view-spinner)
             (mevedel-view-tool-name (:foreground "#51afef") mevedel-view-tool-name))))
      (cl-letf (((symbol-function 'display-color-p) (lambda (&optional _) t))
                ((symbol-function 'display-color-cells) (lambda (&optional _) 256)))
        (should (equal (mevedel-view-animation--colors 'mevedel-view-spinner nil)
                       '("#5b6268" . "#21242b")))
        (should (equal (mevedel-view-animation--colors 'mevedel-view-tool-name nil)
                       '("#51afef" . "#21242b")))
        ;; Anonymous remaps and inherited remaps use Emacs's own resolver.
        (setf (cdr (assq 'default face-remapping-alist))
              '((:foreground "#bbc2cf" :background "#282c34") default))
        (should (equal (mevedel-view-animation--colors 'mevedel-view-tool-summary nil)
                       '("#bbc2cf" . "#282c34"))))))))

(mevedel-deftest mevedel-view-animation-low-color-terminal
  (:doc "Low-color terminals use glyphs even when faces resolve to valid colors.")
  (let ((mevedel-view-animation--cache nil)
        (cells 8))
    (cl-letf (((symbol-function 'display-color-p) (lambda (&optional _frame) t))
              ((symbol-function 'display-graphic-p) (lambda (&optional _frame) nil))
              ((symbol-function 'display-color-cells)
               (lambda (&optional _frame) cells))
              ((symbol-function 'face-attributes-as-vector)
               (lambda (&rest _)
                 (let ((attributes (make-vector (length internal-lisp-face-attributes)
                                                'unspecified)))
                   (aset attributes (cl-position :foreground internal-lisp-face-attributes)
                         "#ffffff")
                   (aset attributes (cl-position :background internal-lisp-face-attributes)
                         "#000000")
                   attributes))))
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
                          'dots "Working" 0.48 'default)
                         ".*.. Working"))
          (should (= (string-width (mevedel-view-animation-frame
                                    'dots "Working" 0 'default))
                     (string-width (mevedel-view-animation-frame
                                    'dots "Working" 0.48 'default)))))))
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
                      'dots "Working" 0.48 'default (list visible))))
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

(mevedel-deftest mevedel-view-animation-reserve
  (:doc "Retains all active labels and releases excess banks after rows leave.")
  (with-temp-buffer
    (mevedel-view-animation-reserve 9)
    (should (= mevedel-view-animation--view-cache-limit 9))
    (setq mevedel-view-animation--view-cache (number-sequence 1 9))
    (mevedel-view-animation-reserve 2)
    (should (equal mevedel-view-animation--view-cache '(1 2 3 4)))
    (should (= mevedel-view-animation--view-cache-limit 4))))

(mevedel-deftest mevedel-view-animation-color-available-p
  (:doc "Semantic preparation protects earlier active banks during a frame move.")
  (let ((mevedel-view-animation--cache nil))
    (with-temp-buffer
      (setq major-mode 'mevedel-view-mode)
      (cl-letf (((symbol-function 'mevedel-view-animation--colors)
                 (lambda (&rest _) nil)))
        (dolist (label '("A" "B" "C" "D"))
          (mevedel-view-animation-color-available-p
           'shimmer label 'default nil))
        (dolist (label '("A" "B" "C"))
          (mevedel-view-animation-color-available-p
           'shimmer label 'default nil))
        (mevedel-view-animation-color-available-p 'shimmer "D" 'default :multiple)
        (setq mevedel-view-animation--cache nil)
        (dolist (label '("A" "B" "C"))
          (should (mevedel-view-animation-color-ready-p
                   'shimmer label 'default nil)))
        (should (mevedel-view-animation-color-ready-p
                 'shimmer "D" 'default :multiple))))))

(mevedel-deftest mevedel-view-animation-sequence
  (:doc "Presenter sequences retain style phases and sleep through equal samples.")
  (let ((mevedel-view-animation--cache nil))
    (cl-letf (((symbol-function 'mevedel-view-animation--colors)
               (lambda (&rest _) '("#ffffff" . "#000000"))))
      (dolist (style '(shimmer breathe bounce braille ascii dots ellipsis))
        (let* ((sequence (mevedel-view-animation-sequence
                          style "Working..." (/ 1.0 30) 'default))
               (cycle (aref sequence 0))
               (previous -1.0) sample)
          (should (= 0.0 (aref (aref sequence 1) 0)))
          (cl-loop for entry across (seq-subseq sequence 1) do
                   (should (> (aref entry 0) previous))
                   (should (< (aref entry 0) cycle))
                   (should-not (equal-including-properties sample (aref entry 1)))
                   (should (equal-including-properties
                            (aref entry 1)
                            (mevedel-view-animation-frame style "Working..."
                                                           (aref entry 0) 'default)))
                   (setq previous (aref entry 0) sample (aref entry 1)))
          (when (eq style 'shimmer)
            (should (<= previous 1.60000001))
            (should (equal-including-properties sample (aref (aref sequence 1) 1))))))
      (should-not (mevedel-view-animation-sequence 'static "Working" 0.1 'default))
      (should-not (mevedel-view-animation-sequence 'breathe "Working" 0 'default)))))

(provide 'test-mevedel-view-animation)
;;; test-mevedel-view-animation.el ends here
