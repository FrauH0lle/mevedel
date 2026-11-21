;;; test-niwa-view-animation.el --- Animation frame tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view-animation)
(require 'niwa-view)
(require 'niwa-view-stream)

(niwa-deftest niwa-view-animation-color-ready-p
  (:doc "Seven live views retain prepared banks after global cache eviction.")
  (let ((niwa-view-animation--cache nil)
        (niwa-user-dir
         (file-name-as-directory
          (make-temp-file "niwa-animation-user-" t)))
        (niwa-permission-mode 'ask)
        (niwa-plugin-extra-roots nil)
        pairs
        (prepares 0))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-view-animation--colors)
                   (lambda (_face _frame) '("#ffffff" . "#000000")))
                  ((symbol-function 'niwa-view-animation--prepare)
                   (let ((original (symbol-function
                                    'niwa-view-animation--prepare)))
                     (lambda (style label colors frame)
                       (cl-incf prepares)
                       (funcall original style label colors frame)))))
          (dotimes (i 7)
            (let ((data (generate-new-buffer " *animation-data*"))
                  (view (generate-new-buffer " *animation-view*")))
              (push (cons view data) pairs)
              (with-current-buffer data
                (niwa--transcript-org-mode)
                (setq-local niwa--current-request nil)
                (setq-local niwa--session nil)
                (setq-local gptel-response-separator "\n\n")
                (setq-local niwa-view--abort-function
                            #'niwa-view-test--abort-interactions))
              (niwa-view--setup view data)
              (with-current-buffer view
                (should (niwa-view-animation-color-available-p
                         'shimmer (format "Working %d" i) 'default nil)))))
          (should (= prepares 7))
          (should (= (length niwa-view-animation--cache) 6))
          (cl-loop for (view . _data) in (reverse pairs)
                   for i from 0 do
                   (with-current-buffer view
                     (let ((label (format "Working %d" i)))
                       (should (niwa-view-animation-color-ready-p
                                'shimmer label 'default nil))
                       (should (niwa-view-animation-frame
                                'shimmer label 1.0 'default)))))
          (should (= prepares 7))
          (with-current-buffer (caar pairs)
            (dotimes (i 12)
              (niwa-view-animation-frame
               'shimmer (format "Replacement %d" i) 1.0 'default))
            (should (<= (length niwa-view-animation--view-cache)
                        niwa-view-animation--view-cache-limit)))
          (niwa-view-animation-invalidate)
          (cl-loop for (view . _data) in pairs
                   for i downfrom 6 do
                   (with-current-buffer view
                     (should-not (niwa-view-animation-color-ready-p
                                  'shimmer (format "Working %d" i)
                                  'default nil))))
          (with-current-buffer (caar pairs)
            (should (niwa-view-animation-color-available-p
                     'shimmer "Working 6" 'default nil))
            (should (niwa-view-animation-color-ready-p
                     'shimmer "Working 6" 'default nil))))
      (dolist (pair pairs)
        (when (buffer-live-p (car pair))
          (with-current-buffer (car pair)
            (niwa-view-stream-stop))
          (kill-buffer (car pair)))
        (when (buffer-live-p (cdr pair))
          (kill-buffer (cdr pair))))
      (when (file-directory-p niwa-user-dir)
        (delete-directory niwa-user-dir t)))))

(niwa-deftest niwa-view-animation-period
  (:doc "Natural frame cadence is independent of the caller's frame ceiling.")
  (progn
  (should (= (niwa-view-animation-period 'shimmer) (/ 1.0 60)))
  (should (= (niwa-view-animation-period 'breathe) (/ 1.0 8)))
  (should (= (niwa-view-animation-period 'bounce) (/ 1.0 8)))
  (should (= (niwa-view-animation-period 'braille) 0.24))
  (should (= (niwa-view-animation-period 'ascii) 0.24))
  (should (= (niwa-view-animation-period 'dots) 0.48))
  (should (= (niwa-view-animation-period 'ellipsis) 0.96))
  (should-not (niwa-view-animation-period 'static))))

(niwa-deftest niwa-view-animation-sweep-phase ()
  ,test
  (test)
  :doc "sweeps one second every four after a short delay"
  (should-not (niwa-view-animation-sweep-phase 0.0))
  (should-not (niwa-view-animation-sweep-phase 0.5))
  (should (< (abs (- 0.2 (niwa-view-animation-sweep-phase 0.8))) 1e-9))
  (should-not (niwa-view-animation-sweep-phase 1.7))
  (should-not (niwa-view-animation-sweep-phase 4.5))
  (should (niwa-view-animation-sweep-phase 4.7)))

(niwa-deftest niwa-view-animation-rest-seconds ()
  ,test
  (test)
  :doc "moves a shimmer to the end of its sweep and a breathe to its cycle start"
  (should (= 0.5 (niwa-view-animation-rest-seconds 'shimmer 0.5)))
  (should-not (niwa-view-animation-sweep-phase
               (niwa-view-animation-rest-seconds 'shimmer 0.8)))
  (should (< (abs (- 1.6 (niwa-view-animation-rest-seconds 'shimmer 0.8))) 1e-5))
  (should (< (abs (- 5.6 (niwa-view-animation-rest-seconds 'shimmer 4.7))) 1e-5))
  (should (= 3.6 (niwa-view-animation-rest-seconds 'breathe 5.0)))
  (should (= 0.0 (niwa-view-animation-rest-seconds 'breathe 1.0)))
  (should (= 1.3 (niwa-view-animation-rest-seconds 'braille 1.3)))
  (should (= 1.3 (niwa-view-animation-rest-seconds 'bounce 1.3))))

(niwa-deftest niwa-view-animation-next-delay ()
  ,test
  (test)
  :doc "sleeps between sweeps and steps through them"
  ;; Before the first sweep and at rest, wait for the next one.
  (should (< (abs (- 0.6 (niwa-view-animation-next-delay 'shimmer 0.0 0.05))) 1e-9))
  (should (< (abs (- 3.0 (niwa-view-animation-next-delay 'shimmer 1.6 0.05))) 1e-9))
  ;; Within a sweep, step by the frame period but stop at its end.
  (should (= 0.05 (niwa-view-animation-next-delay 'shimmer 0.8 0.05)))
  (should (< (abs (- 0.02 (niwa-view-animation-next-delay 'shimmer 1.58 0.05))) 1e-9))
  ;; Rounding just short of a sweep does not wake twice.
  (should (= 0.05 (niwa-view-animation-next-delay 'shimmer (- 4.6 1e-12) 0.05)))
  ;; Other styles move continuously.
  (should (= 0.24 (niwa-view-animation-next-delay 'braille 1.6 0.24))))

(niwa-deftest niwa-view-animation--sweep-sample ()
  ,test
  (test)
  :doc "keeps normal foreground at rest and fades only the moving band"
  (dolist (colors '(("#ffffff" . "#000000") ("#000000" . "#ffffff")))
    (let* ((palette (niwa-view-animation--palette (car colors) (cdr colors)))
           (shades (lambda (tick)
                     (let ((sample (niwa-view-animation--sweep-sample
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

(niwa-deftest niwa-view-animation-frame
  (:doc "Styles retain geometry; color motion fades from normal foreground.")
  (progn
  (dolist (style '(braille ascii dots ellipsis static))
    (let* ((first (niwa-view-animation-frame style "Working" 0 'default))
           (later (niwa-view-animation-frame style "Working" 1.2 'default)))
      (should (= (string-width first) (string-width later)))
      (should (string-match-p "Working" first))))
  (should (equal (niwa-view-animation-frame 'static "Working" 42 'default)
                 "Working"))
  (should (equal (niwa-view-animation-frame 'static "" 42 'default) ""))
  (should (equal (niwa-view-animation-frame 'braille "" 0 'default)
                 "⠋ "))
  (should (equal (niwa-view-animation-frame 'ascii "" 0 'default)
                 "- "))
  (should (equal (niwa-view-animation-frame 'dots "" 0 'default)
                 "●··· "))
  (should (equal (niwa-view-animation-frame 'braille "Work" 0 'default)
                 (niwa-view-animation-frame 'braille "Work" 2.4 'default)))
  (should (equal (niwa-view-animation-frame 'braille "Work" 0.24 'default)
                 "⠙ Work"))
  (should (equal (niwa-view-animation-frame 'ellipsis "Work" 0.96 'default)
                 "Work.  "))
  (dolist (colors '(("#ffffff" . "#000000") ("#000000" . "#ffffff")))
    (let* ((niwa-view-animation--cache nil)
           (palette (niwa-view-animation--palette (car colors) (cdr colors)))
           (normal (aref palette 63))
           (faded (aref palette 0)))
      (cl-letf (((symbol-function 'niwa-view-animation--colors)
                 (lambda (_face _frame) colors)))
        ;; Breathe starts at normal contrast, fades out, then returns.
        (dolist (seconds '(0 1.8 3.6))
          (let ((sample (niwa-view-animation-frame
                         'breathe "Working..." seconds 'default)))
            (dotimes (i (length sample))
              (should (equal (plist-get (get-text-property i 'face sample)
                                        :foreground)
                             (if (= seconds 1.8) faded normal))))))
        ;; Bounce moves the faded band; surrounding letters remain normal.
        (dolist (seconds '(0 1.8))
          (let ((sample (niwa-view-animation-frame
                         'bounce "Working..." seconds 'default)))
            (should (equal (plist-get (get-text-property 0 'face sample)
                                      :foreground)
                           (if (zerop seconds) faded normal)))
            (should (equal (plist-get (get-text-property 7 'face sample)
                                      :foreground)
                           (if (zerop seconds) normal faded))))))))))

(niwa-deftest niwa-view-animation--palette
  (:doc "A theme-derived palette contains exactly 64 shades.")
  (let ((palette (niwa-view-animation--palette "#ffffff" "#000000")))
    (should (= (length palette) 64))
    (should-not (equal (aref palette 0) (aref palette 63)))))

(niwa-deftest niwa-view-animation--prefix-end
  (:doc "The bounded animated prefix never ends inside a combining cluster.")
  (progn
    (should (= (niwa-view-animation--prefix-end
                (concat (make-string 47 ?a) "é" (make-string 30 ?z)))
               47))
    (should (= (niwa-view-animation--prefix-end "é") 2))))

(niwa-deftest niwa-view-animation-invalidate
  (:doc "A theme or face change clears prepared samples and changes shades.")
  (let ((niwa-view-animation--cache nil)
        (foreground "#ffffff"))
    (cl-letf (((symbol-function 'niwa-view-animation--colors)
               (lambda (_face _frame) (cons foreground "#000000"))))
      (let* ((before (niwa-view-animation-frame
                      'breathe "Working" 1 'default))
             (shade (get-text-property 0 'face before)))
        (should (niwa-view-animation-color-ready-p
                 'breathe "Working" 'default nil))
        (setq foreground "#ff0000")
        (should (eq before (niwa-view-animation-frame
                            'breathe "Working" 1 'default)))
        (niwa-view-animation-invalidate)
        (should-not niwa-view-animation--cache)
        (should-not (niwa-view-animation-color-ready-p
                     'breathe "Working" 'default nil))
        (let ((after (niwa-view-animation-frame
                      'breathe "Working" 1 'default)))
          (should-not (equal shade (get-text-property 0 'face after))))))))

(niwa-deftest niwa-view-animation-check-colors
  (:doc "The low-frequency check invalidates stale face and fallback banks.")
  (let ((niwa-view-animation--cache nil)
        (foreground "#ffffff"))
    (cl-letf (((symbol-function 'niwa-view-animation--colors)
               (lambda (_face _frame)
                 (when foreground (cons foreground "#000000")))))
      (niwa-view-animation-frame 'shimmer "Working" 0 'default)
      (should niwa-view-animation--cache)
      (niwa-view-animation-check-colors)
      (should niwa-view-animation--cache)
      (setq foreground "#ff0000")
      (niwa-view-animation-check-colors)
      (should-not niwa-view-animation--cache)
      (setq foreground nil)
      (niwa-view-animation-frame 'shimmer "Working" 0 'default)
      (should niwa-view-animation--cache)
      (setq foreground "#ffffff")
      (niwa-view-animation-check-colors)
      (should-not niwa-view-animation--cache))))

(niwa-deftest niwa-view-animation--color-frame ()
  ,test
  (test)
  :doc "Prepared color samples are bounded, reusable, and time indexed."
  (let ((niwa-view-animation--cache nil))
    (cl-letf (((symbol-function 'niwa-view-animation--colors)
               (lambda (_face _frame) '("#ffffff" . "#000000"))))
      (dolist (style '(shimmer breathe bounce))
        (let* ((first (niwa-view-animation-frame style "Working…" 0 'default))
               (same (niwa-view-animation-frame style "Working…" 3.6 'default))
               (next (niwa-view-animation-frame style "Working…" 1.0 'default)))
          (should (eq first same))
          (should (= (string-width first) (string-width next)))
          (should (equal (substring-no-properties first) "Working…"))
          (should (get-text-property 0 'face first))
          (should-not (equal-including-properties first next))))
      (let* ((long (concat (make-string 48 ?a) "é" (make-string 500 ?x)))
             (sample (niwa-view-animation-frame 'shimmer long 0 'default)))
        (should (equal (substring-no-properties sample) long))
        (should-not (get-text-property 49 'face sample)))
      (dotimes (i 20)
        (niwa-view-animation-frame 'shimmer (format "Status %d" i)
                                      0 'default))
      (should (<= (length niwa-view-animation--cache)
                  niwa-view-animation--cache-limit))))
  :doc "Equal labels retain separate banks for remapped views and in-place edits."
  (let ((niwa-view-animation--cache nil))
    (cl-letf (((symbol-function 'display-color-p) (lambda (&optional _) t))
              ((symbol-function 'display-color-cells) (lambda (&optional _) 256)))
      (with-temp-buffer
        (setq major-mode 'niwa-view-mode)
        (setq-local face-remapping-alist
                    (list (list 'default (list :foreground "#ffffff" :background "#000000")
                                'default)))
        (let ((dark (niwa-view-animation-frame 'shimmer "Working" 1 'default)))
          (with-temp-buffer
            (setq major-mode 'niwa-view-mode)
            (setq-local face-remapping-alist
                        '((default (:foreground "#000000" :background "#ffffff") default)))
            (should-not (equal-including-properties
                         dark (niwa-view-animation-frame 'shimmer "Working" 1 'default)))
            ;; Maintenance must resolve each shared bank with its own remapping.
            (niwa-view-animation-check-colors)
            (should (= 2 (length niwa-view-animation--cache))))
          (should (eq dark (niwa-view-animation-frame 'shimmer "Working" 1 'default)))
          (setf (plist-get (cadr (assq 'default face-remapping-alist)) :foreground)
                "#ff0000")
          (should-not (niwa-view-animation-color-ready-p 'shimmer "Working" 'default nil))
          (should-not (equal-including-properties
                       dark (niwa-view-animation-frame 'shimmer "Working" 1 'default))))))))

(niwa-deftest niwa-view-animation--colors
  (:doc "Unavailable display colors select a readable fixed-width fallback.")
  (progn
  (let ((niwa-view-animation--cache nil)
        (resolutions 0))
    (cl-letf (((symbol-function 'niwa-view-animation--colors)
               (lambda (_face _frame) (cl-incf resolutions) nil)))
      (dolist (style '(shimmer breathe bounce))
        (let ((a (niwa-view-animation-frame style "é Working" 0 'default))
              (b (niwa-view-animation-frame style "é Working" 1 'default)))
          (should (string-match-p "Working" a))
          (should (= (string-width a) (string-width b)))))
      (should (= resolutions 3))))
  (should-not (niwa-view-animation--colors 'nonexistent-face nil))
  (with-temp-buffer
    (let ((face-remapping-alist
           '((default (:background "#21242b") default)
             (niwa-view-spinner (:foreground "#5b6268") niwa-view-spinner)
             (niwa-view-tool-name (:foreground "#51afef") niwa-view-tool-name))))
      (cl-letf (((symbol-function 'display-color-p) (lambda (&optional _) t))
                ((symbol-function 'display-color-cells) (lambda (&optional _) 256)))
        (should (equal (niwa-view-animation--colors 'niwa-view-spinner nil)
                       '("#5b6268" . "#21242b")))
        (should (equal (niwa-view-animation--colors 'niwa-view-tool-name nil)
                       '("#51afef" . "#21242b")))
        ;; Anonymous remaps and inherited remaps use Emacs's own resolver.
        (setf (cdr (assq 'default face-remapping-alist))
              '((:foreground "#bbc2cf" :background "#282c34") default))
        (should (equal (niwa-view-animation--colors 'niwa-view-tool-summary nil)
                       '("#bbc2cf" . "#282c34"))))))))

(niwa-deftest niwa-view-animation-low-color-terminal
  (:doc "Low-color terminals use glyphs even when faces resolve to valid colors.")
  (let ((niwa-view-animation--cache nil)
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
        (should-not (niwa-view-animation--colors 'default nil))
        (let ((sample (niwa-view-animation-frame
                       style "Working" 0 'default)))
          (should (string-match-p "Working" sample))
          (should-not (get-text-property 0 'face sample))
          (should-not (niwa-view-animation-color-available-p
                       style "Working" 'default nil))))
      (setq cells 16)
      (should-not (niwa-view-animation--colors 'default nil))
      (setq cells 256)
      (should (equal (niwa-view-animation--colors 'default nil)
                     '("#ffffff" . "#000000"))))))

(niwa-deftest niwa-view-animation-missing-braille
  (:doc "Colorless and explicit Braille styles fall back to ASCII on unsupported displays.")
  (progn
    (let ((niwa-view-animation--cache nil))
      (cl-letf (((symbol-function 'char-displayable-p)
                 (lambda (_character) nil))
                ((symbol-function 'niwa-view-animation--colors)
                 (lambda (_face _frame) nil)))
        (should (equal (niwa-view-animation-frame
                        'braille "Working" 0 'default)
                       "- Working"))
        (should (equal (niwa-view-animation-frame
                        'shimmer "Working" 0 'default)
                       "- Working"))))
    (cl-letf (((symbol-function 'char-displayable-p)
               (lambda (_character) t)))
      (should (equal (niwa-view-animation-frame
                      'braille "Working" 0 'default :multiple)
                     "- Working")))))

(niwa-deftest niwa-view-animation-missing-dots
  (:doc "Dots retain cadence and width when either glyph is unavailable.")
  (progn
    (dolist (missing '(?● ?·))
      (let ((niwa-view-animation--dots-cache nil))
        (cl-letf (((symbol-function 'char-displayable-p)
                   (lambda (char) (not (eq char missing)))))
          (should (equal (niwa-view-animation-frame
                          'dots "Working" 0 'default)
                         "*... Working"))
          (should (equal (niwa-view-animation-frame
                          'dots "Working" 0.48 'default)
                         ".*.. Working"))
          (should (= (string-width (niwa-view-animation-frame
                                    'dots "Working" 0 'default))
                     (string-width (niwa-view-animation-frame
                                    'dots "Working" 0.48 'default)))))))
    (let ((niwa-view-animation--dots-cache nil))
      (cl-letf (((symbol-function 'char-displayable-p)
                 (lambda (_char) t)))
        (should (equal (niwa-view-animation-frame
                        'dots "Working" 0 'default)
                       "●··· Working"))
        (cl-letf (((symbol-function 'get-buffer-window-list)
                   (lambda (&rest _) nil)))
          (should (equal (niwa-view-animation-frame
                          'dots "Working" 0 'default :multiple)
                         "*... Working")))))))

(niwa-deftest niwa-view-animation-dots-target-cache
  (:doc "Dots probe only their displayed frames and refresh at semantic cadence.")
  (let ((niwa-view-animation--cache nil)
        (niwa-view-animation--dots-cache nil)
        (queries 0)
        (visible (selected-frame)))
    (cl-letf (((symbol-function 'niwa-view-animation--dots-frame-supported-p)
               (lambda (frame)
                 (cl-incf queries)
                 (eq frame visible))))
      (should (equal "●··· Working"
                     (niwa-view-animation-frame
                      'dots "Working" 0 'default (list visible))))
      (should (equal "·●·· Working"
                     (niwa-view-animation-frame
                      'dots "Working" 0.48 'default (list visible))))
      (should (= queries 1))
      (should (equal "*... Working"
                     (niwa-view-animation-frame
                      'dots "Working" 0 'default (list visible 'unsupported))))
      (should (= queries 2))
      (niwa-view-animation-check-colors)
      (should (equal "●··· Working"
                     (niwa-view-animation-frame
                      'dots "Working" 0 'default (list visible))))
      (should (= queries 3)))))

(niwa-deftest niwa-view-animation-dots-selected-frame-cache
  (:doc "An omitted dots display target follows the current selected frame.")
  (let ((niwa-view-animation--dots-cache nil)
        (selected 'supported)
        (queries 0))
    (cl-letf (((symbol-function 'selected-frame) (lambda () selected))
              ((symbol-function 'niwa-view-animation--dots-frame-supported-p)
               (lambda (frame)
                 (cl-incf queries)
                 (eq frame 'supported))))
      (should (equal (niwa-view-animation-frame 'dots "Working" 0 'default)
                     "●··· Working"))
      (setq selected 'unsupported)
      (should (equal (niwa-view-animation-frame 'dots "Working" 0 'default)
                     "*... Working"))
      (should (= queries 2)))))

(niwa-deftest niwa-view-animation-reserve
  (:doc "Retains all active labels and releases excess banks after rows leave.")
  (with-temp-buffer
    (niwa-view-animation-reserve 9)
    (should (= niwa-view-animation--view-cache-limit 9))
    (setq niwa-view-animation--view-cache (number-sequence 1 9))
    (niwa-view-animation-reserve 2)
    (should (equal niwa-view-animation--view-cache '(1 2 3 4)))
    (should (= niwa-view-animation--view-cache-limit 4))))

(niwa-deftest niwa-view-animation-color-available-p
  (:doc "Semantic preparation protects earlier active banks during a frame move.")
  (let ((niwa-view-animation--cache nil))
    (with-temp-buffer
      (setq major-mode 'niwa-view-mode)
      (cl-letf (((symbol-function 'niwa-view-animation--colors)
                 (lambda (&rest _) nil)))
        (dolist (label '("A" "B" "C" "D"))
          (niwa-view-animation-color-available-p
           'shimmer label 'default nil))
        (dolist (label '("A" "B" "C"))
          (niwa-view-animation-color-available-p
           'shimmer label 'default nil))
        (niwa-view-animation-color-available-p 'shimmer "D" 'default :multiple)
        (setq niwa-view-animation--cache nil)
        (dolist (label '("A" "B" "C"))
          (should (niwa-view-animation-color-ready-p
                   'shimmer label 'default nil)))
        (should (niwa-view-animation-color-ready-p
                 'shimmer "D" 'default :multiple))))))

(niwa-deftest niwa-view-animation-sequence
  (:doc "Presenter sequences retain style phases and sleep through equal samples.")
  (let ((niwa-view-animation--cache nil))
    (cl-letf (((symbol-function 'niwa-view-animation--colors)
               (lambda (&rest _) '("#ffffff" . "#000000"))))
      (dolist (style '(shimmer breathe bounce braille ascii dots ellipsis))
        (let* ((sequence (niwa-view-animation-sequence
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
                            (niwa-view-animation-frame style "Working..."
                                                           (aref entry 0) 'default)))
                   (setq previous (aref entry 0) sample (aref entry 1)))
          (when (eq style 'shimmer)
            (should (<= previous 1.60000001))
            (should (equal-including-properties sample (aref (aref sequence 1) 1))))))
      (should-not (niwa-view-animation-sequence 'static "Working" 0.1 'default))
      (should-not (niwa-view-animation-sequence 'breathe "Working" 0 'default)))))

(provide 'test-niwa-view-animation)
;;; test-niwa-view-animation.el ends here
