;;; test-niwa-view-geometry.el --- Native viewport geometry tests -*- lexical-binding: t -*-

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
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-zone)

(defmacro niwa-view-geometry-test--with-window (&rest body)
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

(defun niwa-view-geometry-test--require-frame-resize (&optional timeout)
  "Skip if a plain control frame cannot grow within TIMEOUT seconds or five."
  (let ((frame (make-frame '((width . 80) (height . 30))))
        (frame-resize-pixelwise t)
        (unread-command-events nil))
    (unwind-protect
        (with-selected-frame frame
          (with-temp-buffer
            (set-window-buffer (selected-window) (current-buffer))
            (redisplay t)
            (let ((target (1+ (frame-text-height frame)))
                  (deadline (+ (float-time) (or timeout 5))))
              (set-frame-height frame target nil t)
              (while (and (/= target (frame-text-height frame))
                          (< (float-time) deadline))
                (setq unread-command-events nil)
                (sit-for 0.02)
                (redisplay t))
              (unless (= target (frame-text-height frame))
                (ert-skip (format "Compositor refused plain control-frame resize: requested %d pixels, got %d"
                                  target (frame-text-height frame))))
              t)))
      (when (frame-live-p frame) (delete-frame frame t)))))

(niwa-deftest niwa-view-geometry-test--require-frame-resize ()
  (progn (skip-unless (display-graphic-p)) ,test)
  (test)
  :doc "A presenting compositor accepts the plain control-frame resize"
  (should (niwa-view-geometry-test--require-frame-resize))
  :doc "A refused control resize skips with an explicit compositor diagnostic"
  (cl-letf (((symbol-function 'set-frame-height) #'ignore))
    (let ((failure (should-error
                    (niwa-view-geometry-test--require-frame-resize 0)
                    :type 'ert-test-skipped)))
      (should (string-search "Compositor refused plain control-frame resize"
                             (error-message-string failure))))))

(niwa-deftest niwa-view--viewport-at-y
  (:doc "Zero Y, exact placement, and unchanged viewports work on native displays.")
  (niwa-view-geometry-test--with-window
    (dotimes (_ 80) (insert "history\n"))
    (let* ((window (selected-window))
           (input (save-excursion (forward-line -1) (point)))
           (viewport (niwa-view--viewport-at-y window input 0)))
      (niwa-view--set-viewport window viewport)
      (redisplay t)
      (should (= 0 (niwa-view--position-screen-y input window)))
      (cl-letf (((symbol-function 'set-window-start)
                 (lambda (&rest _) (ert-fail "Redundant start setter")))
                ((symbol-function 'set-window-vscroll)
                 (lambda (&rest _) (ert-fail "Redundant vscroll setter"))))
        (niwa-view--set-viewport window viewport))
      (goto-char (point-min))
      (set-window-start window (point-min) t)
      (redisplay t)
      (should-not (niwa-view--position-screen-y (point-max) window t)))))

(niwa-deftest niwa-view--set-viewport
  (:doc "Changed pixel scrolling survives native redisplay without moving point."
   :before-each ((skip-unless (display-graphic-p))
                 (should (member "DejaVu Sans Mono" (font-family-list)))))
  (let* ((frame-resize-pixelwise t)
         (resize-mini-windows nil)
         (unread-command-events nil)
         (deactivate-mark nil)
         (frame (make-frame '((font . "DejaVu Sans Mono-11")
                              (width . 80) (height . 30)))))
    (unwind-protect
        (with-selected-frame frame
          (with-temp-buffer
            (switch-to-buffer (current-buffer))
            (setq-local header-line-format "Static header" mode-line-format nil
                        tab-line-format nil truncate-lines t word-wrap nil
                        line-spacing nil scroll-margin 0 scroll-conservatively 101
                        auto-window-vscroll nil)
            (dotimes (n 300)
              (insert (format "row %03d 012345678901234567890123456789\n" n)))
                    (insert "> synthetic")
            (add-hook 'window-scroll-functions #'ignore nil t)
            (redisplay t)
            (let* ((window (selected-window))
                   (target (point-max))
                   (row-height (line-pixel-height))
                   (body-target (- (* 27 row-height) 2))
                   (height (+ (frame-text-height frame)
                              (- body-target (window-body-height window t))))
                   (deadline (+ (float-time) 5))
                   (start (save-excursion
                            (goto-char target) (forward-line -26) (point)))
                   (text (buffer-string)))
              (should (> row-height 3))
              ;; One exact configure; no geometry search or fallback.
              (set-frame-height frame height nil t)
              (while (and (/= height (frame-text-height frame))
                          (< (float-time) deadline))
                (setq unread-command-events nil)
                (sit-for 0.02)
                (redisplay t))
              (redisplay t)
              (should (= height (frame-text-height frame)))
              (should (= body-target (window-body-height window t)))
              (goto-char target)
              (set-window-start window start t)
              (set-window-vscroll window 3 t t)
              (redisplay t)
              (let ((row (window-line-height nil window))
                    (top (window-header-line-height window))
                    (visible (pos-visible-in-window-p target window t))
                    clipped)
                (set-window-vscroll window 0 t t)
                (setq clipped (pos-visible-in-window-p target window t))
                (set-window-vscroll window 3 t t)
                (ert-info ((format "Body %S, row %S, header %S, visible %S, zero-scroll %S"
                                   body-target row top visible clipped))
                  (should (= row-height (car row)))
                  (should (zerop (nth 3 row)))
                  (should (>= top row-height))
                  (should (= (length visible) 2))
                  (should (= (length clipped) 6))
                  (should (> (nth 3 clipped) 0))
                  (should (> (cadr clipped) body-target))
                  (should (= 1 (- body-target (- (cadr visible) top) row-height)))))
              (should (= (point) target))
              (set-mark (- target 4))
              (setq mark-active t deactivate-mark nil)
              ;; Artificial pending optional-start state, not a render mutation.
              (recenter -1)
              (set-window-start window start t)
              (set-window-vscroll window 0 t t)
              (niwa-view--set-viewport window (cons start 3))
              (should (= (window-start window) start))
              (should (= (window-vscroll window t) 3))
              (should (= (point) target))
              (should (= (window-point window) target))
              (redisplay t)
              (ert-info ((format "Point %S, window point %S, start %S, vscroll %S"
                                 (point) (window-point window)
                                 (window-start window) (window-vscroll window t)))
                (should (= (point) target))
                (should (= (window-point window) target))
                (should (= (window-start window) start))
                (should (= (window-vscroll window t) 3)))
              (should (equal-including-properties text (buffer-string)))
              (should (= (mark t) (- target 4)))
              (should mark-active)
              (should (eq (selected-window) window)))))
      (when (frame-live-p frame) (delete-frame frame t)))))

(niwa-deftest niwa-view-geometry-wrapped-start
  (:doc "A preserving writer around a wrapped window start keeps pixel scrolling and point."
   :before-each ((skip-unless (display-graphic-p))
                 (should (member "DejaVu Sans Mono" (font-family-list)))))
  (let* ((frame-resize-pixelwise t)
         (resize-mini-windows nil)
         (unread-command-events nil)
         (frame (make-frame '((font . "DejaVu Sans Mono-11")
                              (width . 80) (height . 30)))))
    (unwind-protect
        (with-selected-frame frame
          (niwa-view-test--with-buffers
            (switch-to-buffer view-buf)
            ;; Measure a history reader's transcript-only viewport while the
            ;; frame configure gives deferred window hooks a chance to run.
            (niwa-view-composer-set-historical-visible nil)
            (setq-local header-line-format "Static header" mode-line-format nil
                        tab-line-format nil truncate-lines nil word-wrap nil
                        line-spacing nil auto-window-vscroll nil)
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (forward-line 1)
              (dotimes (n 300) (insert (format "row %03d\n" n)))
              ;; The window start falls a few rows into this wrapped line, as
              ;; in a transcript paragraph.  Redisplay does not recompute a
              ;; start many rows into a logical line.
              (insert (make-string 1600 ?w) "\n")
              (dotimes (n 40) (insert (format "tail %02d\n" n))))
            (redisplay t)
            (let* ((window (selected-window))
                   (row-height (line-pixel-height))
                   (body-target (- (* 27 row-height) 2))
                   (height (+ (frame-text-height frame)
                              (- body-target (window-body-height window t))))
                   (deadline (+ (float-time) 5))
                   ;; A history reader: no composer policy re-chooses its viewport.
                   (target (save-excursion
                             (goto-char (point-min))
                             (search-forward "tail 10\n")
                             (line-beginning-position 0)))
                   start)
              (should (> row-height 3))
              ;; One exact configure; no geometry search or fallback.
              (set-frame-height frame height nil t)
              (while (and (/= height (frame-text-height frame))
                          (< (float-time) deadline))
                (setq unread-command-events nil)
                (sit-for 0.02)
                (redisplay t))
              (redisplay t)
              (should (= body-target (window-body-height window t)))
              (goto-char target)
              (setq start (save-excursion (vertical-motion -26 window) (point)))
              (should-not (eq (char-before start) ?\n))
              (set-window-start window start t)
              (set-window-vscroll window 3 t t)
              ;; This redisplay consumes the one-shot preservation request.
              (redisplay t)
              (let ((visible (pos-visible-in-window-p target window t)))
                (ert-info ((format "Start %S, vscroll %S, visible %S"
                                   (window-start window) (window-vscroll window t)
                                   visible))
                  (should (= start (window-start window)))
                  (should (= 3 (window-vscroll window t)))
                  (should (= (length visible) 2))
                  ;; Strictly inside the body: native redisplay keeps this start.
                  (should (= 1 (- body-target
                                  (- (cadr visible) (window-header-line-height window))
                                  row-height)))))
              ;; Text changes on both sides of the start, as a history row
              ;; refresh and a status reconcile do between two redisplays.
              (niwa-view-call-preserving-user-view-state
               (lambda ()
                 (let ((inhibit-read-only t))
                   (save-excursion
                     ;; Equal-length rewrites keep fixture positions stable.
                     (goto-char (point-min))
                     (search-forward "row 001")
                     (delete-char -1)
                     (insert "1")
                     (search-forward "tail 30")
                     (delete-char -1)
                     (insert "0")))))
              (should (= (point) target))
              (should (= start (window-start window)))
              (redisplay t)
              (ert-info ((format "Point %S of %S, start %S, vscroll %S"
                                 (point) target
                                 (window-start window) (window-vscroll window t)))
                (should (= (point) target))
                (should (= (window-point window) target))
                (should (= start (window-start window)))
                (should (= 3 (window-vscroll window t)))))))
      (when (frame-live-p frame) (delete-frame frame t)))))

(niwa-deftest niwa-view--restore-redisplay-vscroll
  (:before-each ((skip-unless (display-graphic-p))
                 (should (member "DejaVu Sans Mono" (font-family-list)))))
  (let* ((frame-resize-pixelwise t)
         (resize-mini-windows nil)
         (unread-command-events nil)
         (frame (make-frame '((font . "DejaVu Sans Mono-11")
                              (width . 80) (height . 30)))))
    (unwind-protect
        (with-selected-frame frame
          (with-temp-buffer
            (switch-to-buffer (current-buffer))
            (setq-local header-line-format "Static header" mode-line-format nil
                        truncate-lines nil word-wrap nil line-spacing nil
                        scroll-margin 0 scroll-conservatively 101
                        auto-window-vscroll nil)
            (dotimes (n 300) (insert (format "row %03d\n" n)))
            ;; The window start falls inside this wrapped line.
            (insert (make-string 1200 ?w) "\n")
            (dotimes (n 25) (insert (format "tail %02d\n" n)))
            (insert "> synthetic")
            (when ,guard
              (add-hook 'pre-redisplay-functions
                        #'niwa-view--note-redisplay-vscroll nil t)
              (add-hook 'window-scroll-functions
                        #'niwa-view--restore-redisplay-vscroll nil t))
            (redisplay t)
            (let* ((window (selected-window))
                   (target (point-max))
                   (row-height (line-pixel-height))
                   (body-target (- (* 27 row-height) 2))
                   (deadline (+ (float-time) 5))
                   start)
              ;; Re-aim at one exact body: a fresh frame can report its body
              ;; before its first configure settles.
              (while (and (/= body-target (window-body-height window t))
                          (< (float-time) deadline))
                (set-frame-height frame (+ (frame-text-height frame)
                                           (- body-target (window-body-height window t)))
                                  nil t)
                (setq unread-command-events nil)
                (sit-for 0.05)
                (redisplay t))
              (redisplay t)
              (should (= body-target (window-body-height window t)))
              (goto-char target)
              (setq start (save-excursion (vertical-motion -26 window) (point)))
              (should-not (eq (char-before start) ?\n))
              (set-window-start window start t)
              (set-window-vscroll window 3 t t)
              ;; This redisplay consumes the one-shot preservation request.
              (redisplay t)
              (should (= 3 (window-vscroll window t)))
              (should (= 2 (length (pos-visible-in-window-p target window t))))
              ;; A writer changes text above the start and restores it
              ;; without renewing native preservation.
              (save-excursion (goto-char (point-min)) (insert "new\n"))
              (setq start (+ start 4) target (+ target 4))
              (goto-char target)
              (set-window-start window start t)
              (redisplay t)
              (ert-info ((format "Point %S of %S, start %S, vscroll %S"
                                 (point) target (window-start window)
                                 (window-vscroll window t)))
                (should (= start (window-start window)))
                (if ,guard
                    (progn
                      (should (= (point) target))
                      (should (= 3 (window-vscroll window t))))
                  ;; Control: the native reset this guards against happens.
                  (should (/= (point) target))
                  (should (zerop (window-vscroll window t))))))))
      (when (frame-live-p frame) (delete-frame frame t))))
  (guard)
  :doc "Control: without the guard, native redisplay drops pixel scrolling and moves point" nil
  :doc "The guard restores pixel scrolling native redisplay dropped at an unchanged start" t)

(niwa-deftest niwa-view-geometry-reading-anchor ()
  (progn
    (skip-unless (display-graphic-p))
    (niwa-view-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (delete-other-windows)
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (forward-line 1)
          (dotimes (n 100) (insert (format "History line %d\n" n))))
        (niwa-view-test--insert-composer-draft "> draft\nsecond line")
        (let* ((window (selected-window))
               (start (save-excursion
                        (goto-char (point-max))
                        (forward-line -12) (point)))
               (clicked (save-excursion
                          (goto-char start) (forward-line 4) (point))))
          (set-window-start window start t)
          (set-window-vscroll window 7 t)
          (redisplay t)
          (should (niwa-view--position-screen-y
                   (point-max) window))
          (mouse-set-point (list 'mouse-1 (posn-at-point clicked window)))
          (let ((y (niwa-view--position-screen-y clicked window)))
            (should y)
            (should (= 7 (window-vscroll window t)))
            (dolist (count '(4 1 0))
              (let ((update
                     (lambda ()
                       (niwa-view-zone-reconcile
                        'status (niwa-view--status-anchor)
                        (niwa-view--status-anchor)
                        (when (> count 0)
                          (list (list :namespace 'status :id 'reader-test :priority 1
                                      :body (mapconcat
                                             (lambda (n) (format "status row %d\n" n))
                                             (number-sequence 1 count) ""))))))))
                (if ,outer
                    (niwa-view--call-preserving-window-state update)
                  (funcall update)))
              (redisplay t)
              (should (= clicked (point)))
              (should (= start (window-start window)))
              (should (= 7 (window-vscroll window t)))
              (should (= y (niwa-view--position-screen-y clicked window)))
              (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))))))))
  (outer)
  :doc "Native history click keeps pixel scrolling through status growth and shrink" nil
  :doc "An outer render transaction retains the clicked history viewport too" t)

(niwa-deftest niwa-view-geometry-window-size-roundtrip ()
  (progn
    (skip-unless (display-graphic-p))
    (when (eq ,change 'frame-grow)
      (niwa-view-geometry-test--require-frame-resize))
    (save-window-excursion
      (delete-other-windows)
      (niwa-view-test--with-buffers
        (switch-to-buffer view-buf)
        (let* ((reader-frame (selected-frame))
               (frame-height (frame-text-height reader-frame))
               (frame-resize-pixelwise t)
               (resize-mini-windows nil)
               (unread-command-events nil)
               (window (selected-window))
               (other-frame (make-frame `((width . 100) (height . 45)
                                          (font . ,(frame-parameter nil 'font)))))
               (draft "> active draft\nsecond line")
               (change ,change))
          (unwind-protect
              (progn
                (setq-local header-line-format "Header" tab-line-format nil
                            mode-line-format nil)
                (let ((inhibit-read-only t))
                  (goto-char (point-min))
                  (dotimes (n 100) (insert (format "History line %d.\n" n))))
                (niwa-view-test--insert-composer-draft draft)
                (redisplay t)
                (sit-for 0.02)
                (redisplay t)
                (niwa-view--set-viewport
                 window
                 (niwa-view--viewport-at-y
                  window (point-max)
                  (- (cdr (niwa-view--window-body-geometry window)) 250)))
                (goto-char (window-start window))
                (set-window-vscroll window 7 t)
                (redisplay t)
                (select-frame other-frame)
                (switch-to-buffer view-buf)
                (set-window-buffer (split-window-right) data-buf)
                (goto-char (point-min))
                (redisplay t)
                (sit-for 0.02)
                (redisplay t)
                (when (eq change 'frame-grow)
                  ;; A pixelwise frame configure rounds the minibuffer to a
                  ;; character height.  Settle it before capturing the baseline.
                  (dolist (target (list (1+ frame-height) frame-height))
                    (set-frame-height reader-frame target nil t)
                    ;; The window manager configures asynchronously; this is
                    ;; a progress watchdog, not a frame-resize latency test.
                    (let ((deadline (+ (float-time) 5)))
                      (while (and (/= target (frame-text-height reader-frame))
                                  (< (float-time) deadline))
                        (setq unread-command-events nil)
                        (sit-for 0.02)
                        (redisplay t)))
                    (should (= target (frame-text-height reader-frame))))
                  (set-window-vscroll window 7 t)
                  (redisplay t))
                (with-current-buffer view-buf
                  (let ((start (window-start window))
                        (scroll (window-vscroll window t))
                        (total (window-pixel-height window))
                        (mini-lines (window-body-height (minibuffer-window reader-frame)))
                        calls)
                    (should (= 7 scroll))
                    (should (> (niwa-view--window-blank-pixels window) 80))
                    (let ((window-size-change-functions
                           (list (lambda (target)
                                   (when (eq target window)
                                     (push (window-body-height target t) calls))))))
                      (dotimes (_ 3)
                        (pcase change
                          ('chrome
                           (dolist (header (list (propertize "Header" 'face '(:height 1.8))
                                                "Header"))
                             (setq header-line-format header)
                             (redisplay t)))
                          ('popup
                           (let ((popup (split-window window -8 'below)))
                             (set-window-buffer popup data-buf)
                             (redisplay t)
                             (delete-window popup)
                             (redisplay t)))
                          ('frame-grow
                           (dolist (target (list (+ frame-height 12) frame-height))
                             (set-frame-height reader-frame target nil t)
                             (let ((deadline (+ (float-time) 5)))
                               (while (and (/= target (frame-text-height reader-frame))
                                           (< (float-time) deadline))
                                 ;; Window-manager configure events are asynchronous.
                                 (setq unread-command-events nil)
                                 (sit-for 0.02)
                                 (redisplay t)))
                             (redisplay t)
                             (should (= target (frame-text-height reader-frame)))
                             (should (= mini-lines
                                        (window-body-height (minibuffer-window reader-frame)))))))
                        (sit-for 0.02)
                        (redisplay t)
                        (ert-info ((format "Change %S, native size-hook calls: %S" change calls))
                          (should calls)
                          (should (= total (window-pixel-height window)))
                          (should (= start (window-start window)))
                          (should (= scroll (window-vscroll window t)))
                          (should (equal draft (niwa-view-composer-draft-text)))))))))
            (when (frame-live-p other-frame) (delete-frame other-frame t))
            (select-frame reader-frame)
            (unless (= frame-height (frame-text-height reader-frame))
              (set-frame-height reader-frame frame-height nil t))
            (setq unread-command-events nil))))))
  (change)
  :doc "Three native header shrink/grow cycles retain an reader's viewport" 'chrome
  :doc "Three real popup open/close cycles retain deliberate transcript blank space" 'popup
  :doc "Three native frame grow/shrink cycles retain an reader's pixel viewport" 'frame-grow)

(niwa-deftest niwa-view--position-screen-y ()
  (niwa-view-geometry-test--with-window
    (skip-unless (display-graphic-p))
    (setq-local header-line-format "Header" tab-line-format "Tab")
    (dotimes (_ 80) (insert "history\n"))
    (insert "draft")
    (let* ((window (selected-window))
           (input (- (point-max) (length "draft"))))
      (set-window-start window
                        (save-excursion (goto-char input) (forward-line -10) (point))
                        t)
      (set-window-vscroll window 7 t)
      (redisplay t)
      (setq body-height (window-body-height window t))
      (should (= 7 (window-vscroll window t)))
      (should-not (niwa-view--position-screen-y (point-min) window))
      (let ((before (niwa-view--position-screen-y input window)))
        (should before)
        ;; The native visibility query measures fresh chrome, while window
        ;; header/tab height accessors still report the last redisplay.
        (set (make-local-variable ,chrome)
             (propertize "Larger chrome" 'face '(:height 1.8)))
        (should (= before (niwa-view--position-screen-y input window)))
        (let ((viewport (niwa-view--viewport-at-y window input before)))
          (niwa-view--set-viewport window viewport)
          (should (= 7 (window-vscroll window t)))
          (should (= before (niwa-view--position-screen-y input window)))))))
  (chrome)
  :doc "Fresh header height keeps body Y with nonzero vscroll before redisplay." 'header-line-format
  :doc "Fresh tab height keeps body Y with nonzero vscroll before redisplay." 'tab-line-format)

(niwa-deftest niwa-view--window-body-geometry ()
  (niwa-view-geometry-test--with-window
    (skip-unless (display-graphic-p))
    (setq-local header-line-format "Header" tab-line-format "Tab"
                mode-line-format "Mode")
    (dotimes (_ 80) (insert "history\n"))
    (insert "draft")
    (let ((window (selected-window)))
      (set-window-start window
                        (save-excursion (forward-line -10) (point)) t)
      (set-window-vscroll window 7 t)
      (redisplay t)
      (setq body-height (window-body-height window t))
      (should (= 7 (window-vscroll window t)))
      (set (make-local-variable ,chrome)
           (propertize "Larger chrome" 'face '(:height 1.8)))
      (let ((blank (niwa-view--window-blank-pixels window))
            (row (niwa-view--window-end-row window)))
        (should blank)
        (redisplay t)
        ;; The chrome change deliberately changes the body, not the frame.
        (setq body-height (window-body-height window t))
        (should (= 7 (window-vscroll window t)))
        (should (equal row (niwa-view--window-end-row window)))
        (should (= blank (niwa-view--window-blank-pixels window)))
        (should (= body-height
                   (cdr (niwa-view--window-body-geometry window)))))))
  (chrome)
  :doc "Header changes give the same blank pixels before and after redisplay." 'header-line-format
  :doc "Tab changes give the same blank pixels before and after redisplay." 'tab-line-format
  :doc "Mode-line changes give the same blank pixels before and after redisplay." 'mode-line-format)

(niwa-deftest niwa-view--restore-window-blank-pixels
  (:doc "Shrinking chrome repairs new blank pixels before redisplay.")
  (niwa-view-geometry-test--with-window
    (skip-unless (display-graphic-p))
    (setq-local tab-line-format (propertize "Tab" 'face '(:height 1.8)))
    (dotimes (_ 80) (insert "history\n"))
    (insert "draft")
    (let ((window (selected-window)))
      (set-window-start window
                        (save-excursion (forward-line -10) (point)) t)
      (set-window-vscroll window 7 t)
      (redisplay t)
      (setq body-height (window-body-height window t))
      (let ((blank (niwa-view--window-blank-pixels window)))
        (should blank)
        (should (= 7 (window-vscroll window t)))
        (setq-local tab-line-format "Tab")
        (niwa-view--restore-window-blank-pixels window blank)
        (redisplay t)
        (setq body-height (window-body-height window t))
        (should (= blank (niwa-view--window-blank-pixels window)))))))

(niwa-deftest niwa-view-geometry-image ()
  (let ((chrome ,chrome))
    (niwa-view-geometry-test--with-window
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
          (goto-char (overlay-start
                      (niwa-view-zone-reconcile
                       'status (point) (point)
                       '((:namespace status :id row :body "Visited row")))))
          (set-window-start nil image-start t)
          (set-window-vscroll nil 80 t)
          (redisplay t)
          (let* ((window (selected-window))
                 (before (niwa-view--position-screen-y (point) window))
                 (scroll (window-vscroll window t)))
            (should (= 80 scroll))
            (should before)
            (niwa-view--call-preserving-window-state
             (lambda () (delete-region deletion-start (+ deletion-start 5))))
            (redisplay t)
            (should (looking-at-p "Visited row"))
            (should (= before (niwa-view--position-screen-y (point) window)))
            (should (= image-start (window-start window)))
            (should (< (window-vscroll window t) scroll))
            (should (> (window-vscroll window t) 0))
            ;; The top image is clipped; a fully-visible query rejects it.
            (should (niwa-view--position-screen-y image-start window))
            (should-not (niwa-view--position-screen-y image-start window t)))))))
  (chrome)
  :doc "A partially shown image retains Y with the same start and different vscroll." nil
  :doc "Header and tab heights do not contaminate the image anchor's body Y." t)

(niwa-deftest niwa-view-geometry-image-height
  (:doc "An image-height change preserves native Y without a character-count change.")
  (niwa-view-geometry-test--with-window
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
      (goto-char (overlay-start
                  (niwa-view-zone-reconcile
                   'status (point) (point)
                   '((:namespace status :id row :body "Visited row")))))
      (set-window-start nil image-start t)
      (set-window-vscroll nil 80 t)
      (redisplay t)
      (let* ((window (selected-window))
             (before (niwa-view--position-screen-y (point) window)))
        (should before)
        (niwa-view--call-preserving-window-state
         (lambda ()
           (put-text-property image-start (1+ image-start)
                              'display (funcall image 170))))
        (redisplay t)
        (should (looking-at-p "Visited row"))
        (should (= before (niwa-view--position-screen-y (point) window)))
        (should (= image-start (window-start window)))
        (should (= 50 (window-vscroll window t)))))))

(niwa-deftest niwa-view-geometry-wrapped-end-shrink
  (:doc "Shrinking a wrapped transcript tail retains EOF Y and deliberate blank space.")
  (niwa-view-geometry-test--with-window
    (dotimes (_ 100) (insert "history\n"))
    (let* ((tail (point))
           (window (selected-window))
           (y (/ (window-body-height window t) 2)))
      (insert (make-string (* 3 (window-body-width)) ?x))
      (niwa-view--set-viewport
       window (niwa-view--viewport-at-y window (point-max) y))
      (redisplay t)
      (let ((before (niwa-view--position-screen-y (point-max) window))
            (blank (niwa-view--window-blank-pixels window)))
        (should before)
        (delete-region tail (point-max))
        (niwa-view--restore-window-blank-pixels window blank)
        (redisplay t)
        (should (= before (niwa-view--position-screen-y (point-max) window)))))))

(niwa-deftest niwa-view-geometry-final-image-shrink ()
  (let ((blank ,blank))
    (niwa-view-geometry-test--with-window
      (skip-unless (and (display-graphic-p) (image-type-available-p 'svg)))
      (dotimes (_ 100) (insert "history\n"))
      (insert (propertize "I" 'display
                          (create-image
                           "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='180'><rect width='40' height='180' fill='red'/></svg>"
                           'svg t))
              " final transcript line")
      (let* ((tail (line-beginning-position))
             (window (selected-window))
             (height (cdr (window-text-pixel-size window (point-max) (point-max))))
             (y (- (window-body-height window t) height blank)))
        (should (> y 0))
        (niwa-view--set-viewport
         window (niwa-view--viewport-at-y window (point-max) y))
        (redisplay t)
        (let ((before-blank (niwa-view--window-blank-pixels window))
              (before-y (niwa-view--position-screen-y (point-max) window t)))
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
          (delete-region tail (1+ tail))
          (niwa-view--restore-window-blank-pixels window before-blank)
          (redisplay t)
          (should (equal " final transcript line"
                         (buffer-substring-no-properties
                          tail (point-max))))
          (should (= before-blank (niwa-view--window-blank-pixels window)))
          (let ((row (window-line-height nil window)))
            (should row)
            (should (= blank (- (window-body-height window t)
                                (nth 2 row) (car row)))))
          (should (> (niwa-view--position-screen-y (point-max) window t)
                     before-y))))))
  (blank)
  :doc "Shrinking the final image line keeps a full transcript window full." 0
  :doc "Shrinking the final image line retains deliberate blank pixels." 60)

(niwa-deftest niwa-view-geometry-end-row
  (:doc "Fresh EOF rows match native cursor rows before and after mutation.")
  (niwa-view-geometry-test--with-window
    (dotimes (_ 100) (insert "history\n"))
    (insert "draft")
    (let ((window (selected-window)))
      (niwa-view--set-viewport
       window (niwa-view--viewport-at-y window (point-max) 300))
      (set-window-vscroll window 7 t)
      (redisplay t)
      (let* ((native (window-line-height nil window))
             (expected (cons (nth 2 native) (car native))))
        (should (equal expected (niwa-view--window-end-row window)))
        (delete-region (- (point-max) 5) (point-max))
        ;; Query before redisplay; the final line is now empty.
        (should (equal expected (niwa-view--window-end-row window)))
        (redisplay t)
        (let ((row (window-line-height nil window)))
          (should (equal expected (cons (nth 2 row) (car row))))))
      (goto-char (point-min))
      (set-window-start window (point-min) t)
      (redisplay t)
      (should-not (niwa-view--window-end-row window)))))

(provide 'test-niwa-view-geometry)
;;; test-niwa-view-geometry.el ends here
