;;; test-mevedel-view-native.el --- Native presentation tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view-native)

(mevedel-deftest mevedel-view-native--markup
  (:doc "Preserves text and foreground runs without interpreting label markup.")
  (let* ((sample (concat "A<&" (propertize "B>" 'face '(:foreground "#123456"))))
         (markup (mevedel-view-native--markup sample "#abcdef")))
    (should (equal markup (concat "<span foreground=\"#abcdef\">A&lt;&amp;</span>"
                                  "<span foreground=\"#123456\">B&gt;</span>")))
    ;; Tool labels quote model arguments, which may hold control characters.
    (should (equal (mevedel-view-native--markup (concat "a" (string 27) "b") "#000000")
                   "<span foreground=\"#000000\">ab</span>"))))

(mevedel-deftest mevedel-view-native--timeline
  (:doc "Portable glyph samples preserve their existing cadence and text.")
  (cl-letf (((symbol-function 'mevedel-view-animation--colors)
             (lambda (&rest _) '("#ffffff" . "#000000"))))
    (let ((timeline (mevedel-view-native--timeline 'ascii "Work" 0.24 'default nil)))
      (should (= (aref timeline 0) 0.96))
      (should (equal (aref (aref timeline 1) 1)
                     "<span foreground=\"#ffffff\">- Work</span>"))
      (should (equal (aref (aref timeline 2) 1)
                     "<span foreground=\"#ffffff\">\\ Work</span>")))))

(mevedel-deftest mevedel-view-native-available-p
  (:doc "Disabled and batch displays never trigger module construction.")
  (let ((mevedel-view-native-enabled nil))
    (cl-letf (((symbol-function 'mevedel-view-native--load)
               (lambda () (ert-fail "Unexpected native load"))))
      (should-not (mevedel-view-native-available-p (selected-frame)))
      (let ((mevedel-view-native-enabled t))
        (should-not (mevedel-view-native-available-p (selected-frame)))))))

(mevedel-deftest mevedel-view-native--load
  (:doc "Build failures leave no partial artifact and are not retried per frame.")
  (let* ((root (make-temp-file "mevedel-native-build-" t))
         (mevedel-user-dir root)
         (mevedel-view-native--directory root)
         (mevedel-view-native--load-state nil)
         (calls 0))
    (unwind-protect
        (progn
          (make-directory (file-name-concat root "native"))
          (with-temp-file (file-name-concat root "native/mevedel-view-native.c")
            (insert "source"))
          (cl-letf (((symbol-function 'executable-find) (lambda (_) "/fake/compiler"))
                    ((symbol-function 'call-process)
                     (lambda (program &rest _)
                       (cl-incf calls)
                       (if (equal program "pkg-config") 0 1))))
            (should-not (mevedel-view-native--load))
            (should (string-match-p "compilation failed" mevedel-view-native--load-state))
            (should-not (mevedel-view-native--load))
            (should (= calls 2))
            (should-not (directory-files (file-name-concat root "native") nil "^build-"))))
      (delete-directory root t))))

(mevedel-deftest mevedel-view-native--include-flags
  (:doc "Finds emacs-module.h in the running Emacs's prefix or build tree.")
  (let* ((root (make-temp-file "mevedel-native-include-" t))
         (bin (file-name-as-directory (file-name-concat root "bin")))
         (include (file-name-concat root "include")))
    (unwind-protect
        (progn
          (make-directory bin)
          (make-directory include)
          (let ((invocation-directory bin))
            (should-not (mevedel-view-native--include-flags))
            (with-temp-file (file-name-concat include "emacs-module.h"))
            (should (equal (mevedel-view-native--include-flags)
                           (list (concat "-I" (expand-file-name "../include" bin)))))
            (with-temp-file (file-name-concat bin "emacs-module.h"))
            (should (= 2 (length (mevedel-view-native--include-flags))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-view-native--visible-p
  (:doc "Includes partially clipped labels in a window's visible buffer range.")
  (with-temp-buffer
    (insert (make-string 100 ?x))
    (let ((target (cons (copy-marker 10) (copy-marker 20))))
      (cl-letf (((symbol-function 'window-start) (lambda (_) 15))
                ((symbol-function 'window-end) (lambda (_) 80)))
        (should (mevedel-view-native--visible-p target 'window))
        (set-marker (cdr target) 14)
        (should-not (mevedel-view-native--visible-p target 'window))
        (set-marker (car target) nil)
        (should-not (mevedel-view-native--visible-p target 'window))))))

(mevedel-deftest mevedel-view-native--placement
  (:doc "Selections and a cursor on the animated span retain the ordinary renderer.")
  (with-temp-buffer
    (insert "Working...\n")
    (let ((target (cons (copy-marker 1) (copy-marker 11))))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (_) t))
                ((symbol-function 'window-point) (lambda (_) 1)))
        (should-not (mevedel-view-native--placement target (selected-window))))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (_) t))
                ((symbol-function 'window-point) (lambda (_) 12))
                ((symbol-function 'use-region-p) (lambda () t)))
        (should-not (mevedel-view-native--placement target (selected-window)))))))

(mevedel-deftest mevedel-view-native-sync
  (:doc "Reuses a stable surface, replaces changed text and closes on stop.")
  (with-temp-buffer
    (insert "Working...\n")
    (let* ((noninteractive nil)
           (mevedel-view-native--presenting t)
           (target (cons (copy-marker 1) (copy-marker 11)))
           (mevedel-view-native--views (make-hash-table :test #'eq))
           (opens 0) closed)
      (unwind-protect
          (cl-letf (((symbol-function 'get-buffer-window-list) (lambda (&rest _) '(window)))
                    ((symbol-function 'pos-visible-in-window-p) (lambda (&rest _) t))
                    ((symbol-function 'mevedel-view-native--visible-p) (lambda (&rest _) t))
                    ((symbol-function 'mevedel-view-native--placement)
                     (lambda (&rest _) '(frame [8 42 80 21 16] "Mono 16px")))
                    ((symbol-function 'mevedel-view-native-available-p) (lambda (_) t))
                    ((symbol-function 'frame-parameter) (lambda (&rest _) "123"))
                    ((symbol-function 'mevedel-view-animation--colors)
                     (lambda (&rest _) '("#ffffff" . "#000000")))
                    ((symbol-function 'mevedel-view-native--open)
                     (lambda (&rest _) (cl-incf opens)))
                    ((symbol-function 'mevedel-view-native--move) (lambda (&rest _) t))
                    ((symbol-function 'mevedel-view-native--close) (lambda (handle) (push handle closed))))
            (let ((specs (list (list target 'ascii "Working" 'default 0.24))))
              (should (equal (mevedel-view-native-sync specs 0 #'ignore) (list target)))
              (should (equal (mevedel-view-native-sync specs 1 #'ignore) (list target)))
              (should (= opens 1))
              (should-not closed)
              (dolist (inhibit-redisplay '(t nil))
                (let ((mevedel-view-native--presenting nil))
                (mevedel-view-native-sync nil 1 #'ignore)
                (should-not closed)
                (mevedel-view-native-sync specs 1 #'ignore)
                (should (= opens 1))))
              (mevedel-view-native--before-redisplay 'window)
              (should (= opens 1))
              (should-not closed)
              ;; Semantic metadata rebuilds replace markers around identical text.
              (set-marker (car target) nil)
              (set-marker (cdr target) nil)
              (setq target (cons (copy-marker 1) (copy-marker 11)))
              (setf (caar specs) target)
              (should (equal (mevedel-view-native-sync specs 1.5 #'ignore) (list target)))
              (should (= opens 1))
              (should-not closed)
              (setf (nth 2 (car specs)) "Changed")
              (mevedel-view-native-sync specs 2 #'ignore)
              (should (= opens 2))
              (should (equal closed '(1)))
              (mevedel-view-native-sync nil 2 #'ignore)
              (should (equal closed '(2 1)))
              (should-not mevedel-view-native--entries)
              (should (= 0 (hash-table-count mevedel-view-native--views)))))
        (mevedel-view-native-stop)))))

(mevedel-deftest mevedel-view-native-sync/teardown
  (:doc "Queued teardown marks the view's windows for redisplay.")
  (with-temp-buffer
    (let ((noninteractive nil)
          (mevedel-view-native--views (make-hash-table :test #'eq))
          forced)
      (setq mevedel-view-native--entries '(((target window) signature handle)))
      (unwind-protect
          (cl-letf (((symbol-function 'force-window-update)
                     (lambda (object) (push object forced)))
                    ((symbol-function 'mevedel-view-native--close) #'ignore))
            (mevedel-view-native-sync nil 0 #'ignore)
            (should (equal forced (list (current-buffer))))
            (should mevedel-view-native--entries)
            (should (memq #'mevedel-view-native--before-redisplay
                          pre-redisplay-functions)))
        (cl-letf (((symbol-function 'mevedel-view-native--close) #'ignore))
          (mevedel-view-native-stop))))))

(mevedel-deftest mevedel-view-native-sync/failure
  (:doc "An error after opening a surface closes it and keeps text animation.")
  (with-temp-buffer
    (insert "Working...\nTesting...\n")
    (let* ((noninteractive nil)
           (mevedel-view-native--presenting t)
           (first (cons (copy-marker 1) (copy-marker 11)))
           (second (cons (copy-marker 12) (copy-marker 22)))
           (mevedel-view-native--views (make-hash-table :test #'eq))
           (opens 0) closed)
      (unwind-protect
          (cl-letf (((symbol-function 'get-buffer-window-list) (lambda (&rest _) '(window)))
                    ((symbol-function 'mevedel-view-native--visible-p) (lambda (&rest _) t))
                    ((symbol-function 'mevedel-view-native--placement)
                     (lambda (&rest _) '(frame [8 42 80 21 16] "Mono 16px")))
                    ((symbol-function 'mevedel-view-native-available-p) (lambda (_) t))
                    ((symbol-function 'frame-parameter) (lambda (&rest _) "123"))
                    ((symbol-function 'mevedel-view-animation--colors)
                     (lambda (&rest _) '("#ffffff" . "#000000")))
                    ((symbol-function 'mevedel-view-native--timeline)
                     (lambda (_style label &rest _)
                       (if (equal label "Testing") (error "Injected timeline failure") [1.0])))
                    ((symbol-function 'mevedel-view-native--open)
                     (lambda (&rest _) (cl-incf opens)))
                    ((symbol-function 'mevedel-view-native--move) (lambda (&rest _) t))
                    ((symbol-function 'mevedel-view-native--close)
                     (lambda (handle) (push handle closed))))
            (should-not (mevedel-view-native-sync
                         (list (list first 'ascii "Working" 'default 0.24)
                               (list second 'ascii "Testing" 'default 0.24))
                         0 #'ignore))
            (should (= opens 1))
            (should (equal closed '(1)))
            (should-not mevedel-view-native--entries))
        (mevedel-view-native-stop)))))

(mevedel-deftest mevedel-view-native--clear
  (:doc "Closes each live handle once and leaves no retained surface.")
  (with-temp-buffer
    (setq mevedel-view-native--entries '((a config first) (b config second)))
    (let (closed)
      (cl-letf (((symbol-function 'mevedel-view-native--close) (lambda (h) (push h closed))))
        (mevedel-view-native--clear)
        (mevedel-view-native--clear)
        (should (equal closed '(second first)))
        (should-not mevedel-view-native--entries)))))

(mevedel-deftest mevedel-view-native--invalidate
  (:doc "Hides old pixels immediately and coalesces one deferred placement check.")
  (with-temp-buffer
    (let ((mevedel-view-native--views (make-hash-table :test #'eq))
          (calls 0))
      (unwind-protect
          (progn
            (puthash (current-buffer) (lambda () (cl-incf calls)) mevedel-view-native--views)
            (mevedel-view-native--invalidate)
            (let ((old mevedel-view-native--rearm-timer))
              (mevedel-view-native--invalidate)
              (should-not (memq old timer-list)))
            (should mevedel-view-native--settling)
            (let ((timer mevedel-view-native--rearm-timer))
              (cancel-timer timer)
              (funcall (timer--function timer)))
            (should (= calls 1))
            (should-not mevedel-view-native--settling))
        (mevedel-view-native-stop)))))

(mevedel-deftest mevedel-view-native--window-change
  (:doc "Checks only subscribed live views after window changes.")
  (with-temp-buffer
    (let ((mevedel-view-native--views (make-hash-table :test #'eq)) seen)
      (puthash (current-buffer) #'ignore mevedel-view-native--views)
      (cl-letf (((symbol-function 'mevedel-view-native--invalidate)
                 (lambda (&rest _) (push (current-buffer) seen))))
        (mevedel-view-native--window-change nil)
        (should (equal seen (list (current-buffer))))))))

(mevedel-deftest mevedel-view-native-stop
  (:doc "Removes pending timers and observers when the view no longer animates.")
  (with-temp-buffer
    (let ((mevedel-view-native--views (make-hash-table :test #'eq)))
      (puthash (current-buffer) #'ignore mevedel-view-native--views)
      (mevedel-view-native--invalidate)
      (let ((timer mevedel-view-native--rearm-timer))
        (setq mevedel-view-native--pending '(nil 0 ignore))
        (mevedel-view-native-stop)
        (should-not mevedel-view-native--pending)
        (should-not (memq timer timer-list))
        (should-not mevedel-view-native--rearm-timer)
        (should (= 0 (hash-table-count mevedel-view-native--views)))))))

(mevedel-deftest mevedel-view-native-sample
  (:doc "Retains the last submitted phase when geometry invalidation hides pixels.")
  (with-temp-buffer
    (let ((target (cons (copy-marker 1) (copy-marker 1))))
      (setq mevedel-view-native--entries (list (list (list target 'window) nil 'handle)))
      (cl-letf (((symbol-function 'mevedel-view-native--sample) (lambda (_) 1.25))
                ((symbol-function 'mevedel-view-native--close) #'ignore))
        (should (= 1.25 (mevedel-view-native-sample target)))
        (mevedel-view-native--clear)
        (should (= 1.25 (mevedel-view-native-sample target)))
        (mevedel-view-native-stop)
        (should-not (mevedel-view-native-sample target))))))

(mevedel-deftest mevedel-view-native--before-redisplay
  (:doc "Detects inhibited edits and changed geometry without reacting to decoration.")
  (progn
    (with-temp-buffer
      (insert "Working...\n")
      (let* ((target (cons (copy-marker 1) (copy-marker 11)))
             (placement '(frame [8 42 80 21 16] "Mono 16px"))
             (mevedel-view-native--content-tick (buffer-chars-modified-tick))
             closed)
        (setq mevedel-view-native--entries
              (list (list (list target 'window) nil 'handle placement "Working...")))
        (cl-letf (((symbol-function 'mevedel-view-native--placement)
                   (lambda (&rest _) placement))
                  ((symbol-function 'mevedel-view-native--close)
                   (lambda (handle) (push handle closed))))
          (unwind-protect
              (progn
                (with-silent-modifications (put-text-property 1 2 'display "W"))
                (mevedel-view-native--before-redisplay 'window)
                (should-not closed)
                (let ((inhibit-modification-hooks t)) (insert "metadata changed\n"))
                (mevedel-view-native--before-redisplay 'window)
                (should-not closed)
                (let ((inhibit-modification-hooks t))
                  (goto-char (point-min)) (insert "new line\n"))
                (mevedel-view-native--before-redisplay 'window)
                (should (equal closed '(handle)))
                (should mevedel-view-native--settling))
            (mevedel-view-native-stop)))))
    (with-temp-buffer
      (let ((mevedel-view-native--content-tick (buffer-chars-modified-tick)) closed)
        (setq mevedel-view-native--entries
              (list (list (list (cons (copy-marker 1) (copy-marker 1)) 'window)
                          nil 'handle 'old-placement "")))
        (cl-letf (((symbol-function 'mevedel-view-native--placement) (lambda (&rest _) nil))
                  ((symbol-function 'mevedel-view-native--close) (lambda (h) (push h closed))))
          (unwind-protect
              (progn
                (mevedel-view-native--before-redisplay 'window)
                (should (equal closed '(handle)))
                (should mevedel-view-native--settling))
            (mevedel-view-native-stop)))))))

(mevedel-deftest mevedel-view-native-unload-function
  (:doc "Feature teardown releases active views and not-yet-presented hooks.")
  (let ((mevedel-view-native--views (make-hash-table :test #'eq)))
    (with-temp-buffer
      (let ((active (current-buffer)))
        (puthash active #'ignore mevedel-view-native--views)
        (mevedel-view-native--invalidate)
        (with-temp-buffer
          (setq mevedel-view-native--pending '(nil 0 ignore))
          (add-hook 'pre-redisplay-functions #'mevedel-view-native--before-redisplay nil t)
          (should-not (mevedel-view-native-unload-function))
          (should-not mevedel-view-native--pending)
          (should-not (memq #'mevedel-view-native--before-redisplay pre-redisplay-functions))
          (should-not (buffer-local-value 'mevedel-view-native--rearm-timer active))
          (should (= 0 (hash-table-count mevedel-view-native--views))))))))

(provide 'test-mevedel-view-native)
;;; test-mevedel-view-native.el ends here
