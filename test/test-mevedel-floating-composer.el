;;; test-mevedel-floating-composer.el --- Floating composer tests -*- lexical-binding: t -*-

;;; Commentary:

;; Batch cases cover the pure placement.  Child frames are not assertable in
;; batch; the native cases live in test-mevedel-floating-composer-graphical.el.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-floating-composer)
(require 'mevedel-view-segments)

(defvar mevedel-view--agent-transcript-p)

(mevedel-deftest mevedel-floating-composer--place
  (:doc "`mevedel-floating-composer--place' insets, floats and caps the frame")
  ,test
  (test)
  :doc "insets two columns per side and floats half a line above the bottom"
  (should (equal '(754 100 30 504)
                 (mevedel-floating-composer--place '(10 20 810 620) 10 20 100 3)))
  :doc "caps the height at half the host's text height"
  (should (equal '(754 300 30 304)
                 (mevedel-floating-composer--place '(10 20 810 620) 10 20 500 3)))
  :doc "never shrinks below one line"
  (should (equal '(754 20 30 584)
                 (mevedel-floating-composer--place '(10 20 810 620) 10 20 5 3)))
  :doc "a borderless frame uses the full inset width"
  (should (equal '(760 100 30 510)
                 (mevedel-floating-composer--place '(10 20 810 620) 10 20 100 0)))
  :doc "a narrow host shrinks the inset so the frame stays inside it"
  (pcase-let ((`(,width ,_ ,x ,_)
               (mevedel-floating-composer--place '(0 0 40 600) 10 20 100 3)))
    (should (= 10 width))
    (should (= 12 x))
    (should (<= (+ x width 6) 40)))
  :doc "a host narrower than one column drops the inset entirely"
  (pcase-let ((`(,width ,_ ,x ,_)
               (mevedel-floating-composer--place '(0 0 8 600) 10 20 100 3)))
    (should (= 0 x))
    (should (= 2 width))))

(mevedel-deftest mevedel-floating-composer--host-p ()
  (with-temp-buffer
    (save-window-excursion
      (let ((window (selected-window))
            (mevedel-floating-composer t)
            (historical nil))
        (set-window-buffer window (current-buffer))
        (setq-local mevedel-view--agent-transcript-p nil)
        (cl-letf (((symbol-function 'mevedel-view-historical-segment-p)
                   (lambda () historical)))
          ,test))))
  (test)
  :doc "a top-level window showing the live view can host"
  (should (mevedel-floating-composer--host-p window))
  :doc "nothing can host while the option is off"
  (let ((mevedel-floating-composer nil))
    (should-not (mevedel-floating-composer--host-p window)))
  :doc "an agent transcript view has none"
  (progn
    (setq-local mevedel-view--agent-transcript-p t)
    (should-not (mevedel-floating-composer--host-p window)))
  :doc "a historical segment has none"
  (progn
    (setq historical t)
    (should-not (mevedel-floating-composer--host-p window)))
  :doc "a window showing another buffer cannot host"
  (with-temp-buffer
    (should-not (mevedel-floating-composer--host-p window)))
  :doc "a child frame's window, such as a note frame's, cannot host"
  (cl-letf (((symbol-function 'frame-parent) (lambda (&optional _frame) 'parent)))
    (should-not (mevedel-floating-composer--host-p window))))

(mevedel-deftest mevedel-floating-composer--scroll-target ()
  (with-temp-buffer
    (save-window-excursion
      (let* ((window (selected-window))
             (other (split-window)))
        (set-window-buffer window (current-buffer))
        (setq-local other-window-scroll-default
                    #'mevedel-floating-composer--scroll-target)
        (unwind-protect
            ,test
          (set-window-parameter window 'mevedel-floating-composer nil)))))
  (test)
  :doc "from the floating composer it is the host"
  (progn
    (set-window-parameter window 'mevedel-floating-composer t)
    (setq-local mevedel-floating-composer--host other)
    (should (eq other (mevedel-floating-composer--scroll-target))))
  :doc "elsewhere the global default decides, without recursing into itself"
  (should (eq other (mevedel-floating-composer--scroll-target))))

(mevedel-deftest mevedel-floating-composer--on-scroll
  (:doc "pins only a floating composer's window")
  (save-window-excursion
    (let ((window (split-window))
          pinned)
      (cl-letf (((symbol-function 'mevedel-floating-composer--pin)
                 (lambda (window) (push window pinned))))
        (mevedel-floating-composer--on-scroll (selected-window) 1)
        (set-window-parameter window 'mevedel-floating-composer t)
        (mevedel-floating-composer--on-scroll window 1)
        (should (equal (list window) pinned))))))

(mevedel-deftest mevedel-floating-composer--remove-all
  (:doc "removes every view's floating composer and the host check")
  (let ((views (list (generate-new-buffer " *fc-a*") (generate-new-buffer " *fc-b*")))
        removed)
    (unwind-protect
        (progn
          (dolist (buffer views)
            (with-current-buffer buffer
              (setq-local mevedel-floating-composer--frame 'frame)))
          (add-hook 'window-buffer-change-functions
                    #'mevedel-floating-composer--check-hosts)
          (cl-letf (((symbol-function 'mevedel-floating-composer--remove)
                     (lambda () (push (current-buffer) removed))))
            (mevedel-floating-composer--remove-all))
          (should (seq-set-equal-p views removed))
          (should-not (memq #'mevedel-floating-composer--check-hosts
                            (default-value 'window-buffer-change-functions))))
      (remove-hook 'window-buffer-change-functions
                   #'mevedel-floating-composer--check-hosts)
      (mapc #'kill-buffer views))))

;;; test-mevedel-floating-composer.el ends here
