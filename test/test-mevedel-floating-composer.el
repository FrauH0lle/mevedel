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
  (:doc "`mevedel-floating-composer--place' spans the host and sits on its bottom")
  ,test
  (test)
  :doc "covers the text area and both fringes, flush on the bottom, top line added"
  (should (equal '(816 122 2 498)
                 (mevedel-floating-composer--place '(10 20 810 620) '(8 8 nil nil) 20 100 22)))
  :doc "caps the draft at half the host's text height, then adds the top line"
  (should (equal '(816 322 2 298)
                 (mevedel-floating-composer--place '(10 20 810 620) '(8 8 nil nil) 20 500 22)))
  :doc "never shrinks the draft below one line"
  (should (equal '(816 42 2 578)
                 (mevedel-floating-composer--place '(10 20 810 620) '(8 8 nil nil) 20 5 22)))
  :doc "a terminal host has no fringes; its cells are pixels"
  (should (equal '(110 6 0 34)
                 (mevedel-floating-composer--place '(0 0 110 40) '(0 0 nil nil) 1 5 1))))

(mevedel-deftest mevedel-floating-composer--top-line ()
  (with-temp-buffer
    (save-window-excursion
      (let ((window (selected-window))
            (button (string-trim mevedel-floating-composer--button)))
        (set-window-buffer window (current-buffer))
        (insert "transcript\n")
        (let ((start (point)))
          (dotimes (i 30) (insert (format "draft line %d\n" i)))
          (cl-letf (((symbol-function 'mevedel-floating-composer--start) (lambda () start)))
            ,test)))))
  (test)
  :doc "a draft shown from its start has only the button, ruled on a terminal"
  (progn
    (set-window-start window start)
    (let ((line (mevedel-floating-composer--top-line)))
      (should-not (string-search "more" line))
      (should (string-suffix-p (concat button " ") line))
      (should (string-prefix-p "───" line))
      (should (= (window-total-width window) (string-width line)))))
  :doc "a capped draft scrolled down counts the draft lines above it"
  (progn
    (set-window-start window (save-excursion (goto-char start) (forward-line 7) (point)))
    (let ((line (mevedel-floating-composer--top-line)))
      (should (string-prefix-p " ↑ 7 more ─" line))
      (should (string-suffix-p (concat button " ") line))
      (should (= (window-total-width window) (string-width line)))))
  :doc "the button returns to the bottom on mouse-1"
  (should (eq #'mevedel-floating-composer-back-to-bottom
              (keymap-lookup (get-text-property 1 'local-map mevedel-floating-composer--button)
                             "<header-line> <mouse-1>"))))

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
  (:doc "pins only a floating composer's window scrolled above the rail")
  (save-window-excursion
    (let ((window (split-window))
          pinned)
      (cl-letf (((symbol-function 'mevedel-floating-composer--pin)
                 (lambda (window) (push window pinned)))
                ((symbol-function 'mevedel-floating-composer--start) (lambda () 10)))
        (mevedel-floating-composer--on-scroll (selected-window) 1)
        (set-window-parameter window 'mevedel-floating-composer t)
        ;; Redisplay scrolling down to show point is left alone.
        (mevedel-floating-composer--on-scroll window 20)
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
