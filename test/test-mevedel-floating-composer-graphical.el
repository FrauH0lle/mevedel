;;; test-mevedel-floating-composer-graphical.el --- Native floating composer tests -*- lexical-binding: t -*-

;;; Commentary:

;; A real view in a graphical frame, driven through the command loop.  Each
;; case reads what the user sees: the selected window, the cursor's draft
;; offset, whether the floating composer is shown, and the host window's
;; start, vertical scroll, point and rail width.  Batch runs skip them.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mwheel)
(require 'mevedel-floating-composer)
(require 'mevedel-view)
(require 'mevedel-view-agent)
(require 'mevedel-view-composer)
(require 'mevedel-view-interaction)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)

(defconst mevedel-floating-composer-test--draft "> active draft\nsecond line"
  "Multiline draft whose first editable character is `>'.")

(defmacro mevedel-floating-composer-test--with-view (&rest body)
  "Run BODY in a graphical view showing its composer at the window bottom.
The draft holds point at offset 4.  BODY can set `agents', which the
background panel reads; `data-buf' and `view-buf' are bound too."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (display-graphic-p))
     (save-window-excursion
       (delete-other-windows)
       (mevedel-view-test--with-buffers
         (dotimes (i 80)
           (mevedel-view-test--insert-data
            data-buf (format "Line %d of the earlier response.\n" i) 'response))
         (switch-to-buffer view-buf)
         (let ((session (mevedel-session--create :name "floating"))
               (agents nil)
               (mevedel-view-spinner-animate nil)
               (scroll-preserve-screen-position t)
               (mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))
               (mouse-wheel-progressive-speed nil))
           (setq-local mevedel--session session)
           (with-current-buffer data-buf (setq-local mevedel--session session))
           (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _) t))
                     ((symbol-function 'mevedel-view--agent-status-collect)
                      (lambda () agents)))
             (mevedel-view--full-rerender)
             (mevedel-view-test--insert-composer-draft
              mevedel-floating-composer-test--draft 4)
             (mevedel-floating-composer-test--to-bottom)
             ,@body))))))

(defun mevedel-floating-composer-test--to-bottom ()
  "Show the buffer end on the selected window's last row."
  (let* ((window (selected-window))
         (end (point-max))
         (y (- (cdr (mevedel-view--window-body-geometry window))
               (save-excursion (goto-char end) (line-pixel-height)))))
    (mevedel-view--set-viewport window (mevedel-view--viewport-at-y window end y))
    (redisplay t)))

(defun mevedel-floating-composer-test--deliver (events command)
  "Deliver EVENTS through the command loop and wait for COMMAND."
  (let* ((done nil)
         (hook (lambda ()
                 (when (eq this-command command)
                   (setq done t)
                   (run-at-time 0 nil #'exit-recursive-edit))))
         (timer (run-at-time 30 nil #'exit-recursive-edit)))
    (add-hook 'post-command-hook hook)
    (unwind-protect
        (progn
          (setq unread-command-events events)
          (recursive-edit))
      (remove-hook 'post-command-hook hook)
      (cancel-timer timer)
      (setq unread-command-events nil))
    (should done)
    (mevedel-floating-composer-test--settle)))

(defun mevedel-floating-composer-test--keys (keys command)
  "Type KEYS through the command loop, which runs COMMAND."
  (mevedel-floating-composer-test--deliver (listify-key-sequence (kbd keys)) command))

(defun mevedel-floating-composer-test--wheel (direction window)
  "Scroll WINDOW one notch in DIRECTION through the command loop."
  (mevedel-floating-composer-test--deliver
   (list (list direction (posn-at-point (window-start window) window)))
   'mwheel-scroll))

(defun mevedel-floating-composer-test--settle ()
  "Run pending zero-delay timers and redisplay."
  (let ((unread-command-events nil))
    (sit-for 0.05)
    (redisplay t)))

(defun mevedel-floating-composer-test--window ()
  "Return the shown floating composer's window, or nil."
  (when-let* ((window (mevedel-floating-composer--window))
              ((frame-visible-p (window-frame window))))
    window))

(defun mevedel-floating-composer-test--offset ()
  "Return point's offset into the draft."
  (- (point) (mevedel-view--input-start)))

(defun mevedel-floating-composer-test--scroll-away (in-draft)
  "Scroll the selected window a screen up with the cursor IN-DRAFT or not."
  (unless in-draft
    (goto-char (save-excursion (goto-char (mevedel-view--input-start))
                               (forward-line -6)
                               (point))))
  (mevedel-floating-composer-test--keys "M-v" 'scroll-down-command))

(defun mevedel-floating-composer-test--host-state (host)
  "Return HOST's (START VSCROLL POINT RAIL-WIDTH)."
  (list (window-start host) (window-vscroll host t) (window-point host)
        (nth 3 mevedel-view--spinner-rendered-state)))

(defun mevedel-floating-composer-test--rail ()
  "Return the text of the composer rail."
  (let ((region (mevedel-view-zone-region 'progress)))
    (buffer-substring-no-properties (overlay-start region) (overlay-end region))))


;;
;;; Cursor hand-over

(mevedel-deftest mevedel-floating-composer/hand-over ()
  (mevedel-floating-composer-test--with-view
    (let ((host (selected-window)))
      ,test))
  (test)
  :doc "scrolling away with the cursor in the draft moves it into the floating composer"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (let* ((window (mevedel-floating-composer-test--window))
           (frame (and window (window-frame window))))
      (should window)
      (should (eq window (selected-window)))
      (should (eq (window-frame host) (frame-parent frame)))
      (should (= 4 (mevedel-floating-composer-test--offset)))
      (should (equal mevedel-floating-composer-test--draft (mevedel-view--input-text)))
      ;; The host stays a history reader for ADR 0126.
      (should (< (window-point host) (mevedel-view--input-start)))
      (should-not (window-parameter host 'mevedel-view-scroll-return))
      (pcase-let ((`(,left ,_ ,right ,bottom) (window-inside-pixel-edges host))
                  (`(,x . ,y) (frame-position frame)))
        (should (> x left))
        ;; The inset is the same on both sides.
        (should (= (- x left) (- right x (frame-pixel-width frame))))
        (should (< (+ y (frame-pixel-height frame)) bottom)))))
  :doc "scrolling the host back returns the cursor inline at the same offset"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (let ((window (selected-window))
          (notches 0))
      (insert "x")
      (while (and (< notches 60) (eq window (selected-window)))
        (mevedel-floating-composer-test--wheel 'wheel-down host)
        (cl-incf notches))
      (should (eq host (selected-window)))
      (should (= 5 (mevedel-floating-composer-test--offset)))
      (should-not (mevedel-floating-composer-test--window))
      (should (equal "> acxtive draft\nsecond line" (mevedel-view--input-text)))))
  :doc "scrolling away with the cursor in the transcript keeps it there"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (should (mevedel-floating-composer-test--window))
    (should (eq host (selected-window)))
    (should (< (point) (mevedel-view--input-start))))
  :doc "background updates never move the cursor out of the floating composer"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (let ((window (selected-window))
          (start (window-start host)))
      (mevedel-view-test--insert-data data-buf "Arrived meanwhile.\n" 'response)
      (mevedel-view--full-rerender)
      (setq agents (list (list :path "/root/arriving" :status 'running :started-at 990.0)))
      (mevedel-view--render-status)
      (mevedel-floating-composer-test--settle)
      (should (eq window (selected-window)))
      (should (= 4 (mevedel-floating-composer-test--offset)))
      (should (= start (window-start host)))))
  :doc "background updates never move a transcript cursor into the floating composer"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (let ((point (point)))
      (mevedel-view-test--insert-data data-buf "Arrived meanwhile.\n" 'response)
      (mevedel-view--full-rerender)
      (mevedel-floating-composer-test--settle)
      (should (eq host (selected-window)))
      (should (= point (point)))))
  :doc "turning the option off removes it and returns the cursor inline"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (should (mevedel-floating-composer-test--window))
    (unwind-protect
        (customize-set-variable 'mevedel-floating-composer nil)
      (should (eq host (selected-window)))
      (should (= 4 (mevedel-floating-composer-test--offset)))
      (should-not (mevedel-floating-composer--window))
      (customize-set-variable 'mevedel-floating-composer t))))


;;
;;; Geometry exclusion

(mevedel-deftest mevedel-floating-composer/host-geometry ()
  (let (states)
    (dolist (option '(nil t))
      (let ((mevedel-floating-composer option))
        (mevedel-floating-composer-test--with-view
          (let ((host (selected-window)))
            ,setup
            (mevedel-floating-composer-test--scroll-away nil)
            (should (eq option (and (mevedel-floating-composer-test--window) t)))
            ,stimulus
            (mevedel-floating-composer-test--settle)
            (should (eq host (selected-window)))
            (push (mevedel-floating-composer-test--host-state host) states)))))
    (should (equal (car states) (cadr states))))
  (setup stimulus)
  :doc "streamed output leaves the host as without the floating composer"
  (progn
    (mevedel-view-stream-begin-turn
     (mevedel-view--history-insertion-marker)
     (with-current-buffer data-buf (point-max)))
    (mevedel-view-test--insert-data data-buf "Streaming begins.\n" 'response)
    (mevedel-view--render-stream-update data-buf))
  (progn
    (mevedel-view-test--insert-data data-buf "More streamed text.\nAnd a line.\n" 'response)
    (mevedel-view--render-stream-update data-buf))
  :doc "status rows appearing leave the host as without the floating composer"
  nil
  (progn
    (setq agents (list (list :path "/root/one" :status 'running :started-at 990.0)
                       (list :path "/root/two" :status 'running :started-at 990.0)))
    (mevedel-view--render-status))
  :doc "status rows shrinking leave the host as without the floating composer"
  (progn
    (setq agents (list (list :path "/root/one" :status 'running :started-at 990.0)
                       (list :path "/root/two" :status 'running :started-at 990.0)))
    (mevedel-view--render-status))
  (progn
    (setq agents (list (list :path "/root/one" :status 'running :started-at 990.0)))
    (mevedel-view--render-status))
  :doc "a card arriving leaves the host as without the floating composer"
  nil
  (progn
    (mevedel-view--interaction-register
     '(:kind ask :id ask :count 1 :body "A question for the user?\n"))
    (should (string-search "A question for the user?" (buffer-string)))))


;;
;;; Rail

(mevedel-deftest mevedel-floating-composer--show ()
  (mevedel-floating-composer-test--with-view
    (let ((host (selected-window))
          (measured 0)
          (placed 0)
          (measure (symbol-function 'window-text-pixel-size))
          (place (symbol-function 'set-frame-size-and-position-pixelwise)))
      (mevedel-floating-composer-test--scroll-away t)
      (let ((window (mevedel-floating-composer-test--window)))
        (should (eq window (selected-window)))
        (cl-letf (((symbol-function 'window-text-pixel-size)
                   (lambda (&optional w &rest args)
                     (when (eq w window) (cl-incf measured))
                     (apply measure w args)))
                  ((symbol-function 'set-frame-size-and-position-pixelwise)
                   (lambda (&rest args)
                     (cl-incf placed)
                     (apply place args))))
          ,test))))
  (test)
  :doc "scrolling only the host measures and moves nothing"
  (progn
    (mevedel-floating-composer-test--wheel 'wheel-up host)
    (mevedel-floating-composer-test--wheel 'wheel-up host)
    (should (eq window (selected-window)))
    (should (= 0 measured))
    (should (= 0 placed)))
  :doc "a draft edit measures once and keeps an unchanged geometry"
  (progn
    (mevedel-floating-composer-test--keys "a" 'self-insert-command)
    (should (= 1 measured))
    (should (= 0 placed)))
  :doc "a taller draft resizes and moves in one call"
  (progn
    (mevedel-floating-composer-test--keys "C-o" 'open-line)
    (should (= 1 placed))))


;;
;;; Rail

(mevedel-deftest mevedel-floating-composer/rail ()
  (mevedel-floating-composer-test--with-view
    (let ((host (selected-window)))
      (mevedel-view--start-spinner "Working on a long running request label")
      ,test))
  (test)
  :doc "the rail animates while it is visible only in the floating composer"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (should (mevedel-floating-composer-test--window))
    (should-not (mevedel-view--position-screen-y
                 (mevedel-floating-composer--start) host))
    (should (mevedel-view--animation-visible-p))
    (let ((mevedel-floating-composer nil))
      (mevedel-floating-composer--hide)
      (redisplay t)
      (should-not (mevedel-view--animation-visible-p))))
  :doc "redisplay and its own resize never rewrite the host's rail"
  (progn
    (mevedel-view--interaction-register
     '(:kind ask :id ask :count 1 :body "A question for the user?\n"))
    (mevedel-floating-composer-test--scroll-away t)
    (let ((window (mevedel-floating-composer-test--window))
          (rail (mevedel-floating-composer-test--rail))
          (state mevedel-view--spinner-rendered-state))
      (should (eq window (selected-window)))
      (should (string-search "C-c C-j" rail))
      (let ((height (window-pixel-height window)))
        (mevedel-floating-composer-test--keys "C-o" 'open-line)
        (should (> (window-pixel-height window) height)))
      (dotimes (_ 3) (mevedel-floating-composer-test--settle))
      (should (equal rail (mevedel-floating-composer-test--rail)))
      (should (equal state mevedel-view--spinner-rendered-state))
      (should (= (1- (window-body-width host)) (nth 3 state))))))

;;; test-mevedel-floating-composer-graphical.el ends here
