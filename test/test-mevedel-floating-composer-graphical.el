;;; test-mevedel-floating-composer-graphical.el --- Native floating composer tests -*- lexical-binding: t -*-

;;; Commentary:

;; A real view in a graphical frame, driven through the command loop.  Each
;; case reads what the user sees: the selected window, the cursor's draft
;; offset, whether the floating composer is shown, and the host window's
;; start, vertical scroll, point and rail width.  The frame holds real
;; compositor focus; nothing stubs `frame-focus-state'.  Batch runs skip them.

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

(defvar pgtk-wait-for-event-timeout)

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
           (should (mevedel-floating-composer-test--focus (selected-frame)))
           (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
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
  "Deliver EVENTS through the command loop and wait for COMMAND.
COMMAND nil waits for any command."
  (let* ((done nil)
         (hook (lambda ()
                 (when (or (null command) (eq this-command command))
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

(defun mevedel-floating-composer-test--focus (frame)
  "Give FRAME compositor focus and wait until it reports it."
  (select-frame-set-input-focus frame)
  (mevedel-floating-composer-test--focused-p frame))

(defun mevedel-floating-composer-test--focused-p (frame)
  "Return non-nil when FRAME reports compositor focus, after its events arrive.
Focus changes arrive asynchronously, so wait up to half a second."
  (let ((deadline (+ (float-time) 0.5)))
    (while (and (not (eq t (frame-focus-state frame))) (< (float-time) deadline))
      (mevedel-floating-composer-test--settle))
    ;; A loss shows up after the events that follow it, so let them arrive.
    (dotimes (_ 4) (mevedel-floating-composer-test--settle))
    (eq t (frame-focus-state frame))))

(defun mevedel-floating-composer-test--click (window)
  "Click WINDOW's first character through the command loop."
  (let ((posn (posn-at-point (window-start window) window)))
    ;; Queued together, the press's drag tracking would consume the release.
    (mevedel-floating-composer-test--deliver
     (list (list 'down-mouse-1 posn)) 'mouse-drag-region)
    (mevedel-floating-composer-test--deliver (list (list 'mouse-1 posn)) nil)))

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
  :doc "the host's frame keeps compositor focus through hand-over, typing and hand-back"
  (let ((parent (window-frame host))
        states)
    (cl-flet ((note (step)
                (push (list step (mevedel-floating-composer-test--focused-p parent)
                            (not (mevedel-view--unattended-p)))
                      states)))
      (mevedel-floating-composer-test--scroll-away t)
      (should (frame-parent (selected-frame)))
      (note 'hand-over)
      (mevedel-floating-composer-test--keys "a" 'self-insert-command)
      (note 'typing)
      (let ((notches 0))
        (while (and (< notches 60) (frame-parent (selected-frame)))
          (mevedel-floating-composer-test--wheel 'wheel-down host)
          (cl-incf notches)))
      (should (eq host (selected-window)))
      (should-not (mevedel-floating-composer-test--window))
      (note 'hand-back)
      (should-not (frame-focus parent)))
    (should (equal '((hand-over t t) (typing t t) (hand-back t t)) (reverse states))))
  :doc "clicking the host takes the cursor and its keystrokes back"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (let ((frame (selected-frame)))
      (should (eq frame (frame-focus (window-frame host))))
      (mevedel-floating-composer-test--click host)
      (should (eq host (selected-window)))
      (should (memq (frame-focus (window-frame host)) (list nil (window-frame host))))
      (should (mevedel-floating-composer-test--focused-p (window-frame host)))))
  :doc "clicking the floating composer redirects the host frame's keystrokes to it"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (let ((window (mevedel-floating-composer-test--window)))
      (should (eq host (selected-window)))
      (mevedel-floating-composer-test--click window)
      (should (eq window (selected-window)))
      (should (eq (window-frame window) (frame-focus (window-frame host))))))
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
  :doc "a text property change alone measures nothing"
  (progn
    (let ((inhibit-read-only t))
      (put-text-property (mevedel-view--input-start) (point-max) 'mevedel-test t))
    (mevedel-floating-composer--update)
    (should (= 0 measured)))
  :doc "a taller draft resizes and moves in one call"
  (progn
    (mevedel-floating-composer-test--keys "C-o" 'open-line)
    (should (= 1 placed))))


(mevedel-deftest mevedel-floating-composer--hide ()
  (mevedel-floating-composer-test--with-view
    (let ((host (selected-window))
          ;; PGTK's hide and hidden creation would each spin this long.
          (pgtk-wait-for-event-timeout 2.0)
          (start (float-time)))
      (mevedel-floating-composer-test--scroll-away t)
      (should (mevedel-floating-composer-test--window))
      (while (and (< (- (float-time) start) 30) (frame-parent (selected-frame)))
        (mevedel-floating-composer-test--wheel 'wheel-down host))
      (should-not (mevedel-floating-composer-test--window))
      ,test))
  (test)
  :doc "creating and hiding the frame do not wait for PGTK's event timeout"
  (should (< (- (float-time) start) 2.0)))


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
