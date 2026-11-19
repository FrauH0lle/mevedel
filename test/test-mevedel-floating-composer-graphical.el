;;; test-mevedel-floating-composer-graphical.el --- Native floating composer tests -*- lexical-binding: t -*-

;;; Commentary:

;; A real view in a real frame, driven through the command loop.  Each
;; case reads what the user sees: the selected window, the cursor's draft
;; offset, whether the floating composer is shown, and the host window's
;; start, vertical scroll, point and rail width.  The frame holds real
;; compositor focus; nothing stubs `frame-focus-state'.  Batch runs skip them;
;; test/terminal-check.el runs the main cases in a terminal frame.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mwheel)
(require 'pixel-scroll)
(require 'mevedel-chat)
(require 'mevedel-floating-composer)
(require 'mevedel-readiness)
(require 'mevedel-turn)
(require 'mevedel-view)
(require 'mevedel-view-agent)
(require 'mevedel-view-animation)
(require 'mevedel-view-composer)
(require 'mevedel-view-interaction)
(require 'mevedel-view-native)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)

(defvar pgtk-wait-for-event-timeout)
(defvar mevedel-view-segments--buffer)
(defvar mevedel-view-segments--number)

(defconst mevedel-floating-composer-test--draft "> active draft\nsecond line"
  "Multiline draft whose first editable character is `>'.")

(defmacro mevedel-floating-composer-test--with-view (&rest body)
  "Run BODY in a view showing its composer at the window bottom.
The draft holds point at offset 4.  BODY can set `agents', which the
background panel reads; `data-buf' and `view-buf' are bound too."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (not noninteractive))
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

(defun mevedel-floating-composer-test--border-color (face)
  "Return FACE's background, the floating composer's border color for it."
  (face-attribute face :background nil 'default))

(defun mevedel-floating-composer-test--painted-border (frame)
  "Return the color FRAME's border shows.
A terminal draws its box in FRAME's own `border' face."
  (if (display-graphic-p frame)
      (face-background 'child-frame-border frame)
    (face-foreground 'border frame)))

(defun mevedel-floating-composer-test--redirected-p (parent frame)
  "Return non-nil when PARENT's keystrokes reach FRAME.
A terminal sends them to the selected frame, so it must have no redirect."
  (if (display-graphic-p parent)
      (eq frame (frame-focus parent))
    (null (frame-focus parent))))

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

(defun mevedel-floating-composer-test--tall-draft ()
  "Make the shown floating composer's draft taller than its cap, keeping point.
Its window shows the draft's end."
  (save-excursion
    (goto-char (point-max))
    (insert (mapconcat (lambda (i) (format "\ndraft line %d" i)) (number-sequence 1 40) "")))
  (let ((window (mevedel-floating-composer-test--window)))
    (unless (eq window (selected-window))
      (set-window-point window (point-max))))
  (mevedel-floating-composer--update)
  (mevedel-floating-composer-test--settle))

(defun mevedel-floating-composer-test--within-draft-p (window)
  "Return non-nil when floating-composer WINDOW shows no transcript text."
  (and (>= (window-start window) (mevedel-floating-composer--start))
       (zerop (window-vscroll window t))))

(defun mevedel-floating-composer-test--sound (buffer)
  "Assert BUFFER's floating composer, if shown, fits the window worked in.
No floating-composer frame other than BUFFER's own exists.  A shown one
floats over its parent's selected window at the width and draft height
placement gives now, with its cursor visible.  Return its window if shown."
  (let ((own (and (buffer-live-p buffer)
                  (buffer-local-value 'mevedel-floating-composer--frame buffer))))
    (dolist (frame (frame-list))
      (when (frame-parameter frame 'mevedel-view-animate-as-parent)
        (should (eq own frame))))
    (when (and (frame-live-p own) (frame-visible-p own))
      (with-current-buffer buffer
        (let* ((parent (frame-parent own))
               (host (frame-selected-window parent))
               (window (frame-root-window own))
               (edges (window-inside-pixel-edges host))
               (char-height (frame-char-height parent))
               (cap (/ (- (nth 3 edges) (nth 1 edges)) 2))
               ;; Measured afresh, so a stale cached height shows.
               (natural (cdr (window-text-pixel-size
                              window (mevedel-floating-composer--start) (point-max)
                              nil (+ cap char-height)))))
          (should (memq (selected-frame) (list parent own)))
          (should (eq host mevedel-floating-composer--host))
          (should (eq buffer (window-buffer host)))
          (pcase-let ((`(,width ,height ,x ,y)
                       (mevedel-floating-composer--place
                        edges (frame-char-width parent) char-height natural
                        mevedel-note-frame-border-width)))
            (should (equal (list width height x y)
                           (list (frame-text-width own) (frame-text-height own)
                                 (car (frame-position own)) (cdr (frame-position own))))))
          (should (pos-visible-in-window-p (window-point window) window))
          window)))))

(defun mevedel-floating-composer-test--grow-draft (lines)
  "Prepend LINES lines to the draft, leaving point at offset 4."
  (mevedel-view-test--insert-composer-draft
   (mapconcat (lambda (i) (format "> long draft line %d\n" i)) (number-sequence 1 lines))
   4))


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
      (should (mevedel-floating-composer-test--redirected-p (window-frame host) frame))
      (should (equal (mevedel-floating-composer-test--border-color 'mevedel-note-frame-border)
                     (mevedel-floating-composer-test--painted-border frame)))
      (mevedel-floating-composer-test--click host)
      (should (eq host (selected-window)))
      (should (memq (frame-focus (window-frame host)) (list nil (window-frame host))))
      (should (mevedel-floating-composer-test--focused-p (window-frame host)))
      (should (equal (mevedel-floating-composer-test--border-color
                      'mevedel-note-frame-border-inactive)
                     (mevedel-floating-composer-test--painted-border frame)))))
  :doc "removing it clears the redirect after another frame was selected"
  (let ((parent (window-frame host))
        (other (with-current-buffer (get-buffer-create "*scratch*")
                 (make-frame '((width . 30) (height . 5))))))
    (unwind-protect
        (progn
          ;; Mapping OTHER takes compositor focus, asynchronously: let that
          ;; land first, or it can override the parent's request below.
          (mevedel-floating-composer-test--focused-p other)
          (should (mevedel-floating-composer-test--focus parent))
          (mevedel-floating-composer-test--scroll-away t)
          ;; As `other-frame' does, `select-frame' retargets the redirect.
          (select-frame other)
          (should (mevedel-floating-composer-test--redirected-p parent other))
          (with-current-buffer view-buf (mevedel-floating-composer--remove))
          (should-not (frame-focus parent)))
      (select-frame parent)
      (redirect-frame-focus parent nil)
      (delete-frame other)))
  :doc "deleting the host clears the redirect before the next command"
  (let ((parent (window-frame host)))
    (mevedel-floating-composer-test--scroll-away t)
    (should (mevedel-floating-composer-test--redirected-p parent (selected-frame)))
    (set-window-buffer (split-window host nil 'right) (get-buffer-create "*scratch*"))
    (delete-window host)
    (mevedel-floating-composer-test--settle)
    (should-not (frame-focus parent))
    (mevedel-floating-composer-test--settle)
    (should-not (with-current-buffer view-buf (mevedel-floating-composer--window))))
  :doc "releasing one view leaves another view's later redirect alone"
  (let* ((parent (window-frame host))
         (other-buf (get-buffer-create " *floating-other*"))
         (other (make-frame `((parent-frame . ,parent) (visibility . nil)
                              (minibuffer . ,(minibuffer-window parent))))))
    (unwind-protect
        (progn
          (mevedel-floating-composer-test--scroll-away t)
          (set-window-buffer (frame-root-window other) other-buf)
          ;; A second view takes input on the same parent frame.
          (with-current-buffer other-buf
            (mevedel-floating-composer--select (frame-root-window other) 1))
          (should (mevedel-floating-composer-test--redirected-p parent other))
          (with-current-buffer view-buf (mevedel-floating-composer--release))
          (should (mevedel-floating-composer-test--redirected-p parent other)))
      (select-frame parent)
      (redirect-frame-focus parent nil)
      (delete-frame other)
      (kill-buffer other-buf)))
  :doc "the host check runs only while the option is on and a floating composer exists"
  (unwind-protect
      (progn
        (customize-set-variable 'mevedel-floating-composer nil)
        (should-not (memq #'mevedel-floating-composer--check-hosts
                          (default-value 'window-buffer-change-functions)))
        (customize-set-variable 'mevedel-floating-composer t)
        (mevedel-floating-composer-test--scroll-away t)
        (should (memq #'mevedel-floating-composer--check-hosts
                      (default-value 'window-buffer-change-functions))))
    (customize-set-variable 'mevedel-floating-composer t))
  :doc "clicking the floating composer redirects the host frame's keystrokes to it"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (let ((window (mevedel-floating-composer-test--window)))
      (should (eq host (selected-window)))
      (mevedel-floating-composer-test--click window)
      (should (eq window (selected-window)))
      (should (mevedel-floating-composer-test--redirected-p
               (window-frame host) (window-frame window)))))
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
      (customize-set-variable 'mevedel-floating-composer t)))
  :doc "setting the option off with setq removes it after the next command"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (should (mevedel-floating-composer-test--window))
    (unwind-protect
        (progn
          (setq mevedel-floating-composer nil)
          (mevedel-floating-composer-test--keys "a" 'self-insert-command)
          (should (eq host (selected-window)))
          (should (= 5 (mevedel-floating-composer-test--offset)))
          (should-not (mevedel-floating-composer--window))
          (should-not (memq #'mevedel-floating-composer--check-hosts
                            (default-value 'window-buffer-change-functions))))
      (customize-set-variable 'mevedel-floating-composer t))))


;;
;;; Keys and pointer

(mevedel-deftest mevedel-floating-composer/keys ()
  (mevedel-floating-composer-test--with-view
    (let ((host (selected-window)))
      ,test))
  (test)
  :doc "C-c C-SPC toggles between the floating composer and the transcript"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (let ((window (selected-window))
          (parent (window-frame host)))
      (should (window-parameter window 'mevedel-floating-composer))
      (mevedel-floating-composer-test--keys "C-c C-SPC" 'mevedel-floating-composer-toggle)
      (should (eq host (selected-window)))
      (should (< (point) (mevedel-view--input-start)))
      (should (eq window (mevedel-floating-composer-test--window)))
      (should (memq (frame-focus parent) (list nil parent)))
      (mevedel-floating-composer-test--keys "C-c C-SPC" 'mevedel-floating-composer-toggle)
      (should (eq window (selected-window)))
      (should (= 4 (mevedel-floating-composer-test--offset)))
      (should (mevedel-floating-composer-test--redirected-p parent (window-frame window)))
      (should (mevedel-floating-composer-test--focused-p parent))))
  :doc "C-c C-SPC from the transcript enters the floating composer"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (should (eq host (selected-window)))
    (mevedel-floating-composer-test--keys "C-c C-SPC" 'mevedel-floating-composer-toggle)
    (should (window-parameter (selected-window) 'mevedel-floating-composer))
    (should (mevedel-view--position-in-input-region-p (point))))
  :doc "C-c C-SPC toggles the inline draft while the composer is visible"
  (progn
    (mevedel-floating-composer-test--keys "C-c C-SPC" 'mevedel-floating-composer-toggle)
    (should (eq host (selected-window)))
    (should (< (point) (mevedel-view--input-start)))
    (should-not (mevedel-floating-composer-test--window))
    (mevedel-floating-composer-test--keys "C-c C-SPC" 'mevedel-floating-composer-toggle)
    (should (eq host (selected-window)))
    (should (= 4 (mevedel-floating-composer-test--offset))))
  :doc "M-v in a short floating composer never shows transcript text"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (let ((window (selected-window)))
      (dotimes (_ 2)
        (mevedel-floating-composer-test--keys "M-v" 'scroll-down-command)
        (should (eq window (selected-window)))
        (should (mevedel-floating-composer-test--within-draft-p window)))))
  :doc "C-v and M-v page within a capped draft and stop at its top"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (mevedel-floating-composer-test--tall-draft)
    (let ((window (selected-window))
          (start (mevedel-floating-composer--start)))
      (should-not (window-parameter window 'mevedel-floating-composer-pinned))
      (mevedel-floating-composer-test--keys "C-v" 'scroll-up-command)
      (should (eq window (selected-window)))
      (should (> (window-start window) start))
      (dotimes (_ 3)
        (mevedel-floating-composer-test--keys "M-v" 'scroll-down-command)
        (should (eq window (selected-window)))
        (should (mevedel-floating-composer-test--within-draft-p window)))
      (should (= start (window-start window)))
      (should (mevedel-view--position-in-input-region-p (point)))))
  :doc "C-M-v and C-M-S-v scroll the host from the floating composer"
  (progn
    ;; Another window comes first on the host's frame.
    (set-window-buffer (split-window host nil 'left) (get-buffer-create "*scratch*"))
    (mevedel-floating-composer-test--scroll-away t)
    (let ((window (selected-window))
          (start (window-start host)))
      (mevedel-floating-composer-test--keys "C-M-S-v" 'scroll-other-window-down)
      (mevedel-floating-composer-test--keys "C-M-S-v" 'scroll-other-window-down)
      (should (eq window (selected-window)))
      (should (< (window-start host) start))
      (setq start (window-start host))
      (mevedel-floating-composer-test--keys "C-M-v" 'scroll-other-window)
      (should (eq window (selected-window)))
      (should (> (window-start host) start))
      (should (= 4 (mevedel-floating-composer-test--offset)))))
  :doc "the wheel over the host scrolls it without moving focus"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (let ((window (selected-window))
          (start (window-start host)))
      (mevedel-floating-composer-test--wheel 'wheel-up host)
      (should (eq window (selected-window)))
      (should (mevedel-floating-composer-test--redirected-p
               (window-frame host) (window-frame window)))
      (should (< (window-start host) start))))
  :doc "the wheel over the floating composer stops at the draft's edge without moving focus"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (mevedel-floating-composer-test--tall-draft)
    (let ((window (mevedel-floating-composer-test--window))
          (point (point)))
      (should (eq host (selected-window)))
      (should (> (window-start window) (mevedel-floating-composer--start)))
      (dotimes (_ 30)
        (mevedel-floating-composer-test--wheel 'wheel-up window)
        (should (eq host (selected-window)))
        (should (mevedel-floating-composer-test--within-draft-p window)))
      (should (= (mevedel-floating-composer--start) (window-start window)))
      (should (= point (point)))
      (should-not (frame-focus (window-frame host)))))
  :doc "wheel momentum over the floating composer stops at the draft's edge between commands"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (mevedel-floating-composer-test--tall-draft)
    (let ((window (mevedel-floating-composer-test--window)))
      (should (eq host (selected-window)))
      (should (> (window-start window) (mevedel-floating-composer--start)))
      ;; The body of `pixel-scroll-start-momentum': one command scrolls the
      ;; window under the pointer tick by tick, redisplaying in between.
      (dotimes (_ 40)
        (with-selected-window window
          (pixel-scroll-precision-scroll-up 40))
        (redisplay t)
        (should (mevedel-floating-composer-test--within-draft-p window)))
      (should (= (mevedel-floating-composer--start) (window-start window)))
      (should (eq host (selected-window)))))
  :doc "C-x 2 and C-x 3 in the floating composer split the host"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (let ((window (selected-window))
          (parent (window-frame host)))
      (mevedel-floating-composer-test--keys "C-x 2" 'split-window-below)
      (mevedel-floating-composer-test--keys "C-x 3" 'split-window-right)
      (should (= 3 (length (window-list parent 'nomini))))
      (should (eq window (frame-root-window (window-frame window))))
      (should (eq window (selected-window)))
      (should (window-live-p host))))
  :doc "transcript keys keep their meaning while the floating composer is shown"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (should (mevedel-floating-composer-test--window))
    (dolist (key '("n" "p" "TAB" "TAB" "RET" "t"))
      (let ((command (keymap-lookup mevedel-view--display-map key)))
        (should (eq command (key-binding (kbd key))))
        (mevedel-floating-composer-test--keys key command)
        (should (eq host (selected-window)))))))


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
;;; Cards and sending

(defun mevedel-floating-composer-test--answer ()
  "Settle the test card, as answering it does."
  (interactive)
  (mevedel-view--interaction-unregister 'ask))

(defun mevedel-floating-composer-test--card (lines)
  "Register a pending question card with LINES detail lines."
  (mevedel-view--interaction-register
   (list :kind 'ask :id 'ask :count 1
         :body (concat "A question for the user?\n"
                       (mapconcat (lambda (i) (format "Detail %d\n" i))
                                  (number-sequence 1 lines) ""))
         :keymap (define-keymap "RET" #'mevedel-floating-composer-test--answer))))

(defun mevedel-floating-composer-test--above-p (position host window)
  "Return non-nil when POSITION's line shows in HOST above WINDOW's frame."
  (when-let* ((y (mevedel-view--position-screen-y position host t)))
    (<= (+ y (save-excursion (goto-char position) (line-pixel-height)))
        (- (cdr (frame-position (window-frame window)))
           (nth 1 (window-inside-pixel-edges host))))))

(mevedel-deftest mevedel-floating-composer/jump-and-send ()
  (mevedel-floating-composer-test--with-view
    (let ((host (selected-window)))
      (mevedel-floating-composer-test--scroll-away t)
      (let ((window (selected-window)))
        (should (mevedel-floating-composer-test--window))
        (mevedel-floating-composer-test--keys "a" 'self-insert-command)
        ,test)))
  (test)
  :doc "C-c C-j visits a card above the floating composer; settling returns to it"
  (progn
    ;; The cursor sits so far down the draft that the host never shows it.
    (insert (make-string 60 ?\n))
    (mevedel-floating-composer-test--card 40)
    (mevedel-floating-composer-test--settle)
    ;; Arrival moves nothing.
    (should (eq window (selected-window)))
    (should (= 65 (mevedel-floating-composer-test--offset)))
    (mevedel-floating-composer-test--keys "C-c C-j" 'mevedel-view-jump-to-interaction)
    (should (eq host (selected-window)))
    (should (eq 'ask (get-text-property (point) 'mevedel-view-interaction-id)))
    (should (mevedel-floating-composer-test--window))
    (should (mevedel-floating-composer-test--above-p (point) host window))
    (should-not (frame-focus (window-frame host)))
    (mevedel-floating-composer-test--keys "RET" 'mevedel-floating-composer-test--answer)
    (should-not (string-search "A question for the user?" (buffer-string)))
    (should (eq window (selected-window)))
    (should (= 65 (mevedel-floating-composer-test--offset)))
    (should (eq (window-frame window) (frame-focus (window-frame host))))
    ;; The host stays a history reader.
    (should (< (window-point host) (mevedel-view--input-start))))
  :doc "settling with the inline draft visible returns the cursor inline"
  (progn
    (mevedel-floating-composer-test--card 1)
    (mevedel-floating-composer-test--keys "C-c C-j" 'mevedel-view-jump-to-interaction)
    (should (eq host (selected-window)))
    (mevedel-floating-composer-test--to-bottom)
    (should (eq 'ask (get-text-property (window-point host) 'mevedel-view-interaction-id)))
    (mevedel-floating-composer-test--keys "RET" 'mevedel-floating-composer-test--answer)
    (should (eq host (selected-window)))
    (should (= 5 (mevedel-floating-composer-test--offset)))
    (should-not (mevedel-floating-composer-test--window)))
  :doc "sending keeps the host's viewport and the cursor in the emptied floating composer"
  (let ((viewport (cons (window-start host) (window-vscroll host t))))
    (cl-letf (((symbol-function 'mevedel--dispatch-request) #'ignore)
              ((symbol-function 'mevedel-request-assert-target-ready) #'ignore)
              ((symbol-function 'mevedel-readiness-assert) #'ignore))
      (mevedel-floating-composer-test--keys "C-c RET" 'mevedel-view-send))
    (should (string-search "acative draft" (buffer-string)))
    (should (equal "" (mevedel-view--input-text)))
    (should (eq window (selected-window)))
    (should (mevedel-floating-composer-test--window))
    (should (equal viewport (cons (window-start host) (window-vscroll host t)))))
  :doc "input typed while a submission is prepared survives in the floating composer"
  (let ((viewport (cons (window-start host) (window-vscroll host t)))
        (hook (symbol-function 'mevedel-view--run-prompt-submit-hook))
        pending)
    (cl-letf (((symbol-function 'mevedel--dispatch-request) #'ignore)
              ((symbol-function 'mevedel-request-assert-target-ready) #'ignore)
              ((symbol-function 'mevedel-readiness-assert) #'ignore)
              ;; Preparation finishes after the user typed on.
              ((symbol-function 'mevedel-view--run-prompt-submit-hook)
               (lambda (&rest args) (setq pending (lambda () (apply hook args))))))
      (mevedel-floating-composer-test--keys "C-c RET" 'mevedel-view-send)
      (should pending)
      (mevedel-floating-composer-test--keys "b" 'self-insert-command)
      (funcall pending)
      (mevedel-floating-composer-test--settle))
    (should (string-search "acative draft" (buffer-string)))
    (should (equal "> acabtive draft\nsecond line" (mevedel-view--input-text)))
    (should (eq window (selected-window)))
    (should (= 6 (mevedel-floating-composer-test--offset)))
    (should (equal viewport (cons (window-start host) (window-vscroll host t))))))

(mevedel-deftest mevedel-floating-composer-reveal ()
  (mevedel-floating-composer-test--with-view
    (let* ((host (selected-window))
           (below (lambda (lines)
                    (save-excursion (goto-char (window-end host t))
                                    (forward-line lines)
                                    (point)))))
      ,test))
  (test)
  :doc "does nothing while no floating composer floats over the window"
  (let ((viewport (cons (window-start host) (window-vscroll host t))))
    (mevedel-floating-composer-reveal host (point-min) (point-min))
    (should (equal viewport (cons (window-start host) (window-vscroll host t)))))
  :doc "a position already above the floating composer moves nothing"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (let ((viewport (cons (window-start host) (window-vscroll host t))))
      (mevedel-floating-composer-reveal host (window-start host) (window-start host))
      (should (equal viewport (cons (window-start host) (window-vscroll host t))))))
  :doc "a hidden card that fits starts at the window's top"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (let ((start (funcall below 0)))
      (mevedel-floating-composer-reveal host start (funcall below 2))
      (should (= start (window-start host)))))
  :doc "a hidden line of a card too tall for that shows just above the floating composer"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (let ((position (funcall below 2))
          (window (mevedel-floating-composer-test--window)))
      (mevedel-floating-composer-reveal host (point-min) position)
      (redisplay t)
      (should (mevedel-floating-composer-test--above-p position host window))
      (should (> (window-start host) (point-min))))))

(mevedel-deftest mevedel-floating-composer-return ()
  (mevedel-floating-composer-test--with-view
    (let ((host (selected-window))
          (draft (+ (mevedel-view--input-start) 4)))
      ,test))
  (test)
  :doc "returns nil while no floating composer is shown"
  (progn
    (goto-char (point-min))
    (should-not (mevedel-floating-composer-return host draft))
    (should (eq host (selected-window))))
  :doc "returns nil when the window fully shows the position"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (should-not (mevedel-floating-composer-return host (window-start host)))
    (should (eq host (selected-window))))
  :doc "moves the cursor into the floating composer, leaving the host's point"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (let ((point (window-point host)))
      (should (mevedel-floating-composer-return host draft))
      (should (eq (mevedel-floating-composer-test--window) (selected-window)))
      (should (= 4 (mevedel-floating-composer-test--offset)))
      (should (= point (window-point host)))))
  :doc "does nothing for a window that is not selected"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (let ((other (split-window host nil 'below)))
      (should-not (mevedel-floating-composer-return other draft))
      (should (eq host (selected-window))))))

(mevedel-deftest mevedel-floating-composer-leave ()
  (mevedel-floating-composer-test--with-view
    (let ((host (selected-window)))
      ,test))
  (test)
  :doc "moves a cursor in the floating composer to its host at the same position"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (let ((window (selected-window)))
      (should (window-parameter window 'mevedel-floating-composer))
      (mevedel-floating-composer-leave)
      (should (eq host (selected-window)))
      (should (= 4 (mevedel-floating-composer-test--offset)))
      (should-not (frame-focus (window-frame host)))))
  :doc "leaves a cursor in the host where it is"
  (progn
    (mevedel-floating-composer-test--scroll-away nil)
    (let ((point (point)))
      (mevedel-floating-composer-leave)
      (should (eq host (selected-window)))
      (should (= point (point))))))


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
  :doc "a draft edit and the refresh after it measure once and keep the geometry"
  (progn
    (mevedel-floating-composer-test--keys "a" 'self-insert-command)
    (mevedel-floating-composer--update)
    (should (= 1 measured))
    (should (= 0 placed)))
  :doc "a property change that makes the draft taller grows the frame"
  (let ((height (window-pixel-height window)))
    (let ((inhibit-read-only t))
      (put-text-property (1- (point-max)) (point-max) 'face '(:height 4.0)))
    (mevedel-floating-composer--update)
    (should (= 1 measured))
    (mevedel-floating-composer-test--settle)
    (should (> (window-pixel-height window) height)))
  :doc "a second text-scale remapping of the same face measures again"
  (let ((cookies nil))
    (unwind-protect
        (progn
          (push (face-remap-add-relative 'default :height 1.2) cookies)
          (mevedel-floating-composer--update)
          (should (= 1 measured))
          (push (face-remap-add-relative 'default :height 1.2) cookies)
          (mevedel-floating-composer--update)
          (should (= 2 measured)))
      (mapc #'face-remap-remove-relative cookies)))
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
      (should (= (1- (window-body-width host)) (nth 3 state)))))
  :doc "the rail animates at the battery ceiling while only the floating composer shows it"
  (let ((mevedel-view-spinner-animate t)
        (mevedel-view-spinner-style 'shimmer)
        (mevedel-view-spinner-framerate 30)
        (mevedel-view-spinner-battery-framerate 1)
        (mevedel-view-spinner-power-policy 'full)
        specs)
    ;; Keep the label in the Lisp plan and record what native presentation gets.
    (cl-letf (((symbol-function 'mevedel-view-native-sync)
               (lambda (new &rest _) (setq specs new) nil)))
      (cl-flet ((periods ()
                  (setq specs nil)
                  (mevedel-view--start-spinner-timer t)
                  (list (cdr (cl-find 'metadata mevedel-view--spinner-timer-plan
                                      :key #'car :test-not #'eq))
                        (nth 4 (car specs)))))
        (pcase-let ((`(,plan ,native) (periods)))
          (should (< plan 0.5))
          (should (< native 0.5)))
        (mevedel-floating-composer-test--scroll-away nil)
        (should (mevedel-floating-composer-test--window))
        (should-not (mevedel-view--position-screen-y
                     (mevedel-floating-composer--start) host))
        (pcase-let ((`(,plan ,native) (periods)))
          (should (>= plan 1.0))
          (should (>= native 1.0))))))
  :doc "a wake before the label's ceiling period does not repaint it"
  (let ((mevedel-view-spinner-animate t)
        (mevedel-view-spinner-style 'braille)
        (mevedel-view-spinner-framerate 30)
        (mevedel-view-spinner-battery-framerate 1)
        (mevedel-view-spinner-power-policy 'full)
        (painted 0)
        (frame-of (symbol-function 'mevedel-view-animation-frame)))
    (cl-letf (((symbol-function 'mevedel-view-native-sync) (lambda (&rest _) nil))
              ((symbol-function 'mevedel-view-animation-frame)
               (lambda (style label seconds face &rest rest)
                 (when (eq face 'mevedel-view-spinner) (cl-incf painted))
                 (apply frame-of style label seconds face rest))))
      (mevedel-floating-composer-test--scroll-away nil)
      (should (mevedel-floating-composer-test--window))
      (mevedel-view--start-spinner-timer t)
      ;; Shared wakes, as a pending tool row's faster cadence causes.
      (dotimes (_ 3) (mevedel-view--spinner-tick))
      (should (= 1 painted)))))


;;
;;; Windows, frames and focus

(mevedel-deftest mevedel-floating-composer/windows ()
  (mevedel-floating-composer-test--with-view
    (let ((host (selected-window))
          (parent (selected-frame)))
      ,test
      (mevedel-floating-composer-test--sound view-buf)))
  (test)
  :doc "splitting and unsplitting the host refits it over the host"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (let ((window (selected-window))
          (height (window-pixel-height host))
          (other (split-window host nil 'below)))
      (mevedel-floating-composer-test--settle)
      (should (< (window-pixel-height host) height))
      (should (eq window (mevedel-floating-composer-test--sound view-buf)))
      (delete-window other)
      (mevedel-floating-composer-test--settle)
      (should (= height (window-pixel-height host)))
      (should (eq window (mevedel-floating-composer-test--sound view-buf)))
      (should (eq window (selected-window)))
      (should (= 4 (mevedel-floating-composer-test--offset)))))
  :doc "two windows on one view have one, over the selected window only while it is away"
  (let ((other (split-window host nil 'below)))
    (with-selected-window other (mevedel-floating-composer-test--to-bottom))
    (mevedel-floating-composer-test--scroll-away nil)
    (should (mevedel-floating-composer-test--sound view-buf))
    (mevedel-floating-composer-test--keys "C-x o" 'other-window)
    (should (eq other (selected-window)))
    (should-not (mevedel-floating-composer-test--window))
    (mevedel-floating-composer-test--keys "C-x o" 'other-window)
    (should (eq host (selected-window)))
    (should (mevedel-floating-composer-test--sound view-buf)))
  :doc "the host switching buffer removes it and its redirect"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (set-window-buffer host (get-buffer-create "*scratch*"))
    (mevedel-floating-composer-test--settle)
    (should-not (frame-focus parent))
    (mevedel-floating-composer-test--settle)
    (should-not (with-current-buffer view-buf (mevedel-floating-composer--window))))
  :doc "a tab switch hides it and switching back shows it over the host"
  (unwind-protect
      (progn
        (mevedel-floating-composer-test--scroll-away nil)
        (should (mevedel-floating-composer-test--window))
        (let ((tab-bar-new-tab-choice "*scratch*"))
          (mevedel-floating-composer-test--keys "C-x t 2" 'tab-new))
        (mevedel-floating-composer-test--settle)
        (should-not (mevedel-floating-composer-test--window))
        (mevedel-floating-composer-test--sound view-buf)
        (mevedel-floating-composer-test--keys "C-x t o" 'tab-next)
        (should (eq host (selected-window)))
        (should (mevedel-floating-composer-test--sound view-buf)))
    (tab-bar-close-other-tabs)
    (tab-bar-mode -1))
  :doc "another frame hides it; returning neither selects it nor misroutes keys"
  (let ((other (with-current-buffer (get-buffer-create "*scratch*")
                 (make-frame '((width . 30) (height . 5))))))
    ;; As above: OTHER's activation must not override the parent's.
    (mevedel-floating-composer-test--focused-p other)
    (unwind-protect
        (dolist (in-draft '(nil t))
          (should (mevedel-floating-composer-test--focus parent))
          (goto-char (+ (mevedel-view--input-start) 4))
          (mevedel-floating-composer-test--to-bottom)
          (mevedel-floating-composer-test--scroll-away in-draft)
          (should (eq in-draft (and (frame-parent (selected-frame)) t)))
          (let ((frame (window-frame (mevedel-floating-composer-test--window))))
            (should (mevedel-floating-composer-test--focus other))
            (should-not (mevedel-floating-composer-test--window))
            (should (mevedel-floating-composer-test--focus parent))
            ;; A stale redirect would send the parent's keystrokes elsewhere.
            (should (memq (frame-focus parent) (list nil parent)))
            (should (eq host (selected-window)))
            (should (mevedel-floating-composer-test--sound view-buf))
            (should-not (eq t (frame-focus-state frame)))))
      (select-frame parent)
      (redirect-frame-focus parent nil)
      (delete-frame other)))
  :doc "closing its parent frame leaves no floating composer behind"
  (let ((frame (make-frame)))
    (set-window-buffer (frame-root-window frame) view-buf)
    (should (mevedel-floating-composer-test--focus frame))
    (goto-char (+ (mevedel-view--input-start) 4))
    (mevedel-floating-composer-test--to-bottom)
    (mevedel-floating-composer-test--scroll-away t)
    (should (eq frame (frame-parent (selected-frame))))
    (delete-frame frame)
    (should (mevedel-floating-composer-test--focus parent))
    (should-not (frame-focus parent))
    (mevedel-floating-composer-test--sound view-buf)
    (select-window host)
    (goto-char (+ (mevedel-view--input-start) 4))
    (mevedel-floating-composer-test--scroll-away t)
    (should (eq parent (frame-parent (selected-frame)))))
  :doc "moving to another frame tears the old one down before deleting it"
  (let ((other (make-frame '((width . 60) (height . 20))))
        (seen :unset))
    (unwind-protect
        (progn
          (select-frame parent)
          (mevedel-floating-composer-test--scroll-away nil)
          (let* ((old (window-frame (mevedel-floating-composer-test--window)))
                 (record (lambda (frame)
                           (when (eq frame old)
                             (setq seen (buffer-local-value
                                         'mevedel-floating-composer--frame view-buf))))))
            (add-hook 'delete-frame-functions record)
            (unwind-protect
                (progn
                  (set-window-buffer (frame-root-window other) view-buf)
                  (with-current-buffer view-buf
                    (mevedel-floating-composer--show (frame-root-window other)))
                  (should-not (frame-live-p old))
                  (should-not seen))
              (remove-hook 'delete-frame-functions record))))
      (with-current-buffer view-buf (mevedel-floating-composer--remove))
      (select-frame parent)
      (delete-frame other)))
  :doc "killing the view removes it and its redirect"
  (let ((frame (progn (mevedel-floating-composer-test--scroll-away t)
                      (selected-frame))))
    (kill-buffer view-buf)
    (mevedel-floating-composer-test--settle)
    (should-not (frame-live-p frame))
    (should-not (frame-focus parent))
    (should (eq parent (selected-frame))))
  :doc "turning the option off with an update pending leaves none"
  (unwind-protect
      (progn
        (mevedel-floating-composer-test--scroll-away t)
        (mevedel-floating-composer-refresh)
        (should (timerp mevedel-floating-composer--timer))
        (customize-set-variable 'mevedel-floating-composer nil)
        (mevedel-floating-composer-test--settle)
        (should-not (mevedel-floating-composer--window))
        (should (eq host (selected-window))))
    (customize-set-variable 'mevedel-floating-composer t))
  :doc "M-x, completion and C-g return to the draft offset; the border shows the minibuffer"
  (progn
    (mevedel-floating-composer-test--scroll-away t)
    (let* ((window (selected-window))
           (frame (selected-frame))
           (active (mevedel-floating-composer-test--border-color 'mevedel-note-frame-border))
           (inactive (mevedel-floating-composer-test--border-color
                      'mevedel-note-frame-border-inactive))
           (borders nil)
           (record (lambda ()
                     (redisplay t)
                     (push (face-background 'child-frame-border frame) borders))))
      (add-hook 'minibuffer-setup-hook record)
      (unwind-protect
          (dolist (keys '(("M-x ignor TAB RET" . ignore) ("M-x C-g")))
            (mevedel-floating-composer-test--keys (car keys) (cdr keys))
            ;; A queued C-g quits a redisplay in the hook, so record once.
            (remove-hook 'minibuffer-setup-hook record)
            (should-not (active-minibuffer-window))
            (should (eq window (selected-window)))
            (should (= 4 (mevedel-floating-composer-test--offset)))
            (should (eq frame (frame-focus parent)))
            (should (equal active (face-background 'child-frame-border frame)))
            ;; Leaving the minibuffer restores a window configuration.
            (should (eq window (mevedel-floating-composer-test--sound view-buf))))
        (remove-hook 'minibuffer-setup-hook record))
      (should (equal (list inactive) borders))))
  :doc "text scaling refits it without editing the draft"
  (unwind-protect
      (progn
        (mevedel-floating-composer-test--scroll-away t)
        (let ((window (selected-window))
              (height (frame-text-height (selected-frame))))
          (dotimes (_ 3) (mevedel-floating-composer-test--keys "C-x C-+" 'text-scale-adjust))
          (should (eq window (selected-window)))
          (should (> (frame-text-height (selected-frame)) height))
          (should (eq window (mevedel-floating-composer-test--sound view-buf)))
          (should (= 4 (mevedel-floating-composer-test--offset)))
          (should (equal mevedel-floating-composer-test--draft (mevedel-view--input-text)))
          (mevedel-floating-composer-test--keys "C-x C-0" 'text-scale-adjust)
          (should (= height (frame-text-height (selected-frame))))))
    (with-current-buffer view-buf (text-scale-set 0)))
  :doc "global text scaling refits it without editing the draft"
  (unwind-protect
      (progn
        (mevedel-floating-composer-test--scroll-away t)
        (let ((window (selected-window))
              (height (frame-text-height (selected-frame))))
          (dotimes (_ 2)
            (mevedel-floating-composer-test--keys
             "C-x C-M-+" 'global-text-scale-adjust))
          (should (eq window (selected-window)))
          (should (> (frame-text-height (selected-frame)) height))
          (should (eq window (mevedel-floating-composer-test--sound view-buf)))
          (mevedel-floating-composer-test--keys
           "C-x C-M-0" 'global-text-scale-adjust)
          (should (eq window (mevedel-floating-composer-test--sound view-buf)))
          (should (= 4 (mevedel-floating-composer-test--offset)))
          (should (equal mevedel-floating-composer-test--draft
                         (mevedel-view--input-text)))))
    (global-text-scale-adjust 0))
  :doc "a frame resize refits it to the host"
  (let ((width (frame-text-width parent))
        (height (frame-text-height parent)))
    (unwind-protect
        (progn
          (mevedel-floating-composer-test--scroll-away t)
          (let ((window (selected-window))
                (host-width (window-body-width host t))
                (deadline (+ (float-time) 2)))
            (set-frame-size parent (- width (* 12 (frame-char-width parent)))
                            (- height (* 4 (frame-char-height parent))) t)
            (while (and (= host-width (window-body-width host t)) (< (float-time) deadline))
              (mevedel-floating-composer-test--settle))
            (mevedel-floating-composer-test--settle)
            (should (< (window-body-width host t) host-width))
            (should (eq window (mevedel-floating-composer-test--sound view-buf)))
            (should (eq window (selected-window)))
            (should (= 4 (mevedel-floating-composer-test--offset)))))
      (set-frame-size parent width height t)
      (mevedel-floating-composer-test--settle)))
  :doc "a note-frame window on the view never gets one"
  (let ((note (mevedel-note-frame--make view-buf parent)))
    (unwind-protect
        (progn
          (set-frame-size note 60 12)
          (make-frame-visible note)
          (select-window (frame-root-window note))
          (goto-char (+ (mevedel-view--input-start) 4))
          (mevedel-floating-composer-test--to-bottom)
          (mevedel-floating-composer-test--keys "M-v" 'scroll-down-command)
          (should (eq note (selected-frame)))
          (should-not (mevedel-floating-composer--window)))
      (select-window host)
      (delete-frame note)))
  :doc "a historical segment never gets one"
  (progn
    (setq-local mevedel-view-segments--number 1
                mevedel-view-segments--buffer data-buf)
    (mevedel-floating-composer-test--scroll-away t)
    (should (eq host (selected-window)))
    (should-not (mevedel-floating-composer--window)))
  :doc "an agent transcript view never gets one"
  (progn
    (setq-local mevedel-view--agent-transcript-p t)
    (mevedel-floating-composer-test--scroll-away t)
    (should (eq host (selected-window)))
    (should-not (mevedel-floating-composer--window))))


;;
;;; Long drafts

(defun mevedel-floating-composer-test--tall-rail (host row-height vscroll)
  "Make the rail's first row ROW-HEIGHT pixels tall; start HOST VSCROLL into it.
The cursor sits on that row, outside the draft."
  (mevedel-view--start-spinner "Working")
  (let ((start (mevedel-floating-composer--start))
        (inhibit-read-only t))
    (put-text-property start (1+ start) 'display
                       `(space :width 1 :height (,row-height)))
    (goto-char start)
    (mevedel-view--set-viewport host (cons start vscroll))
    (redisplay t)
    (should (= start (window-start host)))
    (should (= vscroll (window-vscroll host t)))
    (should-not (mevedel-view--position-in-input-region-p (window-point host)))))

(defun mevedel-floating-composer-test--rail-at (host y)
  "Show the composer's start at pixel Y of HOST, the cursor two lines above."
  (let ((start (mevedel-floating-composer--start)))
    (mevedel-view--set-viewport host (mevedel-view--viewport-at-y host start y))
    (goto-char (save-excursion (goto-char start) (forward-line -2) (point)))
    (redisplay t)
    (should-not (mevedel-view--position-in-input-region-p (window-point host)))))

(mevedel-deftest mevedel-floating-composer--needed-p ()
  (mevedel-floating-composer-test--with-view
    (let* ((host (selected-window))
           (height (cdr (mevedel-view--window-body-geometry host)))
           (line (frame-char-height)))
      ;; Rail and draft together are taller than the window.
      (mevedel-floating-composer-test--grow-draft (* 2 (window-body-height host)))
      ,test))
  (test)
  :doc "the rail on the window's last full row hides it, however tall the draft"
  (progn
    (mevedel-floating-composer-test--rail-at host (- height line))
    (should-not (mevedel-view--position-screen-y (point-max) host))
    (should-not (mevedel-floating-composer--needed-p host)))
  :doc "a draft filling the window from the rail hides it"
  (progn
    (mevedel-floating-composer-test--rail-at host (* 4 line))
    (should-not (mevedel-floating-composer--needed-p host)))
  :doc "the rail cut off at the window's bottom shows it"
  (progn
    (mevedel-floating-composer-test--rail-at host (- height (/ line 2)))
    (should (mevedel-floating-composer--needed-p host)))
  :doc "an oversized rail row clipped at both edges shows it"
  (progn
    (mevedel-floating-composer-test--tall-rail host (+ height 120) 3)
    (should (mevedel-floating-composer--needed-p host)))
  :doc "a rail row clipped only at the top hides it when the next row shows"
  (progn
    (mevedel-floating-composer-test--tall-rail host (/ height 3) 3)
    (should-not (mevedel-floating-composer--needed-p host)))
  :doc "a window showing only transcript shows it"
  (progn
    (mevedel-view--set-viewport host (cons (point-min) 0))
    (goto-char (point-min))
    (redisplay t)
    (should (mevedel-floating-composer--needed-p host)))
  :doc "the cursor in the draft never shows it"
  (progn
    (mevedel-view--set-viewport host (cons (point-min) 0))
    (redisplay t)
    (goto-char (+ (mevedel-view--input-start) 4))
    (should-not (mevedel-floating-composer--needed-p host))))

(mevedel-deftest mevedel-floating-composer/long-draft ()
  (mevedel-floating-composer-test--with-view
    (let ((host (selected-window)))
      ,test))
  (test)
  :doc "moving the cursor through a capped draft keeps it visible at the cap"
  (progn
    (mevedel-floating-composer-test--grow-draft (+ 4 (/ (window-body-height host) 2)))
    (mevedel-floating-composer-test--to-bottom)
    (mevedel-floating-composer-test--scroll-away t)
    (let* ((window (mevedel-floating-composer-test--window))
           (frame (window-frame window))
           (cap (/ (window-body-height host t) 2))
           (rail (mevedel-floating-composer--start))
           (moves 0))
      (should (eq window (selected-window)))
      (should (= 4 (mevedel-floating-composer-test--offset)))
      (cl-flet ((check ()
                  (should (eq window (selected-window)))
                  (should (mevedel-view--position-screen-y (point) window t))
                  (should (>= (window-start window) rail))
                  (should (= cap (frame-text-height frame)))))
        (check)
        (while (and (< moves 100) (< (line-end-position) (point-max)))
          (mevedel-floating-composer-test--keys "C-n" 'next-line)
          (cl-incf moves)
          (check))
        (should (= (line-end-position) (point-max)))
        (while (and (> moves 0) (> (line-beginning-position) (mevedel-view--input-start)))
          (mevedel-floating-composer-test--keys "C-p" 'previous-line)
          (cl-decf moves)
          (check))
        (should (= 4 (mevedel-floating-composer-test--offset))))))
  :doc "a draft taller than the window returns inline at the cursor's edited offset"
  (progn
    (mevedel-floating-composer-test--grow-draft (* 2 (window-body-height host)))
    ;; The cursor's line sits on the host's last row.
    (mevedel-view--set-viewport
     host (mevedel-view--viewport-at-y
           host (point) (- (cdr (mevedel-view--window-body-geometry host))
                           (line-pixel-height))))
    (redisplay t)
    (should (mevedel-view--position-screen-y (point) host t))
    (mevedel-floating-composer-test--scroll-away t)
    (let ((window (selected-window))
          (notches 0))
      (should (eq window (mevedel-floating-composer-test--window)))
      (should (= 4 (mevedel-floating-composer-test--offset)))
      ;; An inline return still pending at offset 4 would drop this edit's move.
      (mevedel-floating-composer-test--keys "x" 'self-insert-command)
      (while (and (< notches 60) (eq window (selected-window)))
        (mevedel-floating-composer-test--wheel 'wheel-down host)
        (cl-incf notches))
      (should (eq host (selected-window)))
      (should (= 5 (mevedel-floating-composer-test--offset)))
      (should (mevedel-view--position-screen-y (point) host t))
      (should-not (mevedel-view--position-screen-y (point-max) host))
      (should-not (mevedel-floating-composer-test--window))
      (should (string-prefix-p "> loxng draft line 1\n" (mevedel-view--input-text))))))

;;; test-mevedel-floating-composer-graphical.el ends here
