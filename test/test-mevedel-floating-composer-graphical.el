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
(require 'mevedel-floating-composer)
(require 'mevedel-view)
(require 'mevedel-view-agent)
(require 'mevedel-view-animation)
(require 'mevedel-view-composer)
(require 'mevedel-view-interaction)
(require 'mevedel-view-native)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)

(defvar pgtk-wait-for-event-timeout)

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
                     (face-background 'child-frame-border frame)))
      (mevedel-floating-composer-test--click host)
      (should (eq host (selected-window)))
      (should (memq (frame-focus (window-frame host)) (list nil (window-frame host))))
      (should (mevedel-floating-composer-test--focused-p (window-frame host)))
      (should (equal (mevedel-floating-composer-test--border-color
                      'mevedel-note-frame-border-inactive)
                     (face-background 'child-frame-border frame)))))
  :doc "removing it clears the redirect after another frame was selected"
  (let ((parent (window-frame host))
        (other (with-current-buffer (get-buffer-create "*scratch*")
                 (make-frame '((width . 30) (height . 5))))))
    (unwind-protect
        (progn
          ;; Mapping OTHER takes compositor focus; the parent must hold it.
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

;;; test-mevedel-floating-composer-graphical.el ends here
