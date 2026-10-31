;;; test-mevedel-view-user-state.el --- User view state across view writes -*- lexical-binding: t -*-

;;; Commentary:

;; A small driver over real production update paths.  It shows one view in
;; a composer window, holding a multiline draft that starts with `>' and has
;; point at an offset, above a history reader.  Before and after a write it
;; reads the draft, the composer-relative point, the composer's screen row
;; and the reader's point and window start.  Batch runs use the shared
;; unit-row geometry model; the graphical check runs every case natively,
;; including the reproductions that need real scrolling and pixel geometry.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mwheel)
(require 'mevedel-execution)
(require 'mevedel-view)
(require 'mevedel-view-agent)
(require 'mevedel-view-composer)
(require 'mevedel-view-execution)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)
(require 'mevedel-view-zone)

(defconst mevedel-view-user-state-test--draft "> active draft\nsecond line"
  "Multiline draft whose first editable character is `>'.")

(defmacro mevedel-view-user-state-test--with-view (&rest body)
  "Run BODY in a view with history and one running background command.
BODY can set `clock', `executions' and `agents', which the background panel
and elapsed labels read.  `data-buf' and `view-buf' are bound too."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (delete-other-windows)
     (mevedel-view-test--with-buffers
       (dotimes (i 60)
         (mevedel-view-test--insert-data
          data-buf (format "Line %d of the earlier response.\n" i) 'response))
       (switch-to-buffer view-buf)
       (let* ((session (mevedel-session--create :name "user-state"))
              (clock 1000.0)
              (original-float-time (symbol-function 'float-time))
              (executions
               (list (list :execution-id "user-state-exec" :owner "/root"
                           :yielded t :state 'running
                           :description "Inspect runner and group CI conventions"
                           :started-at 848.0)))
              (agents nil)
              (mevedel-view-spinner-animate nil))
         (setq-local mevedel--session session)
         (with-current-buffer data-buf (setq-local mevedel--session session))
         (cl-letf (((symbol-function 'current-time)
                    (lambda () (seconds-to-time clock)))
                   ((symbol-function 'float-time)
                    (lambda (&optional time)
                      (if time (funcall original-float-time time) clock)))
                   ((symbol-function 'frame-focus-state) (lambda (&optional _) t))
                   ((symbol-function 'mevedel-execution-count-user)
                    (lambda (_) (length executions)))
                   ((symbol-function 'mevedel-execution-list-user)
                    (lambda (_) executions))
                   ((symbol-function 'mevedel-view--agent-status-collect)
                    (lambda () agents)))
           (mevedel-view--full-rerender)
           ,@body)))))

(defun mevedel-view-user-state-test--arrange ()
  "Show the current view in a composer window above a history reader.
Insert the draft with point at offset 4 and show the composer at the
bottom of its window; the reader visits a line of earlier history.
Return (COMPOSER . READER)."
  (delete-other-windows)
  (let ((composer (selected-window))
        (reader (split-window-below)))
    (set-window-buffer reader (current-buffer))
    (mevedel-view-test--insert-composer-draft mevedel-view-user-state-test--draft 4)
    (if noninteractive
        (set-window-start composer
                          (save-excursion
                            (goto-char (point-max))
                            (forward-line (- 1 (window-body-height composer)))
                            (point)))
      ;; The exact last page, as growth leaves it.  `recenter' keeps up to a
      ;; line of blank pixels, which the first overflowing growth consumes.
      (let* ((end (point-max))
             (y (- (cdr (mevedel-view--window-body-geometry composer))
                   (with-selected-window composer
                     (save-excursion (goto-char end) (line-pixel-height))))))
        (mevedel-view--set-viewport
         composer (mevedel-view--viewport-at-y composer end y)))
      (redisplay t))
    (let ((line (save-excursion
                  (goto-char (point-min))
                  (search-forward "Line 20 of")
                  (line-beginning-position))))
      (set-window-point reader (+ line 3))
      (set-window-start reader (save-excursion
                                 (goto-char line)
                                 (forward-line -2)
                                 (point))))
    (unless noninteractive (redisplay t))
    (cons composer reader)))

(defun mevedel-view-user-state-test--observe (windows)
  "Return what the user sees in WINDOWS, a (COMPOSER . READER) pair."
  (unless noninteractive (redisplay t))
  (let ((composer (car windows))
        (reader (cdr windows))
        (start (mevedel-view--input-start)))
    (cl-flet ((line-at (position)
                (save-excursion
                  (goto-char position)
                  (list (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))
                        (- position (line-beginning-position))))))
      (list :draft (mevedel-view--input-text)
            :point (- (point) start)
            :composer-point (- (window-point composer) start)
            :composer-y (mevedel-view--position-screen-y start composer)
            :reader-point (line-at (window-point reader))
            :reader-start (line-at (window-start reader))))))

(defmacro mevedel-view-user-state-test--preserves (windows &rest body)
  "Assert that BODY leaves the user's state in WINDOWS unchanged."
  (declare (indent 1) (debug t))
  (let ((before (make-symbol "before")))
    `(let ((,before (mevedel-view-user-state-test--observe ,windows)))
       ,@body
       (should (equal ,before (mevedel-view-user-state-test--observe ,windows))))))

(defun mevedel-view-user-state-test--add-chunk (data-buf text)
  "Append response TEXT to DATA-BUF's transcript."
  (mevedel-view-test--insert-data data-buf text 'response))

(defun mevedel-view-user-state-test--begin-turn (data-buf)
  "Start an in-flight turn of DATA-BUF with streamed text, as a request does."
  (mevedel-view-stream-begin-turn
   (mevedel-view--history-insertion-marker)
   (with-current-buffer data-buf (point-max)))
  (mevedel-view-user-state-test--add-chunk data-buf "Streaming response begins.\n")
  (mevedel-view--render-stream-update data-buf))

(defun mevedel-view-user-state-test--rail ()
  "Return the text of the permanent composer rail."
  (let ((region (mevedel-view-zone-region 'progress)))
    (buffer-substring-no-properties (overlay-start region) (overlay-end region))))

(defun mevedel-view-user-state-test--register-agent (data-buf)
  "Register a tool rendering an agent handle and add two calls to DATA-BUF."
  (mevedel-tool-register
   (mevedel-tool--create
    :name "UserStateAgent" :category "mevedel"
    :renderer (lambda (_name _args result _data)
                (list :header "Agent: user-state worker" :body result
                      :body-mode 'text-mode :vtype 'agent-handle
                      :agent-path "/root/worker" :agent-status 'running
                      :initially-collapsed-p t))))
  (dotimes (n 2)
    (mevedel-view-test--insert-data
     data-buf "(:name \"UserStateAgent\" :args nil)\n\nAgent detail\n"
     (cons 'tool (format "user-state-agent-%d" n)))
    (mevedel-view-test--insert-data data-buf "Between calls\n" 'response))
  (mevedel-view--full-rerender))


;;
;;; Asynchronous writers

(mevedel-deftest mevedel-view-user-state/writers ()
  (mevedel-view-user-state-test--with-view
    ,setup
    (let ((windows (mevedel-view-user-state-test--arrange)))
      (mevedel-view-user-state-test--preserves windows
        ,action)))
  (setup action)
  :doc "a streamed chunk keeps the draft, composer row and reader"
  (mevedel-view-user-state-test--begin-turn data-buf)
  (progn
    (mevedel-view-user-state-test--add-chunk data-buf "More streamed text.\nAnd a line.\n")
    (mevedel-view--render-stream-update data-buf)
    (should (string-search "And a line." (buffer-string))))
  :doc "a full rerender keeps the draft, composer row and reader"
  nil
  (progn
    (mevedel-view-user-state-test--add-chunk data-buf "Rerendered tail.\n")
    (mevedel-view--full-rerender)
    (should (string-search "Rerendered tail." (buffer-string))))
  :doc "settling a response keeps the draft, composer row and reader"
  (mevedel-view-user-state-test--begin-turn data-buf)
  (progn
    (mevedel-view-user-state-test--add-chunk data-buf "Final settled text.\n")
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (should (string-search "Final settled text." (buffer-string))))
  :doc "starting request progress keeps the draft, composer row and reader"
  nil
  (let ((rail (mevedel-view-user-state-test--rail)))
    (mevedel-view--start-spinner "Thinking...")
    (should-not (equal rail (mevedel-view-user-state-test--rail))))
  :doc "a request status update keeps the draft, composer row and reader"
  (mevedel-view--start-spinner "Thinking...")
  (progn
    (mevedel-view--update-spinner "Running a much longer status label" 'request)
    (should (string-search "much longer status" (buffer-string))))
  :doc "stopping request progress keeps the draft, composer row and reader"
  (mevedel-view--start-spinner "Thinking...")
  (let ((rail (mevedel-view-user-state-test--rail)))
    (mevedel-view--stop-spinner)
    (should-not (equal rail (mevedel-view-user-state-test--rail))))
  :doc "a semantic spinner tick keeps the draft, composer row and reader"
  (progn
    (mevedel-view--start-spinner "Working...")
    (setq mevedel-view--spinner-start-time (seconds-to-time 988.0)
          mevedel-view--spinner-last-second 1000)
    (mevedel-view--ensure-request-progress))
  (progn
    (setq clock 1001.0)
    (mevedel-view--spinner-tick)
    (should (string-search "13s" (buffer-string))))
  :doc "a background elapsed tick keeps the draft, composer row and reader"
  nil
  (progn
    (setq clock 1001.0)
    (mevedel-view-execution-tick)
    (should (string-search "2m 33s" (buffer-string))))
  :doc "a background row arriving keeps the draft, composer row and reader"
  nil
  (progn
    (setq agents (list (list :path "/root/arriving" :status 'running
                             :started-at 990.0)))
    (mevedel-view--render-status)
    (should (string-search "/root/arriving" (buffer-string))))
  :doc "a background row leaving keeps the draft, composer row and reader"
  nil
  (progn
    ;; The finished row lingers for its grace period, then leaves.
    (setq executions nil)
    (mevedel-view--render-status)
    (setq clock (+ clock 30))
    (mevedel-view--render-status)
    (should-not (string-search "Inspect runner" (buffer-string))))
  :doc "an agent handle refresh keeps the draft, composer row and reader"
  (mevedel-view-user-state-test--register-agent data-buf)
  (progn
    (should (mevedel-view--refresh-agent-rendering-now "/root/worker"))
    (should (= 2 (mevedel-view-test--count-substring
                  "Agent: user-state worker" (buffer-string))))))

(mevedel-deftest mevedel-view-user-state/nesting ()
  (mevedel-view-user-state-test--with-view
    (mevedel-view-user-state-test--begin-turn data-buf)
    (mevedel-view--start-spinner "Thinking...")
    (let ((windows (mevedel-view-user-state-test--arrange))
          (original (symbol-function ',inner))
          (entered 0))
      (mevedel-view-user-state-test--preserves windows
        (cl-letf (((symbol-function ',inner)
                   (lambda (&rest args)
                     (prog1 (apply original args)
                       (when (and (= entered 0) ,condition)
                         (cl-incf entered)
                         ,nested)))))
          ,outer))
      ;; The injection must have run exactly once inside the outer writer.
      (should (= 1 entered))
      (should (string-search "Nested chunk." (buffer-string)))
      (should (string-search "Nested status" (buffer-string)))))
  (inner condition nested outer)
  :doc "rail reconciliation nested inside a streamed chunk"
  mevedel-view--render-turn t
  (mevedel-view--update-spinner "Nested status" 'request)
  (progn
    (mevedel-view-user-state-test--add-chunk data-buf "Nested chunk.\n")
    (mevedel-view--render-stream-update data-buf))
  :doc "a streamed chunk nested inside rail reconciliation"
  mevedel-view-zone--reconcile (eq (car args) (mevedel-view-zone-region 'progress))
  (progn
    (mevedel-view-user-state-test--add-chunk data-buf "Nested chunk.\n")
    (mevedel-view--render-stream-update data-buf))
  (mevedel-view--update-spinner "Nested status" 'request))


;;
;;; Region promises and request progress geometry

(defvar mevedel-view-user-state-test--region-checks nil
  "Region checks: (PREFIX-DIFFERENCE SUFFIX-DIFFERENCE), nil when unchanged.")

(defun mevedel-view-user-state-test--difference (old new)
  "Return nil when strings OLD and NEW are equal with properties.
Otherwise describe their first difference.  Ignore `fontified': jit-lock
sets it when geometry queries fontify text, which replaces nothing."
  (setq old (copy-sequence old) new (copy-sequence new))
  (remove-text-properties 0 (length old) '(fontified nil) old)
  (remove-text-properties 0 (length new) '(fontified nil) new)
  (unless (equal-including-properties old new)
    (let ((i 0) (limit (min (length old) (length new))))
      (while (and (< i limit)
                  (eq (aref old i) (aref new i))
                  (equal (text-properties-at i old) (text-properties-at i new)))
        (setq i (1+ i)))
      (list :at i :lengths (list (length old) (length new))
            :old (substring old i (min (length old) (+ i 30)))
            :old-properties (and (< i (length old)) (text-properties-at i old))
            :new (substring new i (min (length new) (+ i 30)))
            :new-properties (and (< i (length new)) (text-properties-at i new))))))

(defun mevedel-view-user-state-test--validating-region (original thunk &optional region)
  "Call ORIGINAL with THUNK and REGION, checking text outside REGION.
Text and properties before and after REGION must be identical afterwards:
REGION promises that THUNK replaces only that text."
  (if (not region)
      (funcall original thunk region)
    (let* ((start (copy-marker (car region)))
           (end (copy-marker (cdr region) t))
           (before (buffer-substring (point-min) start))
           (after (buffer-substring end (point-max))))
      (prog1 (funcall original thunk region)
        (push (list (mevedel-view-user-state-test--difference
                     before (buffer-substring (point-min) start))
                    (mevedel-view-user-state-test--difference
                     after (buffer-substring end (point-max))))
              mevedel-view-user-state-test--region-checks)
        (set-marker start nil)
        (set-marker end nil)))))

(mevedel-deftest mevedel-view-user-state/region ()
  (mevedel-view-user-state-test--with-view
    ,setup
    (let ((windows (mevedel-view-user-state-test--arrange))
          (mevedel-view-user-state-test--region-checks nil))
      (unwind-protect
          (progn
            (advice-add 'mevedel-view-call-preserving-user-view-state :around
                        #'mevedel-view-user-state-test--validating-region)
            (mevedel-view-user-state-test--preserves windows
              ,action))
        (advice-remove 'mevedel-view-call-preserving-user-view-state
                       #'mevedel-view-user-state-test--validating-region))
      ;; The writer used the region path, and kept its promise.
      (should mevedel-view-user-state-test--region-checks)
      (dolist (check mevedel-view-user-state-test--region-checks)
        (should (equal check '(nil nil))))))
  (setup action)
  :doc "negative control: a write outside the promised region is detected"
  nil
  (let ((at (save-excursion
              (goto-char (point-min)) (search-forward "Line 5 ") (point)))
        (inhibit-read-only t))
    (mevedel-view-call-preserving-user-view-state
     (lambda () (save-excursion (goto-char (point-min)) (insert "outside ")))
     (cons at (1+ at)))
    (should (car (car mevedel-view-user-state-test--region-checks)))
    ;; Undo it with a kept promise, leaving only valid records.
    (setq mevedel-view-user-state-test--region-checks nil)
    (mevedel-view-call-preserving-user-view-state
     (lambda () (save-excursion (goto-char (point-min)) (delete-char 8)))
     (cons (point-min) (+ (point-min) 8))))
  :doc "a background elapsed tick replaces only its row"
  nil
  (progn
    (setq clock 1001.0)
    (mevedel-view-execution-tick)
    (should (string-search "2m 33s" (buffer-string))))
  :doc "a semantic rail tick replaces only the rail"
  (progn
    (mevedel-view--start-spinner "Working...")
    (setq mevedel-view--spinner-start-time (seconds-to-time 988.0)
          mevedel-view--spinner-last-second 1000)
    (mevedel-view--ensure-request-progress))
  (progn
    (setq clock 1001.0)
    (mevedel-view--spinner-tick)
    (should (string-search "13s" (buffer-string)))))

(mevedel-deftest mevedel-view-user-state/request-progress-width ()
  (mevedel-view-user-state-test--with-view
    (mevedel-view--start-spinner "Thinking about a fairly long status label")
    (let* ((windows (mevedel-view-user-state-test--arrange))
           (rail (mevedel-view-user-state-test--rail)))
      ;; A narrower window now shows the view: the rail must refit and can
      ;; rewrap, while every window keeps the user's state.
      (with-selected-window (car windows) (split-window-right))
      (mevedel-view-user-state-test--preserves windows
        ,action
        (should-not (equal rail (mevedel-view-user-state-test--rail))))))
  (action)
  :doc "a status update refits the rail to a narrower window"
  (mevedel-view--update-spinner "Running another fairly long status label" 'request)
  :doc "stopping progress refits the idle rail to a narrower window"
  (mevedel-view--stop-spinner))


;;
;;; Inbox reproductions (graphical)

(defun mevedel-view-user-state-test--activate-row (text)
  "Put point on the background row showing TEXT and press RET there."
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line)
  (let ((last-nonmenu-event nil))
    (call-interactively (key-binding (kbd "RET")))))

(mevedel-deftest mevedel-view-user-state/collapse-background-row ()
  (progn
    (skip-unless (display-graphic-p))
    (mevedel-view-user-state-test--with-view
      (mevedel-view-test--insert-composer-draft
       mevedel-view-user-state-test--draft 4)
      (redisplay t)
      (mevedel-view-user-state-test--activate-row "Inspect runner")
      (should (looking-at-p "  ▾ Running"))
      ;; The composer sits mid-window with blank space below it, as after
      ;; `recenter' or an earlier shrink.
      (save-excursion
        (goto-char (mevedel-view--input-start))
        (recenter 20))
      (redisplay t)
      (let ((composer-y (mevedel-view-test--composer-screen-y (selected-window))))
        (should (mevedel-view--window-blank-pixels (selected-window)))
        (mevedel-view-user-state-test--activate-row "Inspect runner")
        (redisplay t)
        (should (looking-at-p "  ▸ Running"))
        (should (equal mevedel-view-user-state-test--draft (mevedel-view--input-text)))
        (should (equal composer-y
                       (mevedel-view-test--composer-screen-y (selected-window)))))))
  (test)
  :doc "collapsing a background row keeps a mid-window composer in place"
  nil)

(defun mevedel-view-user-state-test--wheel (direction &optional window)
  "Scroll WINDOW one notch in DIRECTION through the command loop.
WINDOW defaults to the selected window, but need not be selected."
  (let ((window (or window (selected-window))))
    (mevedel-view-user-state-test--deliver-scroll
     (list (list direction (posn-at-point (window-start window) window)))
     'mwheel-scroll)))

(mevedel-deftest mevedel-view-user-state/status-visit ()
  (mevedel-view-user-state-test--with-view
    (mevedel-view-test--insert-composer-draft
     mevedel-view-user-state-test--draft 4)
    (goto-char (point-min))
    (search-forward "[Manage]")
    (backward-char (length "[Manage]"))
    (unless noninteractive (redisplay t))
    (let ((y (mevedel-view--position-screen-y (point) (selected-window))))
      (should y)
      ,action
      (unless noninteractive (redisplay t))
      (should (looking-at-p (regexp-quote "[Manage]")))
      (should (equal y (mevedel-view--position-screen-y (point) (selected-window))))
      (should (equal mevedel-view-user-state-test--draft (mevedel-view--input-text)))))
  (action)
  :doc "a background row arriving above a visited [Manage] row keeps that row still"
  (progn
    (setq agents (list (list :path "/root/arriving" :status 'running
                             :started-at 990.0)))
    (mevedel-view--render-status)
    ;; A visited panel keeps its rows and only counts new work.
    (should (string-search "1 agent" (buffer-string))))
  :doc "a streamed full rerender keeps a visited [Manage] row still"
  (progn
    (mevedel-view-user-state-test--add-chunk data-buf "Arrived meanwhile.\n")
    (mevedel-view--full-rerender)))

(defun mevedel-view-user-state-test--deliver-scroll (events command)
  "Deliver EVENTS through the command loop and wait for scroll COMMAND."
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
    (redisplay t)))

(mevedel-deftest mevedel-view-user-state/scroll-return ()
  (progn
    (skip-unless (display-graphic-p))
    (let ((scroll-preserve-screen-position t)
          (mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))
          (mouse-wheel-progressive-speed nil))
      (mevedel-view-user-state-test--with-view
        (mevedel-view-test--insert-composer-draft
         mevedel-view-user-state-test--draft 4)
        (save-excursion (goto-char (point-max)) (recenter -1))
        (redisplay t)
        (dotimes (_ 3) (mevedel-view-user-state-test--wheel 'wheel-up))
        ;; Emacs keeps point visible, so it left the composer for chrome rows.
        (should-not (mevedel-view--point-in-input-region-p))
        ,redraw
        (let ((notches 0))
          (while (and (< notches 8)
                      (not (mevedel-view--position-screen-y
                            (point-max) (selected-window) t)))
            (mevedel-view-user-state-test--wheel 'wheel-down)
            (cl-incf notches)))
        (should (mevedel-view--position-screen-y (point-max) (selected-window) t))
        (should (equal mevedel-view-user-state-test--draft (mevedel-view--input-text)))
        (should (= 4 (- (point) (mevedel-view--input-start)))))))
  (redraw)
  :doc "scrolling away from the composer and back returns point to the draft"
  nil
  :doc "the return survives redraws while scrolled away"
  (progn
    (setq clock (1+ clock))
    (mevedel-view--spinner-tick)
    (mevedel-view-user-state-test--add-chunk data-buf "Arrived while scrolled away.\n")
    (mevedel-view--full-rerender)
    (setq agents (list (list :path "/root/arriving" :status 'running
                             :started-at 990.0)))
    (mevedel-view--render-status)))

(mevedel-deftest mevedel-view-user-state/scroll-unselected ()
  (progn
    (skip-unless (display-graphic-p))
    (let ((scroll-preserve-screen-position t)
          (mouse-wheel-follow-mouse t)
          (mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))
          (mouse-wheel-progressive-speed nil))
      (mevedel-view-user-state-test--with-view
        (let* ((windows (mevedel-view-user-state-test--arrange))
               (composer (car windows))
               (reader (cdr windows)))
          (with-temp-buffer
            (when ,different-buffer
              (insert "The selected window is not a chat view.\n")
              (set-window-buffer reader (current-buffer)))
            (select-window reader)
            (let ((reader-point (window-point reader))
                  (reader-start (window-start reader)))
              (dotimes (_ ,up-count) ,up)
              (with-current-buffer view-buf
                (should (< (window-point composer) (mevedel-view--input-start))))
              (let ((notches 0))
                (while (and (< notches 8)
                            (with-current-buffer view-buf
                              (not (mevedel-view--position-screen-y
                                    (point-max) composer t))))
                  ,down
                  (cl-incf notches)))
              (with-current-buffer view-buf
                (should (mevedel-view--position-screen-y (point-max) composer t))
                (should (equal mevedel-view-user-state-test--draft
                               (mevedel-view--input-text)))
                (should (= 4 (- (window-point composer) (mevedel-view--input-start)))))
              (should-not (window-parameter composer 'mevedel-view-scroll-return))
              (should (eq reader (selected-window)))
              (should (= reader-point (window-point reader)))
              (should (= reader-start (window-start reader)))))))))
  (different-buffer up-count up down)
  :doc "wheel scrolling a non-selected view window restores its own draft offset"
  nil 3
  (mevedel-view-user-state-test--wheel 'wheel-up composer)
  (mevedel-view-user-state-test--wheel 'wheel-down composer)
  :doc "wheel scrolling a view from another buffer restores its draft offset"
  t 3
  (mevedel-view-user-state-test--wheel 'wheel-up composer)
  (mevedel-view-user-state-test--wheel 'wheel-down composer)
  :doc "scroll-other-window restores the non-selected window's draft offset"
  nil 3
  (mevedel-view-user-state-test--deliver-scroll
   (listify-key-sequence (kbd "C-u 2 C-M-S-v")) 'scroll-other-window-down)
  (mevedel-view-user-state-test--deliver-scroll
   (listify-key-sequence (kbd "C-u 2 C-M-v")) 'scroll-other-window)
  :doc "scroll-other-window from another buffer restores the view's draft offset"
  t 3
  (mevedel-view-user-state-test--deliver-scroll
   (listify-key-sequence (kbd "C-u 2 C-M-S-v")) 'scroll-other-window-down)
  (mevedel-view-user-state-test--deliver-scroll
   (listify-key-sequence (kbd "C-u 2 C-M-v")) 'scroll-other-window))

(mevedel-deftest mevedel-view--return-from-scroll ()
  (mevedel-view-user-state-test--with-view
    (mevedel-view-test--insert-composer-draft
     mevedel-view-user-state-test--draft 4)
    (let* ((transcript (save-excursion
                         (goto-char (point-min))
                         (search-forward "Line 20 ")
                         (point)))
           (bottom (save-excursion
                     (goto-char (point-max))
                     (forward-line (- 1 (window-body-height)))
                     (point)))
           (away (save-excursion (goto-char transcript) (line-beginning-position))))
      (set-window-start nil bottom)
      (cl-flet ((run (command thunk)
                  (let ((this-command command))
                    (run-hooks 'pre-command-hook)
                    (funcall thunk)
                    (run-hooks 'post-command-hook))))
        ;; A scroll displaces point from the draft.
        (run 'scroll-down-command
             (lambda () (set-window-start nil away) (goto-char transcript)))
        (should (= 4 (window-parameter nil 'mevedel-view-scroll-return)))
        ,action)))
  (action)
  :doc "a later scroll showing the draft returns point to its offset"
  (progn
    (run 'mwheel-scroll (lambda () (set-window-start nil bottom)))
    (should (= 4 (- (point) (mevedel-view--input-start))))
    (should-not (window-parameter nil 'mevedel-view-scroll-return)))
  :doc "a deliberate command cancels the return"
  (progn
    (run 'forward-char #'ignore)
    (run 'mwheel-scroll (lambda () (set-window-start nil bottom)))
    (should (= transcript (point)))
    (should-not (window-parameter nil 'mevedel-view-scroll-return))))

(provide 'test-mevedel-view-user-state)
;;; test-mevedel-view-user-state.el ends here
