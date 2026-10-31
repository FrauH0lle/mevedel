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
;;; Inbox reproductions (graphical)

(defun mevedel-view-user-state-test--activate-row (text)
  "Put point on the background row showing TEXT and press RET there."
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line)
  (let ((last-nonmenu-event nil))
    (call-interactively (key-binding (kbd "RET")))))

(mevedel-deftest mevedel-view-user-state/collapse-background-row
    (:expected-result :failed)
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

(defun mevedel-view-user-state-test--wheel (direction)
  "Scroll the selected window one notch in DIRECTION through the command loop."
  (let* ((window (selected-window))
         (event (list direction (posn-at-point (window-start window) window)))
         (done nil)
         (hook (lambda ()
                 (when (eq this-command 'mwheel-scroll)
                   (setq done t)
                   (run-at-time 0 nil #'exit-recursive-edit))))
         (timer (run-at-time 3 nil #'exit-recursive-edit)))
    (add-hook 'post-command-hook hook)
    (unwind-protect
        (progn
          (setq unread-command-events (list event))
          (recursive-edit))
      (remove-hook 'post-command-hook hook)
      (cancel-timer timer)
      (setq unread-command-events nil))
    (should done)
    (redisplay t)))

(mevedel-deftest mevedel-view-user-state/scroll-return
    (:expected-result :failed)
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

(provide 'test-mevedel-view-user-state)
;;; test-mevedel-view-user-state.el ends here
