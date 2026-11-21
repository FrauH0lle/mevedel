;;; test-niwa-view-user-state.el --- User view state across view writes -*- lexical-binding: t -*-

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
(require 'niwa-execution)
(require 'niwa-view)
(require 'niwa-view-agent)
(require 'niwa-view-composer)
(require 'niwa-view-execution)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-view-viewport)
(require 'niwa-view-zone)

(defconst niwa-view-user-state-test--draft "> active draft\nsecond line"
  "Multiline draft whose first editable character is `>'.")

(defmacro niwa-view-user-state-test--with-view (&rest body)
  "Run BODY in a view with history and one running background command.
BODY can set `clock', `executions' and `agents', which the background panel
and elapsed labels read.  `data-buf' and `view-buf' are bound too."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (delete-other-windows)
     (niwa-view-test--with-buffers
       (dotimes (i 60)
         (niwa-view-test--insert-data
          data-buf (format "Line %d of the earlier response.\n" i) 'response))
       (switch-to-buffer view-buf)
       (let* ((session (niwa-session--create :name "user-state"))
              (clock 1000.0)
              (original-float-time (symbol-function 'float-time))
              (executions
               (list (list :execution-id "user-state-exec" :owner "/root"
                           :yielded t :state 'running
                           :description "Inspect runner and group CI conventions"
                           :started-at 848.0)))
              (agents nil)
              (niwa-view-spinner-animate nil))
         (setq-local niwa--session session)
         (with-current-buffer data-buf (setq-local niwa--session session))
         (cl-letf (((symbol-function 'current-time)
                    (lambda () (seconds-to-time clock)))
                   ((symbol-function 'float-time)
                    (lambda (&optional time)
                      (if time (funcall original-float-time time) clock)))
                   ((symbol-function 'frame-focus-state) (lambda (&optional _) t))
                   ((symbol-function 'niwa-execution-count-user)
                    (lambda (_) (length executions)))
                   ((symbol-function 'niwa-execution-list-user)
                    (lambda (_) executions))
                   ((symbol-function 'niwa-view--agent-status-collect)
                    (lambda () agents)))
           (niwa-view--full-rerender)
           ,@body)))))

(defun niwa-view-user-state-test--arrange ()
  "Show the current view and its composer above a second transcript window.
Insert the draft with point at offset 4 and show the composer at the
bottom of its window; the reader visits a line of earlier history.
Return (VIEW-WINDOW . READER)."
  (delete-other-windows)
  (let ((view-window (selected-window))
        (reader (split-window-below)))
    (set-window-buffer reader (current-buffer))
    (niwa-view-test--insert-composer-draft niwa-view-user-state-test--draft 4)
    (should (niwa-view-composer-display view-window))
    (if noninteractive
        (set-window-start view-window
                          (save-excursion
                            (goto-char (point-max))
                            (forward-line (- 1 (window-body-height view-window)))
                            (point)))
      ;; The exact last page, as growth leaves it.  `recenter' keeps up to a
      ;; line of blank pixels, which the first overflowing growth consumes.
      (let* ((end (point-max))
             (y (- (cdr (niwa-view--window-body-geometry view-window))
                   (with-selected-window view-window
                     (save-excursion (goto-char end) (line-pixel-height))))))
        (niwa-view--set-viewport
         view-window (niwa-view--viewport-at-y view-window end y)))
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
    (cons view-window reader)))

(defun niwa-view-user-state-test--draft-offset ()
  "Return the caret offset from the draft start, read in the composer buffer."
  (let ((view (current-buffer)))
    (with-current-buffer (niwa-view-composer-buffer view)
      (- (point) (niwa-view-composer-draft-start view)))))

(defun niwa-view-user-state-test--observe (windows)
  "Return what the user sees in WINDOWS, a (VIEW-WINDOW . READER) pair."
  (unless noninteractive (redisplay t))
  (let ((composer (niwa-view-composer-window (car windows)))
        (reader (cdr windows))
        (start (niwa-view-composer-draft-start)))
    (cl-flet ((line-at (position)
                (save-excursion
                  (goto-char position)
                  (list (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))
                        (- position (line-beginning-position))))))
      (list :draft (niwa-view-composer-draft-text)
            :point (niwa-view-user-state-test--draft-offset)
            :composer-point (- (window-point composer) start)
            :composer-y (niwa-view--position-screen-y start composer)
            :reader-point (line-at (window-point reader))
            :reader-start (line-at (window-start reader))))))

(defmacro niwa-view-user-state-test--preserves (windows &rest body)
  "Assert that BODY leaves the user's state in WINDOWS unchanged."
  (declare (indent 1) (debug t))
  (let ((before (make-symbol "before")))
    `(let ((,before (niwa-view-user-state-test--observe ,windows)))
       ,@body
       (should (equal ,before (niwa-view-user-state-test--observe ,windows))))))

(defun niwa-view-user-state-test--add-chunk (data-buf text)
  "Append response TEXT to DATA-BUF's transcript."
  (niwa-view-test--insert-data data-buf text 'response))

(defun niwa-view-user-state-test--begin-turn (data-buf)
  "Start an in-flight turn of DATA-BUF with streamed text, as a request does."
  (niwa-view-stream-begin-turn
   (niwa-view--history-insertion-marker)
   (with-current-buffer data-buf (point-max)))
  (niwa-view-user-state-test--add-chunk data-buf "Streaming response begins.\n")
  (niwa-view--render-stream-update data-buf))

(defun niwa-view-user-state-test--register-agent (data-buf &optional header)
  "Add two agent calls to DATA-BUF, optionally calling HEADER for their label."
  (niwa-tool-register
   (niwa-tool--create
    :name "UserStateAgent" :category "niwa"
    :renderer (lambda (_name _args result _data)
                (list :header (if header (funcall header) "Agent: user-state worker") :body result
                      :body-mode 'text-mode :vtype 'agent-handle
                      :agent-path "/root/worker" :agent-status 'running
                      :initially-collapsed-p t))))
  (dotimes (n 2)
    (niwa-view-test--insert-data
     data-buf "(:name \"UserStateAgent\" :args nil)\n\nAgent detail\n"
     (cons 'tool (format "user-state-agent-%d" n)))
    (niwa-view-test--insert-data data-buf "Between calls\n" 'response))
  (niwa-view--full-rerender))


;;
;;; Asynchronous writers

(niwa-deftest niwa-view-user-state/writers ()
  (niwa-view-user-state-test--with-view
    ,setup
    (let ((windows (niwa-view-user-state-test--arrange)))
      (niwa-view-user-state-test--preserves windows
        ,action)))
  (setup action)
  :doc "a streamed chunk keeps the draft, composer row and reader"
  (niwa-view-user-state-test--begin-turn data-buf)
  (progn
    (niwa-view-user-state-test--add-chunk data-buf "More streamed text.\nAnd a line.\n")
    (niwa-view--render-stream-update data-buf)
    (should (string-search "And a line." (buffer-string))))
  :doc "a full rerender keeps the draft, composer row and reader"
  nil
  (progn
    (niwa-view-user-state-test--add-chunk data-buf "Rerendered tail.\n")
    (niwa-view--full-rerender)
    (should (string-search "Rerendered tail." (buffer-string))))
  :doc "settling a response keeps the draft, composer row and reader"
  (niwa-view-user-state-test--begin-turn data-buf)
  (progn
    (niwa-view-user-state-test--add-chunk data-buf "Final settled text.\n")
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (should (string-search "Final settled text." (buffer-string))))
  :doc "starting request progress keeps the draft, composer row and reader"
  nil
  (let ((rail (niwa-view-test--rail-text)))
    (niwa-view--start-spinner "Thinking...")
    (should-not (equal rail (niwa-view-test--rail-text))))
  :doc "a request status update keeps the draft, composer row and reader"
  (niwa-view--start-spinner "Thinking...")
  (progn
    (niwa-view--update-spinner "Running a much longer status label" 'request)
    (should (string-search "much longer status" (niwa-view-test--rail-text))))
  :doc "stopping request progress keeps the draft, composer row and reader"
  (niwa-view--start-spinner "Thinking...")
  (let ((rail (niwa-view-test--rail-text)))
    (niwa-view--stop-spinner)
    (should-not (equal rail (niwa-view-test--rail-text))))
  :doc "a semantic spinner tick keeps the draft, composer row and reader"
  (progn
    (niwa-view--start-spinner "Working...")
    (setq niwa-view--spinner-start-time (seconds-to-time 988.0)
          niwa-view--spinner-last-second 1000)
    (niwa-view--ensure-request-progress))
  (let ((transcript-tick (buffer-chars-modified-tick)))
    (setq clock 1001.0)
    ;; The same callback can also update visible execution rows.
    (cl-letf (((symbol-function 'niwa-view--background-metadata-visible-p)
               #'ignore))
      (niwa-view--spinner-tick))
    (should (string-search "13s" (niwa-view-test--rail-text)))
    (should (= transcript-tick (buffer-chars-modified-tick))))
  :doc "a background elapsed tick keeps the draft, composer row and reader"
  nil
  (progn
    (setq clock 1001.0)
    (niwa-view-execution-tick)
    (should (string-search "2m 33s" (buffer-string))))
  :doc "a background row arriving keeps the draft, composer row and reader"
  nil
  (progn
    (setq agents (list (list :path "/root/arriving" :status 'running
                             :started-at 990.0)))
    (niwa-view--render-status)
    (should (string-search "/root/arriving" (buffer-string))))
  :doc "a background row leaving keeps the draft, composer row and reader"
  nil
  (progn
    ;; The finished row lingers for its grace period, then leaves.
    (setq executions nil)
    (niwa-view--render-status)
    (setq clock (+ clock 30))
    (niwa-view--render-status)
    (should-not (string-search "Inspect runner" (buffer-string))))
  :doc "an agent handle refresh keeps the draft, composer row and reader"
  (niwa-view-user-state-test--register-agent data-buf)
  (progn
    (should (niwa-view--refresh-agent-rendering-now "/root/worker"))
    (should (= 2 (niwa-view-test--count-substring
                  "Agent: user-state worker" (buffer-string))))))

(niwa-deftest niwa-view-user-state/agent-refresh-region
  (:doc "An agent handle refresh anchors readers without scanning old history.")
  (niwa-view-user-state-test--with-view
    (niwa-view-user-state-test--register-agent data-buf)
    (let ((windows (niwa-view-user-state-test--arrange))
          (history-end (save-excursion
                         (goto-char (point-min))
                         (search-forward "Line 59 of the earlier response.")))
          (run-end (symbol-function 'niwa-view--render-anchor-run-end))
          scans)
      (niwa-view-user-state-test--preserves windows
        (cl-letf (((symbol-function 'niwa-view--render-anchor-run-end)
                   (lambda (pos limit)
                     (push (+ 0 pos) scans)
                     (funcall run-end pos limit))))
          (should (niwa-view--refresh-agent-rendering-now "/root/worker"))))
      (should (= 2 (niwa-view-test--count-substring
                    "Agent: user-state worker" (buffer-string))))
      (should scans)
      (should (cl-every (lambda (pos) (> pos history-end)) scans)))))

(niwa-deftest niwa-view-user-state/agent-refresh-reader
  (:doc "A reader stays on the second handle when the first handle grows.")
  (niwa-view-user-state-test--with-view
    (let ((label "Agent: user-state worker"))
      (niwa-view-user-state-test--register-agent data-buf (lambda () label))
      (let ((reader (cdr (niwa-view-user-state-test--arrange))))
        (cl-flet ((second-row ()
                    (save-excursion
                      (goto-char (point-min))
                      (search-forward label nil nil 2)
                      (line-beginning-position))))
          (let ((old (second-row)))
            (set-window-start reader old)
            (set-window-point reader (+ old 4))
            (setq label (concat label " processing a much longer request"))
            (should (niwa-view--refresh-agent-rendering-now "/root/worker"))
            (let ((new (second-row)))
              (should (> new old))
              (should (= (window-start reader) new))
              (should (= (window-point reader) (+ new 4))))))))))

(niwa-deftest niwa-view-user-state/agent-refresh-prefix
  (:doc "Repeated refresh skips unchanged history but notices earlier new handles.")
  (niwa-view-user-state-test--with-view
    (niwa-view-user-state-test--register-agent data-buf)
    (let ((path "/root/worker")
          (next (symbol-function 'next-single-property-change))
          (history-end (save-excursion (goto-char (point-min)) (forward-line 40) (point)))
          scans)
      (should (niwa-view--refresh-agent-rendering-now path))
      (cl-letf (((symbol-function 'next-single-property-change)
                 (lambda (pos prop &rest args)
                   (when (eq prop 'niwa-view-agent-path) (push pos scans))
                   (apply next pos prop args))))
        (should (= 2 (length (car (niwa-view--agent-handle-refresh-points path)))))
        (should scans)
        (should (cl-every (lambda (pos) (> pos history-end)) scans)))
      (save-restriction
        (narrow-to-region (point-min) history-end)
        (should (equal (niwa-view--agent-handle-refresh-points path) '(nil))))
      (let ((inhibit-read-only t))
        (add-text-properties (point-min) (1+ (point-min))
                             `(niwa-view-agent-path ,path niwa-view-agent-handle-p t)))
      (should (cdr (niwa-view--agent-handle-refresh-points path))))))

(niwa-deftest niwa-view-user-state/nesting ()
  (niwa-view-user-state-test--with-view
    (niwa-view-user-state-test--begin-turn data-buf)
    (niwa-view--start-spinner "Thinking...")
    (let ((windows (niwa-view-user-state-test--arrange))
          (original (symbol-function ',inner))
          (entered 0))
      (niwa-view-user-state-test--preserves windows
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
      (should (string-search "Nested status" (niwa-view-test--rail-text)))))
  (inner condition nested outer)
  :doc "rail reconciliation nested inside a streamed chunk"
  niwa-view--render-turn t
  (niwa-view--update-spinner "Nested status" 'request)
  (progn
    (niwa-view-user-state-test--add-chunk data-buf "Nested chunk.\n")
    (niwa-view--render-stream-update data-buf))
  :doc "a streamed chunk nested inside rail reconciliation"
  niwa-view--render-request-progress t
  (progn
    (niwa-view-user-state-test--add-chunk data-buf "Nested chunk.\n")
    (niwa-view--render-stream-update data-buf))
  (niwa-view--update-spinner "Nested status" 'request))


;;
;;; Region promises and request progress geometry

(defvar niwa-view-user-state-test--region-checks nil
  "Region checks: (PREFIX-DIFFERENCE SUFFIX-DIFFERENCE), nil when unchanged.")

(defun niwa-view-user-state-test--difference (old new)
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

(defun niwa-view-user-state-test--validating-region (original thunk &optional region follow-output)
  "Call ORIGINAL with THUNK, REGION and FOLLOW-OUTPUT, checking text outside REGION.
Text and properties before and after REGION must be identical afterwards:
REGION promises that THUNK replaces only that text."
  (if (not region)
      (funcall original thunk region follow-output)
    ;; Geometry can trigger composer jit-lock.  Compare already-fontified text
    ;; so ordinary redisplay faces do not look like an out-of-region write.
    (niwa-view--fontify-composer (point-min) (point-max))
    (let* ((start (copy-marker (car region)))
           (end (copy-marker (cdr region) t))
           (before (buffer-substring (point-min) start))
           (after (buffer-substring end (point-max))))
      (prog1 (funcall original thunk region follow-output)
        (push (list (niwa-view-user-state-test--difference
                     before (buffer-substring (point-min) start))
                    (niwa-view-user-state-test--difference
                     after (buffer-substring end (point-max))))
              niwa-view-user-state-test--region-checks)
        (set-marker start nil)
        (set-marker end nil)))))

(niwa-deftest niwa-view-user-state/region ()
  (niwa-view-user-state-test--with-view
    ,setup
    (let ((windows (niwa-view-user-state-test--arrange))
          (niwa-view-user-state-test--region-checks nil))
      (unwind-protect
          (progn
            (advice-add 'niwa-view-call-preserving-user-view-state :around
                        #'niwa-view-user-state-test--validating-region)
            (niwa-view-user-state-test--preserves windows
              ,action))
        (advice-remove 'niwa-view-call-preserving-user-view-state
                       #'niwa-view-user-state-test--validating-region))
      ;; The writer used the region path, and kept its promise.
      (should niwa-view-user-state-test--region-checks)
      (dolist (check niwa-view-user-state-test--region-checks)
        (should (equal check '(nil nil))))))
  (setup action)
  :doc "negative control: a write outside the promised region is detected"
  nil
  (let ((at (save-excursion
              (goto-char (point-min)) (search-forward "Line 5 ") (point)))
        (inhibit-read-only t))
    (niwa-view-call-preserving-user-view-state
     (lambda () (save-excursion (goto-char (point-min)) (insert "outside ")))
     (cons at (1+ at)))
    (should (car (car niwa-view-user-state-test--region-checks)))
    ;; Undo it with a kept promise, leaving only valid records.
    (setq niwa-view-user-state-test--region-checks nil)
    (niwa-view-call-preserving-user-view-state
     (lambda () (save-excursion (goto-char (point-min)) (delete-char 8)))
     (cons (point-min) (+ (point-min) 8))))
  :doc "a background elapsed tick replaces only its row"
  nil
  (progn
    (setq clock 1001.0)
    (niwa-view-execution-tick)
    (should (string-search "2m 33s" (buffer-string)))))

(niwa-deftest niwa-view-user-state/request-progress-width ()
  (niwa-view-user-state-test--with-view
    (niwa-view--start-spinner "Thinking about a fairly long status label")
    (let* ((windows (niwa-view-user-state-test--arrange))
           (rail (niwa-view-test--rail-text)))
      ;; A narrower window now shows the view: the rail must refit and can
      ;; rewrap, while every window keeps the user's state.
      (with-selected-window (car windows) (split-window-right))
      (niwa-view-user-state-test--preserves windows
        ,action
        (should-not (equal rail (niwa-view-test--rail-text))))))
  (action)
  :doc "a status update refits the rail to a narrower window"
  (niwa-view--update-spinner "Running another fairly long status label" 'request)
  :doc "stopping progress refits the idle rail to a narrower window"
  (niwa-view--stop-spinner))


;;
;;; Inbox reproductions (graphical)

(defun niwa-view-user-state-test--activate-row (text)
  "Put point on the background row showing TEXT and press RET there."
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line)
  (let ((last-nonmenu-event nil))
    (call-interactively (key-binding (kbd "RET")))))

(niwa-deftest niwa-view-user-state/status-shrink-reader ()
  (niwa-view-user-state-test--with-view
    (setq executions nil
          agents (list (list :path "/root/worker" :status 'running
                             :started-at 990.0
                             :model-metadata '(:backend "Provider"
                                               :model "Model" :effort "high"))))
    (unless ,toggle
      (dotimes (n 3)
        (push (list :path (format "/root/finished-%d" n)
                    :status 'running :started-at 989.0)
              agents)))
    (niwa-view--render-status)
    (niwa-view-test--insert-composer-draft
     niwa-view-user-state-test--draft 4)
    (when ,toggle
      (niwa-view-user-state-test--activate-row "/root/worker"))
    (let* ((left (selected-window))
           (right (split-window-right))
           (selected (if ,right-selected right left)))
      (set-window-buffer right view-buf)
      (select-window selected)
      (niwa-view--render-status)
      (dolist (window (list left right))
        (let ((position (save-excursion
                          (goto-char (point-min))
                          (search-forward (if ,toggle "/root/worker" "Line 58 of"))
                          (line-beginning-position))))
          (set-window-point window position)
          (set-window-start window
                            (save-excursion
                              (goto-char position)
                              (forward-line (if (eq window left) -3 -5))
                              (point)) t)))
      (unless noninteractive (redisplay t))
      (cl-flet ((observe ()
                  (mapcar (lambda (window)
                            (list (window-start window) (window-point window)
                                  (niwa-view--position-screen-y
                                   (window-point window) window)))
                          (list left right))))
        (let ((before (observe))
              (lines (count-lines (point-min) (point-max))))
          (should (niwa-view--position-screen-y (point-max) left t))
          (should (niwa-view--position-screen-y (point-max) right t))
          (dotimes (_ 6)
            (if ,toggle
                (if ,outer
                    (progn
                      (setq niwa-view-execution--selected-agent
                            (unless niwa-view-execution--selected-agent
                              "/root/worker"))
                      (niwa-view--full-rerender))
                  (let ((last-nonmenu-event nil))
                    (call-interactively (key-binding (kbd "RET")))))
              (setq agents nil clock (+ clock 20.0))
              (niwa-view--render-status))
            (unless noninteractive (redisplay t))
            (should (equal before (observe)))
            (when ,toggle
              (should (= (count-lines (point-min) (point-max))
                         (- lines (if niwa-view-execution--selected-agent 0 4)))))
            (should (eq selected (selected-window)))
            (should (equal niwa-view-user-state-test--draft
                           (niwa-view-composer-draft-text))))
          (unless ,toggle
            (should-not (string-search "/root/worker" (buffer-string)))
            (should-not (string-search "/root/finished-" (buffer-string))))))))
  (toggle right-selected outer)
  :doc "toggling an agent preserves both visited rows, left selected"
  t nil nil
  :doc "toggling an agent preserves both visited rows, right selected"
  t t nil
  :doc "a full rerender shrinking status preserves both visits, left selected"
  t nil t
  :doc "a full rerender shrinking status preserves both visits, right selected"
  t t t
  :doc "completed agents leaving preserve both history readers, left selected"
  nil nil nil
  :doc "completed agents leaving preserve both history readers, right selected"
  nil t nil)

(niwa-deftest niwa-view-user-state/collapse-background-row ()
  (progn
    (skip-unless (display-graphic-p))
    (niwa-view-user-state-test--with-view
      (niwa-view-test--insert-composer-draft
       niwa-view-user-state-test--draft 4)
      (redisplay t)
      (niwa-view-user-state-test--activate-row "Inspect runner")
      (should (looking-at-p "  ▾ Running"))
      ;; The transcript ends mid-window with blank space below it, as after
      ;; `recenter' or an earlier shrink.
      (save-excursion
        (goto-char (point-max))
        (recenter 20))
      (redisplay t)
      (let ((end-y (niwa-view--position-screen-y (point-max) (selected-window)))
            (start (window-start))
            (position (point))
            (row-y (niwa-view--position-screen-y (point) (selected-window))))
        (should (niwa-view--window-blank-pixels (selected-window)))
        (niwa-view-user-state-test--activate-row "Inspect runner")
        (redisplay t)
        (should (looking-at-p "  ▸ Running"))
        (should (equal niwa-view-user-state-test--draft (niwa-view-composer-draft-text)))
        (should (= start (window-start)))
        (should (= position (window-point)))
        (should (equal row-y (niwa-view--position-screen-y
                             (point) (selected-window))))
        (should (< (niwa-view--position-screen-y (point-max) (selected-window))
                   end-y)))))
  (test)
  :doc "collapsing a background row keeps the visited row, not the transcript end"
  nil)

(defun niwa-view-user-state-test--wheel (direction &optional window)
  "Scroll WINDOW one notch in DIRECTION through the command loop.
WINDOW defaults to the selected window, but need not be selected."
  (let ((window (or window (selected-window))))
    (niwa-view-user-state-test--deliver-scroll
     (list (list direction (posn-at-point (window-start window) window)))
     'mwheel-scroll)))

(niwa-deftest niwa-view-user-state/status-visit ()
  (niwa-view-user-state-test--with-view
    (niwa-view-test--insert-composer-draft
     niwa-view-user-state-test--draft 4)
    (unless noninteractive
      (should (niwa-view-composer-display (selected-window))))
    (goto-char (point-min))
    (search-forward "[Manage]")
    (backward-char (length "[Manage]"))
    (when noninteractive
      (set-window-start (selected-window) (line-beginning-position)))
    (unless noninteractive (redisplay t))
    (let ((y (niwa-view--position-screen-y (point) (selected-window))))
      (should y)
      ,action
      (unless noninteractive (redisplay t))
      (should (looking-at-p (regexp-quote "[Manage]")))
      (should (equal y (niwa-view--position-screen-y (point) (selected-window))))
      (should (equal niwa-view-user-state-test--draft (niwa-view-composer-draft-text)))))
  (action)
  :doc "a background row arriving above a visited [Manage] row keeps that row still"
  (progn
    (setq agents (list (list :path "/root/arriving" :status 'running
                             :started-at 990.0)))
    (niwa-view--render-status)
    ;; A visited panel keeps its rows and only counts new work.
    (should (string-search "1 agent" (buffer-string))))
  :doc "a streamed full rerender keeps a visited [Manage] row still"
  (progn
    (niwa-view-user-state-test--add-chunk data-buf "Arrived meanwhile.\n")
    (niwa-view--full-rerender)))

(defun niwa-view-user-state-test--deliver-scroll (events command)
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

(niwa-deftest niwa-view-user-state/scroll-unselected ()
  (progn
    (skip-unless (display-graphic-p))
    (let ((scroll-preserve-screen-position t)
          (mouse-wheel-follow-mouse t)
          (mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))
          (mouse-wheel-progressive-speed nil))
      (niwa-view-user-state-test--with-view
        (let* ((transcript (selected-window))
               (reader (split-window-right))
               ;; Test the targeted scroll independently of C-x o order: a
               ;; second transcript now has a composer of its own.
               (other-window-scroll-default (lambda () transcript)))
          (niwa-view-test--insert-composer-draft
           niwa-view-user-state-test--draft 4)
          (should (niwa-view-composer-display transcript))
          (goto-char (point-max))
          (recenter -1)
          (with-temp-buffer
            (if ,different-buffer
                (progn
                  (insert "The selected window is not a chat view.\n")
                  (set-window-buffer reader (current-buffer)))
              (should (niwa-view-composer-display reader)))
            (select-window reader)
            (redisplay t)
            (let ((reader-point (window-point reader))
                  (reader-start (window-start reader)))
              (dotimes (_ ,up-count) ,up)
              (with-current-buffer view-buf
                (should-not (niwa-view--position-screen-y (point-max) transcript t)))
              (let ((notches 0))
                (while (and (< notches 8)
                            (with-current-buffer view-buf
                              (not (niwa-view--position-screen-y
                                    (point-max) transcript t))))
                  ,down
                  (cl-incf notches)))
              (with-current-buffer view-buf
                (should (niwa-view--position-screen-y (point-max) transcript t))
                (should (equal niwa-view-user-state-test--draft
                               (niwa-view-composer-draft-text)))
                (should (= 4 (niwa-view-user-state-test--draft-offset))))
              (should (eq reader (selected-window)))
              (should (= reader-point (window-point reader)))
              (should (= reader-start (window-start reader)))))))))
  (different-buffer up-count up down)
  :doc "wheel scrolling a non-selected view window leaves the draft offset alone"
  nil 3
  (niwa-view-user-state-test--wheel 'wheel-up transcript)
  (niwa-view-user-state-test--wheel 'wheel-down transcript)
  :doc "wheel scrolling a view from another buffer leaves the draft offset alone"
  t 3
  (niwa-view-user-state-test--wheel 'wheel-up transcript)
  (niwa-view-user-state-test--wheel 'wheel-down transcript)
  :doc "scroll-other-window leaves the draft offset alone"
  nil 3
  (niwa-view-user-state-test--deliver-scroll
   (listify-key-sequence (kbd "C-u 2 C-M-S-v")) 'scroll-other-window-down)
  (niwa-view-user-state-test--deliver-scroll
   (listify-key-sequence (kbd "C-u 2 C-M-v")) 'scroll-other-window)
  :doc "scroll-other-window from another buffer leaves the draft offset alone"
  t 3
  (niwa-view-user-state-test--deliver-scroll
   (listify-key-sequence (kbd "C-u 2 C-M-S-v")) 'scroll-other-window-down)
  (niwa-view-user-state-test--deliver-scroll
   (listify-key-sequence (kbd "C-u 2 C-M-v")) 'scroll-other-window))

(ert-deftest niwa-view-user-state/follows-visible-end ()
  (skip-unless (not noninteractive))
  (niwa-view-user-state-test--with-view
    (niwa-view-test--insert-data data-buf "Continue.\n" nil)
    (niwa-view--full-rerender)
    (niwa-view-user-state-test--begin-turn data-buf)
    (pcase-let* ((`(,transcript . ,reader) (niwa-view-user-state-test--arrange))
                 (composer (niwa-view-composer-window transcript))
                 (chunk (concat (apply #'concat (make-list 50 "\n\nMore response text."))
                                "\n\nEnd of this chunk.")))
      (goto-char (point-max))
      (recenter -1)
      (redisplay t)
      (goto-char (window-start transcript))
      (search-forward "Streaming response begins.")
      (select-window composer)
      (redisplay t)
      (with-current-buffer view-buf
        (should (pos-visible-in-window-p (point-max) transcript))
        (should-not (= (point-max) (window-point transcript)))
        (should-not (pos-visible-in-window-p (point-max) reader))
        (let ((start (window-start transcript)))
          (niwa-view-user-state-test--preserves (cons transcript reader)
            (niwa-view-user-state-test--add-chunk data-buf chunk)
            (niwa-view--render-stream-update data-buf))
          (should (string-search "End of this chunk." (buffer-string)))
          (should (> (window-start transcript) start)))
        (should (pos-visible-in-window-p (point-max) transcript))
        (should-not (pos-visible-in-window-p (point-max) reader))
        (with-selected-window transcript
          (execute-kbd-macro (kbd "M-v")))
        (redisplay t)
        (should-not (pos-visible-in-window-p (point-max) transcript))
        (let ((start (window-start transcript))
              (point (window-point transcript)))
          (niwa-view-user-state-test--add-chunk data-buf chunk)
          (niwa-view--render-stream-update data-buf)
          (redisplay t)
          (should (= start (window-start transcript)))
          (should (= point (window-point transcript))))
        (niwa-view-transcript-show-end transcript)
        (redisplay t)
        (should (pos-visible-in-window-p (point-max) transcript))
        (niwa-view-user-state-test--preserves (cons transcript reader)
          (niwa-view-user-state-test--add-chunk data-buf chunk)
          (niwa-view--render-stream-update data-buf))
        (should (pos-visible-in-window-p (point-max) transcript))
        (should (eq composer (selected-window)))))))

(ert-deftest niwa-view-user-state/stream-keeps-visited-tool-row ()
  (niwa-view-user-state-test--with-view
    (niwa-view-user-state-test--begin-turn data-buf)
    (niwa-view-test--insert-data
     data-buf "(:name \"Read\" :args (:file_path \"last.el\"))\n\nTool detail\n"
     '(tool . "last-tool"))
    (niwa-view--render-stream-update data-buf)
    (goto-char (point-min))
    (search-forward "Read: last.el")
    (beginning-of-line)
    (niwa-view-toggle-section)
    (let ((window (selected-window))
          (position (point))
          (visible (symbol-function 'pos-visible-in-window-p))
          followed)
      (set-window-start window (line-beginning-position) t)
      (should (eq (get-text-property position 'niwa-view-type) 'tool-summary))
      (cl-letf (((symbol-function 'pos-visible-in-window-p)
                 (lambda (&optional pos win partial)
                   (or (and (eq win window) (memq pos (list position (point-max))))
                       (funcall visible pos win partial))))
                ((symbol-function 'niwa-view-transcript-show-end)
                 (lambda (win) (when (eq win window) (setq followed t)))))
        (niwa-view-user-state-test--add-chunk data-buf "\nThe next response chunk.\n")
        (niwa-view--render-stream-update data-buf)
        (should-not followed)
        (should (looking-at-p ".*Read: last\\.el"))
        (should (save-excursion (search-forward "Tool detail" nil t)))
        (setq position (point))
        (with-current-buffer data-buf
          (goto-char (point-max))
          (search-backward "(:name \"Read\"")
          (let ((start (point)))
            (search-forward "Tool detail\n")
            (delete-region start (point))))
        (niwa-view--render-stream-update data-buf)
        (should followed)
        (should-not (string-search "Read: last.el" (buffer-string)))))))

(niwa-deftest niwa-view-user-state/live-reader ()
  ;; The graphical runner repeats every transition after a real wheel event
  ;; in the command loop, including one targeting the non-selected window.
  (dolist (scroll (if noninteractive '(nil) '(nil t)))
    (niwa-view-user-state-test--with-view
      (niwa-view-test--insert-data data-buf "Continue.\n" nil)
      (niwa-view--full-rerender)
      (niwa-view-user-state-test--begin-turn data-buf)
      (niwa-view-user-state-test--add-chunk
       data-buf "\nA paragraph with a stable middle and a stable ending.")
      (niwa-view--render-stream-update data-buf)
      (let* ((windows (niwa-view-user-state-test--arrange))
	     (reader (if ,selected (car windows) (cdr windows)))
	     (position (save-excursion
			 (goto-char (point-min))
			 (search-forward ,needle)
			 (point))))
	(set-window-point reader position)
	(set-window-start reader
			  (save-excursion (goto-char position)
					  (line-beginning-position)))
	(unless noninteractive (redisplay t))
	(when scroll
	  (let ((scroll-preserve-screen-position t)
		(mouse-wheel-follow-mouse t)
		(mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))
		(mouse-wheel-progressive-speed nil))
	    (niwa-view-user-state-test--wheel 'wheel-up reader))
	  (should (= position (window-point reader))))
        (unless noninteractive
          ;; A caret in the live paragraph is a reader only when EOF is hidden.
          (when (pos-visible-in-window-p (point-max) reader)
            (with-selected-window reader (recenter -1))
            (redisplay t))
          (should-not (pos-visible-in-window-p (point-max) reader)))
	(let ((start (window-start reader))
	      (y (niwa-view--position-screen-y position reader))
	      (selected-window (selected-window)))
	  (when (eq ,chunk 'rewrite)
	    ;; A canonical source correction before the retained paragraph
	    ;; invalidates the live tail through its real before-change hook.
	    (with-current-buffer data-buf
	      (goto-char (point-min))
	      (search-forward "Streaming")
	      (delete-region (- (point) 9) (point))
	      (insert (propertize "Continued" 'gptel 'response))))
	  (when (eq ,chunk 'shrink)
	    (with-current-buffer data-buf
	      (goto-char (point-min))
	      (search-forward "A paragraph")
	      (delete-region (match-beginning 0) (point-max))
	      (insert (propertize "A shorter paragraph." 'gptel 'response))))
	  (if (memq ,chunk '(tool rewrite))
	      (progn
		(niwa-view-test--insert-data
		 data-buf "(:name \"Bash\" :args (:command \"echo done\"))\n\ndone\n"
		 '(tool . "live-reader-tool"))
		(niwa-view-user-state-test--add-chunk data-buf "After the tool."))
	    (unless (memq ,chunk '(settle shrink))
	      (niwa-view-user-state-test--add-chunk data-buf ,chunk)))
	  (if (memq ,chunk '(settle shrink))
	      (let ((from (marker-position niwa-view--data-turn-start)))
		(with-current-buffer data-buf
		  (niwa-view-stream-render-response from (point-max))))
	    (niwa-view--render-stream-update data-buf))
	  (unless noninteractive (redisplay t))
	  (when (eq ,chunk 'shrink)
	    (setq position (save-excursion
			     (goto-char (point-min))
			     (search-forward "A shorter paragraph.")
			     (point))))
	  (should (= position (window-point reader)))
	  (should (= start (window-start reader)))
	  (should (equal y (niwa-view--position-screen-y
			    (window-point reader) reader)))
	  (should (eq selected-window (selected-window)))
	  (should (equal niwa-view-user-state-test--draft
			 (niwa-view-composer-draft-text)))))))
  (selected needle chunk)
  :doc "selected reader at paragraph end survives growth"
  t "stable ending." " More text."
  :doc "selected reader in paragraph middle survives growth"
  t "stable middle" " More text."
  :doc "non-selected reader at paragraph end survives growth"
  nil "stable ending." " More text."
  :doc "non-selected reader in paragraph middle survives growth"
  nil "stable middle" " More text."
  :doc "selected reader at paragraph end survives a stable block boundary"
  t "stable ending." "\n\nNext paragraph."
  :doc "selected reader in paragraph middle survives a stable block boundary"
  t "stable middle" "\n\nNext paragraph."
  :doc "non-selected reader at paragraph end survives a stable block boundary"
  nil "stable ending." "\n\nNext paragraph."
  :doc "non-selected reader in paragraph middle survives a stable block boundary"
  nil "stable middle" "\n\nNext paragraph."
  :doc "selected reader at paragraph end survives a tool boundary"
  t "stable ending." 'tool
  :doc "selected reader in paragraph middle survives a tool boundary"
  t "stable middle" 'tool
  :doc "non-selected reader at paragraph end survives a tool boundary"
  nil "stable ending." 'tool
  :doc "non-selected reader in paragraph middle survives a tool boundary"
  nil "stable middle" 'tool
  :doc "selected reader at paragraph end survives settlement"
  t "stable ending." 'settle
  :doc "selected reader in paragraph middle survives settlement"
  t "stable middle" 'settle
  :doc "non-selected reader at paragraph end survives settlement"
  nil "stable ending." 'settle
  :doc "non-selected reader in paragraph middle survives settlement"
  nil "stable middle" 'settle
  :doc "selected reader at paragraph end survives a source-corrected stream update"
  t "stable ending." 'rewrite
  :doc "selected reader in paragraph middle survives a source-corrected stream update"
  t "stable middle" 'rewrite
  :doc "non-selected reader at paragraph end survives a source-corrected stream update"
  nil "stable ending." 'rewrite
  :doc "non-selected reader in paragraph middle survives a source-corrected stream update"
  nil "stable middle" 'rewrite
  :doc "selected reader at paragraph end stays nearest to rewritten text"
  t "stable ending." 'shrink
  :doc "selected reader in paragraph middle stays nearest to rewritten text"
  t "stable middle" 'shrink
  :doc "non-selected reader at paragraph end stays nearest to rewritten text"
  nil "stable ending." 'shrink
  :doc "non-selected reader in paragraph middle stays nearest to rewritten text"
  nil "stable middle" 'shrink)

(niwa-deftest niwa-view-user-state/status-minibuffer-roundtrip ()
  (progn
    (skip-unless (display-graphic-p))
    (niwa-view-user-state-test--with-view
      (let* ((left (selected-window))
             (right (split-window-right))
             (windows (list left right))
             (resize-mini-windows nil)
             (preview (generate-new-buffer "preview")))
        (unwind-protect
            (progn
              (set-window-buffer right view-buf)
              (niwa-view-test--insert-composer-draft
               niwa-view-user-state-test--draft 4)
              (niwa-view--render-status)
              (redisplay t)
              (goto-char (point-min))
              (search-forward "[Manage]")
              (backward-char (length "[Manage]"))
              (let ((visit (point)))
                (should (>= visit niwa-view--status-marker))
                (should (< visit (niwa-view-transcript-tail)))
                (dolist (window windows)
                  (set-window-point window visit)
                  (niwa-view--set-viewport
                   window (niwa-view--viewport-at-y window visit 200)))
                (select-window (if ,right right left))
                (redisplay t)
                (let ((before
                       (mapcar (lambda (window)
                                 (should (> (niwa-view--window-blank-pixels window) 160))
                                 (list (window-start window)
                                       (niwa-view--position-screen-y visit window)))
                               windows)))
                  (condition-case nil
                      (minibuffer-with-setup-hook
                          (lambda ()
                            (enlarge-window 8)
                            (redisplay t)
                            ;; Previewing another buffer invalidates the old
                            ;; size baseline, as consult-buffer does in use.
                            (when ,preview
                              (cl-mapc
                               (lambda (window state)
                                 (set-window-buffer window preview)
                                 (redisplay t)
                                 (set-window-buffer window view-buf)
                                 (set-window-point window visit)
                                 (set-window-start window (car state) t)
                                 (redisplay t))
                               windows before))
                            (dolist (window windows)
                              (should (= visit (window-point window))))
                            (setq unread-command-events (list ?\C-g)))
                        (read-from-minibuffer "Preview: "))
                    (quit nil))
                  (redisplay t)
                  (should (eq (selected-window) (if ,right right left)))
                  (should (equal niwa-view-user-state-test--draft
                                 (niwa-view-composer-draft-text)))
                  (dolist (window windows)
                    (should (= visit (window-point window))))
                  (should
                   (equal before
                          (mapcar (lambda (window)
                                    (list (window-start window)
                                          (niwa-view--position-screen-y visit window)))
                                  windows))))))
          (kill-buffer preview)))))
  (right preview)
  :doc "status readers retain their rows across minibuffer resizing, left selected" nil nil
  :doc "status readers retain their rows across minibuffer resizing, right selected" t nil
  :doc "status readers retain their rows after preview resets the baseline, left selected" nil t
  :doc "status readers retain their rows after preview resets the baseline, right selected" t t)

(ert-deftest niwa-view-user-state/viewport-stream-follow-and-tool ()
  ;; Batch Emacs cannot answer the native visibility query that elects followers.
  (skip-unless (not noninteractive))
  (niwa-view-user-state-test--with-view
    (niwa-view-user-state-test--begin-turn data-buf)
    (niwa-view-test--insert-data
     data-buf "(:name \"Read\" :args (:file_path \"visited.el\"))\n\nTool detail\n"
     '(tool . "visited-tool"))
    (niwa-view--render-stream-update data-buf)
    (let ((visit (selected-window)) (follow (split-window-right)))
      (goto-char (point-min))
      (search-forward "Read: visited.el")
      (beginning-of-line)
      (let ((row (point)))
        (set-window-start visit row t)
        (set-window-start follow row t)
        (set-window-point follow (point-max))
        (redisplay t)
        (should (eq 'tool-summary (get-text-property row 'niwa-view-type)))
        (should (pos-visible-in-window-p row visit))
        (dolist (window (list visit follow))
          (should (pos-visible-in-window-p (point-max) window)))
        (niwa-view-user-state-test--add-chunk
         data-buf (apply #'concat (make-list 60 "\nNext response line.")))
        (niwa-view--render-stream-update data-buf)
        ;; Inspect the start chosen by the writer before redisplay can scroll.
        (should (> (window-start follow) row))
        (should (= row (window-start visit)))
        (should (looking-at-p ".*Read: visited\\.el"))
        (redisplay t)
        (should (pos-visible-in-window-p (point-max) follow))
        (should-not (pos-visible-in-window-p (point-max) visit))))))

(ert-deftest niwa-view-user-state/viewport-zone-visit-and-reader ()
  (niwa-view-user-state-test--with-view
    (pcase-let* ((`(,visit . ,reader) (niwa-view-user-state-test--arrange))
                 (reader-start (window-start reader))
                 (reader-point (window-point reader)))
      (with-selected-window visit
        (goto-char (point-min))
        (search-forward "[Manage]")
        (backward-char (length "[Manage]"))
        (set-window-start visit (line-beginning-position) t)
        (unless noninteractive (redisplay t))
        (let ((y (niwa-view--position-screen-y (point) visit)))
          (should y)
          (dolist (zone '(status interaction))
            (if (eq zone 'status)
                (progn
                  (setq agents (list (list :path "/root/new" :status 'running
                                           :started-at 990.0)))
                  (niwa-view--render-status)
                  (should (string-search "1 agent" (buffer-string))))
              (niwa-view--interaction-register '(:kind ask :id ask :body "Question"))
              (should (niwa-view-zone-region 'interaction)))
            (unless noninteractive (redisplay t))
            (should (looking-at-p (regexp-quote "[Manage]")))
            (should (equal y (niwa-view--position-screen-y (point) visit)))
            (should (= reader-start (window-start reader)))
            (should (= reader-point (window-point reader)))))))))

(provide 'test-niwa-view-user-state)
;;; test-niwa-view-user-state.el ends here
