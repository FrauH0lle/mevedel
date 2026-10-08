;;; test-mevedel-view-render-batch.el --- Batched history projection -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise callback ownership and live reader state during complete rebuilds.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-render)

(defmacro mevedel-batch-test--with-history (&rest body)
  "Run BODY with six exchanges and a multiline composer draft."
  (declare (indent 0))
  `(mevedel-view-test--with-buffers
     (dotimes (index 6)
       (mevedel-view-test--insert-data data-buf (format "Prompt %d\n" index) nil)
       (mevedel-view-test--insert-data data-buf (format "Response %d with target text.\n" index) 'response))
     (with-current-buffer view-buf
       (mevedel-view--full-rerender)
       (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
       ,@body)))

(defun mevedel-batch-test--start-projection (view)
  "Start a settled batch and advance VIEW through source preparation."
  (mevedel-view-render-batched-full)
  (cl-loop repeat 100 while (and mevedel-view-render--batch
                                 (eq 'prepare (plist-get mevedel-view-render--batch :phase)))
           do (mevedel-view-render--batch-step view mevedel-view-render--batch))
  (should (eq 'project (plist-get mevedel-view-render--batch :phase))))

(mevedel-deftest mevedel-view-render-batched-full ()
  ,test
  (test)
  :doc "projects current history first and converges to synchronous output"
  (mevedel-batch-test--with-history
    (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
      (mevedel-batch-test--start-projection view-buf)
      (should mevedel-view-render--batch)
      (should (string-search "Response 5" (buffer-string)))
      (should (string-search "Loading conversation turn" (buffer-string)))
      (cl-loop repeat 30 while mevedel-view-render--batch do
        (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
      (should (equal expected (buffer-substring-no-properties (point-min) (point-max))))))
  :doc "new draft edits and selection survive every callback"
  (mevedel-batch-test--with-history
    (mevedel-batch-test--start-projection view-buf)
    (goto-char (point-max)) (insert "\nnew typing")
    (let ((offset (- (point) (mevedel-view--input-start))))
      (set-mark (- (point) 6)) (setq mark-active t)
      (cl-loop repeat 30 while mevedel-view-render--batch do
        (mevedel-view-render--batch-step view-buf mevedel-view-render--batch)
        (should (equal "> draft\nsecond line\nnew typing" (mevedel-view--input-text)))
        (should (= (- (point) (mevedel-view--input-start)) offset))
        (should mark-active)
        (should (= (- (point) (mark)) 6)))))
  :doc "selected history and its window start are rendered before restoration"
  (mevedel-batch-test--with-history
    (save-window-excursion
      (set-window-buffer (selected-window) view-buf)
      (goto-char (point-min)) (search-forward "Response 2 with target text")
      (set-mark (- (point) 11)) (setq mark-active t)
      (let ((start (line-beginning-position)))
        (set-window-start (selected-window) start t))
      (mevedel-batch-test--start-projection view-buf)
      (cl-loop repeat 30 while mevedel-view-render--batch do
        (should (equal "target text" (buffer-substring-no-properties (region-beginning) (region-end))))
        (should (save-excursion (goto-char (window-start)) (looking-at "Response 2")))
        (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
      (should (equal "target text" (buffer-substring-no-properties (region-beginning) (region-end))))))
  :doc "a reader's pinned prompt survives before deferred turns are rendered"
  (mevedel-batch-test--with-history
    (save-window-excursion
      (set-window-buffer (selected-window) view-buf)
      (goto-char (point-min))
      (search-forward "Response 2 with target text")
      (set-window-start nil (line-beginning-position) t)
      (should (equal "Prompt 2" (cdr (mevedel-view--pinned-prompt
                                      (selected-window)))))
      (mevedel-batch-test--start-projection view-buf)
      (should mevedel-view-render--batch)
      (should (equal "Prompt 2" (cdr (mevedel-view--pinned-prompt
                                      (selected-window)))))
      (should (string-search "Prompt 2" (mevedel-view--sticky-prompt-line)))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
      (cl-loop repeat 30 while mevedel-view-render--batch do
        (mevedel-view-render--batch-step view-buf mevedel-view-render--batch)
        (should (equal "Prompt 2" (cdr (mevedel-view--pinned-prompt
                                        (selected-window))))))))
  :doc "moving to another rendered response during a batch pins its own prompt"
  (mevedel-batch-test--with-history
    (save-window-excursion
      (set-window-buffer (selected-window) view-buf)
      (goto-char (point-min))
      (search-forward "Response 0 with target text")
      (set-window-start nil (line-beginning-position) t)
      (search-forward "Response 4 with target text")
      (mevedel-batch-test--start-projection view-buf)
      (should mevedel-view-render--batch)
      (goto-char (point-min))
      (search-forward "Response 4 with target text")
      (set-window-start nil (line-beginning-position) t)
      (should (equal "Prompt 4" (cdr (mevedel-view--pinned-prompt
                                      (selected-window)))))
      (should (string-search "Prompt 4" (mevedel-view--sticky-prompt-line)))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
      (cl-loop repeat 30 while mevedel-view-render--batch do
        (mevedel-view-render--batch-step view-buf mevedel-view-render--batch)
        (should (equal "Prompt 4" (cdr (mevedel-view--pinned-prompt
                                        (selected-window)))))
        (should (equal "> draft\nsecond line" (mevedel-view--input-text))))))
  :doc "scrolling onto a pending response still pins and reveals its own prompt"
  (mevedel-batch-test--with-history
    (save-window-excursion
      (set-window-buffer (selected-window) view-buf)
      (goto-char (point-min))
      (search-forward "Response 0 with target text")
      (set-window-start nil (line-beginning-position) t)
      (mevedel-batch-test--start-projection view-buf)
      (let ((entry (cl-find-if
                    (lambda (entry)
                      (and (eq 'assistant (plist-get (car entry) :role))
                           (with-current-buffer data-buf
                             (string-search
                              "Response 4"
                              (buffer-substring-no-properties
                               (plist-get (car entry) :start)
                               (plist-get (car entry) :end))))))
                    (plist-get mevedel-view-render--batch :pending))))
        (should entry)
        (set-window-start nil (nth 1 entry) t)
        (should (equal "Prompt 4" (cdr (mevedel-view--pinned-prompt
                                        (selected-window)))))
        (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
        (mevedel-view--jump-to-pinned-prompt
         (car (mevedel-view--pinned-prompt (selected-window))))
        (should (looking-at "You"))
        (should (save-excursion (forward-line 1) (looking-at "Prompt 4")))
        (should mevedel-view-render--batch)
        (should (equal "> draft\nsecond line" (mevedel-view--input-text))))))
  :doc "both windows keep their own pinned prompt during deferred rendering"
  (mevedel-batch-test--with-history
    (save-window-excursion
      (let ((left (selected-window))
            (right (split-window-right)))
        (set-window-buffer left view-buf)
        (set-window-buffer right view-buf)
        (goto-char (point-min))
        (search-forward "Response 1 with target text")
        (set-window-start left (line-beginning-position) t)
        (search-forward "Response 4 with target text")
        (set-window-start right (line-beginning-position) t)
        (mevedel-batch-test--start-projection view-buf)
        (should (equal "Prompt 1" (cdr (mevedel-view--pinned-prompt left))))
        (should (equal "Prompt 4" (cdr (mevedel-view--pinned-prompt right))))
        (should (string-search "Prompt 1"
                               (with-selected-window left
                                 (mevedel-view--sticky-prompt-line))))
        (should (string-search "Prompt 4"
                               (with-selected-window right
                                 (mevedel-view--sticky-prompt-line)))))))
  :doc "in-flight history uses the existing synchronous reconciliation"
  (mevedel-batch-test--with-history
    (mevedel-view-stream-begin-turn
     (mevedel-view--history-insertion-marker)
     (with-current-buffer data-buf (copy-marker (point-max))))
    (mevedel-view-render-batched-full)
    (should-not mevedel-view-render--batch)
    (should-not (string-search "Loading conversation turn" (buffer-string)))
    (should (equal "> draft\nsecond line" (mevedel-view--input-text)))))

(mevedel-deftest mevedel-view-render-batched-full/other-writers ()
  ,test
  (test)
  :doc "a synchronous full projection retires the old batch"
  (mevedel-batch-test--with-history
    (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
      (mevedel-batch-test--start-projection view-buf)
      (let ((old mevedel-view-render--batch))
        (mevedel-view--full-rerender)
        (should-not mevedel-view-render--batch)
        (mevedel-view-render--batch-step view-buf old)
        (should (equal expected (buffer-substring-no-properties (point-min) (point-max)))))))
  :doc "other writers see a complete projection and retain newer draft edits"
  (mevedel-batch-test--with-history
    (mevedel-batch-test--start-projection view-buf)
    (goto-char (point-max)) (insert " new edit")
    (mevedel-view-render-mutate
     'test-reader
     (lambda ()
       (should-not mevedel-view-render--batch)
       (should-not (string-search "Loading conversation turn" (buffer-string)))
       (should (string-search "Response 0" (buffer-string)))))
    (should (equal "> draft\nsecond line new edit" (mevedel-view--input-text)))))

(mevedel-deftest mevedel-view--flush-scheduled-render/batched ()
  ,test
  (test)
  :doc "the normal scheduler starts a batch and explicit immediate work finishes"
  (mevedel-batch-test--with-history
    (let ((mevedel-view-rerender-debounce 60))
      (mevedel-view-rerender view-buf)
      (cancel-timer mevedel-view--render-timer)
      (mevedel-view--flush-scheduled-render view-buf)
      (should mevedel-view-render--batch)
      (mevedel-view--schedule-render 'full data-buf 0)
      (should-not mevedel-view-render--batch)
      (should-not (string-search "Loading conversation turn" (buffer-string)))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text))))))

(mevedel-deftest mevedel-view-render--priority-turns ()
  (mevedel-batch-test--with-history
    (goto-char (point-min)) (search-forward "Response 2")
    (let* ((plan (mevedel-view--full-rerender-plan data-buf data-buf view-buf nil))
           (turns (mevedel-view-render--priority-turns (plist-get plan :turns))))
      (should (= (length turns) 1))
      (should (eq (plist-get (car turns) :role) 'assistant))
      (should (with-current-buffer data-buf
                (string-search "Response 2" (buffer-substring-no-properties
                                             (plist-get (car turns) :start)
                                             (plist-get (car turns) :end))))))))

(mevedel-deftest mevedel-view--group-into-turns/slices ()
  ,test
  (test)
  :doc "a slice boundary preserves lookahead and assistant grouping"
  (mevedel-view-test--with-buffers
    (dolist (entry '(("Prompt\n" . nil)
                     ("First response\n" . response)
                     ("\n" . nil)
                     ("Second response\n" . response)
                     ("Next prompt\n" . nil)
                     ("Final response\n" . response)))
      (mevedel-view-test--insert-data data-buf (car entry) (cdr entry)))
    (let* ((segments (with-current-buffer data-buf
                       (mevedel-transcript-segments (point-min) (point-max))))
           (expected (mevedel-view--group-into-turns segments data-buf))
           (state nil)
           (rest segments))
      (while (cdr rest)
        (setq state (mevedel-view--group-into-turns
                     (list (car rest)) data-buf state (cadr rest))
              rest (cdr rest)))
      (should (equal expected (mevedel-view--group-into-turns rest data-buf state))))))

(mevedel-deftest mevedel-view-render-batched-full/preparation ()
  ,test
  (test)
  :doc "a scheduled rebuild retains the old display until canonical preparation finishes"
  (mevedel-batch-test--with-history
    (let ((before (buffer-substring-no-properties (point-min) (point-max))))
      (mevedel-view-render-batched-full)
      (should mevedel-view-render--batch)
      (should (eq 'prepare (plist-get mevedel-view-render--batch :phase)))
      (should (equal before (buffer-substring-no-properties (point-min) (point-max))))
      (cl-loop repeat 100 while (and mevedel-view-render--batch
                                     (eq 'prepare (plist-get mevedel-view-render--batch :phase)))
               do (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
      (should (eq 'project (plist-get mevedel-view-render--batch :phase)))
      (cl-loop repeat 100 while mevedel-view-render--batch do
        (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
      (should-not mevedel-view-render--batch)
      (should (equal before (buffer-substring-no-properties (point-min) (point-max)))))))

(mevedel-deftest mevedel-view-render-batched-full/fontify-preparation ()
  ,test
  (test)
  :doc "whole-response Markdown work yields before publication and releases its job"
  (mevedel-batch-test--with-history
    (let ((before (buffer-substring-no-properties (point-min) (point-max)))
          (started 0) (steps 0) (cancelled 0))
      (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
                 (lambda () 'markdown-ts-mode))
                ((symbol-function 'mevedel-view--markdown-fontify-job-start)
                 (lambda (text) (cl-incf started) (cons text 0)))
                ((symbol-function 'mevedel-view--markdown-fontify-job-step)
                 (lambda (job _size)
                   (cl-incf steps)
                   (setcdr job (1+ (cdr job)))
                   (>= (cdr job) 2)))
                ((symbol-function 'mevedel-view--markdown-fontify-job-result)
                 (lambda (job)
                   (propertize (car job) 'font-lock-face 'bold)))
                ((symbol-function 'mevedel-view--markdown-fontify-job-cancel)
                 (lambda (_job) (cl-incf cancelled))))
        (mevedel-view-render-batched-full)
        (cl-loop repeat 100 while (and mevedel-view-render--batch
                                       (eq 'prepare (plist-get mevedel-view-render--batch :phase)))
                 do (progn
                      (should (equal before (buffer-substring-no-properties (point-min) (point-max)))))
                      (mevedel-view-render--batch-step view-buf mevedel-view-render--batch)))
        (should (eq 'project (plist-get mevedel-view-render--batch :phase)))
        (should (> started 0))
        (should (>= steps (* 2 started)))
        (should (= started cancelled))
        (should (> (hash-table-count (plist-get mevedel-view-render--batch :fontified)) 0))
        (cl-loop repeat 100 while mevedel-view-render--batch do
          (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
        (should (equal before (buffer-substring-no-properties (point-min) (point-max))))))
  :doc "a source edit cancels an unfinished private Markdown job"
  (mevedel-batch-test--with-history
    (let (cancelled)
      (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
                 (lambda () 'markdown-ts-mode))
                ((symbol-function 'mevedel-view--markdown-fontify-job-start)
                 (lambda (text) (list text)))
                ((symbol-function 'mevedel-view--markdown-fontify-job-step)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel-view--markdown-fontify-job-cancel)
                 (lambda (job) (setq cancelled job))))
        (mevedel-view-render-batched-full)
        (cl-loop repeat 100 until (plist-get mevedel-view-render--batch :fontify)
                 do (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
        (should (plist-get mevedel-view-render--batch :fontify))
        (with-current-buffer data-buf
          (goto-char (point-max)) (insert "edited"))
        (mevedel-view-render--batch-step view-buf mevedel-view-render--batch)
        (should cancelled)
        (should (eq 'prepare (plist-get mevedel-view-render--batch :phase)))))))

(mevedel-deftest mevedel-view-render-batched-full/tool-preparation ()
  ,test
  (test)
  :doc "scheduled projection prepares source-backed tool entries before rendering"
  (mevedel-batch-test--with-history
    (mevedel-view-test--insert-data
     data-buf "(:name \"Unknown\" :args nil)\nTool result.\n" '(tool . "one"))
    (mevedel-view--full-rerender)
    (let ((before (buffer-substring-no-properties (point-min) (point-max)))
          (calls 0)
          (entry (symbol-function 'mevedel-view--tool-segment-entry)))
      (cl-letf (((symbol-function 'mevedel-view--tool-segment-entry)
                 (lambda (&rest args)
                   (cl-incf calls)
                   (apply entry args))))
        (mevedel-batch-test--start-projection view-buf)
        (should (> calls 0))
        (should (> (hash-table-count
                    (plist-get mevedel-view-render--batch :tool-cache)) 0))
        (cl-loop repeat 100 while mevedel-view-render--batch do
          (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
        (should (equal before (buffer-substring-no-properties
                               (point-min) (point-max))))))))

(mevedel-deftest mevedel-view-render--batch-turn ()
  (mevedel-batch-test--with-history
    (mevedel-batch-test--start-projection view-buf)
    (let* ((job mevedel-view-render--batch)
           (entry (car (plist-get job :pending))))
      (mevedel-view-render--batch-turn job entry)
      (should (< (nth 1 entry) (nth 2 entry)))
      (should-not (string-search "Loading conversation turn"
                                (buffer-substring (nth 1 entry) (nth 2 entry)))))))

(mevedel-deftest mevedel-view-render--batch-step ()
  ,test
  (test)
  :doc "one callback consumes exactly one pending turn"
  (mevedel-batch-test--with-history
    (mevedel-batch-test--start-projection view-buf)
    (let* ((job mevedel-view-render--batch)
           (count (length (plist-get job :pending))))
      (mevedel-view-render--batch-step view-buf job)
      (should (eq job mevedel-view-render--batch))
      (should (= (length (plist-get job :pending)) (1- count)))))
  :doc "obsolete callbacks cannot consume a newer job"
  (mevedel-batch-test--with-history
    (mevedel-batch-test--start-projection view-buf)
    (let ((old mevedel-view-render--batch))
      (mevedel-batch-test--start-projection view-buf)
      (let ((current mevedel-view-render--batch)
            (before (buffer-string)))
        (mevedel-view-render--batch-step view-buf old)
        (should (eq current mevedel-view-render--batch))
        (should (equal before (buffer-string))))))
  :doc "source changes retire the plan and eventually project current text"
  (mevedel-batch-test--with-history
    (mevedel-batch-test--start-projection view-buf)
    (let ((old mevedel-view-render--batch))
      (with-current-buffer data-buf
        (goto-char (point-max)) (insert (propertize "Changed source.\n" 'gptel 'response)))
      (mevedel-view-render--batch-step view-buf old)
      (should-not (eq old mevedel-view-render--batch))
      (cl-loop repeat 30 while mevedel-view-render--batch do
        (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
      (should (string-search "Changed source." (buffer-string)))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text)))))
  :doc "failed batch rolls back its partial insertion and recovers canonically"
  (mevedel-batch-test--with-history
    (let ((expected (buffer-substring-no-properties (point-min) (point-max)))
          (render (symbol-function 'mevedel-view--render-turn)) failed)
      (mevedel-batch-test--start-projection view-buf)
      (cl-letf (((symbol-function 'mevedel-view--render-turn)
                 (lambda (&rest args)
                   (if failed (apply render args)
                     (setq failed t)
                     (insert "Partial failed turn")
                     (error "Injected batch failure")))))
        (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
      (should-not mevedel-view-render--batch)
      (should (equal expected (buffer-substring-no-properties (point-min) (point-max)))))))

(mevedel-deftest mevedel-view-render-cancel-batch ()
  ,test
  (test)
  :doc "cancellation releases timer and pending source markers"
  (mevedel-batch-test--with-history
    (mevedel-batch-test--start-projection view-buf)
    (let* ((job mevedel-view-render--batch)
           (timer (plist-get job :timer))
           (start (nth 1 (car (plist-get job :pending)))))
      (should (memq timer timer-list))
      (mevedel-view-render-cancel-batch)
      (should-not mevedel-view-render--batch)
      (should-not (marker-buffer start))
      (should-not (memq timer timer-list))))
  :doc "view death cancels pending callbacks"
  (mevedel-batch-test--with-history
    (mevedel-batch-test--start-projection view-buf)
    (let ((timer (plist-get mevedel-view-render--batch :timer)))
      (kill-buffer view-buf)
      (should-not (memq timer timer-list)))))

(mevedel-deftest mevedel-view-render-resume-batch ()
  ,test
  (test)
  :doc "focus loss pauses callbacks; focus recovery arms exactly one timer"
  (mevedel-batch-test--with-history
    (mevedel-batch-test--start-projection view-buf)
    (let* ((job mevedel-view-render--batch)
           (pending (length (plist-get job :pending))))
      (cl-letf (((symbol-function 'mevedel-view--unattended-p) (lambda (&rest _) t)))
        (mevedel-view-render--batch-step view-buf job)
        (mevedel-view-render-resume-batch)
        (should-not (plist-get job :timer))
        (should (= pending (length (plist-get job :pending)))))
      (goto-char (point-max)) (insert " while paused")
      (mevedel-view--resume-render-if-attended view-buf)
      (let ((timer (plist-get job :timer)))
        (should (memq timer timer-list))
        (mevedel-view-render-resume-batch)
        (should (eq timer (plist-get job :timer))))
      (cl-loop repeat 30 while mevedel-view-render--batch do
        (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
      (should-not mevedel-view-render--batch)
      (should (equal "> draft\nsecond line while paused" (mevedel-view--input-text))))
  :doc "a paused job releases its collection hold until it resumes"
  (mevedel-batch-test--with-history
    (let ((holds (make-hash-table :test #'eq)))
      (cl-letf (((symbol-function 'mevedel--gc-hold)
                 (lambda (key predicate) (puthash key predicate holds)))
                ((symbol-function 'mevedel--gc-release)
                 (lambda (key) (remhash key holds))))
        (mevedel-batch-test--start-projection view-buf)
        (let ((job mevedel-view-render--batch))
          (should (gethash job holds))
          (cl-letf (((symbol-function 'mevedel-view--unattended-p) (lambda (&rest _) t)))
            (mevedel-view-render--batch-step view-buf job))
          (should-not (gethash job holds))
          (mevedel-view--resume-render-if-attended view-buf)
          (should (gethash job holds))
          (should (funcall (gethash job holds)))
          (mevedel-view-render-cancel-batch))))))
  :doc "transport contention postpones a turn without consuming its source"
  (mevedel-batch-test--with-history
    (mevedel-batch-test--start-projection view-buf)
    (let* ((job mevedel-view-render--batch)
           (pending (length (plist-get job :pending))))
      (cl-letf (((symbol-function 'mevedel-transport-busy-p) (lambda (&rest _) t)))
        (mevedel-view-render--batch-step view-buf job)
        (should (= pending (length (plist-get job :pending))))
        (should (memq (plist-get job :timer) timer-list)))
      (mevedel-view-render--batch-step view-buf job)
      (should (= (1- pending) (length (plist-get job :pending)))))))

(mevedel-deftest mevedel-view-render--start-batch ()
  ,test
  (test)
  :doc "a nested request installs its plan only after the owning writer exits"
  (mevedel-batch-test--with-history
    (mevedel-view-render-mutate
     'outer
     (lambda ()
       (mevedel-view-render-batched-full)
       (should-not mevedel-view-render--batch)
       (goto-char (point-max)) (insert " latest edit")))
    (should mevedel-view-render--batch)
    (cl-loop repeat 30 while mevedel-view-render--batch do
      (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
    (should-not mevedel-view-render--batch)
    (should (equal "> draft\nsecond line latest edit" (mevedel-view--input-text))))
  :doc "defers collection during the job and a short grace after it"
  (mevedel-batch-test--with-history
    (let (live-p)
      (cl-letf (((symbol-function 'mevedel--gc-hold)
                 (lambda (_key predicate) (setq live-p predicate))))
        (mevedel-view-render-batched-full))
      (should (funcall live-p))
      (let ((job mevedel-view-render--batch))
        (mevedel-view-render-cancel-batch)
        ;; The finished job keeps its hold through the grace period.
        (should (funcall live-p))
        (setf (plist-get job :settled-until) (1- (float-time)))
        (should-not (funcall live-p)))))
  :doc "a queued writer after batch installation sees complete history"
  (mevedel-batch-test--with-history
    (mevedel-view-render-mutate
     'outer
     (lambda ()
       (mevedel-view-render-batched-full)
       (mevedel-view-render-mutate
        'later
        (lambda ()
          (should-not mevedel-view-render--batch)
          (should-not (string-search "Loading conversation turn" (buffer-string)))
          (should (string-search "Response 0" (buffer-string)))))))
    (should-not mevedel-view-render--batch))
  :doc "failure while installing priority turns releases work and restores the view"
  (mevedel-batch-test--with-history
    (let ((before (buffer-substring-no-properties (point-min) (point-max))) job)
      (cl-letf (((symbol-function 'mevedel-view-render--batch-turn)
                 (lambda (current _entry)
                   (setq job current)
                   (error "Injected initial failure"))))
        (mevedel-view-render-batched-full)
        (cl-loop repeat 100 while mevedel-view-render--batch do
          (mevedel-view-render--batch-step view-buf mevedel-view-render--batch)))
      (should job)
      (should-not mevedel-view-render--batch)
      (dolist (entry (plist-get job :pending))
        (should-not (marker-buffer (nth 1 entry)))
        (should-not (marker-buffer (nth 2 entry))))
      (should (equal before (buffer-substring-no-properties (point-min) (point-max)))))))

(mevedel-deftest mevedel-view-render-batched-full/disclosures ()
  ,test
  (test)
  :doc "expanded tool bodies and manually folded turns survive all callbacks"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Prompt\n" nil)
    (mevedel-view-test--insert-data
     data-buf "(:name \"Unknown\" :args nil)\nBody retained on expansion.\n" '(tool . "one"))
    (dotimes (index 4)
      (mevedel-view-test--insert-data data-buf (format "Prompt %d\n" index) nil)
      (mevedel-view-test--insert-data data-buf (format "Response %d\n" index) 'response))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min)) (search-forward "Unknown")
      (mevedel-view-toggle-section)
      (should (string-search "Body retained on expansion" (buffer-string)))
      (goto-char (point-min)) (search-forward "Response 1")
      (search-backward "Assistant")
      (mevedel-view-toggle-section)
      (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
        (goto-char (point-max))
        (mevedel-batch-test--start-projection view-buf)
        (cl-loop repeat 30 while mevedel-view-render--batch do
          (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
        (should-not mevedel-view-render--batch)
        (should (equal expected (buffer-substring-no-properties (point-min) (point-max))))))))

(mevedel-deftest mevedel-view-render--batch-step/reentry
  (:doc "a timer queued inside a writer consumes its entry only after ownership transfers")
  (mevedel-batch-test--with-history
    (mevedel-batch-test--start-projection view-buf)
    (let* ((job mevedel-view-render--batch)
           (count (length (plist-get job :pending))))
      (mevedel-view-render-mutate
       'full-batch
       (lambda ()
         (mevedel-view-render--batch-step view-buf job)
         (should (= count (length (plist-get job :pending))))))
      (should (= (1- count) (length (plist-get job :pending)))))))

(mevedel-deftest mevedel-view-render--priority-turns/visible-queue
  (:doc "renders prompts and reader anchors immediately, then queues visible responses before offscreen history")
  (mevedel-batch-test--with-history
    (save-window-excursion
      (set-window-buffer (selected-window) view-buf)
      (goto-char (point-min)) (search-forward "Response 1")
      (set-window-start (selected-window) (line-beginning-position) t)
      (let ((end (save-excursion (search-forward "Response 4") (point))))
        (cl-letf (((symbol-function 'window-end) (lambda (&rest _) end)))
          (let* ((plan (mevedel-view--full-rerender-plan data-buf data-buf view-buf nil))
                 (turns (plist-get plan :turns)))
            (should (> (length (mevedel-view-render--priority-turns turns))
                       (length (mevedel-view-render--priority-turns turns t)))))
          (mevedel-view-render-batched-full)))
      (cl-loop repeat 100 while (and mevedel-view-render--batch
                                     (eq 'prepare (plist-get mevedel-view-render--batch :phase)))
               do (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
      (should (string-search "Response 1" (buffer-string)))
      (should (string-search "Prompt 2" (buffer-string)))
      (let ((next (car (car (plist-get mevedel-view-render--batch :pending)))))
        (should (with-current-buffer data-buf
                  (string-search "Response 2"
                                 (buffer-substring-no-properties
                                  (plist-get next :start) (plist-get next :end)))))))))

(provide 'test-mevedel-view-render-batch)
;;; test-mevedel-view-render-batch.el ends here
