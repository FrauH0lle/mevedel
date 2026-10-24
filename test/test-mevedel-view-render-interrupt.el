;;; test-mevedel-view-render-interrupt.el --- Input-aborted preparation -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise real history batches across input aborts, not replacement plan jobs.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-view-prepare)

(defvar mevedel-interrupt-test--ids nil
  "Generated turn identities assigned canonical ordinals during one snapshot.")

(defun mevedel-interrupt-test--value (value)
  "Freeze marker coordinates in VALUE, normalizing only runtime identities."
  (cond
   ((and (symbolp value)
         (string-match-p "\\`mevedel-view-turn-[0-9]+\\'" (symbol-name value)))
    (or (gethash value mevedel-interrupt-test--ids)
        (puthash value (list :turn-id (hash-table-count mevedel-interrupt-test--ids))
                 mevedel-interrupt-test--ids)))
   ((markerp value)
    (list :marker (marker-buffer value) (marker-position value)
          (marker-insertion-type value)))
   ((overlayp value)
    (list :overlay (overlay-buffer value) (overlay-start value) (overlay-end value)))
   ((and (functionp value) (not (symbolp value))) :callback)
   ((consp value)
    (cons (mevedel-interrupt-test--value (car value))
          (mevedel-interrupt-test--value (cdr value))))
   ((hash-table-p value)
    (let (entries)
      (maphash (lambda (key entry)
                 (push (cons (mevedel-interrupt-test--value key)
                             (mevedel-interrupt-test--value entry)) entries)) value)
      (sort entries (lambda (a b) (string< (prin1-to-string (car a))
                                         (prin1-to-string (car b)))))))
   ((vectorp value) (vconcat (mapcar #'mevedel-interrupt-test--value value)))
   (t value)))

(defun mevedel-interrupt-test--snapshot ()
  "Copy all rendered properties and frozen source/view coordinates."
  (let ((text (buffer-string)) (pos 0)
        (mevedel-interrupt-test--ids (make-hash-table :test #'eq)))
    (while (< pos (length text))
      (let ((end (next-property-change pos text (length text))) props)
        (cl-loop for (key value) on (text-properties-at pos text) by #'cddr
                 do (push (cons key (mevedel-interrupt-test--value value)) props))
        (setq props (sort props (lambda (a b) (string< (symbol-name (car a))
                                                     (symbol-name (car b))))))
        (set-text-properties pos end
                             (cl-loop for (key . value) in props append (list key value))
                             text)
        (setq pos end)))
    text))

(defun mevedel-interrupt-test--input ()
  "Deliver input through the evaluator's actual `while-no-input' quit path."
  (should throw-on-input)
  (should-not inhibit-quit)
  (setq unread-command-events '(?x)
        quit-flag throw-on-input)
  ;; A pending event alone does not interrupt Lisp already inside the macro.
  ;; The evaluator observes the same quit flag Emacs sets on arriving input.
  (eval '(identity nil)))

(defun mevedel-interrupt-test--drain (view)
  "Finish VIEW's actual batch with a bounded number of callbacks."
  (cl-loop repeat 300 while mevedel-view-render--batch do
           (mevedel-view-render--batch-step view mevedel-view-render--batch))
  (should-not mevedel-view-render--batch))

(defun mevedel-interrupt-test--advance (view predicate)
  "Advance VIEW's batch until PREDICATE accepts the job."
  (cl-loop repeat 200
           until (and mevedel-view-render--batch
                      (funcall predicate mevedel-view-render--batch))
           do (should mevedel-view-render--batch)
           (mevedel-view-render--batch-step view mevedel-view-render--batch))
  (should (funcall predicate mevedel-view-render--batch)))

(defmacro mevedel-interrupt-test--with-history (&rest body)
  "Run BODY with a long single-line response, real tools and a draft."
  (declare (indent 0))
  `(mevedel-view-test--with-buffers
     (mevedel-view-test--insert-data data-buf "Prompt\n" nil)
     (mevedel-view-test--insert-data
      data-buf (concat ";; long response " (make-string 8192 ?x) "\n") 'response)
     (dotimes (index 3)
       (mevedel-view-test--insert-data
        data-buf
        (format "(:name \"Unknown\" :args nil)\nTool result %d.\n" index)
        (cons 'tool (format "interrupt-%d" index))))
     (mevedel-view-test--insert-data data-buf "Done.\n" 'response)
     (with-current-buffer view-buf
       ;; Grammar availability is not the subject here.  Keep the real private
       ;; Markdown jobs and font-lock engine, using an installed built-in mode.
       (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
                  (lambda () 'emacs-lisp-mode))
                 ((symbol-function 'mevedel-view--markdown-fontify-setup)
                  (lambda (buffer _mode)
                    (with-current-buffer buffer
                      (emacs-lisp-mode)
                      (buffer-disable-undo)))))
         (mevedel-view--full-rerender)
         (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
         (save-window-excursion
           (set-window-buffer (selected-window) view-buf)
           (set-window-start nil (mevedel-view--input-start) t)
           ,@body)))))

(defun mevedel-interrupt-test--source-edit-window-control ()
  "Return before/after window offsets for an uninterrupted source-edit batch."
  (mevedel-interrupt-test--with-history
    (let ((unread-command-events nil) before)
      (mevedel-view-render-batched-full)
      (mevedel-interrupt-test--drain view-buf)
      (clrhash mevedel-view--response-fontify-cache)
      (setq mevedel-view--response-cache-entries 0)
      (mevedel-view-render-batched-full)
      (mevedel-interrupt-test--advance
       view-buf (lambda (job) (plist-get job :fontify)))
      (setq before (- (window-start) (mevedel-view--input-start)))
      (mevedel-view-test--insert-data data-buf "Changed source after input.\n" 'response)
      (mevedel-interrupt-test--drain view-buf)
      (list before (- (window-start) (mevedel-view--input-start))))))

(defun mevedel-interrupt-test--exercise (stage &optional scan-phase extraction mutation)
  "Interrupt STAGE and SCAN-PHASE, or EXTRACTION, then resume with MUTATION."
  (mevedel-interrupt-test--with-history
    (let* ((unread-command-events nil)
          (private-buffers nil)
          (start (symbol-function 'mevedel-view--markdown-fontify-job-start))
          (scan-helper (if (eq stage 'group) 'mevedel-view--group-into-turns
                         (pcase scan-phase
                           ('controls 're-search-forward)
                           ('raw 'mevedel-transcript--next-raw-range)
                           ('finish 'mevedel-transcript--finish-segments)
                           (_ 'mevedel-transcript--classify-gptel-prop))))
          (scan (symbol-function scan-helper))
          (fontify (symbol-function 'font-lock-ensure))
          (promote (symbol-function 'mevedel-view--promote-face-to-font-lock-face))
          (tool (symbol-function 'mevedel-view--tool-segment-entry))
          expected before source source-tick job font-job font-buffer window-offset
          (mutation-window (and mutation (mevedel-interrupt-test--source-edit-window-control)))
          group-state group-remaining
          (injected 0) (scan-calls 0) (calls nil) armed)
      (when (eq scan-phase 'controls)
        (dotimes (index 3)
          (mevedel-view-test--insert-data
           data-buf (mevedel--format-hook-audit-record
                     (list :type 'tool-hook :phase 'post-tool
                           :message (format "Control candidate %d" index)))
           'mevedel-hook-audit)))
      (when (eq stage 'group)
        ;; Keep a multi-segment assistant turn across the first group chunk.
        ;; The following user turn flushes it in the interrupted chunk, so a
        ;; destructive reverse would mutate the earlier retained state.
        (dotimes (index 30)
          (mevedel-view-test--insert-data
           data-buf (format "(:name \"Unknown\" :args nil)\nGroup result %d.\n" index)
           (cons 'tool (format "group-%d" index))))
        (dotimes (index 36)
          (mevedel-view-test--insert-data data-buf (format "Prompt %d\n" index) nil)
          (mevedel-view-test--insert-data data-buf (format "Response %d\n" index) 'response)))
      (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-job-start)
                 (lambda (text)
                   (let ((font-job (funcall start text)))
                     (push (mevedel-view--markdown-fontify-job-buffer font-job)
                           private-buffers)
                     font-job)))
                ((symbol-function 'mevedel-view-render--full-now)
                 (lambda () (ert-fail "Input took synchronous fallback"))))
        ;; Establish the control through the same public batch entry point.
        (clrhash mevedel-view--response-fontify-cache)
        (setq mevedel-view--response-cache-entries 0)
        (mevedel-view-render-batched-full)
        (mevedel-interrupt-test--drain view-buf)
        ;; Input can give jit-lock an opportunity to visit the old display.
        ;; Compare both completed projections after the same real consumer visit;
        ;; the snapshot itself never modifies the view or strips cache flags.
        (jit-lock-fontify-now (point-min) (point-max))
        (setq expected (mevedel-interrupt-test--snapshot))
        (clrhash mevedel-view--response-fontify-cache)
        (setq mevedel-view--response-cache-entries 0)
        (mevedel-view-render-batched-full)
        (mevedel-interrupt-test--advance
         view-buf
         (lambda (current)
           (and (eq stage (plist-get current :prepare-stage))
                (or (not (eq stage 'group)) (plist-get current :group-state))
                (or (not scan-phase)
                    (eq scan-phase (mevedel-transcript--scan-phase
                                    (plist-get current :scan))))
                (or (not (eq stage 'fontify)) (plist-get current :fontify)))))
        (setq job mevedel-view-render--batch
              font-job (plist-get job :fontify)
              font-buffer (and font-job
                               (mevedel-view--markdown-fontify-job-buffer font-job))
              before (buffer-string)
              source (with-current-buffer data-buf (buffer-string))
              source-tick (with-current-buffer data-buf (buffer-modified-tick))
              window-offset (- (window-start) (mevedel-view--input-start))
              group-state (copy-tree (plist-get job :group-state))
              group-remaining (plist-get job :remaining)
              armed t)
        (cl-letf (((symbol-function scan-helper)
                   (lambda (&rest args)
                     (prog1 (apply scan args)
                       (when (and armed (memq stage '(scan group))
                                  (or (eq stage 'group)
                                      (eq (current-buffer) data-buf)))
                         (cl-incf scan-calls)
                         (when (or (not (eq scan-phase 'controls)) (= scan-calls 2))
                         (with-current-buffer view-buf (should mevedel-view-render--owner))
                         (setq armed nil)
                         (cl-incf injected)
                         (mevedel-interrupt-test--input))))))
                  ((symbol-function 'font-lock-ensure)
                   (lambda (&rest args)
                     (prog1 (apply fontify args)
                       (when (and armed (eq stage 'fontify) (not extraction)
                                  (eq (current-buffer) font-buffer))
                         (with-current-buffer view-buf (should mevedel-view-render--owner))
                         (should (> (- (cadr args) (car args)) 8192))
                         (setq armed nil)
                         (cl-incf injected)
                         (mevedel-interrupt-test--input)))))
                  ((symbol-function 'mevedel-view--promote-face-to-font-lock-face)
                   (lambda (text)
                     (prog1 (funcall promote text)
                       (when (and armed extraction)
                         (with-current-buffer view-buf (should mevedel-view-render--owner))
                         (setq armed nil)
                         (cl-incf injected)
                         (mevedel-interrupt-test--input)))))
                  ((symbol-function 'mevedel-view--tool-segment-entry)
                   (lambda (segment data)
                     (when (and (eq 'prepare (plist-get job :phase))
                                (eq 'tools (plist-get job :prepare-stage)))
                       (push (cadr segment) calls)
                       (when (and armed (eq stage 'tools) (= (length calls) 2))
                         (should mevedel-view-render--owner)
                         (setq armed nil)
                         (cl-incf injected)
                         (mevedel-interrupt-test--input)))
                     (funcall tool segment data))))
          (mevedel-view-render--batch-step view-buf job)
          (should (= injected 1))
          (should (equal unread-command-events '(?x)))
          (should-not quit-flag)
          (should (eq job mevedel-view-render--batch))
          (should (eq 'prepare (plist-get job :phase)))
          (when (eq stage 'group)
            (should (equal group-state (plist-get job :group-state)))
            (should (eq group-remaining (plist-get job :remaining))))
          (should (zerop (hash-table-count (plist-get job :cache-used))))
          (should (memq (plist-get job :timer) timer-list))
          (should-not mevedel-view-render--owner)
          (should-not mevedel-view-render--pending)
          (should (equal-including-properties before (buffer-string)))
          (should (equal-including-properties
                   source (with-current-buffer data-buf (buffer-string))))
          (should (= source-tick (with-current-buffer data-buf (buffer-modified-tick))))
          (when font-job
            (should (buffer-live-p font-buffer))
            (should-not (mevedel-view--markdown-fontify-job-cancelled font-job))
            (if extraction
                (should (mevedel-view--markdown-fontify-job-done font-job))
              (should (= 1 (mevedel-view--markdown-fontify-job-next font-job))))
            (should (zerop (hash-table-count mevedel-view--response-fontify-cache))))
          ;; Continuous input must keep the old display, plan, and resources.
          (dotimes (_ 3)
            (mevedel-view-render--batch-step view-buf job)
            (should (eq job mevedel-view-render--batch))
            (should (eq stage (plist-get job :prepare-stage)))
            (should (equal-including-properties before (buffer-string)))
            (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
            (should (= (point) (+ 4 (mevedel-view--input-start))))
            (should (= (- (window-start) (mevedel-view--input-start)) window-offset))
            (should-not mevedel-view-render--owner))
          (setq unread-command-events nil)
          (when mutation
            (mevedel-view-test--insert-data data-buf "Changed source after input.\n" 'response)
            (mevedel-view-render--batch-step view-buf job)
            (should-not (eq job mevedel-view-render--batch))
            (should (mevedel-view--markdown-fontify-job-cancelled font-job))
            (should-not (buffer-live-p font-buffer)))
          (mevedel-interrupt-test--drain view-buf))
        (when (eq stage 'tools)
          ;; First entry commits once; only the interrupted second entry repeats.
          (should (= (length calls) 4))
          (should (= 1 (cl-count (car (last calls)) calls)))
          (should (> (hash-table-count mevedel-view--tool-rendering-cache) 0)))
        (when mutation (should (= window-offset (car mutation-window))))
        (should (= (- (window-start) (mevedel-view--input-start))
                   (if mutation (cadr mutation-window) window-offset)))
        (if mutation
            (progn
              (should (string-search "Changed source after input." (buffer-string)))
              (jit-lock-fontify-now (point-min) (point-max))
              (setq expected (mevedel-interrupt-test--snapshot))
              (mevedel-view-render-batched-full)
              (mevedel-interrupt-test--drain view-buf)))
        (jit-lock-fontify-now (point-min) (point-max))
        (should (equal-including-properties expected (mevedel-interrupt-test--snapshot)))
        (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
        (should (= (point) (+ 4 (mevedel-view--input-start))))
        (dolist (buffer private-buffers) (should-not (buffer-live-p buffer)))))))

(mevedel-deftest mevedel-view-render-batched-full/fontified-control ()
  ,test
  (test)
  :doc "uninterrupted cold and warm batches converge after the same jit-lock visit"
  (mevedel-interrupt-test--with-history
    (mevedel-view-test--insert-data data-buf "Changed source after input.\n" 'response)
    (let (cold warm)
      (clrhash mevedel-view--response-fontify-cache)
      (setq mevedel-view--response-cache-entries 0)
      (mevedel-view-render-batched-full)
      (mevedel-interrupt-test--drain view-buf)
      (jit-lock-fontify-now (point-min) (point-max))
      (setq cold (mevedel-interrupt-test--snapshot))
      (mevedel-view-render-batched-full)
      (mevedel-interrupt-test--drain view-buf)
      (jit-lock-fontify-now (point-min) (point-max))
      (setq warm (mevedel-interrupt-test--snapshot))
      (should (equal-including-properties cold warm)))))

(mevedel-deftest mevedel-view-render--batch-step/input-interrupt ()
  ,test
  (test)
  :doc "input aborts canonical scanning without fallback and resumes the real batch"
  (dolist (phase '(properties controls raw finish))
    (mevedel-interrupt-test--exercise 'scan phase))
  :doc "input aborts grouping after an earlier chunk without corrupting retained turns"
  (mevedel-interrupt-test--exercise 'group)
  :doc "input aborts a huge single-line font-lock chunk without cancelling its job"
  (mevedel-interrupt-test--exercise 'fontify)
  :doc "input aborts fontified output extraction without destroying its retry input"
  (mevedel-interrupt-test--exercise 'fontify nil t)
  :doc "a source edit retires the input-aborted private job before publishing new text"
  (mevedel-interrupt-test--exercise 'fontify nil nil t)
  :doc "input aborts tool preparation and retries only the uncommitted entry"
  (mevedel-interrupt-test--exercise 'tools))

(defun mevedel-interrupt-test--quit (extraction)
  "Quit a real fontification chunk or EXTRACTION without retry or fallback."
  (mevedel-interrupt-test--with-history
    (let ((unread-command-events nil)
          (quit-flag nil))
      (clrhash mevedel-view--response-fontify-cache)
      (setq mevedel-view--response-cache-entries 0)
      (mevedel-view-render-batched-full)
      (mevedel-interrupt-test--advance
       view-buf (lambda (job) (plist-get job :fontify)))
      (let* ((job mevedel-view-render--batch)
             (font-job (plist-get job :fontify))
             (font-buffer (mevedel-view--markdown-fontify-job-buffer font-job))
             (timer (plist-get job :timer))
             (before (buffer-string))
             (source (with-current-buffer data-buf (buffer-string)))
             (source-tick (with-current-buffer data-buf (buffer-modified-tick)))
             (point-before (point))
             (window-before (window-start))
             (seam (if extraction 'mevedel-view--promote-face-to-font-lock-face
                     'font-lock-ensure))
             (original (symbol-function seam))
             injected quit-signaled)
        (unwind-protect
            (cl-letf (((symbol-function seam)
                       (lambda (&rest args)
                         (prog1 (apply original args)
                           (when (and (not injected)
                                      (or extraction (eq (current-buffer) font-buffer)))
                             (should throw-on-input)
                             (should-not inhibit-quit)
                             (with-current-buffer view-buf (should mevedel-view-render--owner))
                             (setq injected t quit-flag t)
                             (eval '(identity nil))))))
                      ((symbol-function 'mevedel-view-render--full-now)
                       (lambda () (ert-fail "Quit took synchronous fallback")))
                      ((symbol-function 'mevedel-view--sweep-tool-rendering-cache)
                       (lambda (&rest _) (ert-fail "Quit swept projection caches"))))
              ;; C-g may propagate to the command boundary, unlike ordinary
              ;; pending input.  Observe its owned cleanup before continuing.
              (condition-case nil
                  (mevedel-view-render--batch-step view-buf job)
                (quit (setq quit-signaled t quit-flag nil)))
              (should injected)
              (should quit-signaled)
              (should-not mevedel-view-render--batch)
              (should-not (plist-get job :timer))
              (should-not (memq timer timer-list))
              (should-not (buffer-live-p font-buffer))
              (should (mevedel-view--markdown-fontify-job-cancelled font-job))
              (should (zerop (hash-table-count mevedel-view--response-fontify-cache)))
              (should-not mevedel-view-render--owner)
              (should-not mevedel-view-render--pending)
              (should (equal-including-properties before (buffer-string)))
              (should (equal-including-properties
                       source (with-current-buffer data-buf (buffer-string))))
              (should (= source-tick (with-current-buffer data-buf (buffer-modified-tick))))
              (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
              (should (= point-before (point)))
              (should (= window-before (window-start)))
              ;; An already dispatched callback from the cancelled plan cannot
              ;; resurrect its cancelled Markdown job or publish old output.
              (mevedel-view-render--batch-step view-buf job)
              (should-not mevedel-view-render--batch)
              (should-not (plist-get job :timer))
              (should (equal-including-properties before (buffer-string)))
              (should (= point-before (point)))
              (should (= window-before (window-start))))
          (setq quit-flag nil))))))

(mevedel-deftest mevedel-view-render--batch-step/quit ()
  ,test
  (test)
  :doc "C-g during a Markdown chunk cancels rather than resuming a dead job"
  (mevedel-interrupt-test--quit nil)
  :doc "C-g during extraction cancels rather than falling back synchronously"
  (mevedel-interrupt-test--quit t))

(mevedel-deftest mevedel-view--markdown-fontify-job-result/nonlocal ()
  ,test
  (test)
  :doc "an arbitrary extraction throw still cancels and releases the private buffer"
  (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
             (lambda () 'emacs-lisp-mode))
            ((symbol-function 'mevedel-view--markdown-fontify-setup)
             (lambda (buffer _mode)
               (with-current-buffer buffer (emacs-lisp-mode)))))
    (let* ((job (mevedel-view--markdown-fontify-job-start ";; response"))
           (buffer (mevedel-view--markdown-fontify-job-buffer job))
           (entered nil))
      (unwind-protect
          (progn
            (should (mevedel-view--markdown-fontify-job-step job 2048))
            (cl-letf (((symbol-function 'mevedel-view--promote-face-to-font-lock-face)
                       (lambda (_text) (setq entered t) (throw 'other-exit :aborted))))
              (should (eq :aborted
                          (catch 'other-exit
                            (mevedel-view--markdown-fontify-job-result job)))))
            (should entered)
            (should-not (buffer-live-p buffer))
            (should (mevedel-view--markdown-fontify-job-cancelled job)))
        (mevedel-view--markdown-fontify-job-cancel job)))))

(defun mevedel-interrupt-test--deferred-lifecycle (stage)
  "Deliver input inside a deferred tool ownership commit at STAGE."
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf "(:name \"Unknown\" :args nil)\nTool body.\n" '(tool . "lifecycle"))
    (with-current-buffer view-buf
      (let* ((mevedel-view-prepare-enabled t)
             (mevedel-view-prepare--min-size 1)
             (unread-command-events nil)
             (end (with-current-buffer data-buf (point-max)))
             (key (mevedel-view--tool-cache-key data-buf 1 end t))
             (seam (pcase stage
                     ('admit 'add-hook) ('timer 'run-at-time) (_ 'remove-hook)))
             (original (symbol-function seam))
             injected job hook timer)
        (unwind-protect
            (progn
              (when (eq stage 'retire)
                (mevedel-view-prepare-get data-buf 1 end key)
                (setq job (car mevedel-view-prepare--jobs)
                      hook (mevedel-view-prepare--job-kill-hook job))
                (cancel-timer mevedel-view-prepare--timer)
                (setq mevedel-view-prepare--timer nil))
              (cl-letf (((symbol-function seam)
                         (lambda (&rest args)
                           (let ((result (apply original args)))
                             (when (and (not injected)
                                        (if (eq stage 'timer)
                                            (eq (nth 2 args) #'mevedel-view-prepare--advance)
                                          (and (eq (current-buffer) data-buf)
                                               (eq (car args) 'kill-buffer-hook))))
                               (should inhibit-quit)
                               (setq injected t unread-command-events '(?x)
                                     quit-flag throw-on-input)
                               (when (eq stage 'timer) (setq timer result))
                               ;; The commit must finish before this pending
                               ;; evaluator quit is permitted to unwind it.
                               (eval '(identity nil)))
                             result))))
                (should (eq t (while-no-input
                                (if (eq stage 'retire)
                                    (mevedel-view-prepare--retire job)
                                  (mevedel-view-prepare-get data-buf 1 end key))))))
              (should injected)
              (should-not quit-flag)
              (setq unread-command-events nil)
              (if (eq stage 'retire)
                  (progn
                    (should-not mevedel-view-prepare--jobs)
                    (should-not (memq hook (buffer-local-value 'kill-buffer-hook data-buf))))
                (should (= 1 (length mevedel-view-prepare--jobs)))
                (setq job (car mevedel-view-prepare--jobs)
                      hook (mevedel-view-prepare--job-kill-hook job))
                (should (= 1 (cl-count hook (buffer-local-value 'kill-buffer-hook data-buf))))
                (when (eq stage 'timer)
                  (should (eq timer mevedel-view-prepare--timer))
                  (should (memq timer timer-list)))
                ;; Retrying admission must find the same job, not duplicate its
                ;; source hook or replace an already owned callback.
                (mevedel-view-prepare-get data-buf 1 end key)
                (should (equal (list job) mevedel-view-prepare--jobs))
                (should (= 1 (cl-count hook (buffer-local-value 'kill-buffer-hook data-buf)))))
              (setq timer mevedel-view-prepare--timer)
              (mevedel-view-prepare-cancel)
              (should-not mevedel-view-prepare--jobs)
              (should-not mevedel-view-prepare--timer)
              (should-not (memq timer timer-list))
              (should-not (memq hook (buffer-local-value 'kill-buffer-hook data-buf))))
          (setq unread-command-events nil)
          (mevedel-view-prepare-cancel)
          (when (timerp timer) (cancel-timer timer)))))))

(mevedel-deftest mevedel-view-prepare-get/input-lifecycle ()
  ,test
  (test)
  :doc "input cannot strand a source kill hook before deferred job admission"
  (mevedel-interrupt-test--deferred-lifecycle 'admit)
  :doc "input cannot strand a deferred preparation timer before ownership transfers"
  (mevedel-interrupt-test--deferred-lifecycle 'timer)
  :doc "input cannot split deferred job retirement from source hook cleanup"
  (mevedel-interrupt-test--deferred-lifecycle 'retire))

(provide 'test-mevedel-view-render-interrupt)
;;; test-mevedel-view-render-interrupt.el ends here
