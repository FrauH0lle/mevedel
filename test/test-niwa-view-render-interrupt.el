;;; test-niwa-view-render-interrupt.el --- Input-aborted preparation -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise real history batches across input aborts, not replacement plan jobs.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-render)
(require 'niwa-view-prepare)

(defvar niwa-interrupt-test--ids nil
  "Generated turn identities assigned canonical ordinals during one snapshot.")

(defun niwa-interrupt-test--value (value)
  "Freeze marker coordinates in VALUE, normalizing only runtime identities."
  (cond
   ((and (symbolp value)
         (string-match-p "\\`niwa-view-turn-[0-9]+\\'" (symbol-name value)))
    (or (gethash value niwa-interrupt-test--ids)
        (puthash value (list :turn-id (hash-table-count niwa-interrupt-test--ids))
                 niwa-interrupt-test--ids)))
   ((markerp value)
    (list :marker (marker-buffer value) (marker-position value)
          (marker-insertion-type value)))
   ((overlayp value)
    (list :overlay (overlay-buffer value) (overlay-start value) (overlay-end value)))
   ((and (functionp value) (not (symbolp value))) :callback)
   ((consp value)
    (cons (niwa-interrupt-test--value (car value))
          (niwa-interrupt-test--value (cdr value))))
   ((hash-table-p value)
    (let (entries)
      (maphash (lambda (key entry)
                 (push (cons (niwa-interrupt-test--value key)
                             (niwa-interrupt-test--value entry)) entries)) value)
      (sort entries (lambda (a b) (string< (prin1-to-string (car a))
                                         (prin1-to-string (car b)))))))
   ((vectorp value) (vconcat (mapcar #'niwa-interrupt-test--value value)))
   (t value)))

(defun niwa-interrupt-test--snapshot ()
  "Copy all rendered properties and frozen source/view coordinates."
  (let ((text (buffer-string)) (pos 0)
        (niwa-interrupt-test--ids (make-hash-table :test #'eq)))
    (while (< pos (length text))
      (let ((end (next-property-change pos text (length text))) props)
        (cl-loop for (key value) on (text-properties-at pos text) by #'cddr
                 do (push (cons key (niwa-interrupt-test--value value)) props))
        (setq props (sort props (lambda (a b) (string< (symbol-name (car a))
                                                     (symbol-name (car b))))))
        (set-text-properties pos end
                             (cl-loop for (key . value) in props append (list key value))
                             text)
        (setq pos end)))
    text))

(defun niwa-interrupt-test--input ()
  "Deliver input through the evaluator's actual `while-no-input' quit path."
  (should throw-on-input)
  (should-not inhibit-quit)
  (setq unread-command-events '(?x)
        quit-flag throw-on-input)
  ;; A pending event alone does not interrupt Lisp already inside the macro.
  ;; The evaluator observes the same quit flag Emacs sets on arriving input.
  (eval '(identity nil)))

(defun niwa-interrupt-test--drain (view)
  "Finish VIEW's actual batch with a bounded number of callbacks."
  (cl-loop repeat 300 while niwa-view-render--batch do
           (niwa-view-render--batch-step view niwa-view-render--batch))
  (should-not niwa-view-render--batch))

(defun niwa-interrupt-test--advance (view predicate)
  "Advance VIEW's batch until PREDICATE accepts the job."
  (cl-loop repeat 200
           until (and niwa-view-render--batch
                      (funcall predicate niwa-view-render--batch))
           do (should niwa-view-render--batch)
           (niwa-view-render--batch-step view niwa-view-render--batch))
  (should (funcall predicate niwa-view-render--batch)))

(defun niwa-interrupt-test--reader-state ()
  "Return the transcript reader's viewport and active selection."
  (list (window-start) (window-point) (point) (mark t) mark-active))

(defun niwa-interrupt-test--check-draft ()
  "Check the displayed composer draft, caret and active mark."
  (let ((window (niwa-view-composer-window))
        (start (niwa-view-composer-draft-start)))
    (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
    (should (= (window-point window) (+ start 4)))
    (with-current-buffer (niwa-view-composer-buffer)
      (should (= (point) (+ start 4)))
      (should (= (mark) (+ start 2)))
      (should mark-active))))

(defmacro niwa-interrupt-test--with-history (&rest body)
  "Run BODY with a long single-line response, real tools and a draft."
  (declare (indent 0))
  `(niwa-view-test--with-buffers
     (niwa-view-test--insert-data data-buf "Prompt\n" nil)
     (niwa-view-test--insert-data
      data-buf (concat ";; long response " (make-string 8192 ?x) "\n") 'response)
     (dotimes (index 3)
       (niwa-view-test--insert-data
        data-buf
        (format "(:name \"Unknown\" :args nil)\nTool result %d.\n" index)
        (cons 'tool (format "interrupt-%d" index))))
     (niwa-view-test--insert-data data-buf "Done.\n" 'response)
     (with-current-buffer view-buf
       ;; Grammar availability is not the subject here.  Keep the real private
       ;; Markdown jobs and font-lock engine, using an installed built-in mode.
       (cl-letf (((symbol-function 'niwa-view--markdown-fontify-mode)
                  (lambda () 'emacs-lisp-mode))
                 ((symbol-function 'niwa-view--markdown-fontify-setup)
                  (lambda (buffer _mode)
                    (with-current-buffer buffer
                      (emacs-lisp-mode)
                      (buffer-disable-undo)))))
         (niwa-view--full-rerender)
         (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
         (save-window-excursion
           (set-window-buffer (selected-window) view-buf)
           (let ((dock (niwa-view-composer-display (selected-window)))
                 (start (niwa-view-composer-draft-start)))
             (set-window-point dock (+ start 4))
             (with-current-buffer (niwa-view-composer-buffer)
               (set-mark (+ start 2))
               (setq mark-active t)))
           (goto-char (point-min))
           (search-forward "Prompt")
           (set-mark (- (point) (length "Prompt")))
           (setq mark-active t)
           (set-window-start nil (line-beginning-position) t)
           ,@body)))))

(defun niwa-interrupt-test--source-edit-window-control ()
  "Return before/after reader state for an uninterrupted source-edit batch."
  (niwa-interrupt-test--with-history
    (let ((unread-command-events nil) before)
      (niwa-view-render-batched-full)
      (niwa-interrupt-test--drain view-buf)
      (clrhash niwa-view--response-fontify-cache)
      (setq niwa-view--response-cache-entries 0)
      (niwa-view-render-batched-full)
      (niwa-interrupt-test--advance
       view-buf (lambda (job) (plist-get job :fontify)))
      (setq before (niwa-interrupt-test--reader-state))
      (niwa-view-test--insert-data data-buf "Changed source after input.\n" 'response)
      (niwa-interrupt-test--drain view-buf)
      (list before (niwa-interrupt-test--reader-state)))))

(defun niwa-interrupt-test--visit-fontification ()
  "Visit each buffer with a registered JIT consumer before comparing output."
  (when jit-lock-functions
    (jit-lock-fontify-now (point-min) (point-max)))
  (with-current-buffer (niwa-view-composer-buffer)
    (should jit-lock-functions)
    (jit-lock-fontify-now (point-min) (point-max))))

(defun niwa-interrupt-test--exercise (stage &optional scan-phase extraction mutation)
  "Interrupt STAGE and SCAN-PHASE, or EXTRACTION, then resume with MUTATION."
  (niwa-interrupt-test--with-history
    (let* ((unread-command-events nil)
          (private-buffers nil)
          (start (symbol-function 'niwa-view--markdown-fontify-job-start))
          (scan-helper (if (eq stage 'group) 'niwa-view--group-into-turns
                         (pcase scan-phase
                           ('controls 're-search-forward)
                           ('raw 'niwa-transcript--next-raw-range)
                           ('finish 'niwa-transcript--finish-segments)
                           (_ 'niwa-transcript--classify-gptel-prop))))
          (scan (symbol-function scan-helper))
          (fontify (symbol-function 'font-lock-ensure))
          (promote (symbol-function 'niwa-view--promote-face-to-font-lock-face))
          (tool (symbol-function 'niwa-view--tool-segment-entry))
          expected before source source-tick job font-job font-buffer reader-state
          (mutation-window (and mutation (niwa-interrupt-test--source-edit-window-control)))
          group-state group-remaining
          (injected 0) (scan-calls 0) (calls nil) armed)
      (when (eq scan-phase 'controls)
        (dotimes (index 3)
          (niwa-view-test--insert-data
           data-buf (niwa--format-hook-audit-record
                     (list :type 'tool-hook :phase 'post-tool
                           :message (format "Control candidate %d" index)))
           'niwa-hook-audit)))
      (when (eq stage 'group)
        ;; Keep a multi-segment assistant turn across the first group chunk.
        ;; The following user turn flushes it in the interrupted chunk, so a
        ;; destructive reverse would mutate the earlier retained state.
        (dotimes (index 30)
          (niwa-view-test--insert-data
           data-buf (format "(:name \"Unknown\" :args nil)\nGroup result %d.\n" index)
           (cons 'tool (format "group-%d" index))))
        (dotimes (index 36)
          (niwa-view-test--insert-data data-buf (format "Prompt %d\n" index) nil)
          (niwa-view-test--insert-data data-buf (format "Response %d\n" index) 'response)))
      (cl-letf (((symbol-function 'niwa-view--markdown-fontify-job-start)
                 (lambda (text)
                   (let ((font-job (funcall start text)))
                     (push (niwa-view--markdown-fontify-job-buffer font-job)
                           private-buffers)
                     font-job)))
                ((symbol-function 'niwa-view-render--full-now)
                 (lambda () (ert-fail "Input took synchronous fallback"))))
        ;; Establish the control through the same public batch entry point.
        (clrhash niwa-view--response-fontify-cache)
        (setq niwa-view--response-cache-entries 0)
        (niwa-view-render-batched-full)
        (niwa-interrupt-test--drain view-buf)
        ;; Input can give jit-lock an opportunity to visit the old display.
        ;; Compare both completed projections after the same real consumer visit;
        ;; the snapshot itself never modifies the view or strips cache flags.
        (niwa-interrupt-test--visit-fontification)
        (setq expected (niwa-interrupt-test--snapshot))
        (clrhash niwa-view--response-fontify-cache)
        (setq niwa-view--response-cache-entries 0)
        (niwa-view-render-batched-full)
        (niwa-interrupt-test--advance
         view-buf
         (lambda (current)
           (and (eq stage (plist-get current :prepare-stage))
                (or (not (eq stage 'group)) (plist-get current :group-state))
                (or (not scan-phase)
                    (eq scan-phase (niwa-transcript--scan-phase
                                    (plist-get current :scan))))
                (or (not (eq stage 'fontify)) (plist-get current :fontify)))))
        (setq job niwa-view-render--batch
              font-job (plist-get job :fontify)
              font-buffer (and font-job
                               (niwa-view--markdown-fontify-job-buffer font-job))
              before (buffer-string)
              source (with-current-buffer data-buf (buffer-string))
              source-tick (with-current-buffer data-buf (buffer-modified-tick))
              reader-state (niwa-interrupt-test--reader-state)
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
                         (with-current-buffer view-buf (should niwa-view-render--owner))
                         (setq armed nil)
                         (cl-incf injected)
                         (niwa-interrupt-test--input))))))
                  ((symbol-function 'font-lock-ensure)
                   (lambda (&rest args)
                     (prog1 (apply fontify args)
                       (when (and armed (eq stage 'fontify) (not extraction)
                                  (eq (current-buffer) font-buffer))
                         (with-current-buffer view-buf (should niwa-view-render--owner))
                         (should (> (- (cadr args) (car args)) 8192))
                         (setq armed nil)
                         (cl-incf injected)
                         (niwa-interrupt-test--input)))))
                  ((symbol-function 'niwa-view--promote-face-to-font-lock-face)
                   (lambda (text)
                     (prog1 (funcall promote text)
                       (when (and armed extraction)
                         (with-current-buffer view-buf (should niwa-view-render--owner))
                         (setq armed nil)
                         (cl-incf injected)
                         (niwa-interrupt-test--input)))))
                  ((symbol-function 'niwa-view--tool-segment-entry)
                   (lambda (segment data)
                     (when (and (eq 'prepare (plist-get job :phase))
                                (eq 'tools (plist-get job :prepare-stage)))
                       (push (cadr segment) calls)
                       (when (and armed (eq stage 'tools) (= (length calls) 2))
                         (should niwa-view-render--owner)
                         (setq armed nil)
                         (cl-incf injected)
                         (niwa-interrupt-test--input)))
                     (funcall tool segment data))))
          (niwa-view-render--batch-step view-buf job)
          (should (= injected 1))
          (should (equal unread-command-events '(?x)))
          (should-not quit-flag)
          (should (eq job niwa-view-render--batch))
          (should (eq 'prepare (plist-get job :phase)))
          (when (eq stage 'group)
            (should (equal group-state (plist-get job :group-state)))
            (should (eq group-remaining (plist-get job :remaining))))
          (should (zerop (hash-table-count (plist-get job :cache-used))))
          (should (memq (plist-get job :timer) timer-list))
          (should-not niwa-view-render--owner)
          (should-not niwa-view-render--pending)
          (should (equal-including-properties before (buffer-string)))
          (should (equal-including-properties
                   source (with-current-buffer data-buf (buffer-string))))
          (should (= source-tick (with-current-buffer data-buf (buffer-modified-tick))))
          (when font-job
            (should (buffer-live-p font-buffer))
            (should-not (niwa-view--markdown-fontify-job-cancelled font-job))
            (if extraction
                (should (niwa-view--markdown-fontify-job-done font-job))
              (should (= 1 (niwa-view--markdown-fontify-job-next font-job))))
            (should (zerop (hash-table-count niwa-view--response-fontify-cache))))
          ;; Continuous input must keep the old display, plan, and resources.
          (dotimes (_ 3)
            (niwa-view-render--batch-step view-buf job)
            (should (eq job niwa-view-render--batch))
            (should (eq stage (plist-get job :prepare-stage)))
            (should (equal-including-properties before (buffer-string)))
            (niwa-interrupt-test--check-draft)
            (should (equal reader-state (niwa-interrupt-test--reader-state)))
            (should-not niwa-view-render--owner))
          (setq unread-command-events nil)
          (when mutation
            (niwa-view-test--insert-data data-buf "Changed source after input.\n" 'response)
            (niwa-view-render--batch-step view-buf job)
            (should-not (eq job niwa-view-render--batch))
            (should (niwa-view--markdown-fontify-job-cancelled font-job))
            (should-not (buffer-live-p font-buffer)))
          (niwa-interrupt-test--drain view-buf))
        (when (eq stage 'tools)
          ;; First entry commits once; only the interrupted second entry repeats.
          (should (= (length calls) 4))
          (should (= 1 (cl-count (car (last calls)) calls)))
          (should (> (hash-table-count niwa-view--tool-rendering-cache) 0)))
        (when mutation (should (equal reader-state (car mutation-window))))
        (should (equal (if mutation (cadr mutation-window) reader-state)
                       (niwa-interrupt-test--reader-state)))
        (if mutation
            (progn
              (should (string-search "Changed source after input." (buffer-string)))
              (niwa-interrupt-test--visit-fontification)
              (setq expected (niwa-interrupt-test--snapshot))
              (niwa-view-render-batched-full)
              (niwa-interrupt-test--drain view-buf)))
        (niwa-interrupt-test--visit-fontification)
        (should (equal-including-properties expected (niwa-interrupt-test--snapshot)))
        (niwa-interrupt-test--check-draft)
        (dolist (buffer private-buffers) (should-not (buffer-live-p buffer)))))))

(niwa-deftest niwa-view-render-batched-full/fontified-control ()
  ,test
  (test)
  :doc "uninterrupted cold and warm batches converge after the same jit-lock visit"
  (niwa-interrupt-test--with-history
    (niwa-view-test--insert-data data-buf "Changed source after input.\n" 'response)
    (let (cold warm)
      (clrhash niwa-view--response-fontify-cache)
      (setq niwa-view--response-cache-entries 0)
      (niwa-view-render-batched-full)
      (niwa-interrupt-test--drain view-buf)
      (niwa-interrupt-test--visit-fontification)
      (setq cold (niwa-interrupt-test--snapshot))
      (niwa-view-render-batched-full)
      (niwa-interrupt-test--drain view-buf)
      (niwa-interrupt-test--visit-fontification)
      (setq warm (niwa-interrupt-test--snapshot))
      (should (equal-including-properties cold warm)))))

(niwa-deftest niwa-view-render--batch-step/input-interrupt ()
  ,test
  (test)
  :doc "input aborts canonical scanning without fallback and resumes the real batch"
  (dolist (phase '(properties controls raw finish))
    (niwa-interrupt-test--exercise 'scan phase))
  :doc "input aborts grouping after an earlier chunk without corrupting retained turns"
  (niwa-interrupt-test--exercise 'group)
  :doc "input aborts a huge single-line font-lock chunk without cancelling its job"
  (niwa-interrupt-test--exercise 'fontify)
  :doc "input aborts fontified output extraction without destroying its retry input"
  (niwa-interrupt-test--exercise 'fontify nil t)
  :doc "a source edit retires the input-aborted private job before publishing new text"
  (niwa-interrupt-test--exercise 'fontify nil nil t)
  :doc "input aborts tool preparation and retries only the uncommitted entry"
  (niwa-interrupt-test--exercise 'tools))

(defun niwa-interrupt-test--quit (extraction)
  "Quit a real fontification chunk or EXTRACTION without retry or fallback."
  (niwa-interrupt-test--with-history
    (let ((unread-command-events nil)
          (quit-flag nil))
      (clrhash niwa-view--response-fontify-cache)
      (setq niwa-view--response-cache-entries 0)
      (niwa-view-render-batched-full)
      (niwa-interrupt-test--advance
       view-buf (lambda (job) (plist-get job :fontify)))
      (let* ((job niwa-view-render--batch)
             (font-job (plist-get job :fontify))
             (font-buffer (niwa-view--markdown-fontify-job-buffer font-job))
             (timer (plist-get job :timer))
             (before (buffer-string))
             (source (with-current-buffer data-buf (buffer-string)))
             (source-tick (with-current-buffer data-buf (buffer-modified-tick)))
             (reader-before (niwa-interrupt-test--reader-state))
             (seam (if extraction 'niwa-view--promote-face-to-font-lock-face
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
                             (with-current-buffer view-buf (should niwa-view-render--owner))
                             (setq injected t quit-flag t)
                             (eval '(identity nil))))))
                      ((symbol-function 'niwa-view-render--full-now)
                       (lambda () (ert-fail "Quit took synchronous fallback")))
                      ((symbol-function 'niwa-view--sweep-tool-rendering-cache)
                       (lambda (&rest _) (ert-fail "Quit swept projection caches"))))
              ;; C-g may propagate to the command boundary, unlike ordinary
              ;; pending input.  Observe its owned cleanup before continuing.
              (condition-case nil
                  (niwa-view-render--batch-step view-buf job)
                (quit (setq quit-signaled t quit-flag nil)))
              (should injected)
              (should quit-signaled)
              (should-not niwa-view-render--batch)
              (should-not (plist-get job :timer))
              (should-not (memq timer timer-list))
              (should-not (buffer-live-p font-buffer))
              (should (niwa-view--markdown-fontify-job-cancelled font-job))
              (should (zerop (hash-table-count niwa-view--response-fontify-cache)))
              (should-not niwa-view-render--owner)
              (should-not niwa-view-render--pending)
              (should (equal-including-properties before (buffer-string)))
              (should (equal-including-properties
                       source (with-current-buffer data-buf (buffer-string))))
              (should (= source-tick (with-current-buffer data-buf (buffer-modified-tick))))
              (niwa-interrupt-test--check-draft)
              (should (equal reader-before (niwa-interrupt-test--reader-state)))
              ;; An already dispatched callback from the cancelled plan cannot
              ;; resurrect its cancelled Markdown job or publish old output.
              (niwa-view-render--batch-step view-buf job)
              (should-not niwa-view-render--batch)
              (should-not (plist-get job :timer))
              (should (equal-including-properties before (buffer-string)))
              (niwa-interrupt-test--check-draft)
              (should (equal reader-before (niwa-interrupt-test--reader-state))))
          (setq quit-flag nil))))))

(niwa-deftest niwa-view-render--batch-step/quit ()
  ,test
  (test)
  :doc "C-g during a Markdown chunk cancels rather than resuming a dead job"
  (niwa-interrupt-test--quit nil)
  :doc "C-g during extraction cancels rather than falling back synchronously"
  (niwa-interrupt-test--quit t))

(niwa-deftest niwa-view--markdown-fontify-job-result/nonlocal ()
  ,test
  (test)
  :doc "an arbitrary extraction throw still cancels and releases the private buffer"
  (cl-letf (((symbol-function 'niwa-view--markdown-fontify-mode)
             (lambda () 'emacs-lisp-mode))
            ((symbol-function 'niwa-view--markdown-fontify-setup)
             (lambda (buffer _mode)
               (with-current-buffer buffer (emacs-lisp-mode)))))
    (let* ((job (niwa-view--markdown-fontify-job-start ";; response"))
           (buffer (niwa-view--markdown-fontify-job-buffer job))
           (entered nil))
      (unwind-protect
          (progn
            (should (niwa-view--markdown-fontify-job-step job 2048))
            (cl-letf (((symbol-function 'niwa-view--promote-face-to-font-lock-face)
                       (lambda (_text) (setq entered t) (throw 'other-exit :aborted))))
              (should (eq :aborted
                          (catch 'other-exit
                            (niwa-view--markdown-fontify-job-result job)))))
            (should entered)
            (should-not (buffer-live-p buffer))
            (should (niwa-view--markdown-fontify-job-cancelled job)))
        (niwa-view--markdown-fontify-job-cancel job)))))

(defun niwa-interrupt-test--deferred-lifecycle (stage)
  "Deliver input inside a deferred tool ownership commit at STAGE."
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf "(:name \"Unknown\" :args nil)\nTool body.\n" '(tool . "lifecycle"))
    (with-current-buffer view-buf
      (let* ((niwa-view-prepare-enabled t)
             (niwa-view-prepare--min-size 1)
             (unread-command-events nil)
             (end (with-current-buffer data-buf (point-max)))
             (key (niwa-view--tool-cache-key data-buf 1 end t))
             (seam (pcase stage
                     ('admit 'add-hook) ('timer 'run-at-time) (_ 'remove-hook)))
             (original (symbol-function seam))
             injected job hook timer)
        (unwind-protect
            (progn
              (when (eq stage 'retire)
                (niwa-view-prepare-get data-buf 1 end key)
                (setq job (car niwa-view-prepare--jobs)
                      hook (niwa-view-prepare--job-kill-hook job))
                (cancel-timer niwa-view-prepare--timer)
                (setq niwa-view-prepare--timer nil))
              (cl-letf (((symbol-function seam)
                         (lambda (&rest args)
                           (let ((result (apply original args)))
                             (when (and (not injected)
                                        (if (eq stage 'timer)
                                            (eq (nth 2 args) #'niwa-view-prepare--advance)
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
                                    (niwa-view-prepare--retire job)
                                  (niwa-view-prepare-get data-buf 1 end key))))))
              (should injected)
              (should-not quit-flag)
              (setq unread-command-events nil)
              (if (eq stage 'retire)
                  (progn
                    (should-not niwa-view-prepare--jobs)
                    (should-not (memq hook (buffer-local-value 'kill-buffer-hook data-buf))))
                (should (= 1 (length niwa-view-prepare--jobs)))
                (setq job (car niwa-view-prepare--jobs)
                      hook (niwa-view-prepare--job-kill-hook job))
                (should (= 1 (cl-count hook (buffer-local-value 'kill-buffer-hook data-buf))))
                (when (eq stage 'timer)
                  (should (eq timer niwa-view-prepare--timer))
                  (should (memq timer timer-list)))
                ;; Retrying admission must find the same job, not duplicate its
                ;; source hook or replace an already owned callback.
                (niwa-view-prepare-get data-buf 1 end key)
                (should (equal (list job) niwa-view-prepare--jobs))
                (should (= 1 (cl-count hook (buffer-local-value 'kill-buffer-hook data-buf)))))
              (setq timer niwa-view-prepare--timer)
              (niwa-view-prepare-cancel)
              (should-not niwa-view-prepare--jobs)
              (should-not niwa-view-prepare--timer)
              (should-not (memq timer timer-list))
              (should-not (memq hook (buffer-local-value 'kill-buffer-hook data-buf))))
          (setq unread-command-events nil)
          (niwa-view-prepare-cancel)
          (when (timerp timer) (cancel-timer timer)))))))

(niwa-deftest niwa-view-prepare-get/input-lifecycle ()
  ,test
  (test)
  :doc "input cannot strand a source kill hook before deferred job admission"
  (niwa-interrupt-test--deferred-lifecycle 'admit)
  :doc "input cannot strand a deferred preparation timer before ownership transfers"
  (niwa-interrupt-test--deferred-lifecycle 'timer)
  :doc "input cannot split deferred job retirement from source hook cleanup"
  (niwa-interrupt-test--deferred-lifecycle 'retire))

(provide 'test-niwa-view-render-interrupt)
;;; test-niwa-view-render-interrupt.el ends here
