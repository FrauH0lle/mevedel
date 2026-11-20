;;; mevedel-docked-composer-bench.el --- Docked composer performance gate -*- lexical-binding: t; -*-

;;; Commentary:

;; Drive the inline and docked composers in separate graphical processes.
;; Run only in the private KWin launched by scripts/bench-docked-composer.py:
;; this fixture replaces frames, focus and options.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mevedel)
(require 'mevedel-chat)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)
(require 'mevedel-session-persistence)

(defvar gcs-done)
(defvar mevedel-session--read-only-mode)

(defvar mevedel-floating-composer)
(declare-function mevedel-view--input-start "mevedel-view-composer" ())
(declare-function mevedel-view-composer-buffer "mevedel-view-composer" (&optional buffer))
(declare-function mevedel-view-composer-draft-start "mevedel-view-composer" (&optional buffer))
(declare-function mevedel-view-composer-window "mevedel-view-pair" (&optional window))

(defvar mevedel-dc-bench--docked nil "Whether the measured build has a dock.")
(defvar mevedel-dc-bench--counters (list :redisplays 0 :repairs 0 :repair-ms 0.0 :fits 0)
  "Cumulative work counters.")
(defvar mevedel-dc-bench--renders nil "Timed stream renders in the current workload.")
(defvar mevedel-dc-bench--stream-count 100 "Chunks per measured streaming workload.")
(defvar mevedel-dc-bench--layout-count 6 "Samples per repeatable layout operation.")
(defvar mevedel-dc-bench--idle-seconds 4 "Seconds per idle workload.")
(defvar mevedel-dc-bench--resume nil "Fixture callback for resuming the saved session.")
(defvar mevedel-dc-bench--pending nil "Non-nil while a measured command runs.")
(defvar mevedel-dc-bench--done nil "Time a measured command finished, or nil.")
(defvar mevedel-dc-bench--records nil "Measurements, newest first.")
(defvar mevedel-dc-bench--kwin nil "Private KWin PID.")

(defun mevedel-dc-bench--cpu-ns (pids)
  "Return the CPU times of PIDS' threads in nanoseconds, as ((TASK . NS) ...)."
  (let (threads)
    (dolist (pid pids)
      (dolist (task (ignore-errors (directory-files (format "/proc/%d/task" pid) t "\\`[0-9]")))
        (with-temp-buffer
          (when (ignore-errors (insert-file-contents (file-name-concat task "schedstat")) t)
            (push (cons task (read (current-buffer))) threads)))))
    threads))

(defun mevedel-dc-bench--cpu-delta (after before)
  "Return the CPU nanoseconds AFTER's threads used since BEFORE.
Per thread, so a thread exiting in between cannot make it negative; that
thread's CPU time since BEFORE is lost."
  (cl-loop for (task . ns) in after
           sum (- ns (or (cdr (assoc task before)) 0))))

(defun mevedel-dc-bench--find-kwin ()
  "Return the PID of the KWin this Emacs runs under."
  (let ((pid (emacs-pid)))
    (while (and pid (not (equal (alist-get 'comm (process-attributes pid)) "kwin_wayland")))
      (setq pid (alist-get 'ppid (process-attributes pid)))
      (when (memq pid '(0 1)) (setq pid nil)))
    (or pid (error "Not running under a private kwin_wayland"))))

(defun mevedel-dc-bench--xwayland ()
  "Return the private KWin's Xwayland PIDs."
  (cl-loop for pid in (list-system-processes)
           for attributes = (process-attributes pid)
           when (and (eql (alist-get 'ppid attributes) mevedel-dc-bench--kwin)
                     (equal (alist-get 'comm attributes) "Xwayland"))
           collect pid))

(defun mevedel-dc-bench--snapshot (xwayland)
  "Return the current counters and CPU times, with XWAYLAND's PIDs.
The caller finds those PIDs: scanning every process would count in Emacs's
CPU time."
  (append (list :emacs-ns (mevedel-dc-bench--cpu-ns (list (emacs-pid)))
                :kwin-ns (mevedel-dc-bench--cpu-ns (list mevedel-dc-bench--kwin))
                :xwayland-ns (mevedel-dc-bench--cpu-ns xwayland)
                :gcs gcs-done :wall (float-time))
          (copy-sequence mevedel-dc-bench--counters)))

(defun mevedel-dc-bench--post-command ()
  "Finish a measured command once its timers and redisplay ran."
  (when (and mevedel-dc-bench--pending (not (minibufferp)))
    (setq mevedel-dc-bench--pending nil)
    (run-at-time 0 nil #'mevedel-dc-bench--finish 0)))

(defun mevedel-dc-bench--finish (tries)
  "Wait until no timer is due (up to TRIES rearms), redisplay, exit."
  (redisplay t)
  (if (cl-some (lambda (timer) (time-less-p (timer--time timer) nil)) timer-list)
      (when (< tries 20)
        (run-at-time 0 nil #'mevedel-dc-bench--finish (1+ tries)))
    (setq mevedel-dc-bench--done (float-time))
    (exit-recursive-edit)))

(defun mevedel-dc-bench--command (events)
  "Run EVENTS as one command; return its latency including redisplay, in ms."
  (setq mevedel-dc-bench--done nil
        mevedel-dc-bench--pending t)
  (let ((guard (run-at-time 10 nil #'exit-recursive-edit))
        (start (float-time)))
    (setq unread-command-events events)
    (unwind-protect (recursive-edit)
      (cancel-timer guard)
      (setq unread-command-events nil mevedel-dc-bench--pending nil))
    (unless mevedel-dc-bench--done (error "Command did not finish: %S" events))
    (* 1000 (- mevedel-dc-bench--done start))))

(defun mevedel-dc-bench--settle (&optional seconds)
  "Let timers run for SECONDS (default 0.03) and redisplay."
  (let ((unread-command-events nil))
    (sit-for (or seconds 0.03))
    (redisplay t)))

(defun mevedel-dc-bench--delta (before after)
  "Return the counters and CPU times from snapshot BEFORE to AFTER."
  (cl-loop for (key value) on after by #'cddr
           nconc (list key (if (listp value)
                               (mevedel-dc-bench--cpu-delta value (plist-get before key))
                             (- value (plist-get before key))))))

(defun mevedel-dc-bench--idle (seconds)
  "Stay idle for SECONDS."
  (let ((end (+ (float-time) seconds)))
    (while (< (float-time) end) (sit-for (- end (float-time)))))
  nil)

(defun mevedel-dc-bench--focus (frame)
  "Give FRAME input focus and wait until the compositor confirms it."
  (let ((tries 0))
    (while (and (not (eq t (frame-focus-state frame))) (< (cl-incf tries) 6))
      (select-frame-set-input-focus frame)
      (let ((deadline (+ (float-time) 1)))
        (while (and (not (eq t (frame-focus-state frame))) (< (float-time) deadline))
          (mevedel-dc-bench--settle 0.05)))))
  (mevedel-dc-bench--check (eq t (frame-focus-state frame))
                           (format "focus on %S; states %S" frame
                                   (mapcar (lambda (f) (cons f (frame-focus-state f)))
                                           (frame-list)))))

(defun mevedel-dc-bench--fresh-frame ()
  "Select a newly mapped, focused 100x45 frame; delete every other frame."
  (let ((frame (make-frame '((width . 100) (height . 45)))))
    (select-frame frame)
    (mevedel-dc-bench--focus frame)
    (dolist (other (frame-list))
      (unless (or (eq other frame) (frame-parent other)) (delete-frame other)))
    (let ((deadline (+ (float-time) 5)))
      (while (and (/= (frame-width) 100) (< (float-time) deadline))
        (mevedel-dc-bench--settle 0.05)))
    (mevedel-dc-bench--check (and (= (frame-width) 100) (eq t (frame-focus-state frame)))
                             "a focused 100-column frame")))

(defun mevedel-dc-bench--check (condition what)
  "Fail unless CONDITION holds, explaining WHAT was expected."
  (unless condition (error "Expected %s; selected %S" what (window-buffer))))

(defun mevedel-dc-bench--instrument ()
  "Count redisplay, pair repair, and fit work; time repair and stream renders."
  (add-function :before pre-redisplay-function
                (lambda (_) (cl-incf (plist-get mevedel-dc-bench--counters :redisplays))))
  (when mevedel-dc-bench--docked
    (advice-add 'mevedel-view-pair--repair :around
                (lambda (function &rest args)
                  (let ((start (float-time)))
                    (prog1 (apply function args)
                      (cl-incf (plist-get mevedel-dc-bench--counters :repairs))
                      (cl-incf (plist-get mevedel-dc-bench--counters :repair-ms)
                               (* 1000 (- (float-time) start)))))))
    (advice-add 'mevedel-composer--fit :before
                (lambda (&rest _)
                  (cl-incf (plist-get mevedel-dc-bench--counters :fits)))))
  (advice-add 'mevedel-view--render-stream-update :around
              (lambda (function &rest args)
                (let ((start (float-time)))
                  (prog1 (apply function args)
                    (push (* 1000 (- (float-time) start)) mevedel-dc-bench--renders))))))

(defun mevedel-dc-bench--draft-start ()
  "Return the draft start in the current editing buffer."
  (if mevedel-dc-bench--docked
      (mevedel-view-composer-draft-start)
    (mevedel-view--input-start)))

(defun mevedel-dc-bench--windows (view count)
  "Check that VIEW has COUNT transcripts and, on the candidate, COUNT docks."
  (let ((views (get-buffer-window-list view nil (selected-frame))))
    (mevedel-dc-bench--check (= count (length views)) (format "%d transcripts" count))
    (when mevedel-dc-bench--docked
      (let ((docks (get-buffer-window-list (mevedel-view-composer-buffer view)
                                          nil (selected-frame))))
        (mevedel-dc-bench--check (= count (length docks)) (format "%d docks" count))
        (dolist (window views)
          (let ((dock (mevedel-view-composer-window window)))
            (mevedel-dc-bench--check
             (and (memq dock docks) (eq dock (window-next-sibling window)))
             "a paired dock immediately below each transcript")
            (setq docks (delq dock docks))))))))

(defun mevedel-dc-bench--with-view (function)
  "Call FUNCTION with the data and view buffers of a displayed chat."
  ;; The private runner deletes this root after reaping journal workers.
  (let* ((root (file-name-as-directory
                (make-temp-file (file-name-concat (getenv "XDG_STATE_HOME")
                                                  "mevedel-bench-") t)))
         (mevedel-user-dir (file-name-concat root "user/"))
         (gptel-display-buffer-action
          '(display-buffer-same-window (body-function . select-window)))
         (workspace (mevedel-workspace--create :type 'project :id root :name "bench" :root root))
         (session (mevedel-session-create "bench" workspace root))
         (data (generate-new-buffer "*Benchmark data*"))
         (view (generate-new-buffer "*Benchmark view*")))
    (unwind-protect
        (progn
          (setf (mevedel-session-preset-name session) 'mevedel-implement)
          (with-current-buffer data
            (mevedel--transcript-org-mode)
            (setq-local mevedel--session session mevedel--workspace workspace
                        mevedel-session--read-only-mode nil
                        gptel-response-separator "\n\n" default-directory root)
            (dotimes (i 300)
              (let ((start (point)))
                (insert (format "Line %d of the earlier response, with **bold prose**.\n" i))
                (put-text-property start (point) 'gptel 'response))))
          (mevedel-session-set-root-buffer session data)
          (mevedel-view--setup view data)
          (with-current-buffer view (mevedel-view--full-rerender))
          (mevedel--display-chat-buffer data)
          (mevedel-dc-bench--settle 0.2)
          (mevedel-dc-bench--windows view 1)
          (mevedel-dc-bench--check
           (eq (window-buffer) (if mevedel-dc-bench--docked
                                   (mevedel-view-composer-buffer view) view))
           "focus in the displayed draft")
          (goto-char (point-max))
          (unless mevedel-dc-bench--docked (recenter -1))
          (mevedel-dc-bench--settle)
          (funcall function data view))
      (when (buffer-live-p data) (mevedel-chat-discard-buffers data t))
      (when (buffer-live-p view) (kill-buffer view)))))

(defun mevedel-dc-bench--measure (scenario function)
  "Record SCENARIO, where FUNCTION returns latency samples."
  (let* ((xwayland (mevedel-dc-bench--xwayland))
         (_ (progn (garbage-collect) (mevedel-dc-bench--settle)))
         (_ (mevedel-dc-bench--check (eq t (frame-focus-state)) "a focused benchmark frame"))
         (before (mevedel-dc-bench--snapshot xwayland))
         (samples (funcall function))
         (after (mevedel-dc-bench--snapshot xwayland)))
    (push (append (list :scenario scenario :samples-ms (vconcat samples))
                  (mevedel-dc-bench--delta before after))
          mevedel-dc-bench--records)))

(defun mevedel-dc-bench--repeat (n events)
  "Run N commands EVENTS with typing-like pacing."
  (cl-loop repeat n collect (prog1 (mevedel-dc-bench--command events)
                              (mevedel-dc-bench--settle))))

(defun mevedel-dc-bench--stream (data view)
  "Send paced chunks through DATA's stream hook and check they reach VIEW."
  (let ((n 0) timer (deadline (+ (float-time) 15)) (mevedel-dc-bench--renders nil))
    (unwind-protect
        (progn
          (setq timer
                (run-at-time
                 0 0.04
                 (lambda ()
                   (with-current-buffer data
                     (goto-char (point-max))
                     (let ((start (point)))
                       (insert (format "streamtoken%d " n)
                               (if (zerop (% n 8)) "and a line break.\n" ""))
                       (put-text-property start (point) 'gptel 'response))
                     (mevedel-view-stream-schedule))
                   (when (= (cl-incf n) mevedel-dc-bench--stream-count)
                     (cancel-timer timer)))))
          (while (and (< n mevedel-dc-bench--stream-count) (< (float-time) deadline))
            (mevedel-dc-bench--settle 0.05))
          (mevedel-dc-bench--check (= n mevedel-dc-bench--stream-count) "every stream chunk delivered")
          (mevedel-dc-bench--settle (+ mevedel-view-stream-render-delay 0.2)))
      (when timer (cancel-timer timer)))
    (with-current-buffer view
      (mevedel-dc-bench--check
       (save-excursion (goto-char (point-min)) (search-forward (format "streamtoken%d" (1- n)) nil t))
       (format "the final streamed token after %d renders; tail: %S"
               (length mevedel-dc-bench--renders)
               (buffer-substring-no-properties (max (point-min) (- (point-max) 350)) (point-max)))))
    (nreverse mevedel-dc-bench--renders)))

(defun mevedel-dc-bench--scroll (view)
  "Page both directions in VIEW; verify every page moves the viewport."
  (select-window (get-buffer-window view))
  (set-window-start nil (point-min))
  (goto-char (point-min))
  (mevedel-dc-bench--settle)
  (cl-loop for i below 12
           for keys = (if (zerop (% i 2)) "C-v" "M-v")
           for start = (window-start)
           for ms = (mevedel-dc-bench--command (listify-key-sequence (kbd keys)))
           collect ms into samples
           do (mevedel-dc-bench--check (/= start (window-start)) "a changed transcript viewport")
           do (mevedel-dc-bench--settle)
           finally return samples))

(defun mevedel-dc-bench--popup ()
  "Open and select a Help-like side window outside the composer pair."
  (interactive)
  (select-window (display-buffer-in-side-window (get-buffer "*Benchmark popup*")
                                               '((side . right) (window-width . 24)))))

(defun mevedel-dc-bench--resume-command ()
  "Run the benchmark's saved-session resume action."
  (interactive)
  (funcall mevedel-dc-bench--resume))

(defun mevedel-dc-bench--layout (data view)
  "Measure real layout commands and session resume for DATA and VIEW."
  (let* ((scratch (get-buffer-create "*scratch*"))
         (popup (get-buffer-create "*Benchmark popup*"))
         (transcript (get-buffer-window view))
         (editor (or (and mevedel-dc-bench--docked (mevedel-view-composer-window transcript))
                     transcript)))
    (with-current-buffer popup (special-mode))
    (dotimes (_ mevedel-dc-bench--layout-count)
      (select-window editor)
      (mevedel-dc-bench--measure
       "split"
       (lambda ()
         (let ((ms (mevedel-dc-bench--command (listify-key-sequence (kbd "C-x 3")))))
           (mevedel-dc-bench--windows view 2)
           (list ms))))
      (mevedel-dc-bench--command (listify-key-sequence (kbd "C-x 1")))
      (mevedel-dc-bench--settle)
      (mevedel-dc-bench--windows view 1))
    (dotimes (_ mevedel-dc-bench--layout-count)
      (select-window (if mevedel-dc-bench--docked (mevedel-view-composer-window transcript) transcript))
      (dolist (target (list scratch view))
        (mevedel-dc-bench--measure
         (if (eq target scratch) "switch-away" "switch-back")
         (lambda ()
           (let ((ms (mevedel-dc-bench--command (append (kbd "C-x b") (buffer-name target) '(?\r)))))
             (mevedel-dc-bench--check (eq target (window-buffer)) "the switched-to buffer")
             (mevedel-dc-bench--windows view (if (eq target scratch) 0 1))
             (list ms))))))
    (dotimes (_ mevedel-dc-bench--layout-count)
      (select-window (if mevedel-dc-bench--docked (mevedel-view-composer-window transcript) transcript))
      (mevedel-dc-bench--measure
       "popup-open"
       (lambda ()
         (let ((ms (mevedel-dc-bench--command '(f9))))
           (mevedel-dc-bench--check
            (and (eq popup (window-buffer)) (eq 'right (window-parameter nil 'window-side)))
            "a popup outside the pair")
           (mevedel-dc-bench--windows view 1)
           (list ms))))
      (mevedel-dc-bench--measure
       "popup-close"
       (lambda ()
         (let ((ms (mevedel-dc-bench--command '(?q))))
           (mevedel-dc-bench--check (not (get-buffer-window popup)) "the popup closed")
           (mevedel-dc-bench--windows view 1)
           (list ms)))))
    (let* ((session (buffer-local-value 'mevedel--session data))
           (workspace (buffer-local-value 'mevedel--workspace data))
           (id (mevedel-session-session-id session))
           reopened
           (mevedel-dc-bench--resume
            (lambda ()
              (setq reopened (mevedel-session-persistence-resume-id workspace id))
              (mevedel--display-chat-buffer reopened))))
      (mevedel-dc-bench--check (mevedel-session-save-path session) "a saved session")
      (mevedel-chat-discard-buffers data t)
      (mevedel-dc-bench--settle)
      (unwind-protect
          (mevedel-dc-bench--measure
           "resume"
           (lambda ()
             (let* ((ms (mevedel-dc-bench--command '(f10)))
                    (restored (buffer-local-value 'mevedel--session reopened)))
               (mevedel-dc-bench--check (and (not (eq restored session))
                                            (equal id (mevedel-session-session-id restored)))
                                        "the saved session restored in a new buffer")
               (mevedel-dc-bench--windows (buffer-local-value 'mevedel--view-buffer reopened) 1)
               (list ms))))
        (when (buffer-live-p reopened) (mevedel-chat-discard-buffers reopened t))))))

(defun mevedel-dc-bench--workloads ()
  "Run a complete fresh-fixture workload set."
  (mevedel-dc-bench--fresh-frame)
  (mevedel-dc-bench--with-view
   (lambda (data view)
     (let ((start (mevedel-dc-bench--draft-start)))
       (mevedel-dc-bench--measure "typing" (lambda () (mevedel-dc-bench--repeat 40 '(?a))))
       (mevedel-dc-bench--check (equal (make-string 40 ?a) (buffer-substring-no-properties start (point-max)))
                                "the forty typed characters")
       ;; This exceeds the dock cap and traverses a soft-wrap boundary during deletion.
       (insert (mapconcat (lambda (i) (format " word%d" i)) (number-sequence 1 220) ""))
       (mevedel-dc-bench--settle 0.2)
       (let ((row (save-excursion (vertical-motion 0) (point))))
         (mevedel-dc-bench--check (> row (+ start 40)) "a wrapped draft")
         (when (> (- (point-max) row) 20)
           (delete-region (+ row 20) (point-max))
           (mevedel-dc-bench--settle))
         (let ((end (point-max)))
           (mevedel-dc-bench--measure "wrapped-deletion" (lambda () (mevedel-dc-bench--repeat 40 '(127))))
           (mevedel-dc-bench--check (= (- end 40) (point-max)) "forty deletions")))
       (delete-region (+ start 40) (point-max))
       (mevedel-dc-bench--settle 0.2))
     (let ((window (get-buffer-window view)))
       (with-selected-window window
         (goto-char (point-max))
         (recenter -1))
       (mevedel-dc-bench--settle)
       (mevedel-dc-bench--check (pos-visible-in-window-p (with-current-buffer view (point-max)) window)
                                "a transcript starting at the visible end"))
     (with-current-buffer view
       (mevedel-view-stream-begin-turn (mevedel-view--history-insertion-marker)
                                       (with-current-buffer data (copy-marker (point-max))))
       (mevedel-view--start-spinner "Working on the benchmark request"))
     (mevedel-dc-bench--measure "stream" (lambda () (mevedel-dc-bench--stream data view)))
     (mevedel-dc-bench--measure "idle-request" (lambda () (mevedel-dc-bench--idle mevedel-dc-bench--idle-seconds)))
     (with-current-buffer view (mevedel-view--stop-spinner))
     (mevedel-dc-bench--measure "idle-quiet" (lambda () (mevedel-dc-bench--idle mevedel-dc-bench--idle-seconds)))
     (mevedel-dc-bench--measure "scrolling" (lambda () (mevedel-dc-bench--scroll view)))
     ;; Pay for the dirty transcript at the same step in both builds, not
     ;; whenever the command loop happens to service keystroke auto-save.
     (mevedel-dc-bench--measure
      "checkpoint"
      (lambda ()
        (let ((start (float-time)))
          (with-current-buffer data (mevedel-save-session))
          (list (* 1000 (- (float-time) start))))))
     (mevedel-dc-bench--check (not (buffer-modified-p data)) "a checkpointed transcript")
     (mevedel-dc-bench--layout data view))))

(defun mevedel-dc-bench--prepare (docked)
  "Check the compiled runtime and configure DOCKED or inline workloads."
  (unless (and (display-graphic-p) (eq window-system 'pgtk) (not noninteractive))
    (error "A graphical PGTK Emacs is required"))
  (setq mevedel-dc-bench--docked docked
        mevedel-floating-composer nil
        mevedel-view-spinner-animate t
        mevedel-view-spinner-power-policy 'full
        mevedel-dc-bench--kwin (mevedel-dc-bench--find-kwin))
  (dolist (grammar '(markdown markdown-inline))
    (unless (treesit-language-available-p grammar)
      (error "Missing tree-sitter grammar: %s" grammar)))
  (when docked (require 'mevedel-view-pair))
  (dolist (f (append '(mevedel-dc-bench--workloads mevedel-view--render-stream-update mevedel--display-chat-buffer)
                     (when docked '(mevedel-composer--fit mevedel-view-pair--repair))))
    (unless (and (compiled-function-p (symbol-function f))
                 (string-suffix-p ".elc" (symbol-file f 'defun)))
      (error "%s is not loaded from bytecode" f)))
  (add-hook 'post-command-hook #'mevedel-dc-bench--post-command 100)
  (global-set-key [f9] #'mevedel-dc-bench--popup)
  (global-set-key [f10] #'mevedel-dc-bench--resume-command)
  (mevedel-dc-bench--instrument))

(defun mevedel-dc-bench-run (output docked smoke)
  "Warm up and measure one round for DOCKED; write OUTPUT.  Shorten for SMOKE."
  (mevedel-dc-bench--prepare docked)
  (let ((mevedel-dc-bench--stream-count 16)
        (mevedel-dc-bench--idle-seconds 0.15)
        (mevedel-dc-bench--layout-count 1))
    (mevedel-dc-bench--workloads))
  (setq mevedel-dc-bench--records nil)
  (when smoke
    (setq mevedel-dc-bench--stream-count 12 mevedel-dc-bench--idle-seconds 0.25
          mevedel-dc-bench--layout-count 1))
  (mevedel-dc-bench--workloads)
  (with-temp-file output
    (insert (json-serialize
             (list :emacs-version emacs-version
                   :library (symbol-file 'mevedel-view--render-stream-update 'defun)
                   :gptel (symbol-file 'gptel-mode 'defun)
                   :records (vconcat (reverse mevedel-dc-bench--records)))))))

(defun mevedel-dc-bench-probe (docked)
  "Type into the displayed composer for the DOCKED or inline build."
  (mevedel-dc-bench--prepare docked)
  (mevedel-dc-bench--fresh-frame)
  (mevedel-dc-bench--with-view
   (lambda (_data _view) (mevedel-dc-bench--repeat 36 '(?a)))))

(provide 'mevedel-docked-composer-bench)
;;; mevedel-docked-composer-bench.el ends here
