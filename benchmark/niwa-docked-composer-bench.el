;;; niwa-docked-composer-bench.el --- Docked composer performance gate -*- lexical-binding: t; -*-

;;; Commentary:

;; Drive two docked builds in separate graphical processes.
;; Run only in the private KWin launched by scripts/bench-docked-composer.py:
;; this fixture replaces frames, focus and options.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'niwa)
(require 'niwa-chat)
(require 'niwa-native)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-native)
(require 'niwa-view-pair)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-session-persistence)

(defvar gcs-done)
(defvar niwa-session--read-only-mode)

(defvar niwa-dc-bench--counters (list :redisplays 0 :repairs 0 :repair-ms 0.0 :fits 0)
  "Cumulative work counters.")
(defvar niwa-dc-bench--renders nil "Timed stream renders in the current workload.")
(defvar niwa-dc-bench--stream-count 100 "Chunks per measured streaming workload.")
(defvar niwa-dc-bench--layout-count 6 "Samples per repeatable layout operation.")
(defvar niwa-dc-bench--idle-seconds 4 "Seconds per idle workload.")
(defvar niwa-dc-bench--resume nil "Fixture callback for resuming the saved session.")
(defvar niwa-dc-bench--pending nil "Non-nil while a measured command runs.")
(defvar niwa-dc-bench--done nil "Time a measured command finished, or nil.")
(defvar niwa-dc-bench--records nil "Measurements, newest first.")
(defvar niwa-dc-bench--kwin nil "Private KWin PID.")
(defvar niwa-dc-bench--require-native nil "Reject Lisp rail fallback when non-nil.")
(defvar niwa-dc-bench--rail-renderer nil "Rail renderer observed in the idle request.")

(defun niwa-dc-bench--cpu-ns (pids)
  "Return the CPU times of PIDS' threads in nanoseconds, as ((TASK . NS) ...)."
  (let (threads)
    (dolist (pid pids)
      (dolist (task (ignore-errors (directory-files (format "/proc/%d/task" pid) t "\\`[0-9]")))
        (with-temp-buffer
          (when (ignore-errors (insert-file-contents (file-name-concat task "schedstat")) t)
            (push (cons task (read (current-buffer))) threads)))))
    threads))

(defun niwa-dc-bench--cpu-delta (after before)
  "Return the CPU nanoseconds AFTER's threads used since BEFORE.
Per thread, so a thread exiting in between cannot make it negative; that
thread's CPU time since BEFORE is lost."
  (cl-loop for (task . ns) in after
           sum (- ns (or (cdr (assoc task before)) 0))))

(defun niwa-dc-bench--find-kwin ()
  "Return the PID of the KWin this Emacs runs under."
  (let ((pid (emacs-pid)))
    (while (and pid (not (equal (alist-get 'comm (process-attributes pid)) "kwin_wayland")))
      (setq pid (alist-get 'ppid (process-attributes pid)))
      (when (memq pid '(0 1)) (setq pid nil)))
    (or pid (error "Not running under a private kwin_wayland"))))

(defun niwa-dc-bench--xwayland ()
  "Return the private KWin's Xwayland PIDs."
  (cl-loop for pid in (list-system-processes)
           for attributes = (process-attributes pid)
           when (and (eql (alist-get 'ppid attributes) niwa-dc-bench--kwin)
                     (equal (alist-get 'comm attributes) "Xwayland"))
           collect pid))

(defun niwa-dc-bench--snapshot (xwayland)
  "Return the current counters and CPU times, with XWAYLAND's PIDs.
The caller finds those PIDs: scanning every process would count in Emacs's
CPU time."
  (append (list :emacs-ns (niwa-dc-bench--cpu-ns (list (emacs-pid)))
                :kwin-ns (niwa-dc-bench--cpu-ns (list niwa-dc-bench--kwin))
                :xwayland-ns (niwa-dc-bench--cpu-ns xwayland)
                :gcs gcs-done :wall (float-time))
          (copy-sequence niwa-dc-bench--counters)))

(defun niwa-dc-bench--post-command ()
  "Finish a measured command once its timers and redisplay ran."
  (when (and niwa-dc-bench--pending (not (minibufferp)))
    (setq niwa-dc-bench--pending nil)
    (run-at-time 0 nil #'niwa-dc-bench--finish 0)))

(defun niwa-dc-bench--finish (tries)
  "Wait until no timer is due (up to TRIES rearms), redisplay, exit."
  (redisplay t)
  (if (cl-some (lambda (timer) (time-less-p (timer--time timer) nil)) timer-list)
      (when (< tries 20)
        (run-at-time 0 nil #'niwa-dc-bench--finish (1+ tries)))
    (setq niwa-dc-bench--done (float-time))
    (exit-recursive-edit)))

(defun niwa-dc-bench--command (events)
  "Run EVENTS as one command; return its latency including redisplay, in ms."
  (setq niwa-dc-bench--done nil
        niwa-dc-bench--pending t)
  (let ((guard (run-at-time 10 nil #'exit-recursive-edit))
        (start (float-time)))
    (setq unread-command-events events)
    (unwind-protect (recursive-edit)
      (cancel-timer guard)
      (setq unread-command-events nil niwa-dc-bench--pending nil))
    (unless niwa-dc-bench--done (error "Command did not finish: %S" events))
    (* 1000 (- niwa-dc-bench--done start))))

(defun niwa-dc-bench--settle (&optional seconds)
  "Let timers run for SECONDS (default 0.03) and redisplay."
  (let ((unread-command-events nil))
    (sit-for (or seconds 0.03))
    (redisplay t)))

(defun niwa-dc-bench--delta (before after)
  "Return the counters and CPU times from snapshot BEFORE to AFTER."
  (cl-loop for (key value) on after by #'cddr
           nconc (list key (if (listp value)
                               (niwa-dc-bench--cpu-delta value (plist-get before key))
                             (- value (plist-get before key))))))

(defun niwa-dc-bench--idle (seconds)
  "Stay idle for SECONDS."
  (let ((end (+ (float-time) seconds)))
    (while (< (float-time) end) (sit-for (- end (float-time)))))
  nil)

(defun niwa-dc-bench--focus (frame)
  "Give FRAME input focus and wait until the compositor confirms it."
  (let ((tries 0))
    (while (and (not (eq t (frame-focus-state frame))) (< (cl-incf tries) 6))
      (select-frame-set-input-focus frame)
      (let ((deadline (+ (float-time) 1)))
        (while (and (not (eq t (frame-focus-state frame))) (< (float-time) deadline))
          (niwa-dc-bench--settle 0.05)))))
  (niwa-dc-bench--check (eq t (frame-focus-state frame))
                           (format "focus on %S; states %S" frame
                                   (mapcar (lambda (f) (cons f (frame-focus-state f)))
                                           (frame-list)))))

(defun niwa-dc-bench--fresh-frame ()
  "Select a newly mapped, focused 100x45 frame; delete every other frame."
  (let ((frame (make-frame '((width . 100) (height . 45)))))
    (select-frame frame)
    (niwa-dc-bench--focus frame)
    (dolist (other (frame-list))
      (unless (or (eq other frame) (frame-parent other)) (delete-frame other)))
    (let ((deadline (+ (float-time) 5)))
      (while (and (/= (frame-width) 100) (< (float-time) deadline))
        (niwa-dc-bench--settle 0.05)))
    (niwa-dc-bench--check (and (= (frame-width) 100) (eq t (frame-focus-state frame)))
                             "a focused 100-column frame")))

(defun niwa-dc-bench--check (condition what)
  "Fail unless CONDITION holds, explaining WHAT was expected."
  (unless condition (error "Expected %s; selected %S" what (window-buffer))))

(defun niwa-dc-bench--instrument ()
  "Count redisplay, pair repair, and fit work; time repair and stream renders."
  (add-function :before pre-redisplay-function
                (lambda (_) (cl-incf (plist-get niwa-dc-bench--counters :redisplays))))
  (advice-add 'niwa-view-pair--repair :around
              (lambda (function &rest args)
                (let ((start (float-time)))
                  (prog1 (apply function args)
                    (cl-incf (plist-get niwa-dc-bench--counters :repairs))
                    (cl-incf (plist-get niwa-dc-bench--counters :repair-ms)
                             (* 1000 (- (float-time) start)))))))
  (advice-add 'niwa-composer--fit :before
              (lambda (&rest _)
                (cl-incf (plist-get niwa-dc-bench--counters :fits))))
  (advice-add 'niwa-view--render-stream-update :around
              (lambda (function &rest args)
                (let ((start (float-time)))
                  (prog1 (apply function args)
                    (push (* 1000 (- (float-time) start)) niwa-dc-bench--renders))))))

(defun niwa-dc-bench--windows (view count)
  "Check that VIEW has COUNT transcripts and COUNT matching docks."
  (let ((views (get-buffer-window-list view nil (selected-frame)))
        (docks (get-buffer-window-list (niwa-view-composer-buffer view)
                                       nil (selected-frame))))
    (niwa-dc-bench--check (= count (length views)) (format "%d transcripts" count))
    (niwa-dc-bench--check (= count (length docks)) (format "%d docks" count))
    (dolist (window views)
      (let ((dock (niwa-view-composer-window window)))
        (niwa-dc-bench--check
         (and (memq dock docks) (eq dock (window-next-sibling window)))
         "a paired dock immediately below each transcript")
        (setq docks (delq dock docks))))))

(defun niwa-dc-bench--with-view (function)
  "Call FUNCTION with the data and view buffers of a displayed chat."
  ;; The private runner deletes this root after reaping journal workers.
  (let* ((root (file-name-as-directory
                (make-temp-file (file-name-concat (getenv "XDG_STATE_HOME")
                                                  "niwa-bench-") t)))
         (niwa-user-dir (file-name-concat root "user/"))
         (gptel-display-buffer-action
          '(display-buffer-same-window (body-function . select-window)))
         (workspace (niwa-workspace--create :type 'project :id root :name "bench" :root root))
         (session (niwa-session-create "bench" workspace root))
         (data (generate-new-buffer "*Benchmark data*"))
         (view (generate-new-buffer "*Benchmark view*")))
    (unwind-protect
        (progn
          (setf (niwa-session-preset-name session) 'niwa-implement)
          (with-current-buffer data
            (niwa--transcript-org-mode)
            (setq-local niwa--session session niwa--workspace workspace
                        niwa-session--read-only-mode nil
                        gptel-response-separator "\n\n" default-directory root)
            (dotimes (i 300)
              (let ((start (point)))
                (insert (format "Line %d of the earlier response, with **bold prose**.\n" i))
                (put-text-property start (point) 'gptel 'response))))
          (niwa-session-set-root-buffer session data)
          (niwa-view--setup view data)
          (with-current-buffer view (niwa-view--full-rerender))
          (niwa--display-chat-buffer data)
          (niwa-dc-bench--settle 0.2)
          (niwa-dc-bench--windows view 1)
          (niwa-dc-bench--check
           (eq (window-buffer) (niwa-view-composer-buffer view))
           "focus in the displayed draft")
          (goto-char (point-max))
          (niwa-dc-bench--settle)
          (funcall function data view))
      (when (buffer-live-p data) (niwa-chat-discard-buffers data t))
      (when (buffer-live-p view) (kill-buffer view)))))

(defun niwa-dc-bench--measure (scenario function)
  "Record SCENARIO, where FUNCTION returns latency samples."
  (let* ((xwayland (niwa-dc-bench--xwayland))
         (_ (progn (garbage-collect) (niwa-dc-bench--settle)))
         (_ (niwa-dc-bench--check (eq t (frame-focus-state)) "a focused benchmark frame"))
         (before (niwa-dc-bench--snapshot xwayland))
         (samples (funcall function))
         (after (niwa-dc-bench--snapshot xwayland)))
    (push (append (list :scenario scenario :samples-ms (vconcat samples))
                  (niwa-dc-bench--delta before after))
          niwa-dc-bench--records)))

(defun niwa-dc-bench--repeat (n events)
  "Run N commands EVENTS with typing-like pacing."
  (cl-loop repeat n collect (prog1 (niwa-dc-bench--command events)
                              (niwa-dc-bench--settle))))

(defun niwa-dc-bench--stream (data view)
  "Send paced chunks through DATA's stream hook and check they reach VIEW."
  (let ((n 0) timer (deadline (+ (float-time) 15)) (niwa-dc-bench--renders nil))
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
                     (niwa-view-stream-schedule))
                   (when (= (cl-incf n) niwa-dc-bench--stream-count)
                     (cancel-timer timer)))))
          (while (and (< n niwa-dc-bench--stream-count) (< (float-time) deadline))
            (niwa-dc-bench--settle 0.05))
          (niwa-dc-bench--check (= n niwa-dc-bench--stream-count) "every stream chunk delivered")
          (niwa-dc-bench--settle (+ niwa-view-stream-render-delay 0.2)))
      (when timer (cancel-timer timer)))
    (with-current-buffer view
      (niwa-dc-bench--check
       (save-excursion (goto-char (point-min)) (search-forward (format "streamtoken%d" (1- n)) nil t))
       (format "the final streamed token after %d renders; tail: %S"
               (length niwa-dc-bench--renders)
               (buffer-substring-no-properties (max (point-min) (- (point-max) 350)) (point-max)))))
    (nreverse niwa-dc-bench--renders)))

(defun niwa-dc-bench--scroll (view)
  "Page both directions in VIEW; verify every page moves the viewport."
  (select-window (get-buffer-window view))
  (set-window-start nil (point-min))
  (goto-char (point-min))
  (niwa-dc-bench--settle)
  (cl-loop for i below 12
           for keys = (if (zerop (% i 2)) "C-v" "M-v")
           for start = (window-start)
           for ms = (niwa-dc-bench--command (listify-key-sequence (kbd keys)))
           collect ms into samples
           do (niwa-dc-bench--check (/= start (window-start)) "a changed transcript viewport")
           do (niwa-dc-bench--settle)
           finally return samples))

(defun niwa-dc-bench--popup ()
  "Open and select a Help-like side window outside the composer pair."
  (interactive)
  (select-window (display-buffer-in-side-window (get-buffer "*Benchmark popup*")
                                               '((side . right) (window-width . 24)))))

(defun niwa-dc-bench--resume-command ()
  "Run the benchmark's saved-session resume action."
  (interactive)
  (funcall niwa-dc-bench--resume))

(defun niwa-dc-bench--layout (data view)
  "Measure real layout commands and session resume for DATA and VIEW."
  (let* ((scratch (get-buffer-create "*scratch*"))
         (popup (get-buffer-create "*Benchmark popup*"))
         (transcript (get-buffer-window view))
         (editor (niwa-view-composer-window transcript)))
    (with-current-buffer popup (special-mode))
    (dotimes (_ niwa-dc-bench--layout-count)
      (select-window editor)
      (niwa-dc-bench--measure
       "split"
       (lambda ()
         (let ((ms (niwa-dc-bench--command (listify-key-sequence (kbd "C-x 3")))))
           (niwa-dc-bench--windows view 2)
           (list ms))))
      (niwa-dc-bench--command (listify-key-sequence (kbd "C-x 1")))
      (niwa-dc-bench--settle)
      (niwa-dc-bench--windows view 1))
    (dotimes (_ niwa-dc-bench--layout-count)
      (select-window (niwa-view-composer-window transcript))
      (dolist (target (list scratch view))
        (niwa-dc-bench--measure
         (if (eq target scratch) "switch-away" "switch-back")
         (lambda ()
           (let ((ms (niwa-dc-bench--command (append (kbd "C-x b") (buffer-name target) '(?\r)))))
             (niwa-dc-bench--check (eq target (window-buffer)) "the switched-to buffer")
             (niwa-dc-bench--windows view (if (eq target scratch) 0 1))
             (list ms))))))
    (dotimes (_ niwa-dc-bench--layout-count)
      (select-window (niwa-view-composer-window transcript))
      (niwa-dc-bench--measure
       "popup-open"
       (lambda ()
         (let ((ms (niwa-dc-bench--command '(f9))))
           (niwa-dc-bench--check
            (and (eq popup (window-buffer)) (eq 'right (window-parameter nil 'window-side)))
            "a popup outside the pair")
           (niwa-dc-bench--windows view 1)
           (list ms))))
      (niwa-dc-bench--measure
       "popup-close"
       (lambda ()
         (let ((ms (niwa-dc-bench--command '(?q))))
           (niwa-dc-bench--check (not (get-buffer-window popup)) "the popup closed")
           (niwa-dc-bench--windows view 1)
           (list ms)))))
    (let* ((session (buffer-local-value 'niwa--session data))
           (workspace (buffer-local-value 'niwa--workspace data))
           (id (niwa-session-session-id session))
           reopened
           (niwa-dc-bench--resume
            (lambda ()
              (setq reopened (niwa-session-persistence-resume-id workspace id))
              (niwa--display-chat-buffer reopened))))
      (niwa-dc-bench--check (niwa-session-save-path session) "a saved session")
      (niwa-chat-discard-buffers data t)
      (niwa-dc-bench--settle)
      (unwind-protect
          (niwa-dc-bench--measure
           "resume"
           (lambda ()
             (let* ((ms (niwa-dc-bench--command '(f10)))
                    (restored (buffer-local-value 'niwa--session reopened)))
               (niwa-dc-bench--check (and (not (eq restored session))
                                            (equal id (niwa-session-session-id restored)))
                                        "the saved session restored in a new buffer")
               (niwa-dc-bench--windows (buffer-local-value 'niwa--view-buffer reopened) 1)
               (list ms))))
        (when (buffer-live-p reopened) (niwa-chat-discard-buffers reopened t))))))

(defun niwa-dc-bench--workloads ()
  "Run a complete fresh-fixture workload set."
  (niwa-dc-bench--fresh-frame)
  (niwa-dc-bench--with-view
   (lambda (data view)
     (let ((start (niwa-view-composer-draft-start)))
       (niwa-dc-bench--measure "typing" (lambda () (niwa-dc-bench--repeat 40 '(?a))))
       (niwa-dc-bench--check (equal (make-string 40 ?a) (buffer-substring-no-properties start (point-max)))
                                "the forty typed characters")
       ;; This exceeds the dock cap and traverses a soft-wrap boundary during deletion.
       (insert (mapconcat (lambda (i) (format " word%d" i)) (number-sequence 1 220) ""))
       (niwa-dc-bench--settle 0.2)
       (let ((row (save-excursion (vertical-motion 0) (point))))
         (niwa-dc-bench--check (> row (+ start 40)) "a wrapped draft")
         (when (> (- (point-max) row) 20)
           (delete-region (+ row 20) (point-max))
           (niwa-dc-bench--settle))
         (let ((end (point-max)))
           (niwa-dc-bench--measure "wrapped-deletion" (lambda () (niwa-dc-bench--repeat 40 '(127))))
           (niwa-dc-bench--check (= (- end 40) (point-max)) "forty deletions")))
       (delete-region (+ start 40) (point-max))
       (niwa-dc-bench--settle 0.2))
     (let ((window (get-buffer-window view)))
       (with-selected-window window
         (goto-char (point-max))
         (recenter -1))
       (niwa-dc-bench--settle)
       (niwa-dc-bench--check (pos-visible-in-window-p (with-current-buffer view (point-max)) window)
                                "a transcript starting at the visible end"))
     (with-current-buffer view
       (niwa-view-stream-begin-turn (niwa-view--history-insertion-marker)
                                       (with-current-buffer data (copy-marker (point-max))))
       (niwa-view--start-spinner "Working on the benchmark request"))
     (niwa-dc-bench--measure "stream" (lambda () (niwa-dc-bench--stream data view)))
     (niwa-dc-bench--measure "idle-request" (lambda () (niwa-dc-bench--idle niwa-dc-bench--idle-seconds)))
     (let ((native (buffer-local-value 'niwa-view-native--entries
                                      (niwa-view-composer-buffer view))))
       (when (and niwa-dc-bench--require-native (not native))
         (error "Native rail requested but the dock rendered with Lisp"))
       (setq niwa-dc-bench--rail-renderer (if native "native" "lisp")))
     (with-current-buffer view (niwa-view--stop-spinner))
     (niwa-dc-bench--measure "idle-quiet" (lambda () (niwa-dc-bench--idle niwa-dc-bench--idle-seconds)))
     (niwa-dc-bench--measure "scrolling" (lambda () (niwa-dc-bench--scroll view)))
     ;; Pay for the dirty transcript at the same step in both builds, not
     ;; whenever the command loop happens to service keystroke auto-save.
     (niwa-dc-bench--measure
      "checkpoint"
      (lambda ()
        (let ((start (float-time)))
          (with-current-buffer data (niwa-save-session))
          (list (* 1000 (- (float-time) start))))))
     (niwa-dc-bench--check (not (buffer-modified-p data)) "a checkpointed transcript")
     (niwa-dc-bench--layout data view))))

(defun niwa-dc-bench--prepare ()
  "Check the compiled runtime and configure the docked workloads."
  (unless (and (display-graphic-p) (eq window-system 'pgtk) (not noninteractive))
    (error "A graphical PGTK Emacs is required"))
  (when (and niwa-dc-bench--require-native (not (niwa-native-load)))
    (error "Native rail requested but the module did not load: %s" niwa-native--load-state))
  (setq niwa-view-spinner-animate t
        niwa-view-spinner-power-policy 'full
        niwa-dc-bench--kwin (niwa-dc-bench--find-kwin))
  (dolist (grammar '(markdown markdown-inline))
    (unless (treesit-language-available-p grammar)
      (error "Missing tree-sitter grammar: %s" grammar)))
  (dolist (f '(niwa-dc-bench--workloads niwa-view--render-stream-update
               niwa--display-chat-buffer niwa-composer--fit niwa-view-pair--repair))
    (unless (and (compiled-function-p (symbol-function f))
                 (string-suffix-p ".elc" (symbol-file f 'defun)))
      (error "%s is not loaded from bytecode" f)))
  (add-hook 'post-command-hook #'niwa-dc-bench--post-command 100)
  (global-set-key [f9] #'niwa-dc-bench--popup)
  (global-set-key [f10] #'niwa-dc-bench--resume-command)
  (niwa-dc-bench--instrument))

(defun niwa-dc-bench-run (output smoke)
  "Warm up and measure one round; write OUTPUT.  Shorten for SMOKE."
  (niwa-dc-bench--prepare)
  (let ((niwa-dc-bench--stream-count 16)
        (niwa-dc-bench--idle-seconds 0.15)
        (niwa-dc-bench--layout-count 1))
    (niwa-dc-bench--workloads))
  (setq niwa-dc-bench--records nil)
  (when smoke
    (setq niwa-dc-bench--stream-count 12 niwa-dc-bench--idle-seconds 0.25
          niwa-dc-bench--layout-count 1))
  (niwa-dc-bench--workloads)
  (with-temp-file output
    (insert (json-serialize
             (list :emacs-version emacs-version
                   :library (symbol-file 'niwa-view--render-stream-update 'defun)
                   :gptel (symbol-file 'gptel-mode 'defun)
                   :rail-renderer niwa-dc-bench--rail-renderer
                   :records (vconcat (reverse niwa-dc-bench--records)))))))

(defun niwa-dc-bench-probe ()
  "Exercise frame delivery with typing and native rail placement."
  (niwa-dc-bench--prepare)
  (niwa-dc-bench--fresh-frame)
  (niwa-dc-bench--with-view
   (lambda (_data view)
     (niwa-dc-bench--repeat 36 '(?a))
     (when (niwa-view-native-available-p (selected-frame))
       ;; Each placement requests a Wayland frame callback on the parent.
       (dotimes (_ 24)
         (with-current-buffer view (niwa-view--start-spinner "Benchmark request"))
         (niwa-dc-bench--settle 0.25)
         (with-current-buffer view (niwa-view--stop-spinner))
         (niwa-dc-bench--settle))))))

(provide 'niwa-docked-composer-bench)
;;; niwa-docked-composer-bench.el ends here
