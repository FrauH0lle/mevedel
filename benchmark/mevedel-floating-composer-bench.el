;;; mevedel-floating-composer-bench.el --- Floating composer performance gate -*- lexical-binding: t; -*-

;;; Commentary:

;; Measurements comparing the floating composer with the option off, run by
;; scripts/bench-floating-composer.py inside a private presenting KWin.  A
;; graphical Emacs drives a real view through the command loop and writes
;; raw samples as JSON; the runner computes statistics.  Never load this
;; into an interactive editor: it replaces frames, focus and options.
;;
;; Each round runs every scenario once, in alternating order, on a fresh
;; view.  Scenarios: `off-inline' (option off, composer at the bottom: the
;; inline baseline and the equal-visible-work control, rail visible inline),
;; `off-away' (option off, view scrolled away), `on-never' (on, never shown),
;; `on-hidden' (on, hidden after one crossing) and `on-visible' (on, view
;; scrolled away, cursor in the floating composer).

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mwheel)
(require 'mevedel)
(require 'mevedel-floating-composer)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)

(defvar gcs-done)
(defvar mevedel-session--read-only-mode)

(defconst mevedel-fc-bench--scenarios
  '(off-inline off-away on-never on-hidden on-visible)
  "Scenarios, in a round's forward order.")

(defconst mevedel-fc-bench--draft
  (concat "> active draft\nsecond line\n"
          (mapconcat (lambda (i) (format "word%d" i)) (number-sequence 1 80) " "))
  "Draft whose last line wraps over several screen lines.")

(defvar mevedel-fc-bench--counters (list :redisplays 0 :placements 0 :visibility 0)
  "Running redisplay and floating-composer frame operation counts.")
(defvar mevedel-fc-bench--pending nil "Non-nil while a measured command runs.")
(defvar mevedel-fc-bench--done nil "Time a measured command finished, or nil.")
(defvar mevedel-fc-bench--records nil "Measurements, newest first.")
(defvar mevedel-fc-bench--kwin nil "Private KWin PID.")


;;
;;; Counters

(defun mevedel-fc-bench--count (key)
  "Increment counter KEY."
  (cl-incf (plist-get mevedel-fc-bench--counters key)))

(defun mevedel-fc-bench--count-frame (key)
  "Return advice counting KEY for floating-composer frames."
  (lambda (frame &rest _)
    (when (and (framep frame) (frame-parameter frame 'mevedel-view-animate-as-parent))
      (mevedel-fc-bench--count key))))

(defun mevedel-fc-bench--instrument ()
  "Count redisplay cycles and floating-composer frame operations."
  (add-function :before pre-redisplay-function
                (lambda (_) (mevedel-fc-bench--count :redisplays)))
  (dolist (f '(set-frame-size set-frame-size-and-position-pixelwise set-frame-position))
    (advice-add f :before (mevedel-fc-bench--count-frame :placements)))
  (dolist (f '(make-frame-visible make-frame-invisible))
    (advice-add f :before (mevedel-fc-bench--count-frame :visibility))))

(defun mevedel-fc-bench--cpu-ns (pids)
  "Return the CPU times of PIDS' threads in nanoseconds, as ((TASK . NS) ...)."
  (let (threads)
    (dolist (pid pids)
      (dolist (task (ignore-errors (directory-files (format "/proc/%d/task" pid) t "\\`[0-9]")))
        (with-temp-buffer
          (when (ignore-errors (insert-file-contents (file-name-concat task "schedstat")) t)
            (push (cons task (read (current-buffer))) threads)))))
    threads))

(defun mevedel-fc-bench--cpu-delta (after before)
  "Return the CPU nanoseconds AFTER's threads used since BEFORE.
Per thread, so a thread exiting in between cannot make it negative; that
thread's CPU time since BEFORE is lost."
  (cl-loop for (task . ns) in after
           sum (- ns (or (cdr (assoc task before)) 0))))

(defun mevedel-fc-bench--find-kwin ()
  "Return the PID of the KWin this Emacs runs under."
  (let ((pid (emacs-pid)))
    (while (and pid (not (equal (alist-get 'comm (process-attributes pid)) "kwin_wayland")))
      (setq pid (alist-get 'ppid (process-attributes pid)))
      (when (memq pid '(0 1)) (setq pid nil)))
    (or pid (error "Not running under a private kwin_wayland"))))

(defun mevedel-fc-bench--xwayland ()
  "Return the private KWin's Xwayland PIDs."
  (cl-loop for pid in (list-system-processes)
           for attributes = (process-attributes pid)
           when (and (eql (alist-get 'ppid attributes) mevedel-fc-bench--kwin)
                     (equal (alist-get 'comm attributes) "Xwayland"))
           collect pid))

(defun mevedel-fc-bench--snapshot (xwayland)
  "Return the current counters and CPU times, with XWAYLAND's PIDs.
The caller finds those PIDs: scanning every process would count in Emacs's
CPU time."
  (append (list :emacs-ns (mevedel-fc-bench--cpu-ns (list (emacs-pid)))
                :kwin-ns (mevedel-fc-bench--cpu-ns (list mevedel-fc-bench--kwin))
                :xwayland-ns (mevedel-fc-bench--cpu-ns xwayland)
                :gcs gcs-done :wall (float-time))
          (copy-sequence mevedel-fc-bench--counters)))


;;
;;; Commands through the command loop

(defun mevedel-fc-bench--post-command ()
  "Finish a measured command once its timers and redisplay ran."
  (when mevedel-fc-bench--pending
    (setq mevedel-fc-bench--pending nil)
    (run-at-time 0 nil #'mevedel-fc-bench--finish 0)))

(defun mevedel-fc-bench--finish (tries)
  "Wait until no timer is due (up to TRIES rearms), redisplay, exit."
  (if (and (< tries 20)
           (cl-some (lambda (timer) (time-less-p (timer--time timer) nil)) timer-list))
      (run-at-time 0 nil #'mevedel-fc-bench--finish (1+ tries))
    (redisplay)
    (setq mevedel-fc-bench--done (float-time))
    (exit-recursive-edit)))

(defun mevedel-fc-bench--command (events)
  "Run EVENTS as one command; return its latency including redisplay, in ms."
  (setq mevedel-fc-bench--done nil
        mevedel-fc-bench--pending t)
  (let ((guard (run-at-time 10 nil #'exit-recursive-edit))
        (start (float-time)))
    (setq unread-command-events events)
    (unwind-protect (recursive-edit)
      (cancel-timer guard)
      (setq unread-command-events nil mevedel-fc-bench--pending nil))
    (unless mevedel-fc-bench--done (error "Command did not finish: %S" events))
    (* 1000 (- mevedel-fc-bench--done start))))

(defun mevedel-fc-bench--settle (&optional seconds)
  "Let timers run for SECONDS (default 0.03) and redisplay."
  (let ((unread-command-events nil))
    (sit-for (or seconds 0.03))
    (redisplay t)))

(defun mevedel-fc-bench--host ()
  "Return the top-level window showing the view."
  (frame-root-window (window-frame (or mevedel-floating-composer--host (selected-window)))))

(defun mevedel-fc-bench--wheel (direction)
  "Scroll the host one notch in DIRECTION; return the latency in ms."
  (let ((host (mevedel-fc-bench--host)))
    (mevedel-fc-bench--command
     (list (list direction (posn-at-point (window-start host) host))))))

(defun mevedel-fc-bench--shown-p ()
  "Return non-nil when the floating composer is shown."
  (when-let* ((window (mevedel-floating-composer--window)))
    (frame-visible-p (window-frame window))))

(defun mevedel-fc-bench--check (condition what)
  "Signal unless CONDITION, describing the expected state WHAT."
  (unless condition
    (error "Benchmark state broken: expected %s; option %s, shown %s, window %s, point %d, input %d, start %d, end %d"
           what mevedel-floating-composer (mevedel-fc-bench--shown-p) (selected-window)
           (point) (mevedel-view-composer-draft-start) (window-start)
           (mevedel-view-composer-draft-end))))

(defun mevedel-fc-bench--away ()
  "Scroll away notch by notch with the draft cursor until it crosses.
The floating composer appears only once the host shows no composer line,
which can take more than one notch.  Return the latency of the notch
that crossed, in ms."
  (let ((notches 0) latency)
    (while (not (and latency
                     (if mevedel-floating-composer
                         (and (mevedel-fc-bench--shown-p)
                              (eq (selected-window) (mevedel-floating-composer--window)))
                       (not (mevedel-view-composer-in-draft-p (point))))))
      (mevedel-fc-bench--check (< (cl-incf notches) 6)
                               (if mevedel-floating-composer
                                   "the floating composer shown and selected"
                                 "the cursor in history"))
      (when latency (mevedel-fc-bench--settle))
      (setq latency (mevedel-fc-bench--wheel 'wheel-up)))
    latency))

(defun mevedel-fc-bench--back ()
  "Scroll back notch by notch until the cursor is inline.
Return the latency of the notch that crossed, in ms."
  (let ((notches 0) latency)
    (while (not (and latency
                     (not (mevedel-fc-bench--shown-p))
                     (not (frame-parent (selected-frame)))
                     (mevedel-view-composer-in-draft-p (point))))
      (mevedel-fc-bench--check (< (cl-incf notches) 6)
                               "the cursor inline with no floating composer")
      (when latency (mevedel-fc-bench--settle))
      (setq latency (mevedel-fc-bench--wheel 'wheel-down)))
    latency))


;;
;;; Fixture

(defun mevedel-fc-bench--with-view (function)
  "Call FUNCTION in a selected graphical view with history and a draft."
  (let ((data-buf (generate-new-buffer "mevedel-bench-data"))
        (view-buf (generate-new-buffer "mevedel-bench-view"))
        (mevedel-user-dir (file-name-as-directory (make-temp-file "mevedel-bench-user-" t))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (mevedel--transcript-org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local mevedel-session--read-only-mode nil)
            (dotimes (i 300)
              (let ((start (point)))
                (insert (format "Line %d of the earlier response, with some ordinary prose.\n" i))
                (put-text-property start (point) 'gptel 'response))))
          (mevedel-view--setup view-buf data-buf)
          (delete-other-windows)
          (switch-to-buffer view-buf)
          (let ((session (mevedel-session--create :name "bench")))
            (setq-local mevedel--session session)
            (with-current-buffer data-buf (setq-local mevedel--session session)))
          (mevedel-view--full-rerender)
          (let* ((start (mevedel-view-composer-draft-start view-buf))
                 (end (mevedel-view-composer-insert mevedel-fc-bench--draft nil view-buf)))
            (with-current-buffer (mevedel-view-composer-buffer view-buf)
              (remove-text-properties start end '(read-only nil mevedel-view-prompt nil
                                                  font-lock-face nil face nil
                                                  front-sticky nil rear-nonsticky nil))))
          (recenter -1)
          (mevedel-fc-bench--settle 0.2)
          (funcall function data-buf))
      (kill-buffer view-buf)
      (kill-buffer data-buf)
      (delete-directory mevedel-user-dir t))))

(defun mevedel-fc-bench--measure (round scenario workload function)
  "Record WORKLOAD of SCENARIO in ROUND; FUNCTION returns latency samples."
  (let* ((xwayland (mevedel-fc-bench--xwayland))
         (_ (progn (garbage-collect) (mevedel-fc-bench--settle)))
         (before (mevedel-fc-bench--snapshot xwayland))
         (state (mevedel-fc-bench--state))
         (samples (funcall function))
         (after (mevedel-fc-bench--snapshot xwayland)))
    (mevedel-fc-bench--record round scenario workload state samples
                              (mevedel-fc-bench--delta before after))))

(defun mevedel-fc-bench--state ()
  "Return the view's state as (:focused FOCUSED :attended ATTENDED)."
  ;; The floating composer never holds compositor focus; its parent does.
  (list :focused (if (eq t (frame-focus-state (or (frame-parent (selected-frame))
                                                  (selected-frame))))
                     t :false)
        :attended (if (mevedel-view--unattended-p) :false t)))

(defun mevedel-fc-bench--delta (before after)
  "Return the counters and CPU times from snapshot BEFORE to AFTER."
  (cl-loop for (key value) on after by #'cddr
           nconc (list key (if (listp value)
                               (mevedel-fc-bench--cpu-delta value (plist-get before key))
                             (- value (plist-get before key))))))

(defun mevedel-fc-bench--record (round scenario workload state samples delta)
  "Record WORKLOAD of SCENARIO in ROUND: STATE, latency SAMPLES and DELTA."
  (push (append (list :round round :scenario (symbol-name scenario)
                      :workload (symbol-name workload) :samples-ms (vconcat samples))
                state delta)
        mevedel-fc-bench--records))

(defun mevedel-fc-bench--crossings (round scenario n)
  "Record N crossings away and back of SCENARIO in ROUND, each direction alone.
Counters cover only the crossing commands, not the pauses between them."
  (let* ((xwayland (mevedel-fc-bench--xwayland))
         (_ (progn (garbage-collect) (mevedel-fc-bench--settle)))
         (state (mevedel-fc-bench--state))
         (sums (list 'cross-away nil 'cross-back nil))
         (samples (list 'cross-away nil 'cross-back nil)))
    (dotimes (_ n)
      (dolist (direction '(cross-away cross-back))
        (let* ((before (mevedel-fc-bench--snapshot xwayland))
               (latency (if (eq direction 'cross-away)
                            (mevedel-fc-bench--away)
                          (mevedel-fc-bench--back)))
               (delta (mevedel-fc-bench--delta before (mevedel-fc-bench--snapshot xwayland))))
          (push latency (plist-get samples direction))
          (setf (plist-get sums direction)
                (if-let* ((sum (plist-get sums direction)))
                    (cl-loop for (key value) on sum by #'cddr
                             nconc (list key (+ value (plist-get delta key))))
                  delta))
          (mevedel-fc-bench--settle))))
    (dolist (direction '(cross-away cross-back))
      (mevedel-fc-bench--record round scenario direction state
                                (nreverse (plist-get samples direction))
                                (plist-get sums direction)))))

(defun mevedel-fc-bench--repeat (n events)
  "Return latencies of N commands EVENTS, paced like typing."
  (cl-loop repeat n
           collect (prog1 (mevedel-fc-bench--command events)
                     (mevedel-fc-bench--settle))))

(defun mevedel-fc-bench--stream (data-buf)
  "Stream 100 chunks into DATA-BUF every 40 ms through the production hook."
  (let ((done nil) (n 0) timer)
    (setq timer
          (run-at-time
           0 0.04
           (lambda ()
             (with-current-buffer data-buf
               (goto-char (point-max))
               (let ((start (point)))
                 (insert (format "streamed token %d " n)
                         (if (zerop (% n 8)) "and a line break.\n" ""))
                 (put-text-property start (point) 'gptel 'response))
               (mevedel-view-stream-schedule))
             (when (= (cl-incf n) 100) (cancel-timer timer) (setq done t)))))
    (while (not done) (mevedel-fc-bench--settle 0.05))
    (mevedel-fc-bench--settle 1.0)
    nil))

(defun mevedel-fc-bench--idle (seconds)
  "Stay idle for SECONDS."
  (let ((end (+ (float-time) seconds)))
    (while (< (float-time) end) (sit-for (- end (float-time)))))
  nil)

(defun mevedel-fc-bench--focus (frame)
  "Give FRAME input focus and wait until the compositor confirms it."
  (let ((tries 0))
    (while (and (not (eq t (frame-focus-state frame))) (< (cl-incf tries) 6))
      (select-frame-set-input-focus frame)
      (let ((deadline (+ (float-time) 1)))
        (while (and (not (eq t (frame-focus-state frame))) (< (float-time) deadline))
          (mevedel-fc-bench--settle 0.05)))))
  (mevedel-fc-bench--check (eq t (frame-focus-state frame))
                           (format "focus on %S; states %S" frame
                                   (mapcar (lambda (f) (cons f (frame-focus-state f)))
                                           (frame-list)))))

(defun mevedel-fc-bench--scenario (round scenario)
  "Run every workload of SCENARIO in ROUND on a fresh view."
  (mevedel-fc-bench--fresh-frame)
  (let ((mevedel-floating-composer (string-prefix-p "on-" (symbol-name scenario))))
    (mevedel-fc-bench--with-view
     (lambda (data-buf)
       (cl-flet ((measure (workload function)
                   (mevedel-fc-bench--measure round scenario workload function)))
         (pcase scenario
           ('on-hidden (mevedel-fc-bench--away) (mevedel-fc-bench--back)
                       (mevedel-fc-bench--check
                        (frame-live-p mevedel-floating-composer--frame) "a hidden frame"))
           ((or 'off-away 'on-visible) (mevedel-fc-bench--away)))
         (unless (eq scenario 'off-away)
           (measure 'typing (lambda () (mevedel-fc-bench--repeat 40 '(?a))))
           (goto-char (mevedel-view-composer-draft-end))
           (measure 'wrapped-deletion (lambda () (mevedel-fc-bench--repeat 30 '(127)))))
         (mevedel-view-stream-begin-turn (mevedel-view--history-insertion-marker)
                                         (with-current-buffer data-buf (point-max)))
         (mevedel-view--start-spinner "Working on the benchmark request")
         (measure 'stream (lambda () (mevedel-fc-bench--stream data-buf)))
         (measure 'idle-request (lambda () (mevedel-fc-bench--idle 4)))
         (mevedel-view--stop-spinner)
         (measure 'idle-quiet (lambda () (mevedel-fc-bench--idle 4)))
         ;; Before any crossing, so `on-never' has never created a frame.  The
         ;; focused frame shows another buffer, so no window attends the view.
         (mevedel-view--start-spinner "Working on the benchmark request")
         (when (eq scenario 'on-never)
           (mevedel-fc-bench--check (not mevedel-floating-composer--frame)
                                    "no floating-composer frame"))
         (let ((view (current-buffer))
               (frame (window-frame (mevedel-fc-bench--host)))
               (other (with-current-buffer (get-buffer-create "*scratch*")
                        (make-frame '((width . 40) (height . 10))))))
           (mevedel-fc-bench--focus other)
           (with-current-buffer view
             (measure 'idle-unfocused (lambda () (mevedel-fc-bench--idle 4))))
           (delete-frame other)
           (mevedel-fc-bench--focus frame)
           (set-buffer view))
         (mevedel-view--stop-spinner)
         (when (memq scenario '(off-inline on-never))
           (mevedel-fc-bench--settle 0.2)
           (unless (mevedel-view-composer-in-draft-p (point))
             (mevedel-fc-bench--back))
           (when mevedel-floating-composer
             (measure 'first-show
                      (lambda ()
                        (cl-loop repeat 6
                                 collect (progn (mevedel-floating-composer--remove)
                                                (mevedel-fc-bench--settle)
                                                (prog1 (mevedel-fc-bench--away)
                                                  (mevedel-fc-bench--settle)
                                                  (mevedel-fc-bench--back)
                                                  (mevedel-fc-bench--settle)))))))
           (mevedel-fc-bench--crossings round scenario 12)))))))

(defun mevedel-fc-bench--fresh-frame ()
  "Select a newly mapped, focused 100x45 frame; delete every other frame."
  (let ((frame (make-frame '((width . 100) (height . 45)))))
    (select-frame frame)
    (mevedel-fc-bench--focus frame)
    (dolist (other (frame-list))
      (unless (or (eq other frame) (frame-parent other)) (delete-frame other)))
    (let ((deadline (+ (float-time) 5)))
      (while (and (/= (frame-width) 100) (< (float-time) deadline))
        (mevedel-fc-bench--settle 0.05)))
    (mevedel-fc-bench--check (and (= (frame-width) 100) (eq t (frame-focus-state frame)))
                             "a focused 100-column frame")))


;;
;;; Entry points

(defun mevedel-fc-bench--prepare ()
  "Check the runtime and set scrolling options."
  (unless (and (display-graphic-p) (not noninteractive))
    (error "A graphical Emacs is required"))
  (dolist (f '(mevedel-floating-composer--update mevedel-view--render-stream-update))
    (unless (compiled-function-p (symbol-function f))
      (error "%s is not compiled" f)))
  (setq mevedel-fc-bench--kwin (mevedel-fc-bench--find-kwin))
  (setq scroll-preserve-screen-position t
        mouse-wheel-scroll-amount '(20 ((shift) . hscroll))
        mouse-wheel-progressive-speed nil
        mevedel-view-spinner-animate t
        ;; Normal rate everywhere, so only the floating composer's own rule
        ;; applies the battery ceiling, whatever the host's power source.
        mevedel-view-spinner-power-policy 'full)
  (add-hook 'post-command-hook #'mevedel-fc-bench--post-command 100))

(defun mevedel-fc-bench-run (output rounds)
  "Run a warm-up round and ROUNDS alternating rounds; write JSON to OUTPUT."
  (mevedel-fc-bench--prepare)
  (dolist (scenario mevedel-fc-bench--scenarios)
    (mevedel-fc-bench--scenario 0 scenario))
  (setq mevedel-fc-bench--records nil)
  (mevedel-fc-bench--instrument)
  (dotimes (round rounds)
    (dolist (scenario (if (cl-oddp round)
                          (reverse mevedel-fc-bench--scenarios)
                        mevedel-fc-bench--scenarios))
      (message "round %d %s" (1+ round) scenario)
      (mevedel-fc-bench--scenario (1+ round) scenario)))
  (with-temp-file output
    (insert (json-serialize
             (list :emacs-version emacs-version
                   :library (symbol-file 'mevedel-floating-composer--update 'defun)
                   :records (vconcat (reverse mevedel-fc-bench--records)))))))

(defun mevedel-fc-bench-probe ()
  "Type into a shown floating composer for the runner's presentation check."
  (mevedel-fc-bench--prepare)
  (mevedel-fc-bench--fresh-frame)
  (let ((mevedel-floating-composer t))
    (mevedel-fc-bench--with-view
     (lambda (_)
       (mevedel-fc-bench--away)
       (mevedel-fc-bench--repeat 30 '(?a))))))

(provide 'mevedel-floating-composer-bench)
;;; mevedel-floating-composer-bench.el ends here
