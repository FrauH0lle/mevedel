;;; mevedel-graphical-bench.el --- Graphical view benchmark support -*- lexical-binding: t; -*-

;;; Commentary:

;; Drive commands through the command loop and measure latency, redisplay
;; and CPU in a private presenting KWin.  Never load this into a user's
;; editor: it replaces frames, focus and options.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mwheel)
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)

(defvar gcs-done)

(defvar mevedel-fc-bench--counters (list :redisplays 0)
  "Running redisplay counts.")
(defvar mevedel-fc-bench--pending nil "Non-nil while a measured command runs.")
(defvar mevedel-fc-bench--done nil "Time a measured command finished, or nil.")
(defvar mevedel-fc-bench--records nil "Measurements, newest first.")
(defvar mevedel-fc-bench--kwin nil "Private KWin PID.")


;;
;;; Counters

(defun mevedel-fc-bench--count (key)
  "Increment counter KEY."
  (cl-incf (plist-get mevedel-fc-bench--counters key)))

(defun mevedel-fc-bench--instrument ()
  "Count redisplay cycles."
  (add-function :before pre-redisplay-function
                (lambda (_) (mevedel-fc-bench--count :redisplays))))

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

(defun mevedel-fc-bench--wheel (direction window)
  "Scroll WINDOW one notch in DIRECTION; return the latency in ms."
  (mevedel-fc-bench--command
   (list (list direction (posn-at-point (window-start window) window)))))

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
  (list :focused (if (eq t (frame-focus-state)) t :false)
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
  (unless (eq t (frame-focus-state frame))
    (error "Expected focus on %S; states %S" frame
           (mapcar (lambda (f) (cons f (frame-focus-state f))) (frame-list)))))

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
    (unless (and (= (frame-width) 100) (eq t (frame-focus-state frame)))
      (error "Expected a focused 100-column frame"))))


;;
;;; Entry points

(defun mevedel-fc-bench--prepare ()
  "Check the runtime and set scrolling options."
  (unless (and (display-graphic-p) (not noninteractive))
    (error "A graphical Emacs is required"))
  (unless (compiled-function-p (symbol-function 'mevedel-view--render-stream-update))
    (error "The stream renderer is not compiled"))
  (setq mevedel-fc-bench--kwin (mevedel-fc-bench--find-kwin))
  (setq scroll-preserve-screen-position t
        mouse-wheel-scroll-amount '(20 ((shift) . hscroll))
        mouse-wheel-progressive-speed nil
        mevedel-view-spinner-animate t
        mevedel-view-spinner-power-policy 'full)
  (add-hook 'post-command-hook #'mevedel-fc-bench--post-command 100))

(defun mevedel-fc-bench-run (output rounds scenarios function)
  "Call FUNCTION with round and scenario over SCENARIOS in alternating order.
Run a warm-up round before ROUNDS measured rounds; write JSON to OUTPUT."
  (mevedel-fc-bench--prepare)
  (dolist (scenario scenarios)
    (funcall function 0 scenario))
  (setq mevedel-fc-bench--records nil)
  (mevedel-fc-bench--instrument)
  (dotimes (round rounds)
    (dolist (scenario (if (cl-oddp round)
                          (reverse scenarios)
                        scenarios))
      (message "round %d %s" (1+ round) scenario)
      (funcall function (1+ round) scenario)))
  (with-temp-file output
    (insert (json-serialize
             (list :emacs-version emacs-version
                   :library (symbol-file 'mevedel-view--render-stream-update 'defun)
                   :records (vconcat (reverse mevedel-fc-bench--records)))))))

(provide 'mevedel-graphical-bench)
;;; mevedel-graphical-bench.el ends here
