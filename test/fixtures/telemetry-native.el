;;; telemetry-native.el --- Isolated native heartbeat fixture -*- lexical-binding: t -*-

;;; Commentary:

;; Run only in an isolated Emacs: the native adapter installs process-wide hooks.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'niwa-native)
(require 'niwa-structs)
(require 'niwa-telemetry)

(defvar niwa-test-native-root)

(let ((root niwa-test-native-root)
      (niwa-user-dir niwa-test-native-root))
  (let* ((session (niwa-session--create :session-id "native-lag"
                                           :working-directory root))
         (origin (generate-new-buffer " *niwa-lag-origin*"))
         (noninteractive nil)
         process calls errors ticks
         (record-tick
          (lambda (&rest _)
            (unless (eq (current-thread) main-thread)
              (push (cons (current-buffer)
                          (buffer-local-value 'face-remapping-alist
                                              (current-buffer)))
                    ticks)))))
    (unwind-protect
        (progn
          (advice-add 'niwa-telemetry--lag-tick :before record-tick)
          (with-current-buffer origin
            ;; A view's face remapping (e.g. solaire) must not reach the worker.
            (setq-local face-remapping-alist '((default . bold)))
            (niwa-telemetry--lag-request session 'request-start "native-lag"))
          (should-not niwa-telemetry--lag-timer)
          (should (thread-live-p niwa-telemetry--lag-thread))
          (let ((worker niwa-telemetry--lag-thread))
            (niwa-telemetry--lag-start)
            (should (eq worker niwa-telemetry--lag-thread)))
          ;; The worker may outlive its caller's buffer.
          (sleep-for 0.12)
          (let ((deadline (+ (float-time) 5)))
            (while (and (not ticks) (< (float-time) deadline))
              (sleep-for 0.02)))
          (should ticks)
          ;; The worker samples from its own buffer, not the starter's.
          (dolist (tick ticks)
            (should-not (eq origin (car tick)))
            (should-not (cdr tick)))
          (advice-remove 'niwa-telemetry--lag-tick record-tick)
          (kill-buffer origin)
          (setq process
                (make-process
                 :name "niwa-native-lag-sentinel" :noquery t
                 :command '("sh" "-c" "sleep 0.01")
                 :connection-type 'pipe
                 :sentinel
                 (lambda (child _event)
                   (push (current-thread) calls)
                   (condition-case err
                       (while (accept-process-output child 0.001 nil 1))
                     (error (push err errors))))))
          ;; Only the worker can run during these yields.  Its wait must
          ;; leave the child's status notification for the main thread.
          (let ((deadline (+ (float-time) 5)))
            (while (and (process-live-p process) (< (float-time) deadline))
              (thread-yield)))
          (should-not (process-live-p process))
          (should-not calls)
          (with-timeout (5 (ert-fail "Main-thread sentinel delivery stalled"))
            (while (not calls) (accept-process-output process 0.01)))
          (should calls)
          (should-not errors)
          (should (cl-every (lambda (thread) (eq thread main-thread)) calls))
          (should (thread-live-p niwa-telemetry--lag-thread))
          ;; Preserve the original 100 ms sampler's busy-loop sensitivity.
          (sleep-for 0.12)
          (let ((deadline (+ (float-time) 0.7)))
            (while (< (float-time) deadline)))
          (sleep-for 0.12)
          (let ((lags (seq-filter
                       (lambda (event) (eq (plist-get event :event) 'event-loop-lag))
                       (niwa-session-telemetry-pending session))))
            (should (= 1 (length lags)))
            (should (<= 550 (plist-get (car lags) :delay-ms) 1000)))
          (let ((worker niwa-telemetry--lag-thread))
            (niwa-telemetry--lag-stop)
            (should-not (thread-live-p worker))
            (should-not niwa-telemetry--lag-thread)
            (should-not (advice-member-p #'niwa-telemetry--lag-time-callback
                                         'timer-event-handler)))
          ;; Disabling telemetry is a global user setting, visible to the worker.
          (niwa-telemetry--lag-request session 'request-start "disabled")
          (let ((enabled niwa-telemetry-enabled)
                (worker niwa-telemetry--lag-thread))
            (unwind-protect
                (progn
                  (setq niwa-telemetry-enabled nil)
                  (let ((deadline (+ (float-time) 5)))
                    (while (and (thread-live-p worker) (< (float-time) deadline))
                      (sleep-for 0.01)))
                  (should-not (thread-live-p worker))
                  (should-not niwa-telemetry--lag-thread))
              (setq niwa-telemetry-enabled enabled)))
          ;; Expiry also stops from inside the sampler rather than joining itself.
          (niwa-telemetry--lag-request session 'request-start "tail")
          (niwa-telemetry--lag-request session 'request-settled "tail")
          (setf (plist-get (alist-get session niwa-telemetry--lag-windows) :until)
                (- (float-time) 1))
          (let ((worker niwa-telemetry--lag-thread)
                (deadline (+ (float-time) 5)))
            (while (and (thread-live-p worker) (< (float-time) deadline))
              (sleep-for 0.01))
            (should-not (thread-live-p worker))
            (should-not niwa-telemetry--lag-thread))
          ;; External cancellation must not leave a dead handle blocking restart.
          (let (notices)
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) notices))))
              (niwa-telemetry--lag-request session 'request-start "quit")
              (let ((worker niwa-telemetry--lag-thread)
                    (deadline (+ (float-time) 5)))
                (sleep-for 0.02)
                (thread-signal worker 'quit nil)
                (while (and (thread-live-p worker) (< (float-time) deadline))
                  (sleep-for 0.01))
                (should-not (thread-live-p worker))
                (should-not niwa-telemetry--lag-thread)))
            (should (equal notices '("niwa [global]: Event-loop lag watching stopped: Quit"))))
          (niwa-telemetry--lag-request session 'request-start "restart")
          (should (thread-live-p niwa-telemetry--lag-thread))
          (niwa-telemetry--lag-stop))
      (advice-remove 'niwa-telemetry--lag-tick record-tick)
      (when (buffer-live-p origin) (kill-buffer origin))
      (when process
        (set-process-sentinel process #'ignore)
        (delete-process process))))
  (niwa-telemetry--lag-stop))

;;; telemetry-native.el ends here
