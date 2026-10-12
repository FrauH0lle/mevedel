;;; telemetry-native.el --- Isolated native heartbeat fixture -*- lexical-binding: t -*-

;;; Commentary:

;; Run only in an isolated Emacs: the native adapter installs process-wide hooks.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'mevedel-native)
(require 'mevedel-structs)
(require 'mevedel-telemetry)

(defvar mevedel-test-native-root)

(let ((root mevedel-test-native-root)
      (mevedel-user-dir mevedel-test-native-root))
  (let* ((session (mevedel-session--create :session-id "native-lag"
                                           :working-directory root))
         (origin (generate-new-buffer " *mevedel-lag-origin*"))
         (noninteractive nil)
         process calls errors)
    (unwind-protect
        (progn
          (with-current-buffer origin
            (mevedel-telemetry--lag-request session 'request-start "native-lag"))
          (should-not mevedel-telemetry--lag-timer)
          (should (thread-live-p mevedel-telemetry--lag-thread))
          (let ((worker mevedel-telemetry--lag-thread))
            (mevedel-telemetry--lag-start)
            (should (eq worker mevedel-telemetry--lag-thread)))
          ;; The worker may outlive its caller's buffer.
          (sleep-for 0.12)
          (kill-buffer origin)
          (setq process
                (make-process
                 :name "mevedel-native-lag-sentinel" :noquery t
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
          (let ((deadline (+ (float-time) 0.2)))
            (while (< (float-time) deadline) (thread-yield)))
          (should-not calls)
          (accept-process-output process 0.01)
          (should calls)
          (should-not errors)
          (should (cl-every (lambda (thread) (eq thread main-thread)) calls))
          (should (thread-live-p mevedel-telemetry--lag-thread))
          ;; Preserve the original 100 ms sampler's busy-loop sensitivity.
          (sleep-for 0.12)
          (let ((deadline (+ (float-time) 0.7)))
            (while (< (float-time) deadline)))
          (sleep-for 0.12)
          (let ((lags (seq-filter
                       (lambda (event) (eq (plist-get event :event) 'event-loop-lag))
                       (mevedel-session-telemetry-pending session))))
            (should (= 1 (length lags)))
            (should (<= 550 (plist-get (car lags) :delay-ms) 1000)))
          (let ((worker mevedel-telemetry--lag-thread))
            (mevedel-telemetry--lag-stop)
            (should-not (thread-live-p worker))
            (should-not mevedel-telemetry--lag-thread)
            (should-not (advice-member-p #'mevedel-telemetry--lag-time-callback
                                         'timer-event-handler)))
          ;; Disabling telemetry is a global user setting, visible to the worker.
          (mevedel-telemetry--lag-request session 'request-start "disabled")
          (let ((enabled mevedel-telemetry-enabled)
                (worker mevedel-telemetry--lag-thread))
            (unwind-protect
                (progn
                  (setq mevedel-telemetry-enabled nil)
                  (let ((deadline (+ (float-time) 1)))
                    (while (and (thread-live-p worker) (< (float-time) deadline))
                      (sleep-for 0.01)))
                  (should-not (thread-live-p worker))
                  (should-not mevedel-telemetry--lag-thread))
              (setq mevedel-telemetry-enabled enabled)))
          ;; Expiry also stops from inside the sampler rather than joining itself.
          (mevedel-telemetry--lag-request session 'request-start "tail")
          (mevedel-telemetry--lag-request session 'request-settled "tail")
          (setf (plist-get (alist-get session mevedel-telemetry--lag-windows) :until)
                (- (float-time) 1))
          (let ((worker mevedel-telemetry--lag-thread)
                (deadline (+ (float-time) 1)))
            (while (and (thread-live-p worker) (< (float-time) deadline))
              (sleep-for 0.01))
            (should-not (thread-live-p worker))
            (should-not mevedel-telemetry--lag-thread))
          ;; External cancellation must not leave a dead handle blocking restart.
          (let (notices)
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) notices))))
              (mevedel-telemetry--lag-request session 'request-start "quit")
              (let ((worker mevedel-telemetry--lag-thread)
                    (deadline (+ (float-time) 1)))
                (sleep-for 0.02)
                (thread-signal worker 'quit nil)
                (while (and (thread-live-p worker) (< (float-time) deadline))
                  (sleep-for 0.01))
                (should-not (thread-live-p worker))
                (should-not mevedel-telemetry--lag-thread)))
            (should (equal notices '("mevedel: event-loop lag watching stopped: Quit"))))
          (mevedel-telemetry--lag-request session 'request-start "restart")
          (should (thread-live-p mevedel-telemetry--lag-thread))
          (mevedel-telemetry--lag-stop))
      (when (buffer-live-p origin) (kill-buffer origin))
      (when process
        (set-process-sentinel process #'ignore)
        (delete-process process))))
  (mevedel-telemetry--lag-stop))

;;; telemetry-native.el ends here
