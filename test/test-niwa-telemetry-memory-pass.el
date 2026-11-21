;;; test-niwa-telemetry-memory-pass.el --- Pass lag watching -*- lexical-binding: t -*-

;;; Commentary:

;; Workspace-only memory pass identity, shared heartbeat and lifecycle tests.

;;; Code:

(require 'niwa-structs)
(require 'niwa-native)
(require 'niwa-telemetry)
(require 'niwa-utilities)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-niwa-telemetry-memory-pass--events (workspace &optional event)
  "Read WORKSPACE's persisted rows, optionally selecting EVENT."
  (let ((path (file-name-concat (niwa-workspace-root workspace)
                                ".niwa" "state" "diagnostics"
                                niwa-telemetry-file-name))
        rows)
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (condition-case nil
            (while t (push (read (current-buffer)) rows))
          (end-of-file))))
    (seq-filter (lambda (row) (or (null event) (eq event (plist-get row :event))))
                (nreverse rows))))

(defun test-niwa-telemetry-memory-pass--sample (delay)
  "Deliver the production heartbeat with a synthetic DELAY in seconds."
  (setq niwa-telemetry--lag-due (- (float-time) delay))
  (niwa--coalesced-timer-call niwa-telemetry--lag-timer))

(niwa-deftest niwa-telemetry-memory-pass-start
  (:vars ((niwa-native--load-state "unavailable in fallback tests"))
   :after-each (niwa-telemetry--lag-stop))
  ,test
  (test)
  :doc "production timer detects a blocked loop only when the pass was watched"
  (dolist (watch '(nil t))
    (let* ((root (make-temp-file "niwa-pass-lag-" t))
           (workspace (niwa-workspace--create :root root))
           (noninteractive nil)
           ;; Keep the real sampler and matched unwatched control.  A 200 ms
           ;; stall clears the 100 ms gate by 80 ms after one sample interval.
           (niwa-telemetry-lag-threshold .1)
           (niwa-telemetry--lag-interval .02)
           (niwa-telemetry--lag-count-threshold .04)
           (handle (let ((noninteractive (not watch)))
                     (niwa-telemetry-memory-pass-start workspace "pass-production"))))
      (unwind-protect
          (progn
            ;; The negative control omits registration, not the stall or timer
            ;; delivery.  No internal lag clock, counter or emitter is stubbed.
            (sleep-for 0.03)
            (let ((end (+ (float-time) 0.2)))
              (while (< (float-time) end)))
            (sleep-for 0.03)
            (let ((lags (test-niwa-telemetry-memory-pass--events
                         workspace 'event-loop-lag)))
              (if watch
                  (progn
                    (should handle)
                    (should lags)
                    (should-not niwa-telemetry--lag-windows)
                    (dolist (row lags)
                      (should (equal "pass-production" (plist-get row :pass-id)))
                      (should (eq 'memory (plist-get row :workload)))
                      (should (<= 150 (plist-get row :delay-ms)))
                      (should (numberp (plist-get row :cpu-ms)))
                      (should-not (plist-get row :settled))
                      (dolist (key '(:session-id :request-id :goal-id :preset
                                     :workspace :session :dropped-keys))
                        (should-not (plist-member row key)))
                      (should-not (string-match-p "#<\\|#s(" (prin1-to-string row)))))
                (should-not handle)
                (should-not lags)
                (should-not (file-exists-p (file-name-concat root ".niwa")))))
            (niwa-telemetry-memory-pass-settle handle))
        (niwa-telemetry--lag-stop)
        (delete-directory root t))))

  :doc "disabled telemetry and batch starts allocate no watcher or diagnostics"
  (let* ((root (make-temp-file "niwa-pass-disabled-" t))
         (workspace (niwa-workspace--create :root root)))
    (unwind-protect
        (progn
          (let ((noninteractive nil)
                (niwa-telemetry-enabled nil))
            (should-not (niwa-telemetry-memory-pass-start workspace "disabled")))
          (let ((noninteractive t))
            (should-not (niwa-telemetry-memory-pass-start workspace "batch")))
          (should-not (niwa-telemetry-memory-pass-settle nil))
          (should-not niwa-telemetry--lag-timer)
          (should-not niwa-telemetry--lag-thread)
          (should-not niwa-telemetry--lag-pass-windows)
          (should-not (file-exists-p (file-name-concat root ".niwa"))))
      (delete-directory root t)))

  :doc "sessions and passes share one sampler in either start order"
  (dolist (pass-first '(nil t))
    (let* ((root (make-temp-file "niwa-pass-session-" t))
           (workspace (niwa-workspace--create :root root))
           (session (niwa-session-create "main" workspace))
           (noninteractive nil)
           handle first)
      (unwind-protect
          (progn
            (if pass-first
                (setq handle (niwa-telemetry-memory-pass-start workspace "same-id"))
              (niwa-telemetry-record session 'request-start :request-id "same-id"))
            (setq first niwa-telemetry--lag-timer)
            (if pass-first
                (niwa-telemetry-record session 'request-start :request-id "same-id")
              (setq handle (niwa-telemetry-memory-pass-start workspace "same-id")))
            (should (eq first niwa-telemetry--lag-timer))
            (should-not niwa-telemetry--lag-thread)
            (should (= 1 (length niwa-telemetry--lag-windows)))
            (should (= 1 (length niwa-telemetry--lag-pass-windows)))
            (test-niwa-telemetry-memory-pass--sample 0.75)
            ;; Slow control programs can precede the lag row in the session's
            ;; pending stream.  Assert the exact lag cohort, not stream order.
            (let ((rows (seq-filter (lambda (row) (eq 'event-loop-lag (plist-get row :event)))
                                    (niwa-session-telemetry-pending session))))
              (should (= 1 (length rows)))
              (let ((row (car rows)))
                (should (eq 'event-loop-lag (plist-get row :event)))
                (should (equal "same-id" (plist-get row :request-id)))
                (should-not (plist-member row :pass-id))))
            (let ((rows (test-niwa-telemetry-memory-pass--events workspace 'event-loop-lag)))
              (should (= 1 (length rows)))
              (should (equal "same-id" (plist-get (car rows) :pass-id))))
            ;; Session-only synchronous measurement must not treat a workspace
            ;; object as a session, even while the shared heartbeat watches it.
            (niwa-telemetry-measure nil 'control-program 0 (lambda () 'ok))
            (should (= 1 (length (test-niwa-telemetry-memory-pass--events workspace))))
            (niwa-telemetry-memory-pass-settle handle)
            (setf (plist-get (cdr handle) :until) (- (float-time) 1))
            (test-niwa-telemetry-memory-pass--sample 0)
            (should (eq first niwa-telemetry--lag-timer))
            (should-not niwa-telemetry--lag-pass-windows)
            (niwa-telemetry-record session 'request-settled :request-id "same-id")
            (setf (plist-get (alist-get session niwa-telemetry--lag-windows) :until)
                  (- (float-time) 1))
            (test-niwa-telemetry-memory-pass--sample 0)
            (should-not niwa-telemetry--lag-timer)
            (should-not (niwa--ui-timer-pending-p first)))
        (niwa-telemetry--lag-stop)
        (delete-directory root t))))

  :doc "sampler startup failure is diagnostic only and releases its pass handle"
  (let* ((root (make-temp-file "niwa-pass-start-failure-" t))
         (workspace (niwa-workspace--create :root root))
         (noninteractive nil)
         messages)
    (unwind-protect
        (progn
          (niwa-test--with-captured-messages messages
            (cl-letf (((symbol-function 'niwa-telemetry--lag-start)
                       (lambda () (error "Injected sampler failure"))))
              (should-not (niwa-telemetry-memory-pass-start workspace "failed"))))
          (should (string-match-p "memory pass lag watching failed: Injected sampler failure"
                                  messages))
          (should-not niwa-telemetry--lag-pass-windows)
          (should-not niwa-telemetry--lag-timer)
          (should-not (file-exists-p (file-name-concat root ".niwa"))))
      (delete-directory root t))))

(niwa-deftest niwa-telemetry-memory-pass-settle
  (:vars ((niwa-native--load-state "unavailable in fallback tests"))
   :after-each (niwa-telemetry--lag-stop))
  ,test
  (test)
  :doc "summarizes once, keeps pass-tagged tail and stops after its expiry"
  (let* ((root (make-temp-file "niwa-pass-tail-" t))
         (workspace (niwa-workspace--create :root root))
         (noninteractive nil)
         (handle (niwa-telemetry-memory-pass-start workspace "tail"))
         (timer niwa-telemetry--lag-timer))
    (unwind-protect
        (progn
          (test-niwa-telemetry-memory-pass--sample 0.75)
          (should-not (niwa-telemetry-memory-pass-settle handle))
          (let ((until (plist-get (cdr handle) :until)))
            (should (< (float-time) until (+ (float-time) 121)))
            (niwa-telemetry-memory-pass-settle handle)
            (should (= until (plist-get (cdr handle) :until))))
          (test-niwa-telemetry-memory-pass--sample 1.25)
          (let* ((rows (test-niwa-telemetry-memory-pass--events workspace))
                 (summary (nth 1 rows))
                 (tail (nth 2 rows)))
            (should (= 3 (length rows)))
            (should (eq 'event-loop-lag-summary (plist-get summary :event)))
            (should (equal "tail" (plist-get summary :pass-id)))
            (should (= 1 (plist-get summary :over-200-ms)))
            (should (= 1 (plist-get summary :over-500-ms)))
            (should (= 0 (plist-get summary :over-1000-ms)))
            (should (<= 750 (plist-get summary :max-ms)))
            (should (eq 'event-loop-lag (plist-get tail :event)))
            (should (equal "tail" (plist-get tail :pass-id)))
            (should (eq t (plist-get tail :settled))))
          (setf (plist-get (cdr handle) :until) (- (float-time) 1))
          (test-niwa-telemetry-memory-pass--sample 0.75)
          (should-not niwa-telemetry--lag-timer)
          (should-not niwa-telemetry--lag-pass-windows)
          (should-not (niwa--ui-timer-pending-p timer))
          (should-not (advice-member-p #'niwa-telemetry--lag-time-callback
                                       'timer-event-handler))
          (should-not (advice-member-p #'niwa-telemetry--lag-time-callback
                                       'niwa--coalesced-timer-call))
          (should-not (advice-member-p #'niwa-telemetry--lag-time-remote
                                       'niwa-transport-call-as-remote-operation))
          (niwa-telemetry-memory-pass-settle handle)
          (should (= 3 (length (test-niwa-telemetry-memory-pass--events workspace)))))
      (niwa-telemetry--lag-stop)
      (delete-directory root t)))

  :doc "overlapping workspaces and passes keep independent identity and counts"
  (let* ((root (make-temp-file "niwa-pass-overlap-" t))
         (one (progn (make-directory (file-name-concat root "one"))
                     (niwa-workspace--create :root (file-name-concat root "one"))))
         (two (progn (make-directory (file-name-concat root "two"))
                     (niwa-workspace--create :root (file-name-concat root "two"))))
         (noninteractive nil)
         (a (niwa-telemetry-memory-pass-start one "same"))
         (b (niwa-telemetry-memory-pass-start two "same"))
         (timer niwa-telemetry--lag-timer))
    (unwind-protect
        (progn
          (test-niwa-telemetry-memory-pass--sample 0.75)
          (niwa-telemetry-memory-pass-settle a)
          (let ((c (niwa-telemetry-memory-pass-start one "next")))
            (should (eq timer niwa-telemetry--lag-timer))
            (test-niwa-telemetry-memory-pass--sample 0.75)
            (niwa-telemetry-memory-pass-settle b)
            (niwa-telemetry-memory-pass-settle c)
            (let ((one-rows (test-niwa-telemetry-memory-pass--events one))
                  (two-summary (car (test-niwa-telemetry-memory-pass--events
                                     two 'event-loop-lag-summary))))
              (should (= 5 (length one-rows)))
              (should (equal '("same" "same" "next" "same" "next")
                             (mapcar (lambda (row) (plist-get row :pass-id)) one-rows)))
              (should (plist-get (nth 3 one-rows) :settled))
              (should-not (plist-get (nth 2 one-rows) :settled))
              (should (= 2 (plist-get two-summary :over-500-ms)))
              (should (= 1 (plist-get (car (last one-rows)) :over-500-ms)))))
          (niwa-telemetry--lag-stop)
          ;; A stale callback cannot settle a later registration, even with
          ;; the same workspace and pass identifier.
          (let ((fresh (niwa-telemetry-memory-pass-start one "same")))
            (niwa-telemetry-memory-pass-settle a)
            (should-not (plist-get (cdr fresh) :until))))
      (niwa-telemetry--lag-stop)
      (delete-directory root t)))

  :doc "disabling mid-pass clears every window without a summary or tail"
  (dolist (settle '(nil t))
    (let* ((root (make-temp-file "niwa-pass-disable-running-" t))
           (workspace (niwa-workspace--create :root root))
           (noninteractive nil)
           (handle (niwa-telemetry-memory-pass-start workspace "disabled"))
           (timer niwa-telemetry--lag-timer))
      (unwind-protect
          (progn
            (let ((niwa-telemetry-enabled nil))
              (if settle
                  (niwa-telemetry-memory-pass-settle handle)
                (niwa--coalesced-timer-call timer)))
            (should-not niwa-telemetry--lag-pass-windows)
            (should-not niwa-telemetry--lag-windows)
            (should-not niwa-telemetry--lag-timer)
            (should-not (niwa--ui-timer-pending-p timer))
            (niwa-telemetry-memory-pass-settle handle)
            (should-not (file-exists-p (file-name-concat root ".niwa"))))
        (niwa-telemetry--lag-stop)
        (delete-directory root t)))))

(provide 'test-niwa-telemetry-memory-pass)
;;; test-niwa-telemetry-memory-pass.el ends here
