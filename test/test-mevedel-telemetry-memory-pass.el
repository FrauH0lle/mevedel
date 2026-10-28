;;; test-mevedel-telemetry-memory-pass.el --- Pass lag watching -*- lexical-binding: t -*-

;;; Commentary:

;; Workspace-only memory pass identity, shared heartbeat and lifecycle tests.

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-native)
(require 'mevedel-telemetry)
(require 'mevedel-utilities)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-telemetry-memory-pass--events (workspace &optional event)
  "Read WORKSPACE's persisted rows, optionally selecting EVENT."
  (let ((path (file-name-concat (mevedel-workspace-root workspace)
                                ".mevedel" "state" "diagnostics"
                                mevedel-telemetry-file-name))
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

(defun test-mevedel-telemetry-memory-pass--sample (delay)
  "Deliver the production heartbeat with a synthetic DELAY in seconds."
  (setq mevedel-telemetry--lag-due (- (float-time) delay))
  (mevedel--coalesced-timer-call mevedel-telemetry--lag-timer))

(mevedel-deftest mevedel-telemetry-memory-pass-start
  (:vars ((mevedel-native--load-state "unavailable in fallback tests"))
   :after-each (mevedel-telemetry--lag-stop))
  ,test
  (test)
  :doc "production timer detects a blocked loop only when the pass was watched"
  (dolist (watch '(nil t))
    (let* ((root (make-temp-file "mevedel-pass-lag-" t))
           (workspace (mevedel-workspace--create :root root))
           (noninteractive nil)
           (handle (let ((noninteractive (not watch)))
                     (mevedel-telemetry-memory-pass-start workspace "pass-production"))))
      (unwind-protect
          (progn
            ;; The negative control omits registration, not the stall or timer
            ;; delivery.  No internal lag clock, counter or emitter is stubbed.
            (sleep-for 0.15)
            (let ((end (+ (float-time) 0.75)))
              (while (< (float-time) end)))
            (sleep-for 0.15)
            (let ((lags (test-mevedel-telemetry-memory-pass--events
                         workspace 'event-loop-lag)))
              (if watch
                  (progn
                    (should handle)
                    (should lags)
                    (should-not mevedel-telemetry--lag-windows)
                    (dolist (row lags)
                      (should (equal "pass-production" (plist-get row :pass-id)))
                      (should (eq 'memory (plist-get row :workload)))
                      (should (<= 500 (plist-get row :delay-ms)))
                      (should (numberp (plist-get row :cpu-ms)))
                      (should-not (plist-get row :settled))
                      (dolist (key '(:session-id :request-id :goal-id :preset
                                     :workspace :session :dropped-keys))
                        (should-not (plist-member row key)))
                      (should-not (string-match-p "#<\\|#s(" (prin1-to-string row)))))
                (should-not handle)
                (should-not lags)
                (should-not (file-exists-p (file-name-concat root ".mevedel")))))
            (mevedel-telemetry-memory-pass-settle handle))
        (mevedel-telemetry--lag-stop)
        (delete-directory root t))))

  :doc "disabled telemetry and batch starts allocate no watcher or diagnostics"
  (let* ((root (make-temp-file "mevedel-pass-disabled-" t))
         (workspace (mevedel-workspace--create :root root)))
    (unwind-protect
        (progn
          (let ((noninteractive nil)
                (mevedel-telemetry-enabled nil))
            (should-not (mevedel-telemetry-memory-pass-start workspace "disabled")))
          (let ((noninteractive t))
            (should-not (mevedel-telemetry-memory-pass-start workspace "batch")))
          (should-not (mevedel-telemetry-memory-pass-settle nil))
          (should-not mevedel-telemetry--lag-timer)
          (should-not mevedel-telemetry--lag-thread)
          (should-not mevedel-telemetry--lag-pass-windows)
          (should-not (file-exists-p (file-name-concat root ".mevedel"))))
      (delete-directory root t)))

  :doc "sessions and passes share one sampler in either start order"
  (dolist (pass-first '(nil t))
    (let* ((root (make-temp-file "mevedel-pass-session-" t))
           (workspace (mevedel-workspace--create :root root))
           (session (mevedel-session-create "main" workspace))
           (noninteractive nil)
           handle first)
      (unwind-protect
          (progn
            (if pass-first
                (setq handle (mevedel-telemetry-memory-pass-start workspace "same-id"))
              (mevedel-telemetry-record session 'request-start :request-id "same-id"))
            (setq first mevedel-telemetry--lag-timer)
            (if pass-first
                (mevedel-telemetry-record session 'request-start :request-id "same-id")
              (setq handle (mevedel-telemetry-memory-pass-start workspace "same-id")))
            (should (eq first mevedel-telemetry--lag-timer))
            (should-not mevedel-telemetry--lag-thread)
            (should (= 1 (length mevedel-telemetry--lag-windows)))
            (should (= 1 (length mevedel-telemetry--lag-pass-windows)))
            (test-mevedel-telemetry-memory-pass--sample 0.75)
            (let ((row (car (mevedel-session-telemetry-pending session))))
              (should (eq 'event-loop-lag (plist-get row :event)))
              (should (equal "same-id" (plist-get row :request-id)))
              (should-not (plist-member row :pass-id)))
            (should (equal "same-id" (plist-get
                                      (car (test-mevedel-telemetry-memory-pass--events workspace))
                                      :pass-id)))
            ;; Session-only synchronous measurement must not treat a workspace
            ;; object as a session, even while the shared heartbeat watches it.
            (mevedel-telemetry-measure nil 'control-program 0 (lambda () 'ok))
            (should (= 1 (length (test-mevedel-telemetry-memory-pass--events workspace))))
            (mevedel-telemetry-memory-pass-settle handle)
            (setf (plist-get (cdr handle) :until) (- (float-time) 1))
            (test-mevedel-telemetry-memory-pass--sample 0)
            (should (eq first mevedel-telemetry--lag-timer))
            (should-not mevedel-telemetry--lag-pass-windows)
            (mevedel-telemetry-record session 'request-settled :request-id "same-id")
            (setf (plist-get (alist-get session mevedel-telemetry--lag-windows) :until)
                  (- (float-time) 1))
            (test-mevedel-telemetry-memory-pass--sample 0)
            (should-not mevedel-telemetry--lag-timer)
            (should-not (mevedel--ui-timer-pending-p first)))
        (mevedel-telemetry--lag-stop)
        (delete-directory root t))))

  :doc "sampler startup failure is diagnostic only and releases its pass handle"
  (let* ((root (make-temp-file "mevedel-pass-start-failure-" t))
         (workspace (mevedel-workspace--create :root root))
         (noninteractive nil)
         messages)
    (unwind-protect
        (progn
          (mevedel-test--with-captured-messages messages
            (cl-letf (((symbol-function 'mevedel-telemetry--lag-start)
                       (lambda () (error "Injected sampler failure"))))
              (should-not (mevedel-telemetry-memory-pass-start workspace "failed"))))
          (should (string-match-p "memory pass lag watching failed: Injected sampler failure"
                                  messages))
          (should-not mevedel-telemetry--lag-pass-windows)
          (should-not mevedel-telemetry--lag-timer)
          (should-not (file-exists-p (file-name-concat root ".mevedel"))))
      (delete-directory root t))))

(mevedel-deftest mevedel-telemetry-memory-pass-settle
  (:vars ((mevedel-native--load-state "unavailable in fallback tests"))
   :after-each (mevedel-telemetry--lag-stop))
  ,test
  (test)
  :doc "summarizes once, keeps pass-tagged tail and stops after its expiry"
  (let* ((root (make-temp-file "mevedel-pass-tail-" t))
         (workspace (mevedel-workspace--create :root root))
         (noninteractive nil)
         (handle (mevedel-telemetry-memory-pass-start workspace "tail"))
         (timer mevedel-telemetry--lag-timer))
    (unwind-protect
        (progn
          (test-mevedel-telemetry-memory-pass--sample 0.75)
          (should-not (mevedel-telemetry-memory-pass-settle handle))
          (let ((until (plist-get (cdr handle) :until)))
            (should (< (float-time) until (+ (float-time) 121)))
            (mevedel-telemetry-memory-pass-settle handle)
            (should (= until (plist-get (cdr handle) :until))))
          (test-mevedel-telemetry-memory-pass--sample 1.25)
          (let* ((rows (test-mevedel-telemetry-memory-pass--events workspace))
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
          (test-mevedel-telemetry-memory-pass--sample 0.75)
          (should-not mevedel-telemetry--lag-timer)
          (should-not mevedel-telemetry--lag-pass-windows)
          (should-not (mevedel--ui-timer-pending-p timer))
          (should-not (advice-member-p #'mevedel-telemetry--lag-time-callback
                                       'timer-event-handler))
          (should-not (advice-member-p #'mevedel-telemetry--lag-time-callback
                                       'mevedel--coalesced-timer-call))
          (should-not (advice-member-p #'mevedel-telemetry--lag-time-remote
                                       'mevedel-transport-call-as-remote-operation))
          (mevedel-telemetry-memory-pass-settle handle)
          (should (= 3 (length (test-mevedel-telemetry-memory-pass--events workspace)))))
      (mevedel-telemetry--lag-stop)
      (delete-directory root t)))

  :doc "overlapping workspaces and passes keep independent identity and counts"
  (let* ((root (make-temp-file "mevedel-pass-overlap-" t))
         (one (mevedel-workspace--create :root (file-name-concat root "one")))
         (two (mevedel-workspace--create :root (file-name-concat root "two")))
         (noninteractive nil)
         (a (mevedel-telemetry-memory-pass-start one "same"))
         (b (mevedel-telemetry-memory-pass-start two "same"))
         (timer mevedel-telemetry--lag-timer))
    (unwind-protect
        (progn
          (test-mevedel-telemetry-memory-pass--sample 0.75)
          (mevedel-telemetry-memory-pass-settle a)
          (let ((c (mevedel-telemetry-memory-pass-start one "next")))
            (should (eq timer mevedel-telemetry--lag-timer))
            (test-mevedel-telemetry-memory-pass--sample 0.75)
            (mevedel-telemetry-memory-pass-settle b)
            (mevedel-telemetry-memory-pass-settle c)
            (let ((one-rows (test-mevedel-telemetry-memory-pass--events one))
                  (two-summary (car (test-mevedel-telemetry-memory-pass--events
                                     two 'event-loop-lag-summary))))
              (should (= 5 (length one-rows)))
              (should (equal '("same" "same" "next" "same" "next")
                             (mapcar (lambda (row) (plist-get row :pass-id)) one-rows)))
              (should (plist-get (nth 3 one-rows) :settled))
              (should-not (plist-get (nth 2 one-rows) :settled))
              (should (= 2 (plist-get two-summary :over-500-ms)))
              (should (= 1 (plist-get (car (last one-rows)) :over-500-ms)))))
          (mevedel-telemetry--lag-stop)
          ;; A stale callback cannot settle a later registration, even with
          ;; the same workspace and pass identifier.
          (let ((fresh (mevedel-telemetry-memory-pass-start one "same")))
            (mevedel-telemetry-memory-pass-settle a)
            (should-not (plist-get (cdr fresh) :until))))
      (mevedel-telemetry--lag-stop)
      (delete-directory root t)))

  :doc "disabling mid-pass clears every window without a summary or tail"
  (dolist (settle '(nil t))
    (let* ((root (make-temp-file "mevedel-pass-disable-running-" t))
           (workspace (mevedel-workspace--create :root root))
           (noninteractive nil)
           (handle (mevedel-telemetry-memory-pass-start workspace "disabled"))
           (timer mevedel-telemetry--lag-timer))
      (unwind-protect
          (progn
            (let ((mevedel-telemetry-enabled nil))
              (if settle
                  (mevedel-telemetry-memory-pass-settle handle)
                (mevedel--coalesced-timer-call timer)))
            (should-not mevedel-telemetry--lag-pass-windows)
            (should-not mevedel-telemetry--lag-windows)
            (should-not mevedel-telemetry--lag-timer)
            (should-not (mevedel--ui-timer-pending-p timer))
            (mevedel-telemetry-memory-pass-settle handle)
            (should-not (file-exists-p (file-name-concat root ".mevedel"))))
        (mevedel-telemetry--lag-stop)
        (delete-directory root t)))))

(provide 'test-mevedel-telemetry-memory-pass)
;;; test-mevedel-telemetry-memory-pass.el ends here
