;;; test-mevedel-telemetry-lag-observer.el --- Diagnostic observer cost -*- lexical-binding: t -*-

;;; Commentary:

;; Count real target programs and inject slow persistence through the production
;; remote-operation wrapper.  Observer exclusion is not a performance claim:
;; synchronous batched persistence still blocks input while its program runs.

;;; Code:

(require 'mevedel-native)
(require 'mevedel-session-control-fs)
(require 'mevedel-telemetry)
(require 'mevedel-transport)
(require 'mevedel-utilities)
(require 'tramp)
(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-telemetry-lag-observer--rows (root)
  "Return persisted diagnostic rows below ROOT."
  (with-temp-buffer
    (insert-file-contents (file-name-concat root ".mevedel" "state" "diagnostics" mevedel-telemetry-file-name))
    (goto-char (point-min))
    (let (rows)
      (condition-case nil (while t (push (read (current-buffer)) rows))
        (end-of-file (nreverse rows))))))

(mevedel-deftest mevedel-telemetry--append-workspace
  (:vars ((mevedel-native--load-state "unavailable in observer tests"))
   :after-each (mevedel-telemetry--lag-stop))
  ,test
  (test)
  :doc "one and three pass windows use one program per distinct workspace and quiet samples write nothing"
  (dolist (count '(1 3))
    (let* ((root (make-temp-file "mevedel-observer-batch-" t))
           (other (make-temp-file "mevedel-observer-other-" t))
           (workspace (mevedel-workspace--create :root root))
           (second (mevedel-workspace--create :root other))
           (noninteractive nil)
           (run (symbol-function 'mevedel-session-control-fs-run-program))
           (programs 0) handles)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                     (lambda (operations &optional lock)
                       (when (seq-some (lambda (op) (eq (plist-get op :op) 'append-rotating)) operations)
                         (cl-incf programs)
                         (should (= 4 (length operations)))
                         (should lock))
                       (funcall run operations lock))))
            (dotimes (index count)
              (push (mevedel-telemetry-memory-pass-start workspace (format "pass-%d" index)) handles))
            (setq mevedel-telemetry--lag-due (- (float-time) .75))
            (mevedel-telemetry--lag-tick)
            (should (= 1 programs))
            (let ((rows (test-mevedel-telemetry-lag-observer--rows root)))
              (should (= count (length rows)))
              (should (= count (length (delete-dups (mapcar (lambda (row) (plist-get row :pass-id)) rows))))))
            (setq mevedel-telemetry--lag-due (+ (float-time) 1))
            (mevedel-telemetry--lag-tick)
            (should (= 1 programs))
            (mevedel-telemetry-memory-pass-start second "second-workspace")
            (setq mevedel-telemetry--lag-due (- (float-time) .75))
            (mevedel-telemetry--lag-tick)
            (should (= 3 programs))
            (should (= 1 (length (test-mevedel-telemetry-lag-observer--rows other))))
            (dolist (handle handles)
              (mevedel-telemetry-memory-pass-settle handle)
              (mevedel-telemetry-memory-pass-settle handle))
            (should (= count (length (seq-filter (lambda (row) (eq (plist-get row :event) 'event-loop-lag-summary))
                                                 (test-mevedel-telemetry-lag-observer--rows root))))))
        (mevedel-telemetry--lag-stop)
        (delete-directory root t)
        (delete-directory other t)))))

(mevedel-deftest mevedel-telemetry--lag-observer-call
  (:vars ((mevedel-native--load-state "unavailable in observer tests"))
   :after-each (mevedel-telemetry--lag-stop))
  ,test
  (test)
  :doc "slow target appends do not renew lag on fallback or native due clocks; equal ordinary operations remain visible"
  (dolist (clock '(fallback native-clock))
    (let* ((root (make-temp-file "mevedel-observer-feedback-" t))
           (workspace (mevedel-workspace--create :root root))
           (noninteractive nil)
           (handle (mevedel-telemetry-memory-pass-start workspace "observed"))
           (timer (timer-create))
           (run (symbol-function 'mevedel-session-control-fs-run-program))
           (mevedel-telemetry--lag-thread (and (eq clock 'native-clock) main-thread))
           (mevedel-telemetry--lag-timer (and (eq clock 'fallback) mevedel-telemetry--lag-timer))
           (slow (lambda () (let ((end (+ (float-time) .6))) (while (< (float-time) end))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                     (lambda (operations &optional lock)
                       (if (seq-some (lambda (op) (eq (plist-get op :op) 'append-rotating)) operations)
                           (progn
                             (mevedel-transport-call-as-remote-operation
                              (lambda ()
                                (funcall slow)
                                ;; Model either sampler waking while a slow
                                ;; diagnostic target program is still in flight.
                                (mevedel-telemetry--lag-tick)))
                             (funcall run operations lock))
                         (funcall run operations lock)))))
            (setq mevedel-telemetry--lag-due (- (float-time) .75))
            (mevedel-telemetry--lag-tick)
            (should (= 1 (length (test-mevedel-telemetry-lag-observer--rows root))))
            (mevedel-telemetry--lag-tick)
            (should (= 1 (length (test-mevedel-telemetry-lag-observer--rows root))))
            ;; The observer wait must not be attributed to an enclosing timer.
            (timer-set-time timer (current-time))
            (timer-set-function timer (lambda () (mevedel-telemetry-record-workspace workspace 'memory-consolidation-fired)))
            (mevedel-telemetry--lag-time-callback #'mevedel--coalesced-timer-call timer)
            (should (< mevedel-telemetry--lag-late .2))
            (mevedel-telemetry--lag-tick)
            (should (= 2 (length (test-mevedel-telemetry-lag-observer--rows root))))
            ;; Negative controls retain real remote and timer stalls, including
            ;; the timer name and measured duration in the persisted event.
            (mevedel-transport-call-as-remote-operation slow)
            (mevedel-telemetry--lag-tick)
            (should (= 3 (length (test-mevedel-telemetry-lag-observer--rows root))))
            (timer-set-time timer (current-time))
            (timer-set-function timer slow)
            (mevedel-telemetry--lag-time-callback #'mevedel--coalesced-timer-call timer)
            (mevedel-telemetry--lag-tick)
            (let ((row (car (last (test-mevedel-telemetry-lag-observer--rows root)))))
              (should (eq 'event-loop-lag (plist-get row :event)))
              (should (stringp (plist-get row :timer-callback)))
              (should (>= (plist-get row :timer-ms) 500)))
            (should-not (plist-get (cdr handle) :until)))
        (mevedel-telemetry--lag-stop)
        (delete-directory root t)))))

(mevedel-deftest mevedel-telemetry--lag-time-callback/observer
  (:vars ((mevedel-native--load-state "unavailable in observer tests"))
   :after-each (mevedel-telemetry--lag-stop))
  ,test
  (test)
  :doc "nested timers retain ordinary work while synchronous transport plumbing remains diagnostic IO"
  (dolist (omit-nested-accounting '(nil t))
  (dolist (work '(diagnostic application transport))
    (let* ((root (make-temp-file "mevedel-observer-nested-" t))
           (other (make-temp-file "mevedel-observer-nested-other-" t))
           (workspace (mevedel-workspace--create :root root))
           (second (mevedel-workspace--create :root other))
           (noninteractive nil)
           (timer (timer-create))
           (run (symbol-function 'mevedel-session-control-fs-run-program))
           (observe (symbol-function 'mevedel-telemetry--lag-observer-call))
           (slow (lambda () (let ((end (+ (float-time) .6))) (while (< (float-time) end)))))
           injected)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-telemetry--lag-observer-call)
                     (lambda (function)
                       ;; Discriminating negative control: reproduce the
                       ;; uncharged nested append, without changing its IO.
                       (if (and omit-nested-accounting mevedel-telemetry--lag-observer-start)
                           (funcall function)
                         (funcall observe function))))
                    ((symbol-function 'mevedel-session-control-fs-run-program)
                     (lambda (operations &optional lock)
                       (when (seq-some (lambda (op) (eq (plist-get op :op) 'append-rotating)) operations)
                         (if injected
                             (funcall slow)
                           (setq injected t)
                           (should mevedel-telemetry--lag-observer-start)
                           (if (eq work 'transport)
                               ;; Synchronous nesting is transport plumbing,
                               ;; not evidence that a callback was dispatched.
                               (mevedel-transport-call-as-remote-operation
                                (lambda () (mevedel-transport-call-as-remote-operation slow)))
                             (timer-set-time timer (current-time))
                             (timer-set-function
                              timer (if (eq work 'application) slow
                                      (lambda () (mevedel-telemetry-record-workspace
                                                  second 'memory-consolidation-fired))))
                             (mevedel-telemetry--lag-time-callback #'mevedel--coalesced-timer-call timer))))
                       (funcall run operations lock))))
            (mevedel-telemetry-memory-pass-start workspace "nested")
            (mevedel-telemetry-record-workspace workspace 'memory-consolidation-fired)
            (should injected)
            (mevedel-telemetry--lag-tick)
            (let ((lags (seq-filter (lambda (row) (eq (plist-get row :event) 'event-loop-lag))
                                    (test-mevedel-telemetry-lag-observer--rows root))))
              (should (= (if (or (eq work 'application)
                                (and omit-nested-accounting (eq work 'diagnostic))) 1 0)
                         (length lags)))))
        (mevedel-telemetry--lag-stop)
        (delete-directory root t)
        (delete-directory other t))))))

(mevedel-deftest mevedel-telemetry--lag-time-remote/ownership
  (:vars ((mevedel-native--load-state "unavailable in observer tests"))
   :quiet t
   :after-each (mevedel-telemetry--lag-stop))
  ,test
  (test)
  :doc "production pipe and request-file transport nesting is diagnostic IO, unlike an actually dispatched sentinel"
  (dolist (omit-dispatch-discrimination '(nil t))
  (dolist (scenario (if omit-dispatch-discrimination '(pipe) '(pipe request-file sentinel)))
    (let* ((root (make-temp-file "mevedel-observer-ownership-" t))
           (noninteractive nil)
           (enabled mevedel-transport--enabled-p)
           (handler (symbol-function 'tramp-file-name-handler))
           (spawn (symbol-function 'make-process))
           (classify (symbol-function 'mevedel-telemetry--lag-dispatched-in-observer-p))
           (mevedel-session-control-fs--stage-local nil)
           (slow (lambda () (let ((end (+ (float-time) .6))) (while (< (float-time) end)))))
           (injections 0) sentinel-process sentinel-ran)
      (unwind-protect
          (mevedel-test--with-local-shell-tramp '("observer-ownership")
            (let ((workspace (mevedel-workspace--create
                              :root (if (eq scenario 'request-file)
                                        (format "/mevedelmock:observer-ownership:%s/" root)
                                      root))))
              (mevedel-transport-install)
              (mevedel-telemetry-memory-pass-start workspace "owned-transport")
              (cl-letf (((symbol-function 'mevedel-telemetry--lag-dispatched-in-observer-p)
                         (lambda ()
                           ;; Recreate the old same-level classification with
                           ;; exactly the same production pipe and handler IO.
                           (or omit-dispatch-discrimination (funcall classify))))
                        ((symbol-function 'mevedel-session-control-fs--program-arguments) #'ignore)
                        ((symbol-function 'mevedel-session-control-fs--pipe-capable-p)
                         (lambda (_) (not (eq scenario 'request-file))))
                        ((symbol-function 'tramp-file-name-handler)
                         (lambda (operation &rest args)
                           ;; Retain the enabled production transport advice;
                           ;; inject only the handler's underlying operation.
                           (mevedel-transport--handler-advice
                            (lambda (&rest _)
                              (when (and (zerop injections) (eq scenario 'request-file)
                                         (eq operation 'copy-file)
                                         (string-match-p "mevedel-control-fs-program" (car args)))
                                (cl-incf injections)
                                (funcall slow))
                              (apply handler operation args))
                            operation args)))
                        ((symbol-function 'make-process)
                         (lambda (&rest args)
                           (let ((process
                                  (if (and (eq scenario 'pipe)
                                           (equal (plist-get args :name) "mevedel-control-fs")
                                           (zerop injections))
                                      ;; Use the existing forced-local pipe
                                      ;; fixture; inject just the TRAMP handler
                                      ;; edge that remote make-process adds.
                                      (mevedel-transport--handler-advice
                                       (lambda (&rest _)
                                         (cl-incf injections)
                                         (funcall slow)
                                         (apply spawn args))
                                       'make-process args)
                                    (apply spawn args))))
                             (when (and (eq scenario 'sentinel)
                                        (equal (plist-get args :name) "mevedel-control-fs")
                                        (zerop injections))
                               (cl-incf injections)
                               (let ((default-directory root))
                                 (setq sentinel-process
                                       (funcall spawn :name "observer-unrelated" :command '("sh" "-c" "exit 0")
                                                :noquery t :connection-type 'pipe
                                                :sentinel
                                                (lambda (child _event)
                                                  (when (and (not sentinel-ran) (not (process-live-p child)))
                                                    (setq sentinel-ran t)
                                                    (should mevedel-telemetry--lag-observer-start)
                                                    (mevedel-transport-call-as-remote-operation slow)))))
                                 (let ((deadline (+ (float-time) 5)))
                                   (while (and (not sentinel-ran) (< (float-time) deadline))
                                     (accept-process-output sentinel-process .01)))))
                             process))))
                (mevedel-telemetry-record-workspace workspace 'memory-consolidation-fired))
              (should (= 1 injections))
              (should (eq (eq scenario 'sentinel) sentinel-ran))
              (mevedel-telemetry--lag-tick)
              (let ((lags (seq-filter (lambda (row) (eq (plist-get row :event) 'event-loop-lag))
                                      (test-mevedel-telemetry-lag-observer--rows root))))
                (should (= (if (or omit-dispatch-discrimination (eq scenario 'sentinel)) 1 0)
                           (length lags))))))
        (when (process-live-p sentinel-process) (delete-process sentinel-process))
        (unless enabled (mevedel-transport-uninstall))
        (mevedel-telemetry--lag-stop)
        (delete-directory root t))))))

(mevedel-deftest mevedel-telemetry--lag-dispatched-in-observer-p ()
  ,test
  (test)
  :doc "bounded function-only scans require a dispatch boundary before the exact observer frame"
  (dolist (case '(((accept-process-output mevedel-telemetry--lag-observer-remote-call) 96 t)
                  ((mevedel-telemetry--lag-observer-remote-call accept-process-output) 96 nil)
                  ((accept-process-output unknown) 96 nil)
                  ((accept-process-output mevedel-telemetry--lag-observer-remote-call) 1 nil)
                  ((sit-for mevedel-telemetry--lag-observer-remote-call) 0 nil)))
    (let ((mevedel-telemetry--lag-dispatch-scan-limit (cadr case)))
      (cl-letf (((symbol-function 'backtrace-frame)
                 (lambda (index)
                   (when-let* ((function (nth index (car case)))) (list t function)))))
        (should (eq (nth 2 case) (mevedel-telemetry--lag-dispatched-in-observer-p)))))))

(mevedel-deftest mevedel-telemetry--lag-time-remote/threads
  (:vars ((mevedel-native--load-state "unavailable in observer tests"))
   :after-each (mevedel-telemetry--lag-stop))
  ,test
  (test)
  :doc "another thread's global observer marker cannot exempt ordinary main-thread remote work"
  (let* ((root (make-temp-file "mevedel-observer-thread-" t))
         (workspace (mevedel-workspace--create :root root))
         (noninteractive nil))
    (unwind-protect
        (progn
          (mevedel-telemetry-memory-pass-start workspace "thread-ownership")
          (let ((mevedel-telemetry--lag-observer-start (float-time))
                (mevedel-telemetry--lag-observer-level 0))
            (mevedel-transport-call-as-remote-operation
             (lambda () (let ((end (+ (float-time) .6))) (while (< (float-time) end))))))
          (should (>= mevedel-telemetry--lag-late .5))
          (mevedel-telemetry--lag-tick)
          (should (= 1 (length (test-mevedel-telemetry-lag-observer--rows root))))
          ;; Within this thread's own diagnostic scope, an exhausted or missing
          ;; frame scan retains observer ownership rather than inventing work.
          (let ((mevedel-telemetry--lag-observer-start (float-time))
                (mevedel-telemetry--lag-observer-level 1)
                (mevedel-telemetry--lag-remote-observer-level 1)
                (mevedel-telemetry--lag-dispatch-scan-limit 0)
                (mevedel-telemetry--lag-late 0))
            (mevedel-transport-call-as-remote-operation
             (lambda () (let ((end (+ (float-time) .6))) (while (< (float-time) end)))))
            (should (< mevedel-telemetry--lag-late .2))))
      (mevedel-telemetry--lag-stop)
      (delete-directory root t))))

(mevedel-deftest mevedel-telemetry--lag-observer-call/threads
  (:vars ((mevedel-native--load-state "unavailable in observer tests"))
   :after-each (mevedel-telemetry--lag-stop))
  ,test
  (test)
  :doc "crossed thread completion clears shared ownership without subtracting worker IO from main-thread work"
  (dolist (scenario '(crossed ordinary restart))
    (let* ((root (make-temp-file "mevedel-observer-crossed-" t))
           (workspace (mevedel-workspace--create :root root))
           (noninteractive nil)
           (before mevedel-telemetry--lag-observer-seconds)
           worker entered release done failure worker-cost)
      (cl-labels ((await (predicate)
                    (let ((deadline (+ (float-time) 5)))
                      (while (and (not (funcall predicate)) (< (float-time) deadline))
                        (thread-yield))
                      (should (funcall predicate))))
                  (finish-worker ()
                    (setq release t)
                    (await (lambda () done))
                    (thread-join worker)
                    (should-not failure)
                    (should (> worker-cost 0)))
                  (ordinary-work ()
                    (mevedel-transport-call-as-remote-operation
                     (lambda ()
                       (when (not done) (finish-worker))
                       (let ((end (+ (float-time) .6))) (while (< (float-time) end)))))))
        (unwind-protect
            (progn
              (mevedel-telemetry-memory-pass-start workspace "crossed-thread")
              (setq worker
                    (make-thread
                     (lambda ()
                       ;; This is the native watcher's thread-local accounting
                       ;; context; coordinate actual helpers, not fake clocks.
                       (let ((mevedel-telemetry--lag-observer-seconds 0))
                         (unwind-protect
                             (condition-case err
                                 (progn
                                   (mevedel-telemetry--lag-observer-call
                                    (lambda ()
                                      (setq entered t)
                                      (while (not release) (thread-yield))))
                                   (setq worker-cost mevedel-telemetry--lag-observer-seconds))
                               (error (setq failure err)))
                           (setq done t))))
                     "observer-ownership-control"))
              (await (lambda () entered))
              (should (= 1 mevedel-telemetry--lag-observer-count))
              (if (eq scenario 'crossed)
                  (progn
                    (mevedel-telemetry--lag-observer-call
                     (lambda ()
                       (should (= 2 mevedel-telemetry--lag-observer-count))
                       (finish-worker)
                       (should (= 1 mevedel-telemetry--lag-observer-count))
                       (should mevedel-telemetry--lag-observer-start)))
                    (mevedel-telemetry--lag-tick)
                    (should-not (file-exists-p (file-name-concat root ".mevedel")))
                    (ordinary-work))
                (when (eq scenario 'restart)
                  (mevedel-telemetry--lag-stop)
                  (should (= 1 mevedel-telemetry--lag-observer-count))
                  (mevedel-telemetry-memory-pass-start workspace "restarted-thread"))
                (ordinary-work)
                (should (= before mevedel-telemetry--lag-observer-seconds)))
              (should (= 0 mevedel-telemetry--lag-observer-count))
              (should-not mevedel-telemetry--lag-observer-start)
              (should (>= mevedel-telemetry--lag-late .5))
              (mevedel-telemetry--lag-tick)
              (should (= 1 (length (test-mevedel-telemetry-lag-observer--rows root)))))
          (setq release t)
          (when (and (threadp worker) (thread-live-p worker))
            (thread-signal worker 'error '("Observer control cleanup")))
          (when (threadp worker) (ignore-errors (thread-join worker)))
          (mevedel-telemetry--lag-stop)
          (delete-directory root t))))))

(mevedel-deftest mevedel-telemetry--lag-observer-call/restart-clock
  nil
  ,test
  (test)
  :doc "observer completion excludes only overlap with a restarted baseline and preserves ordinary timer cost"
  (dolist (baseline '(90.0 110.0))
    (let ((mevedel-telemetry--lag-timer t)
          (mevedel-telemetry--lag-thread nil)
          (mevedel-telemetry--lag-since 90.0)
          (mevedel-telemetry--lag-due 100.1)
          (mevedel-telemetry--lag-observer-count 0)
          (mevedel-telemetry--lag-observer-start nil)
          (mevedel-telemetry--lag-observer-due nil)
          (mevedel-telemetry--lag-observer-seconds 0)
          (mevedel-telemetry--lag-late 0)
          (mevedel-telemetry--lag-slowest nil)
          (clock (symbol-function 'float-time))
          (now 100.0)
          (timer (timer-create)))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional time) (if time (funcall clock time) now))))
        (mevedel-telemetry--lag-observer-call
         (lambda ()
           (setq mevedel-telemetry--lag-since baseline)
           (when (= baseline 110.0) (setq mevedel-telemetry--lag-due 110.1))
           (setq now 112.0)))
        (should (= mevedel-telemetry--lag-since (if (= baseline 110.0) 112.0 102.0)))
        (should (<= mevedel-telemetry--lag-since now))
        (should (= mevedel-telemetry--lag-due (+ 112.0 mevedel-telemetry--lag-interval)))
        (should (zerop mevedel-telemetry--lag-observer-count))
        (should-not mevedel-telemetry--lag-observer-start)
        (timer-set-time timer (seconds-to-time now))
        (timer-set-function timer #'ignore)
        (mevedel-telemetry--lag-time-callback (lambda (_timer) (setq now 112.6)) timer)
        (should (> mevedel-telemetry--lag-late .59))
        (should (> (plist-get mevedel-telemetry--lag-slowest :seconds) .59))))))

(provide 'test-mevedel-telemetry-lag-observer)
;;; test-mevedel-telemetry-lag-observer.el ends here
