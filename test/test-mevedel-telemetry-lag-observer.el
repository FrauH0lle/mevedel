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
  :doc "timer and sentinel reentry in observer IO exclude diagnostic appends but retain ordinary work"
  (dolist (omit-nested-accounting '(nil t))
  (dolist (work '(diagnostic application remote))
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
                           (if (eq work 'remote)
                               ;; The outer remote wait belongs to diagnostics;
                               ;; its sentinel reenters unrelated remote work
                               ;; without any timer wrapper to rescue timing.
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
              (should (= (if (or omit-nested-accounting (memq work '(application remote))) 1 0)
                         (length lags)))))
        (mevedel-telemetry--lag-stop)
        (delete-directory root t)
        (delete-directory other t))))))

(provide 'test-mevedel-telemetry-lag-observer)
;;; test-mevedel-telemetry-lag-observer.el ends here
