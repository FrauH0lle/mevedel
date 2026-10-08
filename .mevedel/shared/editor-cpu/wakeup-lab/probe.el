;;; probe.el --- Disposable wakeup attribution -*- lexical-binding: t -*-

;; Diagnostic only: dropping timers deliberately removes their behavior.
;; Alignment may move housekeeping deadlines by up to 250 ms.  Neither is a
;; production policy; counts and CPU distinguish computation from wakeups.
(require 'cl-lib)
(require 'timer)
(declare-function cpuh--name "cpuh-harness" (function))
(defvar wakeup-lab-case (getenv "MEVEDEL_LAB_CASE"))
(defvar wakeup-lab-changed (make-hash-table :test #'equal))

(defun wakeup-lab-group (timer)
  "Return the diagnostic group of TIMER, or nil."
  (let ((name (cpuh--name (timer--function timer))))
    (cond
     ((equal name "mevedel-telemetry--lag-tick") 'telemetry)
     ((equal name "mevedel--gc-maintain") 'gc)
     ((equal name "mevedel-execution--emit-progress") 'progress)
     ((equal name "closure:mevedel-execution-process--ended") 'watch)
     ((equal name "mevedel-session-collection--step") 'collection)
     ((equal name "mevedel-view--realign-markdown") 'layout)
     ((equal name "closure:mevedel--ui-timer-cancel") 'view))))

(defun wakeup-lab-activate (original timer &rest arguments)
  "Apply the selected disposable experiment to TIMER activation."
  (let* ((group (wakeup-lab-group timer))
         (drop (and group
                    (or (equal wakeup-lab-case (concat "no-" (symbol-name group)))
                        (and (equal wakeup-lab-case "no-housekeeping")
                             (memq group '(telemetry gc watch collection)))))))
    (cond
     (drop (puthash group (1+ (gethash group wakeup-lab-changed 0))
                    wakeup-lab-changed))
     (t
      (when (and group (string-prefix-p "align" wakeup-lab-case)
                 (not (timer--idle-delay timer)))
        (let* ((now (float-time))
               (due (float-time (timer--time timer)))
               (delay (- due now)))
          (when (and (> delay 0.35) (< delay 1.1))
            (timer-set-time timer (seconds-to-time (/ (round (* 2 due)) 2.0))
                            (timer--repeat-delay timer))
            (puthash group (1+ (gethash group wakeup-lab-changed 0))
                     wakeup-lab-changed))))
      (apply original timer arguments)))))

(advice-add 'timer-activate :around #'wakeup-lab-activate)
(advice-add 'timer-activate-when-idle :around #'wakeup-lab-activate)
(defun request-lab-diagnostic-result ()
  "Return bounded counts of modified timer activations."
  (let (result)
    (maphash (lambda (group count) (push (cons group count) result)) wakeup-lab-changed)
    result))

;; Bounded timestamps in a separate trace run, never file I/O in callbacks.
(defvar wakeup-lab-trace nil)
(defvar wakeup-lab-tracing nil)
(defun wakeup-lab-note (name)
  (when (and wakeup-lab-tracing (< (length wakeup-lab-trace) 500))
    (push (list (float-time) name) wakeup-lab-trace)))
(defun wakeup-lab-trace-start (&rest _)
  (setq wakeup-lab-trace nil wakeup-lab-tracing t))
(defun wakeup-lab-trace-stop (&rest _)
  (setq wakeup-lab-tracing nil)
  (with-temp-file (getenv "MEVEDEL_LAB_TIMELINE")
    (dolist (row (reverse wakeup-lab-trace)) (prin1 row (current-buffer)) (terpri (current-buffer)))))
(defun wakeup-lab-trace-timer (timer)
  (wakeup-lab-note (cpuh--name (timer--function timer))))
(defun wakeup-lab-trace-display (&rest _) (wakeup-lab-note "redisplay"))
(when (string-suffix-p "trace" wakeup-lab-case)
  (advice-add 'cpuh-start :after #'wakeup-lab-trace-start)
  (advice-add 'cpuh-stop :after #'wakeup-lab-trace-stop)
  (advice-add 'timer-event-handler :before #'wakeup-lab-trace-timer)
  (add-hook 'pre-redisplay-functions #'wakeup-lab-trace-display))
(provide 'wakeup-lab-probe)

;; A disposable shared-dispatch experiment.  The owning timer APIs are
;; intercepted here only to measure the design before changing production.
(defvar wakeup-lab-held (make-hash-table :test #'eq))
(defvar wakeup-lab-clock nil)
(defun wakeup-lab-dispatch ()
  (let ((now (current-time)) due)
    (maphash (lambda (timer _) (when (time-less-p (timer--time timer) now)
                                (push timer due))) wakeup-lab-held)
    (dolist (timer due)
      (when (gethash timer wakeup-lab-held)
        (remhash timer wakeup-lab-held)
        ;; Let Emacs preserve error isolation, repeat and buffer semantics.
        (push timer timer-list)
        (timer-event-handler timer)))))
(defun wakeup-lab-coalesce (original timer &rest args)
  (let ((group (wakeup-lab-group timer)))
    (if (and (not (timer--idle-delay timer))
             (memq group (if (equal wakeup-lab-case "coalesce-housekeeping")
                             '(telemetry gc watch) '(telemetry gc watch progress view)))
             (> (- (float-time (timer--time timer)) (float-time)) 0.35))
        (progn
          (timer-set-time timer
                          (seconds-to-time (/ (round (* 2 (float-time (timer--time timer)))) 2.0))
                          (timer--repeat-delay timer))
          (puthash timer t wakeup-lab-held)
          (puthash group (1+ (gethash group wakeup-lab-changed 0)) wakeup-lab-changed)
          (unless wakeup-lab-clock
            (setq wakeup-lab-clock (run-at-time t 0.5 #'wakeup-lab-dispatch))))
      (apply original timer args))))
(defun wakeup-lab-cancel (timer &rest _) (remhash timer wakeup-lab-held))
(defun wakeup-lab-pending (original timer)
  (or (gethash timer wakeup-lab-held) (funcall original timer)))
(when (string-prefix-p "coalesce" wakeup-lab-case)
  (advice-add 'timer-activate :around #'wakeup-lab-coalesce)
  (advice-add 'cancel-timer :before #'wakeup-lab-cancel)
  (advice-add 'mevedel--ui-timer-cancel :before #'wakeup-lab-cancel)
  (advice-add 'mevedel--ui-timer-pending-p :around #'wakeup-lab-pending))

;; Attribute the remaining missing-row recovery render without changing the
;; progress event, cache, native surfaces or elapsed-time refresh.
(when (equal wakeup-lab-case "no-recovery")
  (advice-add 'mevedel-view-stream--schedule-execution-row-recovery :override #'ignore))
